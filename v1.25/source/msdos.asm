; 86-dos  high-performance operating system for the 8086  version 1.25
;       by tim paterson


; ****************** revision history *************************
;          >> every change must noted below!! <<
;
; 0.34 12/29/80 general release, updating all past customers
; 0.42 02/25/81 32-byte directory entries added
; 0.56 03/23/81 variable record and sector sizes
; 0.60 03/27/81 ctrl-c exit changes, including register save on user stack
; 0.74 04/15/81 recognize i/o devices with file names
; 0.75 04/17/81 improve and correct buffer handling
; 0.76 04/23/81 correct directory size when not 2^n entries
; 0.80 04/27/81 add console input without echo, functions 7 & 8
; 1.00 04/28/81 renumber for general release
; 1.01 05/12/81 fix bug in `store'
; 1.10 07/21/81 fatal error trapping, nul device, hidden files, date & time,
;               rename fix, general cleanup
; 1.11 09/03/81 don't set current block to 0 on open; fix set file size
; 1.12 10/09/81 zero high half of current block after all (cp/m programs don't)
; 1.13 10/29/81 fix classic "no write-through" error in buffer handling
; 1.20 12/31/81 add time to fcb; separate fat from dpt; kill smalldir;
;               add flush and mapdev calls; allow disk mapping in dskchg;
;               lots of smaller improvements
; 1.21 01/06/82 highmem switch to run dos in high memory
; 1.22 01/12/82 add verify system call to enable/disable verify after write
; 1.23 02/11/82 add defaulting to parser; use variable escape character
;               don't zero extent field in ibm version (back to 1.01!)
; 1.24 03/01/82 restore fcn. 27 to 1.0 level; add fcn. 28
; 1.25 03/03/82 put marker (00) at end of directory to speed searches
;
; *************************************************************


; interrupt entry points:

; intbase:      abort
; intbase+4:    command
; intbase+8:    base exit address
; intbase+c:    control-c abort
; intbase+10h:  fatal error abort
; intbase+14h:  bios disk read
; intbase+18h:  bios disk write
; intbase+40h:  long jump to call entry point

        if      ibm
escch   equ     0
cancel  equ     1bh             ;cancel with esc
toglins equ     true            ;one key toggles insert mode
toglprn equ     true            ;one key toggles printer echo
numdev  equ     6               ;include "com1" as i/o device name
zeroext equ     true
        else
escch   equ     1bh
cancel  equ     "x"-"@"         ;cancel with ctrl-x
toglins equ     false           ;separate keys for insert mode on and off
toglprn equ     false           ;separate keys for printer echo on and off
numdev  equ     5               ;number of i/o device names
zeroext equ     false
        endif

maxcall equ     36
maxcom  equ     46
intbase equ     80h
inttab  equ     20h
entrypointseg   equ     0ch
entrypoint      equ     intbase+40h
contc   equ     inttab+3
exit    equ     intbase+8
longjump equ    0eah
longcall equ    9ah
maxdif  equ     0fffh
savexit equ     10

; field definition for fcbs

fcblock struc
        db      12 dup (?)              ;drive code and name
extent  dw      ?
recsiz  dw      ?       ;size of record (user settable)
filsiz  dw      ?       ;size of file in bytes
drvbp   dw      ?       ;bp for search first and search next
fdate   dw      ?       ;date of last writing
ftime   dw      ?       ;time of last writing
devid   db      ?       ;device id number, bits 0-5
                        ;bit 7=0 for file, bit 7=1 for i/o device
                        ;if file, bit 6=0 if dirty
                        ;if i/o device, bit 6=0 if eof (input)
firclus dw      ?       ;first cluster of file
lstclus dw      ?       ;last cluster accessed
cluspos dw      ?       ;position of last cluster accessed
        db      ?       ;forces nr to offset 32
nr      db      ?       ;next record
rr      db      3 dup (?)               ;random record
fcblock ends
fildirent       = filsiz                ;used only by search first and search next

; description of 32-byte directory entry (same as returned by search first
; and search next, functions 17 and 18).
;
; location      bytes   description
;
;    0          11      file name and extension ( 0e5h if empty)
;   11           1      attributes. bits 1 or 2 make file hidden
;   12          10      zero field (for expansion)
;   22           2      time. bits 0-4=seconds/2, bits 5-10=minute, 11-15=hour
;   24           2      date. bits 0-4=day, bits 5-8=month, bits 9-15=year-1980
;   26           2      first allocation unit ( < 4080 )
;   28           4      file size, in bytes (lsb first, 30 bits max.)
;
; the file allocation table uses a 12-bit entry for each allocation unit on
; the disk. these entries are packed, two for every three bytes. the contents
; of entry number n is found by 1) multiplying n by 1.5; 2) adding the result
; to the base address of the allocation table; 3) fetching the 16-bit word at
; this address; 4) if n was odd (so that n*1.5 was not an integer), shift the
; word right four bits; 5) mask to 12 bits (and with 0fff hex). entry number
; zero is used as an end-of-file trap in the os and as a flag for directory
; entry size (if smalldir selected). entry 1 is reserved for future use. the
; first available allocation unit is assigned entry number two, and even
; though it is the first, is called cluster 2. entries greater than 0ff8h are
; end of file marks; entries of zero are unallocated. otherwise, the contents
; of a fat entry is the number of the next cluster in the file.


; field definition for drive parameter block

dpblock struc
devnum  db      ?       ;i/o driver number
drvnum  db      ?       ;physical unit number
secsiz  dw      ?       ;size of physical sector in bytes
clusmsk db      ?       ;sectors/cluster - 1
clusshft db     ?       ;log2 of sectors/cluster
firfat  dw      ?       ;starting record of fats
fatcnt  db      ?       ;number of fats for this drive
maxent  dw      ?       ;number of directory entries
firrec  dw      ?       ;first sector of first cluster
maxclus dw      ?       ;number of clusters on drive + 1
fatsiz  db      ?       ;number of records occupied by fat
firdir  dw      ?       ;starting record of directory
fat     dw      ?       ;pointer to start of fat
dpblock ends

dpbsiz  equ     20      ;size of the structure in bytes
dirsec  =       firrec  ;number of dir. sectors (init temporary)
dsksiz  =       maxclus ;size of disk (temp used during init only)

;the following are all of the segments used
;they are declared in the order that they should be placed in the executable

code    segment
code    ends

constants       segment byte
constants       ends

data    segment word
data    ends

dosgroup        group   code,constants,data

segbios segment
segbios ends


; bois entry point definitions

        if      ibm
biosseg equ     60h
        endif
        if      not ibm
biosseg equ     40h
        endif

segbios         segment at biosseg
                org     0
                db      3 dup (?)       ;reserve room for jump to init code
biosstat        db      3 dup (?)       ;console input status check
biosin          db      3 dup (?)       ;get console character
biosout         db      3 dup (?)       ;output console character
biosprint       db      3 dup (?)       ;output to printer
biosauxin       db      3 dup (?)       ;get byte from auxilliary
biosauxout      db      3 dup (?)       ;output byte to auxilliary
biosread        db      3 dup (?)       ;disk read
bioswrite       db      3 dup (?)       ;disk write
biosdskchg      db      3 dup (?)       ;dsik-change status
biossetdate     db      3 dup (?)       ;set date
biossettime     db      3 dup (?)       ;set time
biosgettime     db      3 dup (?)       ;get time and date
biosflush       db      3 dup (?)       ;clear console input buffer
biosmapdev      db      3 dup (?)       ;dynamic disk table mapper

segbios ends
; location of user registers relative user stack pointer

stkptrs struc
axsave  dw      ?
bxsave  dw      ?
cxsave  dw      ?
dxsave  dw      ?
sisave  dw      ?
disave  dw      ?
bpsave  dw      ?
dssave  dw      ?
essave  dw      ?
ipsave  dw      ?
cssave  dw      ?
fsave   dw      ?
stkptrs ends

; start of code

code    segment
assume  cs:dosgroup,ds:dosgroup,es:dosgroup,ss:dosgroup

        org     0
codstrt equ     $
        jmp     dosinit

escchar db      escch   ;lead-in character for escape sequences
esctab: 
        if      not ibm
        db      "s"     ;copy one char
        db      "v"     ;skip one char
        db      "t"     ;copy to char
        db      "w"     ;skip to char
        db      "u"     ;copy line
        db      "e"     ;kill line (no change in template)
        db      "j"     ;reedit line (new template)
        db      "d"     ;backspace
        db      "p"     ;enter insert mode
        db      "q"     ;exit insert mode
        db      "r"     ;escape character
        db      "r"     ;end of table
        endif
        if      ibm
        db      64      ;crtl-z - f6
        db      77      ;copy one char - -->
        db      59      ;copy one char - f1
        db      83      ;skip one char - del
        db      60      ;copy to char - f2
        db      62      ;skip to char - f4
        db      61      ;copy line - f3
        db      61      ;kill line (no change to template ) - not used
        db      63      ;reedit line (new template) - f5
        db      75      ;backspace - <--
        db      82      ;enter insert mode - ins (toggle)
        db      65      ;escape character - f7
        db      65      ;end of table
        endif

esctablen equ   $-esctab
        if      not ibm
header  db      13,10,"ms-dos version 1.25"
        if      highmem
        db      "h"
        endif
        if      dsktest
        db      "d"
        endif

        db      13,10
        db      "copyright 1981,82 microsoft, inc.",13,10,"$"
        endif

quit:
        mov     ah,0
        jmp     short savregs

command: ;interrupt call entry point
        cmp     ah,maxcom
        jbe     savregs
badcall:
        mov     al,0
iret:   iret

entry:  ;system call entry point and dispatcher
        pop     ax              ;ip from the long call at 5
        pop     ax              ;segment from the long call at 5
        pop     cs:[temp]       ;ip from the call 5
        pushf                   ;start re-ordering the stack
        cli
        push    ax              ;save segment
        push    cs:[temp]       ;stack now ordered as if int had been used
        cmp     cl,maxcall      ;this entry point doesn't get as many calls
        ja      badcall
        mov     ah,cl
savregs:
        push    es
        push    ds
        push    bp
        push    di
        push    si
        push    dx
        push    cx
        push    bx
        push    ax

        if      dsktest
        mov     ax,cs:[spsave]
        mov     cs:[nsp],ax
        mov     ax,cs:[sssave]
        mov     cs:[nss],ax
        pop     ax
        push    ax
        endif

        mov     cs:[spsave],sp
        mov     cs:[sssave],ss
        mov     sp,cs
        mov     ss,sp
redisp:
        mov     sp,offset dosgroup:iostack
        sti                     ;stack ok now
        mov     bl,ah
        mov     bh,0
        shl     bx,1
        cld
        cmp     ah,12
        jle     samstk
        mov     sp,offset dosgroup:dskstack
samstk:
        call    cs:[bx+dispatch]
leave:
        cli
        mov     sp,cs:[spsave]
        mov     ss,cs:[sssave]
        mov     bp,sp
        mov     byte ptr [bp.axsave],al

        if      dsktest
        mov     ax,cs:[nsp]
        mov     cs:[spsave],ax
        mov     ax,cs:[nss]
        mov     cs:[sssave],ax
        endif

        pop     ax
        pop     bx
        pop     cx
        pop     dx
        pop     si
        pop     di
        pop     bp
        pop     ds
        pop     es
        iret
; standard functions
dispatch dw     abort           ;0
        dw      conin
        dw      conout
        dw      reader
        dw      punch
        dw      list            ;5
        dw      rawio
        dw      rawinp
        dw      in
        dw      prtbuf
        dw      bufin           ;10
        dw      constat
        dw      flushkb
        dw      dskreset
        dw      seldsk
        dw      open            ;15
        dw      close
        dw      srchfrst
        dw      srchnxt
        dw      delete
        dw      seqrd           ;20
        dw      seqwrt
        dw      create
        dw      rename
        dw      inuse
        dw      getdrv          ;25
        dw      setdma
        dw      getfatpt
        dw      getfatptdl
        dw      getrdonly
        dw      setattrib       ;30
        dw      getdskpt
        dw      usercode
        dw      rndrd
        dw      rndwrt
        dw      filesize        ;35
        dw      setrndrec
; extended functions
        dw      setvect
        dw      newbase
        dw      blkrd
        dw      blkwrt          ;40
        dw      makefcb
        dw      getdate
        dw      setdate
        dw      gettime
        dw      settime         ;45
        dw      verify

inuse:
getio:
setio:
getrdonly:
setattrib:
usercode:
        mov     al,0
        ret

verify:
        and     al,1
        mov     cs:verflg,al
        ret

flushkb:
        push    ax
        call    far ptr biosflush
        pop     ax
        mov     ah,al
        cmp     al,1
        jz      redispj
        cmp     al,6
        jz      redispj
        cmp     al,7
        jz      redispj
        cmp     al,8
        jz      redispj
        cmp     al,10
        jz      redispj
        mov     al,0
        ret

redispj:jmp     redisp

reader:
auxin:
        call    statchk
        call    far ptr biosauxin 
        ret

punch:
        mov     al,dl
auxout:
        push    ax
        call    statchk
        pop     ax
        call    far ptr biosauxout 
        ret


unpack:

; inputs:
;       ds = cs
;       bx = cluster number
;       bp = base of drive parameters
;       si = pointer to drive fat
; outputs:
;       di = contents of fat for given cluster
;       zero set means di=0 (free cluster)
; no other registers affected. fatal error if cluster too big.

        cmp     bx,[bp.maxclus]
        ja      hurtfat
        lea     di,[si+bx]
        shr     bx,1
        mov     di,[di+bx]
        jnc     havclus
        shr     di,1
        shr     di,1
        shr     di,1
        shr     di,1
        stc
havclus:
        rcl     bx,1
        and     di,0fffh
        ret
hurtfat:
        push    ax
        mov     ah,80h          ;signal bad fat to int 24h handler
        mov     di,0fffh        ;in case int 24h returns (it shouldn't)
        call    fatal
        pop     ax              ;try to ignore bad fat
        ret


pack:

; inputs:
;       ds = cs
;       bx = cluster number
;       dx = data
;       si = pointer to drive fat
; outputs:
;       the data is stored in the fat at the given cluster.
;       bx,dx,di all destroyed
;       no other registers affected

        mov     di,bx
        shr     bx,1
        add     bx,si
        add     bx,di
        shr     di,1
        mov     di,[bx]
        jnc     aligned
        shl     dx,1
        shl     dx,1
        shl     dx,1
        shl     dx,1
        and     di,0fh
        jmp     short packin
aligned:
        and     di,0f000h
packin:
        or      di,dx
        mov     [bx],di
        ret

devname:
        mov     si,offset dosgroup:ioname       ;list of i/o devices with file names
        mov     bh,numdev               ;bh = number of device names
lookio:
        mov     di,offset dosgroup:name1
        mov     cx,4                    ;all devices are 4 letters
        repe    cmpsb                   ;check for name in list
        jz      iochk                   ;if first 3 letters ok, check for the rest
        add     si,cx                   ;point to next device name
        dec     bh
        jnz     lookio
cret:
        stc                             ;not found
        ret

iochk:
        if      ibm
        cmp     bh,numdev       ;is it the first device?
        jnz     notcom1
        mov     bh,2            ;make it the same as aux
notcom1:
        endif
        neg     bh
        mov     cx,2            ;check rest of name but not extension
        mov     ax,2020h
        repe    scasw           ;make sure rest of name is blanks
        jnz     cret
ret1:   ret                     ;zero set so create works

getfile:
; same as getname except es:di points to fcb on successful return
        call    movname
        jc      ret1
        push    dx
        push    ds
        call    findname
        pop     es
        pop     di
ret2:   ret


getname:

; inputs:
;       ds,dx point to fcb
; function:
;       find file name in disk directory. first byte is
;       drive number (0=current disk). "?" matches any
;       character.
; outputs:
;       carry set if file not found
;       else
;       zero set if attributes match (always except when creating)
;       bp = base of drive parameters
;       ds = cs
;       es = cs
;       bx = pointer into directory buffer
;       si = pointer to first cluster field in directory entry
;       [dirbuf] has directory record with match
;       [name1] has file name
; all other registers destroyed.

        call    movname
        jc      ret2            ;bad file name?
findname:
        mov     ax,cs
        mov     ds,ax
        call    devname
        jnc     ret2
        call    startsrch
contsrch:
        call    getentry
        jc      ret2
srch:
        mov     ah,byte ptr [bx]
        or      ah,ah                   ;end of directory?
        jz      free
        cmp     ah,[delall]             ;free entry?
        jz      free
        mov     si,bx
        mov     di,offset dosgroup:name1
        mov     cx,11
wildcrd:
        repe    cmpsb
        jz      found
        cmp     byte ptr [di-1],"?"
        jz      wildcrd
nextent:
        call    nextentry
        jnc     srch
ret3:   ret

free:
        cmp     [entfree],-1            ;found a free entry before?
        jnz     tstall                  ;if so, ignore this one
        mov     cx,[lastent]
        mov     [entfree],cx
tstall:
        cmp     ah,[delall]             ;at end of directory?
        jz      nextent                 ;no - continue search
        stc                             ;report not found
        ret
 
found:
;check if attributes allow finding it
        mov     ah,[attrib]             ;attributes of search
        not     ah
        and     ah,[si]                 ;compare with attributes of file
        add     si,15   
        and     ah,6                    ;only look at bits 1 and 2
        jz      ret3
        test    byte ptr [creating],-1  ;pass back mismatch if creating
        jz      nextent                 ;otherwise continue searching
        ret


getentry:

; inputs:
;       [lastent] has previously searched directory entry
; function:
;       locates next sequential directory entry in preparation for search
; outputs:
;       carry set if none
;       else
;       al = current directory block
;       bx = pointer to next directory entry in [dirbuf]
;       dx = pointer to first byte after end of dirbuf
;       [lastent] = new directory entry number

        mov     ax,[lastent]
        inc     ax                      ;start with next entry
        cmp     ax,[bp.maxent]
        jae     none
getent:
        mov     [lastent],ax
        mov     cl,4
        shl     ax,cl
        xor     dx,dx
        shl     ax,1
        rcl     dx,1                    ;account for overflow in last shift
        mov     bx,[bp.secsiz]
        and     bl,255-31               ;must be multiple of 32
        div     bx
        mov     bx,dx                   ;position within sector
        mov     ah,[bp.devnum]          ;al=directory sector no.
        cmp     ax,[dirbufid]
        jz      havdirbuf
        push    bx
        call    dirread
        pop     bx
havdirbuf:
        mov     dx,offset dosgroup:dirbuf
        add     bx,dx
        add     dx,[bp.secsiz]
        ret

nextentry:

; inputs:
;       same as outputs of getentry, above
; function:
;       update al, bx, and [lastent] for next directory entry.
;       carry set if no more.

        mov     di,[lastent]
        inc     di
        cmp     di,[bp.maxent]
        jae     none
        mov     [lastent],di
        add     bx,32
        cmp     bx,dx
        jb      havit
        inc     al                      ;next directory sector
        push    dx                      ;save limit
        call    dirread
        pop     dx
        mov     bx,offset dosgroup:dirbuf
havit:
        clc
        ret

none:
        call    chkdirwrite
        stc
ret4:   ret


delete: ; system call 19
        call    movname
        mov     al,-1
        jc      ret4
        mov     al,cs:[attrib]
        and     al,6                    ;look only at hidden bits
        cmp     al,6                    ;both must be set
        jnz     notall
        mov     cx,11
        mov     al,"?"
        mov     di,offset dosgroup:name1
        repe    scasb                   ;see if name is *.*
        jnz     notall
        mov     byte ptr cs:[delall],0  ;del *.* - flag deleting all
notall:
        call    findname
        mov     al,-1
        jc      ret4
        or      bh,bh           ;check if device name
        js      ret4            ;can't delete i/o devices
delfile:
        mov     byte ptr [dirtydir],-1
        mov     ah,[delall]
        mov     byte ptr [bx],ah
        mov     bx,[si]
        mov     si,[bp.fat]
        or      bx,bx
        jz      delnxt
        cmp     bx,[bp.maxclus]
        ja      delnxt
        call    release
delnxt:
        call    contsrch
        jnc     delfile
        call    fatwrt
        call    chkdirwrite
        xor     al,al
        ret


rename: ;system call 23
        call    movname
        jc      erret
        add     si,5
        mov     di,offset dosgroup:name2
        call    lodname
        jc      erret           ;report error if second name invalid
        call    findname
        jc      erret
        or      bh,bh           ;check if i/o device name
        js      erret           ;if so, can't rename it
        mov     si,offset dosgroup:name1
        mov     di,offset dosgroup:name3
        mov     cx,6            ;6 words (12 bytes)--include attribute byte
        rep     movsw           ;copy name to search for
renfil:
        mov     di,offset dosgroup:name1
        mov     si,offset dosgroup:name2
        mov     cx,11
newnam:
        lodsb
        cmp     al,"?"
        jnz     nochg
        mov     al,[bx]
nochg:
        stosb
        inc     bx
        loop    newnam
        mov     byte ptr [di],6 ;stop duplicates with any attributes
        call    devname         ;check if giving it a device name
        jnc     renerr
        push    [lastent]       ;save position of match
        mov     [lastent],-1    ;search entire directory for duplicate
        call    contsrch        ;see if new name already exists
        pop     ax
        jnc     renerr                  ;error if found
        call    getent                  ;re-read matching entry
        mov     di,bx
        mov     si,offset dosgroup:name1
        mov     cx,5
        movsb
        rep     movsw                   ;replace old name with new one
        mov     byte ptr [dirtydir],-1  ;flag change in directory
        mov     si,offset dosgroup:name3
        mov     di,offset dosgroup:name1
        mov     cx,6                    ;include attribute byte
        rep     movsw                   ;copy name back into search buffer
        call    contsrch
        jnc     renfil
        call    chkdirwrite
        xor     al,al
        ret

renerr:
        call    chkdirwrite
erret:
        mov     al,-1
ret5:   ret


movname:

; inputs:
;       ds, dx point to fcb or extended fcb
; outputs:
;       ds:dx point to normal fcb
;       es = cs
;       if file name ok:
;       bp has base of driver parameters
;       [name1] has name in upper case
; all registers except dx destroyed
; carry set if bad file name or drive

        mov     cs:word ptr [creating],0e500h   ;not creating, not del *.*
        mov     ax,cs
        mov     es,ax
        mov     di,offset dosgroup:name1
        mov     si,dx
        lodsb
        mov     cs:[extfcb],al  ;set flag if extended fcb in use
        mov     ah,0            ;set default attributes
        cmp     al,-1           ;is it an extended fcb?
        jnz     havattrb
        add     dx,7            ;adjust to point to normal fcb
        add     si,6            ;point to drive select byte
        mov     ah,[si-1]       ;get attribute byte
        lodsb           ;get drive select byte
havattrb:
        mov     cs:[attrib],ah  ;save attributes
        call    getthisdrv
lodname:
; this entry point copies a file name from ds,si
; to es,di converting to upper case.
        cmp     byte ptr [si]," "       ;don't allow blank as first letter
        stc                     ;in case of error
        jz      ret5
        mov     cx,11
movchk:
        call    getlet
        jb      ret5
        jnz     stolet          ;is it a delimiter?
        cmp     al," "          ;this is the only delimiter allowed
        stc                     ;in case of error
        jnz     ret5
stolet:
        stosb
        loop    movchk
        clc                     ;got through whole name - no error
ret6:   ret

getthisdrv:
        cmp     cs:[numdrv],al
        jc      ret6
        dec     al
        jns     phydrv
        mov     al,cs:[curdrv]
phydrv:
        mov     cs:[thisdrv],al
        ret
        

open:   ;system call 15
        call    getfile
doopen:
; enter here to perform open on file already found
; in directory. ds=cs, bx points to directory
; entry in dirbuf, si points to first cluster field, and
; es:di point to the fcb to be opened. this entry point
; is used by create.
        jc      erret
        or      bh,bh           ;check if file is i/o device
        js      opendev         ;special handler if so
        mov     al,[thisdrv]
        inc     ax
        stosb
        xor     ax,ax
        if      zeroext
        add     di,11
        stosw                   ;zero low byte of extent field if ibm only
        endif
        if      not zeroext
        add     di,12           ;point to high half of current block field
        stosb                   ;set it to zero (cp/m programs set low byte)
        endif
        mov     al,128          ;default record size
        stosw                   ;set record size
        lodsw                   ;get starting cluster
        mov     dx,ax           ;save it for the moment
        movsw                   ;transfer size to fcb
        movsw
        mov     ax,[si-8]       ;get date
        stosw                   ;save date in fcb
        mov     ax,[si-10]      ;get time
        stosw                   ;save it in fcb
        mov     al,[bp.devnum]
        or      al,40h
        stosb
        mov     ax,dx           ;restore starting cluster
        stosw                   ; first cluster
        stosw                   ; last cluster accessed
        xor     ax,ax
        stosw                   ; position of last cluster
        ret


opendev:
        add     di,13           ;point to 2nd half of extent field
        xor     ax,ax
        stosb                   ;set it to zero
        mov     al,128
        stosw                   ;set record size to 128
        xor     ax,ax
        stosw
        stosw                   ;set current size to zero
        call    date16
        stosw                   ;date is todays
        xchg    ax,dx
        stosw                   ;use current time
        mov     al,bh           ;get device number
        stosb
        xor     al,al           ;no error
        ret
faterr:
        xchg    ax,di           ;put error code in di
        mov     ah,2            ;while trying to read fat
        mov     al,[thisdrv]    ;tell which drive
        call    fatal1
        jmp     short fatread
startsrch:
        mov     ax,-1
        mov     [lastent],ax
        mov     [entfree],ax
fatread:

; inputs:
;       ds = cs
; function:
;       if disk may have been changed, fat is read in and buffers are
;       flagged invalid. if not, no action is taken.
; outputs:
;       bp = base of drive parameters
;       carry set if invalid drive returned by mapdev
; all other registers destroyed

        mov     al,[thisdrv]
        xor     ah,ah           ;set default response to zero & clear carry
        call    far ptr biosdskchg      ;see what bios has to say
        jc      faterr
        call    getbp
        mov     al,[thisdrv]    ;use physical unit number
        mov     si,[bp.fat]
        or      ah,[si-1]       ;dirty byte for fat
        js      newdsk          ;if either say new disk, then it's so
        jnz     mapdrv
        mov     ah,1
        cmp     ax,word ptr [bufdrvno]  ;does buffer have dirty sector of this drive?
        jz      mapdrv
newdsk:
        cmp     al,[bufdrvno]   ;see if buffer is for this drive
        jnz     bufok           ;if not, don't touch it
        mov     [bufsecno],0    ;flag buffers invalid
        mov     word ptr [bufdrvno],00ffh
bufok:
        mov     [dirbufid],-1
        call    figfat
nextfat:
        push    ax
        call    dskread
        pop     ax
        jc      badfat
        sub     al,[bp.fatcnt]
        jz      newfat
        call    fatwrt
newfat:
        mov     si,[bp.fat]
        mov     al,[bp.devnum]
        mov     ah,[si]         ;get first byte of fat
        or      ah,0f8h         ;put in range
        call    far ptr biosmapdev
        mov     ah,0
        mov     [si-2],ax       ;set device no. and reset dirty bit
mapdrv:
        mov     al,[si-2]       ;get device number
getbp:
        mov     bp,[drvtab]     ;just in case drive isn't valid
        and     al,3fh          ;mask out dirty bit
        cmp     al,[numio]
        cmc
        jc      ret7
        push    ax
        mov     ah,dpbsiz
        mul     ah
        add     bp,ax
        pop     ax
ret7:   ret

badfat:
        mov     cx,di
        add     dx,cx
        dec     al
        jnz     nextfat
        call    figfat                          ;reset registers
        call    dread                           ;try first fat once more
        jmp     short newfat

okret1:
        mov     al,0
        ret

close:  ;system call 16
        mov     di,dx
        cmp     byte ptr [di],-1                ;check for extended fcb
        jnz     normfcb3
        add     di,7
normfcb3:
        test    byte ptr [di.devid],0c0h        ;allow only dirty files
        jnz     okret1                          ;can't close if i/o device, or not writen
        mov     al,[di]                         ;get physical unit number
        dec     al                              ;make zero = drive a
        mov     ah,1                            ;look for dirty buffer
        cmp     ax,cs:word ptr [bufdrvno]
        jnz     fnddir
;write back dirty buffer if on same drive
        push    dx
        push    ds
        push    cs
        pop     ds
        mov     byte ptr [dirtybuf],0
        mov     bx,[buffer]
        mov     cx,1
        mov     dx,[bufsecno]
        mov     bp,[bufdrvbp]
        call    dwrite
        pop     ds
        pop     dx
fnddir:
        call    getfile
badclosej:
        jc      badclose
        mov     cx,es:[di.firclus]
        mov     [si],cx
        mov     dx,es:word ptr [di.filsiz]
        mov     [si+2],dx
        mov     dx,es:word ptr [di.filsiz+2]
        mov     [si+4],dx
        mov     dx,es:[di.fdate]
        mov     [si-2],dx
        mov     dx,es:[di.ftime]
        mov     [si-4],dx
        call    dirwrite

chkfatwrt:
; do fatwrt only if fat is dirty and uses same i/o driver
        mov     si,[bp.fat]
        mov     al,[bp.devnum]
        mov     ah,1
        cmp     [si-2],ax       ;see if fat dirty and uses same driver
        jnz     okret

fatwrt:

; inputs:
;       ds = cs
;       bp = base of drive parameter table
; function:
;       write the fat back to disk and reset fat
;       dirty bit.
; outputs:
;       al = 0
;       bp unchanged
; all other registers destroyed

        call    figfat
        mov     byte ptr [bx-1],0
eachfat:
        push    dx
        push    cx
        push    bx
        push    ax
        call    dwrite
        pop     ax
        pop     bx
        pop     cx
        pop     dx
        add     dx,cx
        dec     al
        jnz     eachfat
okret:
        mov     al,0
        ret

badclose:
        mov     si,[bp.fat]
        mov     byte ptr [si-1],0
        mov     al,-1
        ret


figfat:
; loads registers with values needed to read or
; write a fat.
        mov     al,[bp.fatcnt]
        mov     bx,[bp.fat]
        mov     cl,[bp.fatsiz]  ;no. of records occupied by fat
        mov     ch,0
        mov     dx,[bp.firfat]  ;record number of start of fats
        ret


dircomp:
; prepare registers for directory read or write
        cbw
        add     ax,[bp.firdir]
        mov     dx,ax
        mov     bx,offset dosgroup:dirbuf
        mov     cx,1
        ret


create: ;system call 22
        call    movname
        jc      erret3
        mov     di,offset dosgroup:name1
        mov     cx,11
        mov     al,"?"
        repne   scasb
        jz      erret3
        mov     cs:byte ptr [creating],-1
        push    dx
        push    ds
        call    findname
        jnc     existent
        mov     ax,[entfree]    ;first free entry found in findname
        cmp     ax,-1
        jz      errpop
        call    getent          ;point at that free entry
        jmp     short freespot
errpop:
        pop     ds
        pop     dx
erret3:
        mov     al,-1
        ret

existent:
        jnz     errpop          ;error if attributes don't match
        or      bh,bh           ;check if file is i/o device
        js      openjmp         ;if so, no action
        mov     cx,[si]         ;get pointer to clusters
        jcxz    freespot
        cmp     cx,[bp.maxclus]
        ja      freespot
        push    bx
        mov     bx,cx
        mov     si,[bp.fat]
        call    release         ;free any data already allocated
        call    fatwrt
        pop     bx
freespot:
        mov     di,bx
        mov     si,offset dosgroup:name1
        mov     cx,5
        movsb
        rep     movsw
        mov     al,[attrib]
        stosb
        xor     ax,ax
        mov     cl,5
        rep     stosw
        call    date16
        xchg    ax,dx
        stosw
        xchg    ax,dx
        stosw
        xor     ax,ax
        push    di
        mov     cl,6
smallent:
        rep     stosb
        push    bx
        call    dirwrite
        pop     bx
        pop     si
openjmp:
        clc                     ;clear carry so open won't fail
        pop     es
        pop     di
        jmp     doopen


dirread:

; inputs:
;       ds = cs
;       al = directory block number
;       bp = base of drive parameters
; function:
;       read the directory block into dirbuf.
; outputs:
;       ax,bp unchanged
; all other registers destroyed.

        push    ax
        call    chkdirwrite
        pop     ax
        push    ax
        mov     ah,[bp.devnum]
        mov     [dirbufid],ax
        call    dircomp
        call    dread
        pop     ax
ret8:   ret


dread:

; inputs:
;       bx,ds = transfer address
;       cx = number of sectors
;       dx = absolute record number
;       bp = base of drive parameters
; function:
;       calls bios to perform disk read. if bios reports
;       errors, will call harderr for further action.
; bp preserved. all other registers destroyed.

        call    dskread
        jnc     ret8
        mov     cs:byte ptr [readop],0
        call    harderr
        cmp     al,1            ;check for retry
        jz      dread
        ret                     ;ignore otherwise


harderr:

;hard disk error handler. entry conditions:
;       ds:bx = original disk transfer address
;       dx = original logical sector number
;       cx = number of sectors to go (first one gave the error)
;       ax = hardware error code
;       di = original sector transfer count
;       bp = base of drive parameters
;       [readop] = 0 for read, 1 for write

        xchg    ax,di           ;error code in di, count in ax
        sub     ax,cx           ;number of sectors successfully transferred
        add     dx,ax           ;first sector number to retry
        push    dx
        mul     [bp.secsiz]     ;number of bytes transferred
        pop     dx
        add     bx,ax           ;first address for retry
        mov     ah,0            ;flag disk section in error
        cmp     dx,[bp.firfat]  ;in reserved area?
        jb      errint
        inc     ah              ;flag for fat
        cmp     dx,[bp.firdir]  ;in fat?
        jb      errint
        inc     ah
        cmp     dx,[bp.firrec]  ;in directory?
        jb      errint
        inc     ah              ;must be in data area
errint:
        shl     ah,1            ;make room for read/write bit
        or      ah,cs:[readop]
fatal:
        mov     al,[bp.drvnum]  ;get drive number
fatal1:
        push    bp              ;the only thing we preserve
        mov     cs:[contstk],sp
        cli                     ;prepare to play with stack
        mov     ss,cs:[sssave]
        mov     sp,cs:[spsave]  ;user stack pointer restored
        int     24h             ;fatal error interrupt vector
        mov     cs:[spsave],sp
        mov     cs:[sssave],ss
        mov     sp,cs
        mov     ss,sp
        mov     sp,cs:[contstk]
        sti
        pop     bp
        cmp     al,2
        jz      error
        ret

dskread:
        mov     al,[bp.devnum]
        push    bp
        push    bx
        push    cx
        push    dx
        call    far ptr biosread 
        pop     dx
        pop     di
        pop     bx
        pop     bp
ret9:   ret


chkdirwrite:
        test    byte ptr [dirtydir],-1
        jz      ret9

dirwrite:

; inputs:
;       ds = cs
;       al = directory block number
;       bp = base of drive parameters
; function:
;       write the directory block into dirbuf.
; outputs:
;       bp unchanged
; all other registers destroyed.

        mov     byte ptr [dirtydir],0
        mov     al,byte ptr [dirbufid]
        call    dircomp


dwrite:

; inputs:
;       bx,ds = transfer address
;       cx = number of sectors
;       dx = absolute record number
;       bp = base of drive parameters
; function:
;       calls bios to perform disk write. if bios reports
;       errors, will call harderr for further action.
; bp preserved. all other registers destroyed.

        mov     al,[bp.devnum]
        mov     ah,cs:verflg
        push    bp
        push    bx
        push    cx
        push    dx
        call    far ptr bioswrite 
        pop     dx
        pop     di
        pop     bx
        pop     bp
        jnc     ret9
        mov     cs:byte ptr [readop],1
        call    harderr
        cmp     al,1            ;check for retry
        jz      dwrite
        ret


abort:
        lds     si,cs:dword ptr [spsave]
        mov     ds,[si.cssave]
        xor     ax,ax
        mov     es,ax
        mov     si,savexit
        mov     di,exit
        movsw
        movsw
        movsw
        movsw
        movsw
        movsw
error:
        mov     ax,cs
        mov     ds,ax
        mov     es,ax
        call    wrtfats
        xor     ax,ax
        cli
        mov     ss,[sssave]
        mov     sp,[spsave]
        mov     ds,ax
        mov     si,exit
        mov     di,offset dosgroup:exithold
        movsw
        movsw
        pop     ax
        pop     bx
        pop     cx
        pop     dx
        pop     si
        pop     di
        pop     bp
        pop     ds
        pop     es
        sti             ;stack ok now
        jmp     cs:dword ptr [exithold]


seqrd:  ;system call 20
        call    getrec
        call    load
        jmp     short finseq

seqwrt: ;system call 21
        call    getrec
        call    store
finseq:
        jcxz    setnrex
        add     ax,1
        adc     dx,0
        jmp     short setnrex

rndrd:  ;system call 33
        call    getrrpos1
        call    load
        jmp     short finrnd

rndwrt: ;system call 34
        call    getrrpos1
        call    store
        jmp     short finrnd

blkrd:  ;system call 39
        call    getrrpos
        call    load
        jmp     short finblk

blkwrt: ;system call 40
        call    getrrpos
        call    store
finblk:
        lds     si,dword ptr [spsave]
        mov     [si.cxsave],cx
        jcxz    finrnd
        add     ax,1
        adc     dx,0
finrnd:
        mov     es:word ptr [di.rr],ax
        mov     es:[di.rr+2],dl
        or      dh,dh
        jz      setnrex
        mov     es:[di.rr+3],dh ;save 4 byte of recpos only if significant
setnrex:
        mov     cx,ax
        and     al,7fh
        mov     es:[di.nr],al
        and     cl,80h
        shl     cx,1
        rcl     dx,1
        mov     al,ch
        mov     ah,dl
        mov     es:[di.extent],ax
        mov     al,cs:[dskerr]
        ret

getrrpos1:
        mov     cx,1
getrrpos:
        mov     di,dx
        cmp     byte ptr [di],-1
        jnz     normfcb1
        add     di,7
normfcb1:
        mov     ax,word ptr [di.rr]
        mov     dx,word ptr [di.rr+2]
        ret

nofilerr:
        xor     cx,cx
        mov     byte ptr [dskerr],4
        pop     bx
        ret

setup:

; inputs:
;       ds:di point to fcb
;       dx:ax = record position in file of disk transfer
;       cx = record count
; outputs:
;       ds = cs
;       es:di point to fcb
;       bl = devid from fcb
;       cx = no. of bytes to transfer
;       bp = base of drive parameters
;       si = fat pointer
;       [reccnt] = record count
;       [recpos] = record position in file
;       [fcb] = di
;       [nextadd] = displacement of disk transfer within segment
;       [secpos] = position of first sector
;       [bytpos] = byte position in file
;       [bytsecpos] = byte position in first sector
;       [clusnum] = first cluster
;       [seccluspos] = sector within first cluster
;       [dskerr] = 0 (no errors yet)
;       [trans] = 0 (no transfers yet)
;       [thisdrv] = physical drive unit number
; if setup detects no records will be transfered, it returns 1 level up 
; with cx = 0.

        push    ax
        mov     al,[di]
        dec     al
        mov     cs:[thisdrv],al
        mov     al,[di.devid]
        mov     si,[di.recsiz]
        or      si,si
        jnz     havrecsiz
        mov     si,128
        mov     [di.recsiz],si
havrecsiz:
        push    ds
        pop     es              ;set es to ds
        push    cs
        pop     ds              ;set ds to cs
        or      al,al           ;is it a device?
        jns     notdevice
        mov     al,0            ;fake in drive 0 so we can get sp
notdevice:
        call    getbp
        pop     ax
        jc      nofilerr
        cmp     si,64           ;check if highest byte of recpos is significant
        jb      smalrec
        mov     dh,0            ;ignore msb if record >= 64 bytes
smalrec:
        mov     [reccnt],cx
        mov     word ptr [recpos],ax
        mov     word ptr [recpos+2],dx
        mov     [fcb],di
        mov     bx,[dmaadd]
        mov     [nextadd],bx
        mov     byte ptr [dskerr],0
        mov     byte ptr [trans],0
        mov     bx,dx
        mul     si
        mov     word ptr [bytpos],ax
        push    dx
        mov     ax,bx
        mul     si
        pop     bx
        add     ax,bx
        adc     dx,0            ;ripple carry
        jnz     eoferr
        mov     word ptr [bytpos+2],ax
        mov     dx,ax
        mov     ax,word ptr [bytpos]
        mov     bx,[bp.secsiz]
        cmp     dx,bx           ;see if divide will overflow
        jnc     eoferr
        div     bx
        mov     [secpos],ax
        mov     [bytsecpos],dx
        mov     dx,ax
        and     al,[bp.clusmsk]
        mov     [seccluspos],al
        mov     ax,cx           ;record count
        mov     cl,[bp.clusshft]
        shr     dx,cl
        mov     [clusnum],dx
        mul     si              ;multiply by bytes per record
        mov     cx,ax
        add     ax,[dmaadd]     ;see if it will fit in one segment
        adc     dx,0
        jz      ok              ;must be less than 64k
        mov     ax,[dmaadd]
        neg     ax              ;amount of room left in segment
        jnz     partseg         ;all 64k available?
        dec     ax              ;if so, reduce by one
partseg:
        xor     dx,dx
        div     si              ;how many records will fit?
        mov     [reccnt],ax
        mul     si              ;translate that back into bytes
        mov     byte ptr [dskerr],2     ;flag that trimming took place
        mov     cx,ax
        jcxz    noroom
ok:
        mov     bl,es:[di.devid]
        mov     si,[bp.fat]
        ret

eoferr:
        mov     byte ptr [dskerr],1
        xor     cx,cx
noroom:
        pop     bx              ;kill return address
        ret

breakdown:

;inputs:
;       ds = cs
;       cx = length of disk transfer in bytes
;       bp = base of drive parameters
;       [bytsecpos] = byte position witin first sector
;outputs:
;       [bytcnt1] = bytes to transfer in first sector
;       [seccnt] = no. of whole sectors to transfer
;       [bytcnt2] = bytes to transfer in last sector
;ax, bx, dx destroyed. no other registers affected.

        mov     ax,[bytsecpos]
        mov     bx,cx
        or      ax,ax
        jz      savfir          ;partial first sector?
        sub     ax,[bp.secsiz]
        neg     ax              ;max number of bytes left in first sector
        sub     bx,ax           ;subtract from total length
        jae     savfir
        add     ax,bx           ;don't use all of the rest of the sector
        xor     bx,bx           ;and no bytes are left
savfir:
        mov     [bytcnt1],ax
        mov     ax,bx
        xor     dx,dx
        div     [bp.secsiz]     ;how many whole sectors?
        mov     [seccnt],ax
        mov     [bytcnt2],dx    ;bytes remaining for last sector
ret10:  ret


fndclus:

; inputs:
;       ds = cs
;       cx = no. of clusters to skip
;       bp = base of drive parameters
;       si = fat pointer
;       es:di point to fcb
; outputs:
;       bx = last cluster skipped to
;       cx = no. of clusters remaining (0 unless eof)
;       dx = position of last cluster
; di destroyed. no other registers affected.

        mov     bx,es:[di.lstclus]
        mov     dx,es:[di.cluspos]
        or      bx,bx
        jz      noclus
        sub     cx,dx
        jnb     findit
        add     cx,dx
        xor     dx,dx
        mov     bx,es:[di.firclus]
findit:
        jcxz    ret10
skpclp:
        call    unpack
        cmp     di,0ff8h
        jae     ret10
        xchg    bx,di
        inc     dx
        loop    skpclp
        ret
noclus:
        inc     cx
        dec     dx
        ret


bufsec:
; inputs:
;       al = 0 if buffer must be read, 1 if no pre-read needed
;       bp = base of drive parameters
;       [clusnum] = physical cluster number
;       [seccluspos] = sector position of transfer within cluster
;       [bytcnt1] = size of transfer
; function:
;       insure specified sector is in buffer, flushing buffer before
;       read if necessary.
; outputs:
;       si = pointer to buffer
;       di = pointer to transfer address
;       cx = number of bytes
;       [nextadd] updated
;       [trans] set to indicate a transfer will occur

        mov     dx,[clusnum]
        mov     bl,[seccluspos]
        call    figrec
        mov     [preread],al
        cmp     dx,[bufsecno]
        jnz     getsec
        mov     al,[bufdrvno]
        cmp     al,[thisdrv]
        jz      finbuf          ;already have it?
getsec:
        xor     al,al
        xchg    [dirtybuf],al   ;read dirty flag and reset it
        or      al,al
        jz      rdsec
        push    dx
        push    bp
        mov     bp,[bufdrvbp]
        mov     bx,[buffer]
        mov     cx,1
        mov     dx,[bufsecno]
        call    dwrite
        pop     bp
        pop     dx
rdsec:
        test    byte ptr [preread],-1
        jnz     setbuf
        xor     ax,ax
        mov     [bufsecno],ax           ;set buffer valid in case of disk error
        dec     ax
        mov     [bufdrvno],al
        mov     bx,[buffer]
        mov     cx,1
        push    dx
        call    dread
        pop     dx
setbuf:
        mov     [bufsecno],dx
        mov     al,[thisdrv]
        mov     [bufdrvno],al
        mov     [bufdrvbp],bp
finbuf:
        mov     byte ptr [trans],1      ;a transfer is taking place
        mov     di,[nextadd]
        mov     si,di
        mov     cx,[bytcnt1]
        add     si,cx
        mov     [nextadd],si
        mov     si,[buffer]
        add     si,[bytsecpos]
        ret

bufrd:
        xor     al,al           ;pre-read necessary
        call    bufsec
        push    es
        mov     es,[dmaadd+2]
        shr     cx,1
        jnc     evenrd
        movsb
evenrd:
        rep     movsw
        pop     es
        ret

bufwrt:
        mov     ax,[secpos]
        inc     ax              ;set for next sector
        mov     [secpos],ax
        cmp     ax,[valsec]     ;has sector been written before?
        mov     al,1
        ja      noread          ;skip preread if secpos>valsec
        mov     al,0
noread:
        call    bufsec
        xchg    di,si
        push    ds
        push    es
        push    cs
        pop     es
        mov     ds,[dmaadd+2]
        shr     cx,1
        jnc     evenwrt
        movsb
evenwrt:
        rep     movsw
        pop     es
        pop     ds
        mov     byte ptr [dirtybuf],1
        ret

nextsec:
        test    byte ptr [trans],-1
        jz      clret
        mov     al,[seccluspos]
        inc     al
        cmp     al,[bp.clusmsk]
        jbe     savpos
        mov     bx,[clusnum]
        cmp     bx,0ff8h
        jae     nonext
        mov     si,[bp.fat]
        call    unpack
        mov     [clusnum],di
        inc     [lastpos]
        mov     al,0
savpos:
        mov     [seccluspos],al
clret:
        clc
        ret
nonext:
        stc
        ret

tranbuf:
        lodsb
        stosb
        cmp     al,13           ;check for carriage return
        jnz     normch
        mov     byte ptr [si],10
normch:
        cmp     al,10
        loopnz  tranbuf
        jnz     endrdcon
        call    out             ;transmit linefeed
        xor     si,si
        or      cx,cx
        jnz     getbuf
        or      al,1            ;clear zero flag--not end of file
endrdcon:
        mov     [contpos],si
endrddev:
        mov     [nextadd],di
        pop     es
        jnz     setfcbj         ;zero set if ctrl-z found in input
        mov     di,[fcb]
        and     es:byte ptr [di.devid],0ffh-40h ;mark as no more data available
setfcbj:
        jmp     setfcb

readdev:
        push    es
        les     di,dword ptr [dmaadd]
        inc     bl
        jz      readcon
        inc     bl
        jnz     endrddev
readaux:
        call    auxin
        stosb
        cmp     al,1ah
        loopnz  readaux
        jmp     short endrddev

readcon:
        push    cs
        pop     ds
        mov     si,[contpos]
        or      si,si
        jnz     tranbuf
        cmp     byte ptr [conbuf],128
        jz      getbuf
        mov     word ptr [conbuf],0ff80h        ;set up 128-byte buffer with no template
getbuf:
        push    cx
        push    es
        push    di
        mov     dx,offset dosgroup:conbuf
        call    bufin           ;get input buffer
        pop     di
        pop     es
        pop     cx
        mov     si,2 + offset dosgroup:conbuf
        cmp     byte ptr [si],1ah       ;check for ctrl-z in first character
        jnz     tranbuf
        mov     al,1ah
        stosb
        mov     al,10
        call    out             ;send linefeed
        xor     si,si
        jmp     short endrdcon

rderr:
        xor     cx,cx
        jmp     wrterr

rdlastj:jmp     rdlast

load:

; inputs:
;       ds:di point to fcb
;       dx:ax = position in file to read
;       cx = no. of records to read
; outputs:
;       dx:ax = position of last record read
;       cx = no. of bytes read
;       es:di point to fcb
;       lstclus, cluspos fields in fcb set

        call    setup
        or      bl,bl           ;check for named device i/o
        js      readdev
        mov     ax,es:word ptr [di.filsiz]
        mov     bx,es:word ptr [di.filsiz+2]
        sub     ax,word ptr [bytpos]
        sbb     bx,word ptr [bytpos+2]
        jb      rderr
        jnz     enuf
        or      ax,ax
        jz      rderr
        cmp     ax,cx
        jae     enuf
        mov     cx,ax
enuf:
        call    breakdown
        mov     cx,[clusnum]
        call    fndclus
        or      cx,cx
        jnz     rderr
        mov     [lastpos],dx
        mov     [clusnum],bx
        cmp     [bytcnt1],0
        jz      rdmid
        call    bufrd
rdmid:
        cmp     [seccnt],0
        jz      rdlastj
        call    nextsec
        jc      setfcb
        mov     byte ptr [trans],1      ;a transfer is taking place
onsec:
        mov     dl,[seccluspos]
        mov     cx,[seccnt]
        mov     bx,[clusnum]
rdlp:
        call    optimize
        push    di
        push    ax
        push    ds
        mov     ds,[dmaadd+2]
        push    dx
        push    bx
        pushf                   ;save carry flag
        call dread
        popf                    ;restore carry flag
        pop     di              ;initial transfer address
        pop     ax              ;first sector transfered
        pop     ds
        jc      notbuffed       ;was one of those sectors in the buffer?
        cmp     byte ptr [dirtybuf],0   ;is buffer dirty?
        jz      notbuffed       ;if not no problem
;we have transfered in a sector from disk when a dirty copy of it is in the buffer.
;we must transfer the sector from the buffer to correct memory address
        sub     ax,[bufsecno]   ;how many sectors into the transfer?
        neg     ax
        mov     cx,[bp.secsiz]
        mul     cx              ;how many bytes into the transfer?
        add     di,ax
        mov     si,[buffer]
        push    es
        mov     es,[dmaadd+2]   ;get disk transfer segment
        shr     cx,1
        rep     movsw
        jnc     evenmov
        movsb
evenmov:
        pop     es
notbuffed:
        pop     cx
        pop     bx
        jcxz    rdlast
        cmp     bx,0ff8h
        jae     setfcb
        mov     dl,0
        inc     [lastpos]       ;we'll be using next cluster
        jmp     short rdlp

setfcb:
        mov     si,[fcb]
        mov     ax,[nextadd]
        mov     di,ax
        sub     ax,[dmaadd]     ;number of bytes transfered
        xor     dx,dx
        mov     cx,es:[si.recsiz]
        div     cx              ;number of records
        cmp     ax,[reccnt]     ;check if all records transferred
        jz      fullrec
        mov     byte ptr [dskerr],1
        or      dx,dx
        jz      fullrec         ;if remainder 0, then full record transfered
        mov     byte ptr [dskerr],3     ;flag partial last record
        sub     cx,dx           ;bytes left in last record
        push    es
        mov     es,[dmaadd+2]
        xchg    ax,bx           ;save the record count temporarily
        xor     ax,ax           ;fill with zeros
        shr     cx,1
        jnc     evenfil
        stosb
evenfil:
        rep     stosw
        xchg    ax,bx           ;restore record count to ax
        pop     es
        inc     ax              ;add last (partial) record to total
fullrec:
        mov     cx,ax
        mov     di,si           ;es:di point to fcb
setclus:
        mov     ax,[clusnum]
        mov     es:[di.lstclus],ax
        mov     ax,[lastpos]
        mov     es:[di.cluspos],ax
addrec:
        mov     ax,word ptr [recpos]
        mov     dx,word ptr [recpos+2]
        jcxz    ret28           ;if no records read, don't change position
        dec     cx
        add     ax,cx           ;update current record position
        adc     dx,0
        inc     cx      
ret28:  ret

rdlast:
        mov     ax,[bytcnt2]
        or      ax,ax
        jz      setfcb
        mov     [bytcnt1],ax
        call    nextsec
        jc      setfcb
        mov     [bytsecpos],0
        call    bufrd
        jmp     short setfcb

wrtdev:
        push    ds
        lds     si,dword ptr [dmaadd]
        or      bl,40h
        inc     bl
        jz      wrtcon
        inc     bl
        jz      wrtaux
        inc     bl
        jz      endwrdev        ;done if device is nul
wrtlst:
        lodsb
        cmp     al,1ah
        jz      endwrdev
        call    listout
        loop    wrtlst
        jmp     short endwrdev

wrtaux:
        lodsb
        call    auxout
        cmp     al,1ah
        loopnz  wrtaux
        jmp     short endwrdev

wrtcon:
        lodsb
        cmp     al,1ah
        jz      endwrdev
        call    out
        loop    wrtcon
endwrdev:
        pop     ds
        mov     cx,[reccnt]
        mov     di,[fcb]
        jmp     short addrec

havstart:
        mov     cx,ax
        call    skpclp
        jcxz    dowrtj
        call    allocate
        jnc     dowrtj
wrterr:
        mov     byte ptr [dskerr],1
lvdsk:
        mov     ax,word ptr [recpos]
        mov     dx,word ptr [recpos+2]
        mov     di,[fcb]
        ret

dowrtj: jmp     dowrt

wrteofj:
        jmp     wrteof

store:

; inputs:
;       ds:di point to fcb
;       dx:ax = position in file of disk transfer
;       cx = record count
; outputs:
;       dx:ax = position of last record written
;       cx = no. of records written
;       es:di point to fcb
;       lstclus, cluspos fields in fcb set

        call    setup
        call    date16
        mov     es:[di.fdate],ax
        mov     es:[di.ftime],dx
        or      bl,bl
        js      wrtdev
        and     bl,3fh          ;mark file as dirty
        mov     es:[di.devid],bl
        call    breakdown
        mov     ax,word ptr [bytpos]
        mov     dx,word ptr [bytpos+2]
        jcxz    wrteofj
        dec     cx
        add     ax,cx
        adc     dx,0            ;ax:dx=last byte accessed
        div     [bp.secsiz]     ;ax=last sector accessed
        mov     cl,[bp.clusshft]
        shr     ax,cl           ;last cluster to be accessed
        push    ax
        mov     ax,es:word ptr [di.filsiz]
        mov     dx,es:word ptr [di.filsiz+2]
        div     [bp.secsiz]
        or      dx,dx
        jz      norndup
        inc     ax              ;round up if any remainder
norndup:
        mov     [valsec],ax     ;number of sectors that have been written
        pop     ax
        mov     cx,[clusnum]    ;first cluster accessed
        call    fndclus
        mov     [clusnum],bx
        mov     [lastpos],dx
        sub     ax,dx           ;last cluster minus current cluster
        jz      dowrt           ;if we have last clus, we must have first
        jcxz    havstart        ;see if no more data
        push    cx              ;no. of clusters short of first
        mov     cx,ax
        call    allocate
        pop     ax
        jc      wrterr
        mov     cx,ax
        mov     dx,[lastpos]
        inc     dx
        dec     cx
        jz      noskip
        call    skpclp
noskip:
        mov     [clusnum],bx
        mov     [lastpos],dx
dowrt:
        cmp     [bytcnt1],0
        jz      wrtmid
        mov     bx,[clusnum]
        call    bufwrt  
wrtmid:
        mov     ax,[seccnt]
        or      ax,ax
        jz      wrtlast
        add     [secpos],ax
        call    nextsec
        mov     byte ptr [trans],1      ;a transfer is taking place
        mov     dl,[seccluspos]
        mov     bx,[clusnum]
        mov     cx,[seccnt]
wrtlp:
        call    optimize
        jc      notinbuf        ;is one of the sectors buffered?
        mov     [bufsecno],0    ;if so, invalidate the buffer since we're
        mov     word ptr [bufdrvno],0ffh        ;       completely rewritting it
notinbuf:
        push    di
        push    ax
        push    ds
        mov     ds,[dmaadd+2]
        call    dwrite
        pop     ds
        pop     cx
        pop     bx
        jcxz    wrtlast
        mov     dl,0
        inc     [lastpos]       ;we'll be using next cluster
        jmp     short wrtlp
wrtlast:
        mov     ax,[bytcnt2]
        or      ax,ax
        jz      finwrt
        mov     [bytcnt1],ax
        call    nextsec
        mov     [bytsecpos],0
        call    bufwrt
finwrt:
        mov     ax,[nextadd]
        sub     ax,[dmaadd]
        add     ax,word ptr [bytpos]
        mov     dx,word ptr [bytpos+2]
        adc     dx,0
        mov     cx,dx
        mov     di,[fcb]
        cmp     ax,es:word ptr [di.filsiz]
        sbb     cx,es:word ptr [di.filsiz+2]
        jb      samsiz
        mov     es:word ptr [di.filsiz],ax
        mov     es:word ptr [di.filsiz+2],dx
samsiz:
        mov     cx,[reccnt]
        jmp     setclus


wrterrj:jmp     wrterr

wrteof:
        mov     cx,ax
        or      cx,dx
        jz      killfil
        sub     ax,1
        sbb     dx,0
        div     [bp.secsiz]
        mov     cl,[bp.clusshft]
        shr     ax,cl
        mov     cx,ax
        call    fndclus
        jcxz    relfile
        call    allocate
        jc      wrterrj
update:
        mov     di,[fcb]
        mov     ax,word ptr [bytpos]
        mov     es:word ptr [di.filsiz],ax
        mov     ax,word ptr [bytpos+2]
        mov     es:word ptr [di.filsiz+2],ax
        xor     cx,cx
        jmp     addrec

relfile:
        mov     dx,0fffh
        call    relblks
setdirt:
        mov     byte ptr [si-1],1
        jmp     short update

killfil:
        xor     bx,bx
        xchg    bx,es:[di.firclus]
        or      bx,bx
        jz      update
        call    release
        jmp     short setdirt


optimize:

; inputs:
;       ds = cs
;       bx = physical cluster
;       cx = no. of records
;       dl = sector within cluster
;       bp = base of drives parameters
;       [nextadd] = transfer address
; outputs:
;       ax = no. of records remaining
;       bx = transfer address
;       cx = no. or records to be transferred
;       dx = physical sector address
;       di = next cluster
;       carry clear if a sector to transfer is in the buffer
;       carry set otherwise
;       [clusnum] = last cluster accessed
;       [nextadd] updated
; bp unchanged. note that segment of transfer not set.

        push    dx
        push    bx
        mov     al,[bp.clusmsk]
        inc     al              ;number of sectors per cluster
        mov     ah,al
        sub     al,dl           ;al = number of sectors left in first cluster
        mov     dx,cx
        mov     si,[bp.fat]
        mov     cx,0
optclus:
;al has number of sectors available in current cluster
;ah has number of sectors available in next cluster
;bx has current physical cluster
;cx has number of sequential sectors found so far
;dx has number of sectors left to transfer
;si has fat pointer
        call    unpack
        add     cl,al
        adc     ch,0
        cmp     cx,dx
        jae     blkdon
        mov     al,ah
        inc     bx
        cmp     di,bx
        jz      optclus
        dec     bx
finclus:
        mov     [clusnum],bx    ;last cluster accessed
        sub     dx,cx           ;number of sectors still needed
        push    dx
        mov     ax,cx
        mul     [bp.secsiz]     ;number of sectors times sector size
        mov     si,[nextadd]
        add     ax,si           ;adjust by size of transfer
        mov     [nextadd],ax
        pop     ax              ;number of sectors still needed
        pop     dx              ;starting cluster
        sub     bx,dx           ;number of new clusters accessed
        add     [lastpos],bx
        pop     bx              ;bl = sector postion within cluster
        call    figrec
        mov     bx,si
;now let's see if any of these sectors are already in the buffer
        cmp     [bufsecno],dx
        jc      ret100          ;if dx > [bufsecno] then not in buffer
        mov     si,dx
        add     si,cx           ;last sector + 1
        cmp     [bufsecno],si
        cmc
        jc      ret100          ;if si <= [bufsecno] then not in buffer
        push    ax
        mov     al,[bp.devnum]
        cmp     al,[bufdrvno]   ;is buffer for this drive?
        pop     ax
        jz      ret100          ;if so, then we match 
        stc                     ;no match
ret100: ret
blkdon:
        sub     cx,dx           ;number of sectors in cluster we don't want
        sub     ah,cl           ;number of sectors in cluster we accepted
        dec     ah              ;adjust to mean position within cluster
        mov     [seccluspos],ah
        mov     cx,dx           ;anyway, make the total equal to the request
        jmp     short finclus


figrec:

;inputs:
;       dx = physical cluster number
;       bl = sector postion within cluster
;       bp = base of drive parameters
;outputs:
;       dx = physical sector number
;no other registers affected.

        push    cx
        mov     cl,[bp.clusshft]
        dec     dx
        dec     dx
        shl     dx,cl
        or      dl,bl
        add     dx,[bp.firrec]
        pop     cx
        ret

getrec:

; inputs:
;       ds:dx point to fcb
; outputs:
;       cx = 1
;       dx:ax = record number determined by extent and nr fields
;       ds:di point to fcb
; no other registers affected.

        mov     di,dx
        cmp     byte ptr [di],-1        ;check for extended fcb
        jnz     normfcb2
        add     di,7
normfcb2:
        mov     cx,1
        mov     al,[di.nr]
        mov     dx,[di.extent]
        shl     al,1
        shr     dx,1
        rcr     al,1
        mov     ah,dl
        mov     dl,dh
        mov     dh,0
        ret


allocate:

; inputs:
;       ds = cs
;       es = segment of fcb
;       bx = last cluster of file (0 if null file)
;       cx = no. of clusters to allocate
;       dx = position of cluster bx
;       bp = base of drive parameters
;       si = fat pointer
;       [fcb] = displacement of fcb within segment
; outputs:
;       if insufficient space
;         then
;       carry set
;       cx = max. no. of records that could be added to file
;         else
;       carry clear
;       bx = first cluster allocated
;       fat is fully updated including dirty bit
;       firclus field of fcb set if file was null
; si,bp unchanged. all other registers destroyed.

        push    [si]
        push    dx
        push    cx
        push    bx
        mov     ax,bx
alloc:
        mov     dx,bx
findfre:
        inc     bx
        cmp     bx,[bp.maxclus]
        jle     tryout
        cmp     ax,1
        jg      tryin
        pop     bx
        mov     dx,0fffh
        call    relblks
        pop     ax              ;no. of clusters requested
        sub     ax,cx           ;ax=no. of clusters allocated
        pop     dx
        pop     [si]
        inc     dx              ;position of first cluster allocated
        add     ax,dx           ;ax=max no. of cluster in file
        mov     dl,[bp.clusmsk]
        mov     dh,0
        inc     dx              ;dx=records/cluster
        mul     dx              ;ax=max no. of records in file
        mov     cx,ax
        sub     cx,word ptr [recpos]    ;cx=max no. of records that could be written
        ja      maxrec
        xor     cx,cx           ;if cx was negative, zero it
maxrec:
        stc
ret11:  ret

tryout:
        call    unpack
        jz      havfre
tryin:
        dec     ax
        jle     findfre
        xchg    ax,bx
        call    unpack
        jz      havfre
        xchg    ax,bx
        jmp     short findfre
havfre:
        xchg    bx,dx
        mov     ax,dx
        call    pack
        mov     bx,ax
        loop    alloc
        mov     dx,0fffh
        call    pack
        mov     byte ptr [si-1],1
        pop     bx
        pop     cx              ;don't need this stuff since we're successful
        pop     dx
        call    unpack
        pop     [si]
        xchg    bx,di
        or      di,di
        jnz     ret11
        mov     di,[fcb]
        mov     es:[di.firclus],bx
ret12:  ret


release:

; inputs:
;       ds = cs
;       bx = cluster in file
;       si = fat pointer
;       bp = base of drive parameters
; function:
;       frees cluster chain starting with [bx]
; ax,bx,dx,di all destroyed. other registers unchanged.

        xor     dx,dx
relblks:
; enter here with dx=0fffh to put an end-of-file mark
; in the first cluster and free the rest in the chain.
        call    unpack
        jz      ret12
        mov     ax,di
        call    pack
        cmp     ax,0ff8h
        mov     bx,ax
        jb      release
ret13:  ret


geteof:

; inputs:
;       bx = cluster in a file
;       si = base of drive fat
;       ds = cs
; outputs:
;       bx = last cluster in the file
; di destroyed. no other registers affected.

        call    unpack
        cmp     di,0ff8h
        jae     ret13
        mov     bx,di
        jmp     short geteof


srchfrst: ;system call 17
        call    getfile
savplce:
; search-for-next enters here to save place and report
; findings.
        jc      killsrch
        or      bh,bh
        js      srchdev
        mov     ax,[lastent]
        mov     es:[di.fildirent],ax
        mov     es:[di.drvbp],bp
;information in directory entry must be copied into the first
; 33 bytes starting at the disk transfer address.
        mov     si,bx
        les     di,dword ptr [dmaadd]
        mov     ax,00ffh
        cmp     al,[extfcb]
        jnz     normfcb
        stosw
        inc     al
        stosw
        stosw
        mov     al,[attrib]
        stosb
normfcb:
        mov     al,[thisdrv]
        inc     al
        stosb   ;set drive number
        mov     cx,16
        rep     movsw   ;copy remaining 10 characters of name
        xor     al,al
        ret

killsrch:
killsrch1       equ     killsrch+1
;the purpose of the killsrch1 label is to provide a jump label to the following
;   instruction which leaves out the segment override.
        mov     word ptr es:[di.fildirent],-1
        mov     al,-1
        ret

srchdev:
        mov     es:[di.fildirent],bx
        les     di,dword ptr [dmaadd]
        xor     ax,ax
        stosb           ;zero drive byte
        sub     si,4            ;point to device name
        movsw
        movsw
        mov     ax,2020h
        stosb
        stosw
        stosw
        stosw                   ;fill with 8 blanks
        xor     ax,ax
        mov     cx,10
        rep     stosw
        stosb
ret14:  ret

srchnxt: ;system call 18
        call    movname
        mov     di,dx
        jc      near ptr killsrch1
        mov     bp,[di.drvbp]
        mov     ax,[di.fildirent]
        or      ax,ax
        js      near ptr killsrch1
        push    dx
        push    ds
        push    cs
        pop     ds
        mov     [lastent],ax
        call    contsrch
        pop     es
        pop     di
        jmp     savplce


filesize: ;system call 35
        call    getfile
        mov     al,-1
        jc      ret14
        add     di,33           ;write size in rr field
        mov     cx,es:[di.recsiz-33]
        or      cx,cx
        jnz     recok
        mov     cx,128
recok:
        xor     ax,ax
        xor     dx,dx           ;intialize size to zero
        or      bh,bh           ;check for named i/o device
        js      devsiz
        inc     si
        inc     si              ;point to length field
        mov     ax,[si+2]       ;get high word of size
        div     cx
        push    ax              ;save high part of result
        lodsw           ;get low word of size
        div     cx
        or      dx,dx           ;check for zero remainder
        pop     dx
        jz      devsiz
        inc     ax              ;round up for partial record
        jnz     devsiz          ;propagate carry?
        inc     dx
devsiz:
        stosw
        mov     ax,dx
        stosb
        mov     al,0
        cmp     cx,64
        jae     ret14           ;only 3-byte field if recsiz >= 64
        mov     es:[di],ah
        ret


setdma: ;system call 26
        mov     cs:[dmaadd],dx
        mov     cs:[dmaadd+2],ds
        ret

nosuchdrv:
        mov     al,-1
        ret

getfatpt: ;system call 27
        mov     dl,0                    ;use default drive

getfatptdl:     ;system call 28
        push    cs
        pop     ds
        mov     al,dl
        call    getthisdrv
        jc      nosuchdrv
        call    fatread
        mov     bx,[bp.fat]
        mov     al,[bp.clusmsk]
        inc     al
        mov     dx,[bp.maxclus]
        dec     dx
        mov     cx,[bp.secsiz]
        lds     si,dword ptr [spsave]
        mov     [si.bxsave],bx
        mov     [si.dxsave],dx
        mov     [si.cxsave],cx
        mov     [si.dssave],cs
        ret


getdskpt: ;system call 31
        push    cs
        pop     ds
        mov     al,[curdrv]
        mov     [thisdrv],al
        call    fatread
        lds     si,dword ptr [spsave]
        mov     [si.bxsave],bp
        mov     [si.dssave],cs
        ret


dskreset: ;system call 13
        push    cs
        pop     ds
wrtfats:
; ds=cs. writes back all dirty fats. all registers destroyed.
        xor     al,al
        xchg    al,[dirtybuf]
        or      al,al
        jz      nobuf
        mov     bp,[bufdrvbp]
        mov     dx,[bufsecno]
        mov     bx,[buffer]
        mov     cx,1
        call    dwrite
nobuf:
        mov     cl,[numio]
        mov     ch,0
        mov     bp,[drvtab]
wrtfat:
        push    cx
        call    chkfatwrt
        pop     cx
        add     bp,dpbsiz
        loop    wrtfat
        ret


getdrv: ;system call 25
        mov     al,cs:[curdrv]
ret15:  ret


setrndrec: ;system call 36
        call    getrec
        mov     [di+33],ax
        mov     [di+35],dl
        cmp     [di.recsiz],64
        jae     ret15
        mov     [di+36],dh      ;set 4th byte only if record size < 64
ret16:  ret


seldsk: ;system call 14
        mov     al,cs:[numdrv]
        cmp     dl,al
        jnb     ret17
        mov     cs:[curdrv],dl
ret17:  ret

bufin:  ;system call 10
        mov     ax,cs
        mov     es,ax
        mov     si,dx
        mov     ch,0
        lodsw
        or      al,al
        jz      ret17
        mov     bl,ah
        mov     bh,ch
        cmp     al,bl
        jbe     noedit
        cmp     byte ptr [bx+si],0dh
        jz      editon
noedit:
        mov     bl,ch
editon:
        mov     dl,al
        dec     dx
newlin:
        mov     al,cs:[carpos]
        mov     cs:[startpos],al
        push    si
        mov     di,offset dosgroup:inbuf
        mov     ah,ch
        mov     bh,ch
        mov     dh,ch
getch:
        call    in
        cmp     al,"f"-"@"      ;ignore ^f
        jz      getch
        cmp     al,cs:escchar
        jz      esc
        cmp     al,7fh
        jz      backsp
        cmp     al,8
        jz      backsp
        cmp     al,13
        jz      endlin
        cmp     al,10
        jz      phycrlf
        cmp     al,cancel
        jz      kilnew
savch:
        cmp     dh,dl
        jae     bufful
        stosb
        inc     dh
        call    bufout
        or      ah,ah
        jnz     getch
        cmp     bh,bl
        jae     getch
        inc     si
        inc     bh
        jmp     short getch

bufful:
        mov     al,7
        call    out
        jmp     short getch

esc:
        call    in
        mov     cl,esctablen
        push    di
        mov     di,offset dosgroup:esctab
        repne   scasb
        pop     di
        shl     cx,1
        mov     bp,cx
        jmp     [bp+offset dosgroup:escfunc]

endlin:
        stosb
        call    out
        pop     di
        mov     [di-1],dh
        inc     dh
copynew:
        mov     bp,es
        mov     bx,ds
        mov     es,bx
        mov     ds,bp
        mov     si,offset dosgroup:inbuf
        mov     cl,dh
        rep     movsb
        ret
crlf:
        mov     al,13
        call    out
        mov     al,10
        jmp     out

phycrlf:
        call    crlf
        jmp     short getch

kilnew:
        mov     al,"\"
        call    out
        pop     si
putnew:
        call    crlf
        mov     al,cs:[startpos]
        call    tab
        jmp     newlin

backsp:
        or      dh,dh
        jz      oldbak
        call    backup
        mov     al,es:[di]
        cmp     al," "
        jae     oldbak
        cmp     al,9
        jz      baktab
        call    backmes
oldbak:
        or      ah,ah
        jnz     getch1
        or      bh,bh
        jz      getch1
        dec     bh
        dec     si
getch1:
        jmp     getch
baktab:
        push    di
        dec     di
        std
        mov     cl,dh
        mov     al," "
        push    bx
        mov     bl,7
        jcxz    figtab
fndpos:
        scasb
        jna     chkcnt
        cmp     es:byte ptr [di+1],9
        jz      havtab
        dec     bl
chkcnt:
        loop    fndpos
figtab:
        sub     bl,cs:[startpos]
havtab:
        sub     bl,dh
        add     cl,bl
        and     cl,7
        cld
        pop     bx
        pop     di
        jz      oldbak
tabbak:
        call    backmes
        loop    tabbak
        jmp     short oldbak
backup:
        dec     dh
        dec     di
backmes:
        mov     al,8
        call    out
        mov     al," "
        call    out
        mov     al,8
        jmp     out

twoesc:
        mov     al,escch
        jmp     savch

copylin:
        mov     cl,bl
        sub     cl,bh
        jmp     short copyeach

copystr:
        call    findold
        jmp     short copyeach

copyone:
        mov     cl,1
copyeach:
        mov     ah,0
        cmp     dh,dl
        jz      getch2
        cmp     bh,bl
        jz      getch2
        lodsb
        stosb
        call    bufout
        inc     bh
        inc     dh
        loop    copyeach
getch2:
        jmp     getch

skipone:
        cmp     bh,bl
        jz      getch2
        inc     bh
        inc     si
        jmp     getch

skipstr:
        call    findold
        add     si,cx
        add     bh,cl
        jmp     getch

findold:
        call    in
        mov     cl,bl
        sub     cl,bh
        jz      notfnd
        dec     cx
        jz      notfnd
        push    es
        push    ds
        pop     es
        push    di
        mov     di,si
        inc     di
        repne   scasb
        pop     di
        pop     es
        jnz     notfnd
        not     cl
        add     cl,bl
        sub     cl,bh
ret30:  ret
notfnd:
        pop     bp
        jmp     getch

reedit:
        mov     al,"@"
        call    out
        pop     di
        push    di
        push    es
        push    ds
        call    copynew
        pop     ds
        pop     es
        pop     si
        mov     bl,dh
        jmp     putnew

enterins:
        if      toglins
        not     ah
        jmp     getch
        endif
        if      not toglins
        mov     ah,-1
        jmp     getch

exitins:
        mov     ah,0
        jmp     getch
        endif

escfunc dw      getch
        dw      twoesc
        if      not toglins
        dw      exitins
        endif
        dw      enterins
        dw      backsp
        dw      reedit
        dw      kilnew
        dw      copylin
        dw      skipstr
        dw      copystr
        dw      skipone
        dw      copyone

        if      ibm
        dw      copyone
        dw      ctrlz
ctrlz:
        mov     al,"z"-"@"
        jmp     savch
        endif
bufout:
        cmp     al," "
        jae     out
        cmp     al,9
        jz      out
        push    ax
        mov     al,"^"
        call    out
        pop     ax
        or      al,40h
        jmp     short out

nostop:
        cmp     al,"p"-"@"
        jz      inchk
        if      not toglprn
        cmp     al,"n"-"@"
        jz      inchk
        endif
        cmp     al,"c"-"@"
        jz      inchk
        ret

conout: ;system call 2
        mov     al,dl
out:
        cmp     al,20h
        jb      ctrlout
        cmp     al,7fh
        jz      outch
        inc     cs:byte ptr [carpos]
outch:
        push    ax
        call    statchk
        pop     ax
        call    far ptr biosout 
        test    cs:byte ptr [pflag],-1
        jz      ret18
        call    far ptr biosprint 
ret18:  ret

statchk:
        call    far ptr biosstat 
        jz      ret18
        cmp     al,'s'-'@'
        jnz     nostop
        call    far ptr biosin          ;eat cntrl-s
inchk:
        call    far ptr biosin 
        cmp     al,'p'-'@'
        jz      printon
        if      not toglprn
        cmp     al,'n'-'@'
        jz      printoff
        endif
        cmp     al,'c'-'@'
        jnz     ret18
; ctrl-c handler.
; "^c" and cr/lf is printed. then the user registers are restored and the
; user ctrl-c handler is executed. at this point the top of the stack has
; 1) the interrupt return address should the user ctrl-c handler wish to
; allow processing to continue; 2) the original interrupt return address
; to the code that performed the function call in the first place. if the
; user ctrl-c handler wishes to continue, it must leave all registers
; unchanged and iret. the function that was interrupted will simply be
; repeated.
        mov     al,3            ;display "^c"
        call    bufout
        call    crlf
        cli                     ;prepare to play with stack
        mov     ss,cs:[sssave]
        mov     sp,cs:[spsave]  ;user stack now restored
        pop     ax
        pop     bx
        pop     cx
        pop     dx
        pop     si
        pop     di
        pop     bp
        pop     ds
        pop     es              ;user registers now restored
        int     contc           ;execute user ctrl-c handler
        jmp     command         ;repeat command otherwise

printon:
        if      toglprn
        not     cs:byte ptr [pflag]
        ret
        endif
        if      not toglprn
        mov     cs:byte ptr [pflag],1
        ret

printoff:
        mov     cs:byte ptr [pflag],0
        ret
        endif

ctrlout:
        cmp     al,13
        jz      zerpos
        cmp     al,8
        jz      backpos
        cmp     al,9
        jnz     outchj
        mov     al,cs:[carpos]
        or      al,0f8h
        neg     al
tab:
        push    cx
        mov     cl,al
        mov     ch,0
        jcxz    poptab
tablp:
        mov     al," "
        call    out
        loop    tablp
poptab:
        pop     cx
ret19:  ret

zerpos:
        mov     cs:byte ptr [carpos],0
outchj: jmp     outch

backpos:
        dec     cs:byte ptr [carpos]
        jmp     outch


constat: ;system call 11
        call    statchk
        mov     al,0 
        jz      ret19
        or      al,-1
        ret


conin:  ;system call 1
        call    in
        push    ax
        call    out
        pop     ax
        ret


in:     ;system call 8
        call    inchk
        jz      in
ret29:  ret

rawio:  ;system call 6
        mov     al,dl
        cmp     al,-1
        jnz     rawout
        lds     si,dword ptr cs:[spsave]                ;get pointer to register save area
        call    far ptr biosstat
        jnz     resflg
        or      byte ptr [si.fsave],40h ;set user's zero flag
        xor     al,al
        ret

resflg:
        and     byte ptr [si.fsave],0ffh-40h    ;reset user's zero flag
rawinp: ;system call 7
        call    far ptr biosin 
        ret
rawout:
        call    far ptr biosout 
        ret

list:   ;system call 5
        mov     al,dl
listout:
        push    ax
        call    statchk
        pop     ax
        call    far ptr biosprint 
ret20:  ret

prtbuf: ;system call 9
        mov     si,dx
outstr:
        lodsb
        cmp     al,"$"
        jz      ret20
        call    out
        jmp     short outstr

outmes: ;string output for internal messages
        lods    cs:byte ptr [si]
        cmp     al,"$"
        jz      ret20
        call    out
        jmp     short outmes


makefcb: ;interrupt call 41
drvbit  equ     2
nambit  equ     4
extbit  equ     8
        mov     dl,0            ;flag--not ambiguous file name
        test    al,drvbit       ;use current drive field if default?
        jnz     defdrv
        mov     byte ptr es:[di],0      ;no - use default drive
defdrv:
        inc     di
        mov     cx,8
        test    al,nambit       ;use current name fiels as defualt?
        xchg    ax,bx           ;save bits in bx
        mov     al," "
        jz      fillb           ;if not, go fill with blanks
        add     di,cx
        xor     cx,cx           ;don't fill any
fillb:
        rep     stosb
        mov     cl,3
        test    bl,extbit       ;use current extension as default
        jz      fillb2
        add     di,cx
        xor     cx,cx
fillb2:
        rep     stosb
        xchg    ax,cx           ;put zero in ax
        stosw
        stosw                   ;initialize two words after to zero
        sub     di,16           ;point back at start
        test    bl,1            ;scan off separators if not zero
        jz      skpspc
        call    scanb           ;peel off blanks and tabs
        call    delim           ;is it a one-time-only delimiter?
        jnz     noscan
        inc     si              ;skip over the delimiter
skpspc:
        call    scanb           ;always kill preceding blanks and tabs
noscan:
        call    getlet
        jbe     nodrv           ;quit if termination character
        cmp     byte ptr[si],":"        ;check for potential drive specifier
        jnz     nodrv
        inc     si              ;skip over colon
        sub     al,"@"          ;convert drive letter to binary drive number
        jbe     baddrv          ;valid drive numbers are 1-15
        cmp     al,cs:[numdrv]
        jbe     havdrv
baddrv:
        mov     dl,-1
havdrv:
        stosb           ;put drive specifier in first byte
        inc     si
        dec     di      ;counteract next two instructions
nodrv:
        dec     si      ;back up
        inc     di      ;skip drive byte
        mov     cx,8
        call    getword         ;get 8-letter file name
        cmp     byte ptr [si],"."
        jnz     nodot
        inc     si              ;skip over dot if present
        mov     cx,3            ;get 3-letter extension
        call    mustgetword
nodot:
        lds     bx,cs:dword ptr [spsave]
        mov     [bx.sisave],si
        mov     al,dl
        ret

nonam:
        add     di,cx
        dec     si
        ret

getword:
        call    getlet
        jbe     nonam           ;exit if invalid character
        dec     si
mustgetword:
        call    getlet
        jbe     fillnam
        jcxz    mustgetword
        dec     cx
        cmp     al,"*"          ;check for ambiguous file specifier
        jnz     nostar
        mov     al,"?"
        rep     stosb
nostar:
        stosb
        cmp     al,"?"
        jnz     mustgetword
        or      dl,1            ;flag ambiguous file name
        jmp     mustgetword
fillnam:
        mov     al," "
        rep     stosb
        dec     si
ret21:  ret

scanb:
        lodsb
        call    spchk
        jz      scanb
        dec     si
        ret

getlet:
;get a byte from [si], convert it to upper case, and compare for delimiter.
;zf set if a delimiter, cy set if a control character (other than tab).
        lodsb
        and     al,7fh
        cmp     al,"a"
        jb      chk
        cmp     al,"z"
        ja      chk
        sub     al,20h          ;convert to upper case
chk:
        cmp     al,"."
        jz      ret21
        cmp     al,'"'
        jz      ret21
        cmp     al,"/"
        jz      ret21
        cmp     al,"["
        jz      ret21
        cmp     al,"]"
        jz      ret21

        if      ibm
delim:
        endif
        cmp     al,":"          ;allow ":" as separator in ibm version
        jz      ret21
        if      not ibm
delim:
        endif

        cmp     al,"+"
        jz      ret101
        cmp     al,"="
        jz      ret101
        cmp     al,";"
        jz      ret101
        cmp     al,","
        jz      ret101
spchk:
        cmp     al,9            ;filter out tabs too
        jz      ret101
;warning! " " must be the last compare
        cmp     al," "
ret101: ret

setvect: ; interrupt call 37
        xor     bx,bx
        mov     es,bx
        mov     bl,al
        shl     bx,1
        shl     bx,1
        mov     es:[bx],dx
        mov     es:[bx+2],ds
        ret


newbase: ; interrupt call 38
        mov     es,dx
        lds     si,cs:dword ptr [spsave]
        mov     ds,[si.cssave]
        xor     si,si
        mov     di,si
        mov     ax,ds:[2]
        mov     cx,80h
        rep     movsw

setmem:

; inputs:
;       ax = size of memory in paragraphs
;       dx = segment
; function:
;       completely prepares a program base at the 
;       specified segment.
; outputs:
;       ds = dx
;       es = dx
;       [0] has int 20h
;       [2] = first unavailable segment ([endmem])
;       [5] to [9] form a long call to the entry point
;       [10] to [13] have exit address (from int 22h)
;       [14] to [17] have ctrl-c exit address (from int 23h)
;       [18] to [21] have fatal error address (from int 24h)
; dx,bp unchanged. all other registers destroyed.

        xor     cx,cx
        mov     ds,cx
        mov     es,dx
        mov     si,exit
        mov     di,savexit
        movsw
        movsw
        movsw
        movsw
        movsw
        movsw
        mov     es:[2],ax
        sub     ax,dx
        cmp     ax,maxdif
        jbe     havdif
        mov     ax,maxdif
havdif:
        mov     bx,entrypointseg
        sub     bx,ax
        shl     ax,1
        shl     ax,1
        shl     ax,1
        shl     ax,1
        mov     ds,dx
        mov     ds:[6],ax
        mov     ds:[8],bx
        mov     ds:[0],20cdh    ;"int inttab"
        mov     ds:(byte ptr [5]),longcall
        ret

date16:
        push    cx
        call    readtime
        shl     cl,1            ;minutes to left part of byte
        shl     cl,1
        shl     cx,1            ;push hours and minutes to left end
        shl     cx,1
        shl     cx,1
        shr     dh,1            ;count every two seconds
        or      cl,dh           ;combine seconds with hours and minutes
        mov     dx,cx
        pop     cx
        mov     ax,word ptr [month]     ;fetch month and year
        shl     al,1                    ;push month to left to make room for day
        shl     al,1
        shl     al,1
        shl     al,1
        shl     ax,1
        or      al,[day]
ret22:  ret

fouryears       equ     3*365+366

readtime:
;gets time in cx:dx. figures new date if it has changed.
;uses ax, cx, dx.
        call    far ptr biosgettime 
        cmp     ax,[daycnt]     ;see if day count is the same
        jz      ret22
        cmp     ax,fouryears*30 ;number of days in 120 years
        jae     ret22           ;ignore if too large
        mov     [daycnt],ax
        push    si
        push    cx
        push    dx              ;save time
        xor     dx,dx
        mov     cx,fouryears    ;number of days in 4 years
        div     cx              ;compute number of 4-year units
        shl     ax,1
        shl     ax,1
        shl     ax,1            ;multiply by 8 (no. of half-years)
        mov     cx,ax           ;<240 implies ah=0
        mov     si,offset dosgroup:yrtab        ;table of days in each year
        call    dslide          ;find out which of four years we're in
        shr     cx,1            ;convert half-years to whole years
        jnc     sk              ;extra half-year?
        add     dx,200
sk:
        call    setyear
        mov     cl,1            ;at least at first month in year
        mov     si,offset dosgroup:montab       ;table of days in each month
        call    dslide          ;find out which month we're in
        mov     [month],cl
        inc     dx              ;remainder is day of month (start with one)
        mov     [day],dl
        call    wkday           ;set day of week
        pop     dx
        pop     cx
        pop     si
ret23:  ret

dslide:
        mov     ah,0
dslide1:
        lodsb           ;get count of days
        cmp     dx,ax           ;see if it will fit
        jb      ret23           ;if not, done
        sub     dx,ax
        inc     cx              ;count one more month/year
        jmp     short dslide1

setyear:
;set year with value in cx. adjust length of february for this year.
        mov     byte ptr [year],cl
chkyr:
        test    cl,3            ;check for leap year
        mov     al,28
        jnz     savfeb          ;28 days if no leap year
        inc     al              ;add leap day
savfeb:
        mov     [montab+1],al   ;store for february
        ret

;days in year
yrtab   db      200,166         ;leap year
        db      200,165
        db      200,165
        db      200,165

;days of each month
montab  db      31              ;january
        db      28              ;february--reset each time year changes
        db      31              ;march
        db      30              ;april
        db      31              ;may
        db      30              ;june
        db      31              ;july
        db      31              ;august
        db      30              ;september
        db      31              ;october
        db      30              ;november
        db      31              ;december

getdate: ;function call 42
        push    cs
        pop     ds
        call    readtime        ;check for rollover to next day
        mov     ax,[year]
        mov     bx,word ptr [day]
        lds     si,dword ptr [spsave]   ;get pointer to user registers
        mov     [si.dxsave],bx  ;dh=month, dl=day
        add     ax,1980         ;put bias back
        mov     [si.cxsave],ax  ;cx=year
        mov     al,cs:[weekday]
ret24:  ret

setdate: ;function call 43
        mov     al,-1           ;be ready to flag error
        sub     cx,1980         ;fix bias in year
        jc      ret24           ;error if not big enough
        cmp     cx,119          ;year must be less than 2100
        ja      ret24
        or      dh,dh
        jz      ret24
        or      dl,dl
        jz      ret24           ;error if either month or day is 0
        cmp     dh,12           ;check against max. month
        ja      ret24
        push    cs
        pop     ds
        call    chkyr           ;set feb. up for new year
        mov     al,dh
        mov     bx,offset dosgroup:montab-1
        xlat                    ;look up days in month
        cmp     al,dl
        mov     al,-1           ;restore error flag, just in case
        jb      ret24           ;error if too many days
        call    setyear
        mov     word ptr [day],dx       ;set both day and month
        shr     cx,1
        shr     cx,1
        mov     ax,fouryears
        mov     bx,dx
        mul     cx
        mov     cl,byte ptr [year]
        and     cl,3
        mov     si,offset dosgroup:yrtab
        mov     dx,ax
        shl     cx,1            ;two entries per year, so double count
        call    dsum            ;add up the days in each year
        mov     cl,bh           ;month of year
        mov     si,offset dosgroup:montab
        dec     cx              ;account for months starting with one
        call    dsum            ;add up days in each month
        mov     cl,bl           ;day of month
        dec     cx              ;account for days starting with one
        add     dx,cx           ;add in to day total
        xchg    ax,dx           ;get day count in ax
        mov     [daycnt],ax
        call    far ptr biossetdate 
wkday:
        mov     ax,[daycnt]
        xor     dx,dx
        mov     cx,7
        inc     ax
        inc     ax              ;first day was tuesday
        div     cx              ;compute day of week
        mov     [weekday],dl
        xor     al,al           ;flag ok
ret25:  ret

dsum:
        mov     ah,0
        jcxz    ret25
dsum1:
        lodsb
        add     dx,ax
        loop    dsum1
        ret

gettime: ;function call 44
        push    cs
        pop     ds
        call    readtime
        lds     si,dword ptr [spsave]   ;get pointer to user registers
        mov     [si.dxsave],dx
        mov     [si.cxsave],cx
        xor     al,al
ret26:  ret

settime: ;function call 45
;time is in cx:dx in hours, minutes, seconds, 1/100 sec.
        mov     al,-1           ;flag in case of error
        cmp     ch,24           ;check hours
        jae     ret26
        cmp     cl,60           ;check minutes
        jae     ret26
        cmp     dh,60           ;check seconds
        jae     ret26
        cmp     dl,100          ;check 1/100's
        jae     ret26
        call    far ptr biossettime 
        xor     al,al
        ret


; default handler for division overflow trap
divov:
        push    si
        push    ax
        mov     si,offset dosgroup:divmes
        call    outmes
        pop     ax
        pop     si
        int     23h             ;use ctrl-c abort on divide overflow
        iret

codsiz  equ     $-codstrt       ;size of code segment
code    ends


;***** data area *****
constants       segment byte
        org     0
constrt equ     $               ;start of constants segment

ioname:
        if      not ibm
        db      "prn ","lst ","nul ","aux ","con "
        endif
        if      ibm
        db      "com1","prn ","lpt1","nul ","aux ","con "
        endif
divmes  db      13,10,"divide overflow",13,10,"$"
carpos  db      0
startpos db     0
pflag   db      0
dirtydir db     0               ;dirty buffer flag
numdrv  db      0       ;number of drives
numio   db      ?       ;number of disk tables
verflg  db      0       ;initialize with verify off
contpos dw      0
dmaadd  dw      80h             ;user's disk transfer address (disp/seg)
        dw      ?
endmem  dw      ?
maxsec  dw      0
buffer  dw      ?
bufsecno dw     0
bufdrvno db     -1
dirtybuf db     0
bufdrvbp dw     ?
dirbufid dw     -1
day     db      0
month   db      0
year    dw      0
daycnt  dw      -1
weekday db      0
curdrv  db      0               ;default to drive a
drvtab  dw      0               ;address of start of dpbs
doslen  equ     codsiz+($-constrt)      ;size of code + constants segments
constants       ends

data    segment word
; init code overlaps with data area below

        org     0
inbuf   db      128 dup (?)
conbuf  db      131 dup (?)             ;the rest of inbuf and console buffer
lastent dw      ?
exithold db     4 dup (?)
fatbase dw      ?
name1   db      11 dup (?)              ;file name buffer
attrib  db      ?
name2   db      11 dup (?)
name3   db      12 dup (?)
extfcb  db      ?
;warning - the following two items are accessed as a word
creating db     ?
delall  db      ?
temp    label   word
spsave  dw      ?
sssave  dw      ?
contstk dw      ?
seccluspos db   ?       ;position of first sector within cluster
dskerr  db      ?
trans   db      ?
preread db      ?       ;0 means preread; 1 means optional
readop  db      ?
thisdrv db      ?

        even
fcb     dw      ?       ;address of user fcb
nextadd dw      ?
recpos  db      4 dup (?)
reccnt  dw      ?
lastpos dw      ?
clusnum dw      ?
secpos  dw      ?       ;position of first sector accessed
valsec  dw      ?       ;number of valid (previously written) sectors
bytsecpos dw    ?       ;position of first byte within sector
bytpos  db      4 dup (?)               ;byte position in file of access
bytcnt1 dw      ?       ;no. of bytes in first sector
bytcnt2 dw      ?       ;no. of bytes in last sector
seccnt  dw      ?       ;no. of whole sectors
entfree dw      ?

        db      80h dup (?)     ;stack space
iostack label   byte
        db      80h dup (?)
dskstack label  byte 

        if      dsktest
nss     dw      ?
nsp     dw      ?
        endif

dirbuf label    word

;init code below overlaps with data area above

        org     0

movfat:
;this section of code is safe from being overwritten by block move
        rep     movs    byte ptr [di],[si]
        cld
        mov     es:[dmaadd+2],dx
        mov     si,[drvtab]     ;address of first dpb
        mov     al,-1
        mov     cl,[numio]      ;number of dpbs
flgfat:
        mov     di,es:[si.fat]  ;get pointer to fat
        dec     di              ;point to dirty byte
        stosb                   ;flag as unused
        add     si,dpbsiz       ;point to next dpb
        loop    flgfat
        mov     ax,[endmem]
        call    setmem          ;set up segment

xxx     proc far
        ret
xxx     endp

dosinit:
        cli
        cld
        push    cs
        pop     es
        mov     es:[endmem],dx
        lodsb                   ;get no. of drives & no. of i/o drivers
        mov     es:[numio],al
        mov     di,offset dosgroup:memstrt
perdrv:
        mov     bp,di
        mov     al,es:[drvcnt]
        stosb           ;devnum
        lodsb           ;physical unit no.
        stosb           ;drvnum
        cmp     al,15
        ja      badinit
        cbw             ;index into fat size table
        shl     ax,1
        add     ax,offset dosgroup:fatsiztab
        xchg    bx,ax
        lodsw           ;pointer to dpt
        push    si
        mov     si,ax
        lodsw
        stosw           ;secsiz
        mov     dx,ax
        cmp     ax,es:[maxsec]
        jbe     notmax
        mov     es:[maxsec],ax
notmax:
        lodsb
        dec     al
        stosb           ;clusmsk
        jz      havshft
        cbw
figshft:
        inc     ah
        sar     al,1
        jnz     figshft
        mov     al,ah
havshft:
        stosb           ;clusshft
        movsw           ;firfat (= number of reserved sectors)
        movsb           ;fatcnt
        movsw           ;maxent
        mov     ax,dx           ;secsiz again
        mov     cl,5
        shr     ax,cl
        mov     cx,ax           ;directory entries per sector
        dec     ax
        add     ax,es:[bp.maxent]
        xor     dx,dx
        div     cx
        stosw           ;dirsec (temporarily)
        movsw                   ;dsksiz (temporarily)
fndfatsiz:
        mov     al,1
        mov     dx,1
getfatsiz:
        push    dx
        call    figfatsiz
        pop     dx
        cmp     al,dl           ;compare newly computed fat size with trial
        jz      havfatsiz       ;has sequence converged?
        cmp     al,dh           ;compare with previous trial
        mov     dh,dl
        mov     dl,al           ;shuffle trials
        jnz     getfatsiz       ;continue iterations if not oscillating
        dec     word ptr es:[bp.dsksiz] ;damp those oscillations
        jmp     short fndfatsiz ;try again

badinit:
        mov     si,offset dosgroup:badmes
        call    outmes
        sti
        hlt

havfatsiz:
        stosb                   ;fatsiz
        mul     es:byte ptr[bp.fatcnt]  ;space occupied by all fats
        add     ax,es:[bp.firfat]
        stosw                   ;firdir
        add     ax,es:[bp.dirsec]
        mov     es:[bp.firrec],ax       ;destroys dirsec
        call    figmax
        mov     es:[bp.maxclus],cx
        mov     ax,bx           ;pointer into fat size table
        stosw                   ;allocate space for fat pointer
        mov     al,es:[bp.fatsiz]
        xor     ah,ah
        mul     es:[bp.secsiz]
        cmp     ax,es:[bx]      ;bigger than already allocated
        jbe     smfat
        mov     es:[bx],ax
smfat:
        pop     si              ;restore pointer to init. table
        mov     al,es:[drvcnt]
        inc     al
        mov     es:[drvcnt],al
        cmp     al,es:[numio]
        jae     continit
        jmp     perdrv  

badinitj:
        jmp     badinit

continit:
        push    cs
        pop     ds
;calculate true address of buffers, fats, free space
        mov     bp,[maxsec]
        mov     ax,offset dosgroup:dirbuf
        add     ax,bp
        mov     [buffer],ax     ;start of buffer
        add     ax,bp
        mov     [drvtab],ax     ;start of dpbs
        shl     bp,1            ;two sectors - directory and buffer
        add     bp,di           ;allocate buffer space
        add     bp,adjfac       ;true address of fats
        push    bp
        mov     si,offset dosgroup:fatsiztab
        mov     di,si
        mov     cx,16
totfatsiz:
        inc     bp              ;add one for dirty byte
        inc     bp              ;add one for i/o device number
        lodsw                   ;get size of this fat
        xchg    ax,bp
        stosw                   ;save address of this fat
        add     bp,ax           ;compute size of next fat
        cmp     ax,bp           ;if size was zero done
        loopnz  totfatsiz
        mov     al,15
        sub     al,cl           ;compute number of fats used
        mov     [numdrv],al
        xor     ax,ax           ;set zero flag
        repz    scasw           ;make sure all other entries are zero
        jnz     badinitj
        add     bp,15           ;true start of free space
        mov     cl,4
        shr     bp,cl           ;first free segment
        mov     dx,cs
        add     dx,bp
        mov     bx,0fh
        mov     cx,[endmem]
        cmp     cx,1            ;use memory scan?
        jnz     setend
        mov     cx,dx           ;start scanning just after dos
memscan:
        inc     cx
        jz      setend
        mov     ds,cx
        mov     al,[bx]
        not     al
        mov     [bx],al
        cmp     al,[bx]
        not     al
        mov     [bx],al
        jz      memscan
setend:
        if      highmem
        sub     cx,bp
        mov     bp,cx           ;segment of dos
        mov     dx,cs           ;program segment
        endif
        if      not highmem
        mov     bp,cs
        endif
; bp has segment of dos (whether to load high or run in place)
; dx has program segment (whether after dos or overlaying dos)
; cx has size of memory in paragraphs (reduced by dos size if highmem)
        mov     cs:[endmem],cx
        if      highmem
        mov     es,bp
        xor     si,si
        mov     di,si
        mov     cx,(doslen+1)/2
        push    cs
        pop     ds
        rep movsw               ;move dos to high memory
        endif
        xor     ax,ax
        mov     ds,ax
        mov     es,ax
        mov     di,intbase
        mov     ax,offset dosgroup:quit
        stosw                   ;set abort address--displacement
        mov     ax,bp
        mov     byte ptr ds:[entrypoint],longjump
        mov     word ptr ds:[entrypoint+1],offset dosgroup:entry
        mov     word ptr ds:[entrypoint+3],ax
        mov     word ptr ds:[0],offset dosgroup:divov   ;set default divide trap address
        mov     ds:[2],ax
        mov     cx,9
        rep stosw               ;set 5 segments (skip 2 between each)
        mov     word ptr ds:[intbase+4],offset dosgroup:command
        mov     word ptr ds:[intbase+12],offset dosgroup:iret   ;ctrl-c exit
        mov     word ptr ds:[intbase+16],offset dosgroup:iret   ;fatal error exit
        mov     ax,offset biosread
        stosw
        mov     ax,biosseg
        stosw
        stosw                   ;add 2 to di
        stosw
        mov     word ptr ds:[intbase+18h],offset bioswrite
        mov     word ptr ds:[exit],100h
        mov     word ptr ds:[exit+2],dx
        if      not ibm
        mov     si,offset dosgroup:header
        call    outmes
        endif
        push    cs
        pop     ds
        push    cs
        pop     es
;move the fats into position
        mov     al,[numio]
        cbw
        xchg    ax,cx
        mov     di,offset dosgroup:memstrt.fat
fatpoint:
        mov     si,word ptr [di]        ;get address within fat address table
        movsw                           ;set address of this fat
        add     di,dpbsiz-2             ;point to next dpb
        loop    fatpoint
        pop     cx                      ;true address of first fat
        mov     si,offset dosgroup:memstrt      ;place to move dpbs from
        mov     di,[drvtab]             ;place to move dpbs to
        sub     cx,di                   ;total length of dpbs
        cmp     di,si
        jbe     movjmp                  ;are we moving to higher or lower memory?
        dec     cx                      ;move backwards to higher memory
        add     di,cx
        add     si,cx
        inc     cx
        std
movjmp:
        mov     es,bp
        jmp     movfat

figfatsiz:
        mul     es:byte ptr[bp.fatcnt]
        add     ax,es:[bp.firfat]
        add     ax,es:[bp.dirsec]
figmax:
;ax has equivalent of firrec
        sub     ax,es:[bp.dsksiz]
        neg     ax
        mov     cl,es:[bp.clusshft]
        shr     ax,cl
        inc     ax
        mov     cx,ax           ;maxclus
        inc     ax
        mov     dx,ax
        shr     dx,1
        adc     ax,dx           ;size of fat in bytes
        mov     si,es:[bp.secsiz]
        add     ax,si
        dec     ax
        xor     dx,dx
        div     si
        ret

badmes:
        db      13,10,"init table bad",13,10,"$"

fatsiztab:
        dw      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

drvcnt  db      0

memstrt label   word
adjfac  equ     dirbuf-memstrt
data    ends
        end
