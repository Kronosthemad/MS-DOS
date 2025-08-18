; command version 1.17
;
; this version of command is divided into three distinct parts. first
; is the resident portion, which includes handlers for interrupts
; 22h (terminate), 23h (cntrl-c), 24h (fatal error), and 27h (stay
; resident); it also has code to test and, if necessary, reload the
; transient portion. following the resident is the init code, which is
; overwritten after use. then comes the transient portion, which
; includes all command processing (whether internal or external).
; the transient portion loads at the end of physical memory, and it may
; be overlayed by programs that need as much memory as possible. when
; the resident portion of command regains control from a user program,
; a checksum is performed on the transient portion to see if it must be
; reloaded. thus programs which do not need maximum memory will save
; the time required to reload command when they terminate.

;use the following booleans to set assembly flags
false   equ     0
true    equ     not false

ibmver  equ     false   ;switch to build ibm version of command
msver   equ     true    ;switch to build ms-dos version of command

highmem equ     true    ;run resident part above transient (high memory)

linperpag       equ     23
normperlin      equ     1
wideperlin      equ     5

        if      ibmver
sym     equ     ">"
comdrv  equ     1
        endif

        if      msver
sym     equ     ":"
comdrv  equ     0
        endif

fcb     equ     5ch
dskreset equ    13
setbase equ     38
srchfrst equ    17
srchnxt equ     18
renam   equ     23
inchar  equ     1
getfat  equ     27
open    equ     15
close   equ     16
make    equ     22
delete  equ     19
rdblk   equ     39
wrblk   equ     40
setdma  equ     26
seldrv  equ     14
getdrv  equ     25
printbuf equ    9
outch   equ     2
inbuf   equ     10
getdate equ     2ah
setdate equ     2bh
gettime equ     2ch
settime equ     2dh
rr      equ     33
reclen  equ     14
fillen  equ     16
offdate equ     20


;the following are all of the segments used in the load order

coderes segment
coderes ends

datares segment byte
datares ends

init    segment byte
init    ends

tail    segment para
tail    ends

trancode        segment para
trancode        ends

trandata        segment byte
trandata        ends

transpace       segment byte
transpace       ends

resgroup        group   coderes,datares,init,tail
trangroup       group   trancode,trandata,transpace

;data for resident portion

datares segment byte
        org     0
zero    =       $
mesbas  dw      offset resgroup:err0
        dw      offset resgroup:err2
        dw      offset resgroup:err4
        dw      offset resgroup:err6
        dw      offset resgroup:err8
        dw      offset resgroup:err10
        dw      offset resgroup:err12
err0    db      "write protect$"
err2    db      "not ready$"
err4    db      "data$"
err6    db      "seek$"
err8    db      "sector not found$"
err10   db      "write fault$"
err12   db      "disk$"
read    db      "read$"
write   db      "writ$"
errmes  db      " error "
iotyp   db      "writing"
drvnum  db      " drive "
drvlet  db      "a"
newlin  db      13,10,"$"
request db      "abort, retry, ignore? $"
badfat  db      13,10,"file allocation table bad,$"
combad  db      13,10,"invalid command.com"
needcom db      13,10,"insert dos disk in "
        if      ibmver
        db      "drive a"
        else
        db      "default drive"
        endif
prompt  db      13,10,"and strike any key when ready",13,10,"$"
needbat db      13,10,"insert disk with batch file$"
endbatmes db    13,10,"terminate batch job (y/n)? $"
loading db      0
batfcb  db      1,"autoexecbat"
        db      21 dup(?)
        dw      0
        dw      0               ;initialize rr field to zero
parmtab dw      10 dup(-1)      ;no parameters initially
batch   db      1               ;assume batch mode initially
comfcb  db      comdrv,"command com"
        db      25 dup(?)
trans   dw      offset trangroup:command
trnseg  dw      ?
batbyt  db      ?
memsiz  dw      ?
sum     dw      ?
initadd db      4 dup(?)
resdatasize     equ     $-zero
datares ends

;data for transient portion

trandata        segment byte
        org     0
zero    equ     $
badnam  db      "bad command or file name",13,10,"$"
misnam  db      "missing file name$"
renerr  db      "duplicate file name or "
notfnd  db      "file not found$"
exebad  db      "error in exe file$"
nospace db      "insufficient disk space",13,10,"$"
fuldir  db      "file creation error",13,10,"$"
overwr  db      "file cannot be copied onto itself",13,10,"$"
losterr db      "content of destination lost before copy",13,10,"$"
copied  db      " file(s) copied$"
dirmes  db      " file(s)$"
toobig  db      "program too big to fit in memory$"
baddrv  db      "invalid drive specification$"
pausmes db      "strike a key when ready . . . $"
badswt  db      "illegal switch",13,10,"$"
weektab db      "sunmontuewedthufrisat"
baddat  db      13,10,"invalid date$"
curdat  db      "current date is $"
newdat  db      13,10,"enter new date: $"
badtim  db      13,10,"invalid time$"
curtim  db      "current time is $"
newtim  db      13,10,"enter new time: $"
suremes db      "are you sure (y/n)? $"

comtab  db      4,"dir",1
        dw      offset trangroup:catalog
        db      7,"rename",1
        dw      offset trangroup:rename
        db      4,"ren",1
        dw      offset trangroup:rename
        db      6,"erase",1
        dw      offset trangroup:erase
        db      4,"del",1
        dw      offset trangroup:erase
        db      5,"type",1
        dw      offset trangroup:typefil
        db      4,"rem",1
        dw      offset trangroup:command
        db      5,"copy",1
        dw      offset trangroup:copy
        db      6,"pause",1
        dw      offset trangroup:pause
        db      5,"date",0
        dw      offset trangroup:date
        db      5,"time",0
        dw      offset trangroup:time
        db      0               ;terminate command table

combuf  db      128,1,13

trandatasize    equ     $-zero
trandata        ends

;uninitialized transient data
transpace       segment byte
        org     0
zero    =       $
        db      128 dup(?)
tpa     dw      1 dup(?)
resseg  dw      1 dup(?)
chkdrv  db      1 dup(?)
filtyp  db      1 dup(?)
curdrv  db      1 dup(?)
parm1   db      1 dup(?)
parm2   db      1 dup(?)
comsw   dw      1 dup(?)
arg1s   dw      1 dup(?)
arg2s   dw      1 dup(?)
flager  db      1 dup(?)
cflag   db      1 dup(?)
specdrv db      1 dup(?)
bytcnt  dw      1 dup(?)
nxtadd  dw      1 dup(?)
lincnt  db      1 dup(?)
linlen  db      1 dup(?)
filecnt dw      1 dup(?)
exefcb  label word
idlen   db      1 dup(?)
id      db      8 dup(?)
com     db      3 dup(?)
dest    db      37 dup(?)
destname db     11 dup(?)
dirbuf  db      37 dup(?)
bits    dw      1 dup(?)
fullscr dw      1 dup(?)
exeend  dw      1 dup(?)
;header variables for exe file load
;these are overlapped with copy variables, below
runvar  label word
relpt   dw      1 dup(?)
relseg  dw      1 dup(?)
psize   label   word
pages   dw      1 dup(?)
relcnt  dw      1 dup(?)
headsiz dw      1 dup(?)
        dw      1 dup(?)
loadlow dw      1 dup(?)
initss  dw      1 dup(?)
initsp  dw      1 dup(?)
        dw      1 dup(?)
initip  dw      1 dup(?)
initcs  dw      1 dup(?)
reltab  dw      1 dup(?)
runvarsiz       equ     $-runvar

        db      80h dup(?)
stack   label   word

pretrlen        equ     $-zero          ;used later to compute trnlen

        org     runvar-zero                     ;overlaps exe variables

srcpt   dw      1 dup(?)
inexact db      1 dup(?)
append  db      1 dup(?)
nowrite db      1 dup(?)
ascii   db      1 dup(?)
plus    db      1 dup(?)
source  db      11 dup(?)
transpacesize   equ     $-zero
transpace       ends


;start of resident portion

coderes segment
assume  cs:resgroup,ds:resgroup,es:resgroup,ss:resgroup
        org     0
zero    =       $
parmbuf label   word

        org     100h

rstack  label   word

progstart:
        jmp     conproc

ltpa    dw      0               ;will store tpa segment here
myseg   dw      0               ;put our own segment here

contc:
        mov     ax,cs
        mov     ds,ax
        mov     ss,ax
        mov     sp,offset resgroup:rstack
        sti
        call    setvect
        mov     ah,dskreset
        int     33              ;reset disks in case files were open
        test    [batch],-1
        jz      lodcom
askend:
        mov     dx,offset resgroup:endbatmes
        mov     ah,printbuf
        int     33
        mov     ax,0c00h+inchar
        int     33
        and     al,5fh
        cmp     al,"n"
        jz      lodcom
        cmp     al,"y"
        jnz     askend
        mov     [batch],0
lodcom:
        mov     ax,cs
        mov     ss,ax
        mov     sp,offset resgroup:rstack
        mov     ds,ax
        call    setvect
        call    chksum
        cmp     dx,[sum]
        jz      havcom
        mov     [loading],1
        call    loadcom
chksame:
        call    chksum
        cmp     dx,[sum]
        jz      havcom
        call    wrongcom
        jmp     short chksame
havcom:
        mov     [loading],0
        mov     si,offset resgroup:ltpa
        mov     di,offset trangroup:tpa
        mov     es,[trnseg]
        cld
        movsw           ;move tpa segment to transient storage
        movsw           ;move resident segment too
        mov     ax,[memsiz]
        mov     word ptr es:[2],ax
        jmp     dword ptr [trans]

resident:
        add     dx,15
        mov     cl,4
        shr     dx,cl           ;number of paragraphs of new addition
        add     cs:[ltpa],dx
        xor     ax,ax
        mov     ds,ax
        jmp     dword ptr ds:[80h]              ;pretend user executed int 20h

dskerr:
        ;******************************************************
        ;       this is the default disk error handling code 
        ;       available to all users if they do not try to 
        ;       intercept interrupt 24h.
        ;******************************************************
        sti
        push    ds
        push    cs
        pop     ds              ;set up local data segment
        push    dx
        call    crlf
        pop     dx
        add     al,"a"          ;compute drive letter
        mov     [drvlet],al
        test    ah,80h          ;check if hard disk error
        jnz     faterr
        mov     si,offset resgroup:read
        test    ah,1
        jz      savmes
        mov     si,offset resgroup:write
savmes:
        lodsw
        mov     word ptr [iotyp],ax
        lodsw
        mov     word ptr [iotyp+2],ax
        and     di,0ffh
        cmp     di,12
        jbe     havcod
        mov     di,12
havcod:
        mov     di,word ptr [di+mesbas] ;get pointer to error message
        xchg    di,dx           ;may need dx later
        mov     ah,printbuf
        int     33              ;print error type
        mov     dx,offset resgroup:errmes
        int     33
        cmp     [loading],0
        jnz     getcomdsk
ask:
        mov     dx,offset resgroup:request
        mov     ah,printbuf
        int     33
        mov     ax,0c00h+inchar
        int     33              ;get response
        call    crlf
        or      al,20h          ;convert to lower case
        mov     ah,0            ;return code for ignore
        cmp     al,"i"          ;ignore?
        jz      exit
        inc     ah
        cmp     al,"r"          ;retry?
        jz      exit
        inc     ah
        cmp     al,"a"          ;abort?
        jnz     ask
exit:
        mov     al,ah
        mov     dx,di
        pop     ds
        iret

faterr:
        mov     dx,offset resgroup:badfat
        mov     ah,printbuf
        int     33
        mov     dx,offset resgroup:drvnum
        int     33
        mov     al,2            ;abort
        pop     ds
        iret

getcomdsk:
        mov     dx,offset resgroup:needcom
        mov     ah,printbuf
        int     33
        mov     ax,0c07h        ;get char without testing or echo
        int     33
        jmp     lodcom

crlf:
        mov     dx,offset resgroup:newlin
        push    ax
        mov     ah,printbuf
        int     33
        pop     ax
ret10:  ret

loadcom:
        push    ds
        mov     ds,[trnseg]
        mov     dx,100h
        mov     ah,setdma
        int     33
        pop     ds
        mov     dx,offset resgroup:comfcb
        mov     ah,open
        int     33              ;open command.com
        or      al,al
        jz      readcom
        mov     dx,offset resgroup:needcom
promptcom:
        mov     ah,printbuf
        int     33
        mov     ax,0c07h        ;get char without testing or echo
        int     33
        jmp     short loadcom
readcom:
        mov     word ptr[comfcb+rr],offset resgroup:transtart
        xor     ax,ax
        mov     word ptr[comfcb+rr+2],ax
        mov     [comfcb],al             ;use default drive
        inc     ax
        mov     word ptr[comfcb+reclen],ax
        mov     cx,comlen
        mov     dx,offset resgroup:comfcb
        mov     ah,rdblk
        int     33
        or      al,al
        jz      ret10
wrongcom:
        mov     dx,offset resgroup:combad
        jmp     short promptcom

chksum:
        cld
        push    ds
        mov     ds,[trnseg]
        mov     si,100h
        mov     cx,comlen
        shr     cx,1
        xor     dx,dx
chk:
        lodsw
        add     dx,ax
        loop    chk
        pop     ds
        ret

setvect:
        mov     dx,offset resgroup:lodcom
        mov     ax,2522h        ;set terminate address
        int     21h
        mov     dx,offset resgroup:contc
        mov     ax,2523h        ;set ctrl-c address
        int     21h
        mov     dx,offset resgroup:dskerr
        mov     ax,2524h        ;set hard disk error address
        int     33
        mov     dx,offset resgroup:resident
        mov     ax,2527h        ;set terminate and stay resident address
        int     33
        ret
rescodesize     equ     $-zero
coderes ends

;*******************************************************************
;start of init portion
;this code is overlayed the first time the tpa is used.

init    segment byte

        org     0
zero    =       $
conproc:
        mov     sp,offset resgroup:rstack

        if      highmem
        mov     ax,word ptr ds:[2]
        sub     ax,((rescodesize+resdatasize)+15)/16            ;subtract size of resident
        mov     word ptr ds:[2],ax
        mov     es,ax
        mov     si,100h
        mov     di,si
        mov     cx,((rescodesize+resdatasize)-100h+1)/2 ;length of resident in words
        rep     movsw                   ;move to end of memory
        mov     ds,ax
        mov     [ltpa],cs
        endif

        if      not highmem
        mov     ax,cs
        add     ax,((rescodesize+resdatasize)+15)/16            ;compute segment of tpa
        mov     [ltpa],ax
        mov     ax,word ptr ds:[2]
        endif

        mov     [myseg],ds
        mov     [memsiz],ax
        sub     ax,trnlen               ;subtract size of transient
        mov     [trnseg],ax
        call    setvect
        call    loadcom
        call    chksum
        mov     [sum],dx

        if msver
        if      highmem
        push    ds
        push    cs
        pop     ds
        endif
        mov     dx,offset resgroup:header
        mov     ah,printbuf
        int     33
        if      highmem
        pop     ds
        endif
        endif

        mov     dx,offset resgroup:batfcb
        mov     ah,open
        int     33                      ;see if autoexec.bat exists
        mov     word ptr[batfcb+reclen],1       ;set record length to 1
        or      al,al                   ;zero means file found
        jz      drv0
        mov     [batch],0               ;not found--turn off batch job
        mov     ax,offset trangroup:datinit
        mov     word ptr[initadd],ax
        mov     ax,[trnseg]
        mov     word ptr[initadd+2],ax
        call    dword ptr ds:[initadd]

        if ibmver
        mov     dx,offset resgroup:header
        mov     ah,printbuf
        int     33
        endif

drv0:
        jmp     havcom


        if msver
header  db      13,10,"command v. 1.17"
        if      highmem
        db      "h"
        endif
        db      13,10,"$"
        endif

        if ibmver
header  db      13,10,13,10,"the ibm personal computer dos",13,10
        db      "version 1.10 (c)copyright ibm corp 1981, 1982",13,10,"$"
        db      "licensed material - program property of ibm"
        endif

initsize        equ     $-zero
init    ends

;this tail segment is used to produce a para aligned label in the resident
; group which is the location where the transient segments will be loaded
; initialy.

tail    segment para
        org     0
transtart       label   word
tail    ends

;********************************************************************
;start of transient portion
;this code is loaded at the end of memory and may be overwritten by
;memory-intensive user programs.

trancode        segment para
assume  cs:trangroup,ds:trangroup,es:trangroup,ss:trangroup

wswitch equ     1               ;wide display during dir
pswitch equ     2               ;pause (or page) mode during dir
vswitch equ     4               ;verify during copy
aswitch equ     8               ;ascii mode during copy
bswitch equ     10h             ;binary mode during copy

        org     0
zero    =       $

        org     100h            ;allow for 100h parameter area

setdrv:
        mov     ah,seldrv
        int     21h
command:
        cld
        mov     ax,cs
        mov     ss,ax
        mov     sp,offset trangroup:stack
        mov     es,ax
        mov     ds,ax
        sti
        mov     ax,46*100h
        mov     dl,0
        int     33              ;turn off verify after write
        mov     ax,cs           ;get segment we're in
        sub     ax,[tpa]        ;ax=size ot tpa in paragraphs
        mov     dx,16
        mul     dx              ;dx:ax=size of tpa in bytes
        or      dx,dx           ;see if over 64k
        jz      savsiz          ;ok if not
        mov     ax,-1           ;if so, limit to 65535 bytes
savsiz:
        mov     [bytcnt],ax     ;max no. of bytes that can be buffered
        call    crlf2
getcom:
        mov     ah,getdrv
        int     21h
        mov     [curdrv],al
        add     al,"a"
        call    out             ;print letter for default drive
        mov     al,sym
        call    out
        mov     ds,[resseg]     ;all batch work must use resident seg.
assume  ds:resgroup
        test    [batch],-1
        jnz     readbat
        push    cs
        pop     ds              ;need local segment to point to buffer
assume  ds:trangroup
        mov     dx,offset trangroup:combuf
        mov     ah,inbuf
        int     21h             ;get a command
        jmp     docom

;all batch proccessing has ds set to segment of resident portion
assume  ds:resgroup
needparm:
        call    getbatbyt
        cmp     al,"%"          ;check for two consecutive %
        jz      savbatbyt
        cmp     al,13           ;check for end-of-line
        jz      savbatbyt
        sub     al,"0"
        jb      rdbat           ;ignore parameter reference if invalid
        cmp     al,9
        ja      rdbat
        cbw
        mov     si,ax
        shl     si,1            ;two bytes per entry
        mov     si,[si+offset resgroup:parmtab] ;get pointer to corresponding parameter
        cmp     si,-1           ;check if parameter exists
        jz      rdbat           ;ignore if it doesn't
        mov     ah,outch
rdparm:
        lodsb           ;from resident segment
        cmp     al,0dh          ;check for end of parameter
        jz      rdbat
        stosb           ;to transient segment
        mov     dl,al
        int     33              ;display paramters too
        jmp     short rdparm

promptbat:
        mov     ah,printbuf
        mov     dx,offset resgroup:needbat
        int     33              ;prompt for batch file
        mov     ah,printbuf
        mov     dx,offset resgroup:prompt
        int     33
        mov     ax,0c00h+inchar
        int     33
        jmp     command

badcomj1:jmp    badcom

readbat:
        mov     dx,offset resgroup:batfcb
        mov     ah,open
        int     33              ;make sure batch file still exists
        or      al,al
        jnz     promptbat       ;if open fails, prompt for disk
        mov     word ptr [batfcb+reclen],1
        mov     dx,offset resgroup:batbyt
        mov     ah,setdma
        int     33
        mov     di,offset trangroup:combuf+2
rdbat:
        call    getbatbyt
        cmp     al,"%"          ;check for parameter
        jz      needparm
savbatbyt:
        stosb
        call    out             ;display batched command line
        cmp     al,0dh
        jnz     rdbat
        sub     di,offset trangroup:combuf+3
        mov     ax,di
        mov     es:[combuf+1],al        ;set length of line
        call    getbatbyt       ;eat linefeed
        push    cs
        pop     ds              ;go back to local segment
assume ds:trangroup
docom:
;all segments are local for command line processing
        mov     al,10
        call    out
        mov     si,offset trangroup:combuf+2
        mov     di,offset trangroup:idlen
        mov     ax,2901h        ;make fcb with blank scan-off
        int     21h
        cmp     al,1            ;check for ambiguous command name
        jz      badcomj1        ;ambiguous commands not allowed
        cmp     al,-1
        jnz     drvgd
        jmp     drvbad
drvgd:
        mov     al,[di]
        mov     [specdrv],al
        mov     al," "
        mov     cx,9
        inc     di
        repne   scasb           ;count no. of letters in command name
        mov     al,9
        sub     al,cl
        mov     [idlen],al
        mov     di,81h
        mov     cx,0
        push    si
comtail:
        lodsb
        stosb           ;move command tail to 80h
        cmp     al,13
        loopnz  comtail
        not     cl
        mov     byte ptr ds:[80h],cl
        pop     si
;if the command has 0 parameters must check here for
;any switches that might be present.
;si -> first character after the command.
        mov     [flager],0      ;set error flag before any calls to switch 
        call    switch          ;is the next character a "/"
        mov     [comsw],ax
        mov     di,fcb
        mov     ax,2901h
        int     21h
        mov     [parm1],al      ;save result of parse
        call    switch
        mov     [arg1s],ax
        mov     di,fcb+10h
        mov     ax,2901h
        int     21h             ;parse file name
        mov     [parm2],al      ;save result
        call    switch
        mov     [arg2s],ax
        mov     al,[idlen]
        mov     dl,[specdrv]
        or      dl,dl           ;check if drive was specified
        jz      ok
        jmp     drvchk
ok:     dec     al              ;check for null command
        jnz     fndcom
        jmp     getcom

retsw:
        xchg    ax,bx           ;put switches in ax
        ret

switch:
        xor     bx,bx           ;initialize - no switches set
swloop:
        call    scanoff         ;skip any delimiters
        cmp     al,"/"          ;is it a switch specifier?
        jnz     retsw           ;no -- we're finished
        inc     si              ;skip over "/"
        call    scanoff
        inc     si
;convert lower case input to upper case
        cmp     al,"a"
        jb      savchr
        cmp     al,"z"
        ja      savchr
        sub     al,20h          ;lower-case changed to upper-case
savchr:
        mov     di,offset trangroup:swlist
        mov     cx,swcount
        repne   scasb                   ;look for matching switch
        jnz     badsw
        mov     ax,1
        shl     ax,cl           ;set a bit for the switch
        or      bx,ax
        jmp     short swloop

badsw:
        mov     [flager],1      ;record error in switch
        jmp     short swloop

swlist  db      "bavpw"
swcount equ     $-swlist

drvbad:
        mov     dx,offset trangroup:baddrv
        jmp     error

fndcom:
        mov     si,offset trangroup:comtab      ;prepare to search command table
        mov     ch,0
findcom:
        mov     di,offset trangroup:idlen
        mov     cl,[si]
        jcxz    external
        repe    cmpsb
        lahf
        add     si,cx           ;bump to next position without affecting flags
        sahf
        lodsb           ;get flag for drive check
        mov     [chkdrv],al
        lodsw           ;get address of command
        jnz     findcom
        mov     dx,ax
        cmp     [chkdrv],0
        jz      nocheck
        mov     al,[parm1]
        or      al,[parm2]      ;check if either parm. had invalid drive
        cmp     al,-1
        jz      drvbad
nocheck:call    dx
comjmp: jmp     command

badcomj:jmp     badcom

setdrv1:
        jmp     setdrv

drvchk:
        dec     dl              ;adjust for correct drive number
        dec     al              ;check if anything else is on line
        jz      setdrv1
external:
        mov     al,[specdrv]
        mov     [idlen],al
        mov     word ptr[com],4f00h+"c" ;"co"
        mov     byte ptr[com+2],"m"
        mov     dx,offset trangroup:idlen
        mov     ah,open
        int     33              ;check if command to be executed
        mov     [filtyp],al     ;0 for com files, -1 for exe files
        or      al,al
        jz      execute
        mov     word ptr[com],5800h+"e" ;"ex"
        mov     byte ptr[com+2],"e"
        int     33              ;check for exe file
        or      al,al
        jz      execute
        mov     word ptr[com],4100h+"b" ;"ba"
        mov     byte ptr[com+2],"t"
        int     33              ;check if batch file to be executed
        or      al,al
        jnz     badcomj
batcom:
;batch parameters are read with es set to segment of resident part
        mov     es,[resseg]
assume  es:resgroup
        mov     di,offset resgroup:parmtab
        mov     ax,-1
        mov     cx,10
        rep     stosw           ;zero parameter pointer table
        mov     si,offset trangroup:combuf+2
        mov     di,offset resgroup:parmbuf
        mov     bx,offset resgroup:parmtab
eachparm:
        call    scanoff
        cmp     al,0dh
        jz      havparm
        mov     es:[bx],di              ;set pointer table to point to actual parameter
        inc     bx
        inc     bx
movparm:
        lodsb
        call    delim
        jz      endparm         ;check for end of parameter
        stosb
        cmp     al,0dh
        jz      havparm
        jmp     short movparm
endparm:
        mov     al,0dh
        stosb           ;end-of-parameter marker
        cmp     bx,offset resgroup:parmtab+20   ;maximum number of parameters?
        jb      eachparm
havparm:
        mov     si,offset trangroup:idlen
        mov     di,offset resgroup:batfcb
        mov     cx,16
        rep     movsw           ;move into private batch fcb
        xor     ax,ax
        push    es
        pop     ds                      ;simply batch fcb setup
assume  ds:resgroup
        mov     word ptr[batfcb+rr],ax
        mov     word ptr[batfcb+rr+2],ax        ;zero rr field
        inc     ax
        mov     word ptr[batfcb+reclen],ax      ;set record length to 1 byte
        mov     [batch],al              ;flag batch job in progress
        jmp     command
assume  ds:trangroup,es:trangroup

execute:
        mov     ax,word ptr[idlen+16]
        or      ax,word ptr[idlen+18]           ;see if zero length
        jz      badcom                  ;if so, error
        xor     ax,ax
        mov     word ptr[idlen+rr],ax
        mov     word ptr[idlen+rr+2],ax         ;set rr field to zero
        inc     ax
        mov     word ptr[idlen+reclen],ax       ;set record length field to 1
        mov     dx,[tpa]
        mov     bx,dx
        mov     ah,setbase
        int     21h
        test    [filtyp],-1             ;check if file is com or exe
        jz      comload
        jmp     exeload
comload:push    ds
        mov     ds,dx
        mov     dx,100h
        mov     ah,setdma
        int     21h
        pop     ds
        mov     cx,[bytcnt]
        sub     cx,100h
        mov     dx,offset trangroup:idlen
        mov     ah,rdblk
        int     21h
        dec     al
        mov     dx,offset trangroup:toobig
        jnz     error
;set up exit conditions
        mov     cx,[bytcnt]
        mov     ds,bx
        mov     es,bx
        cli
        mov     ss,bx
        mov     sp,cx
        sti
        sub     cx,100h         ;allow some stack space
        xor     ax,ax
        push    ax
        mov     ax,100h
        push    bx
        push    ax
        call    setup
xxx     proc    far
        ret
xxx     endp
badcom:
        mov     dx,offset trangroup:badnam
error:
        mov     ah,printbuf
        int     21h
        jmp     command

chkcnt:
        test    [filecnt],-1
        jnz     enddir
        mov     dx,offset trangroup:notfnd
        jmp     error

enddir:
;make sure last line ends with cr/lf
        mov     al,[linlen]
        cmp     al,[lincnt]     ;will be equal if just had cr/lf
        jz      message
        call    crlf2
message:                
        mov     si,[filecnt]
        xor     di,di
        call    disp32bits
        mov     dx,offset trangroup:dirmes
        mov     ah,printbuf
        int     21h
        ret

catalog:
        mov     al,"?"                  ;*.* is default file spec.
        mov     di,5dh
        mov     cx,11
        rep     stosb
        mov     si,81h
        call    switch
        mov     di,5ch
        mov     ax,41*100h+0dh          ;parse with default name and extension
        int     33

;begin by processing any switches that may have been specified.
;bits will contain any information about switches that was
;found when the command line was parsed.

setswt:
        mov     ax,[comsw]              ;get switches from command
        or      ax,[arg1s]              ;or in switches from first parameter
        mov     [bits],ax
        mov     byte ptr[fullscr],linperpag
        test    al,1                    ;look for /w
        mov     al,normperlin
        jz      dir
        mov     al,wideperlin
dir:
        mov     [linlen],al             ;set number of entries per line
        mov     [lincnt],al
        mov     [filecnt],0     ;keep track of how many files found
        mov     dx,offset trangroup:dirbuf      ;set disk transfer address
        mov     ah,setdma
        int     21h             
        mov     ah,srchfrst
showdir:
        mov     dx,5ch          ;dx -> unopened fcb
        int     21h             ;search for a file to match fcb
        inc     al              ;ff = file not found
        jnz     again           ;either an error or we are finished
        jmp     chkcnt
again:
        inc     [filecnt]       ;keep track of how many we find
        mov     si,offset trangroup:dirbuf+1    ;si -> information returned by sys call
        call    shoname
        test    byte ptr[bits],1        ;/w set?
        jnz     nexent          ;if so, no size, date, or time
        call    dispsize        ;print size of file
        call    twospc
        mov     ax,word ptr[dirbuf+25]  ;get date
        or      ax,ax
        jz      nexent          ;skip if no date
        mov     dx,ax
        mov     cl,5
        shr     ax,cl           ;align month
        and     al,0fh
        mov     bh,"0"-" "      ;enable zero suppression
        call    out2
        mov     al,"-"
        call    out
        mov     al,dl
        and     al,1fh          ;mask to day
        call    out2
        mov     al,"-"
        call    out
        mov     al,dh
        shr     al,1            ;align year
        add     ax,80           ;relative 1980
        cmp     al,100
        jb      millenium
        sub     al,100
millenium:
        call    out2
        mov     bx,word ptr[dirbuf+23]  ;get time
        or      bx,bx           ;time field present?
        jz      nexent
        call    twospc  
        shr     bx,1
        shr     bx,1
        shr     bx,1
        shr     bl,1
        shr     bl,1            ;hours in bh, minutes in bl
        mov     al,bh
        mov     dh,"a"          ;assume a.m.
        cmp     al,12           ;in the afternoon?
        jb      morn
        mov     dh,"p"
        je      morn
        sub     al,12           ;keep it to 12 hours or less
morn:
        or      al,al           ;before 1 am?
        jnz     shohours
        mov     al,12
shohours:
        mov     bh,"0"-" "      ;enable zero suppression
        call    out2
        mov     al,":"
        call    out
        mov     al,bl           ;output minutes
        call    out2
        mov     al,dh           ;get "a" or "p"
        call    out
nexent:
        dec     [lincnt]
        jnz     samlin
nexlin:
        mov     al,[linlen]
        mov     [lincnt],al
        call    crlf2
        test    byte ptr[bits],2        ;/p switch present?
        jz      scroll          ;if not, just continue
        dec     byte ptr[fullscr]
        jnz     scroll
        mov     byte ptr[fullscr],linperpag
        mov     ah,printbuf
        mov     dx,offset trangroup:pausmes
        int     33
        mov     ax,0c08h        ;wait for any character to be typed
        int     21h
        call    crlf2
scroll:
        mov     ah,srchnxt
        jmp     showdir

samlin:
        mov     al,9            ;output a tab
        call    out
        jmp     short scroll

shoname:
        mov     cx,8
        call    outcnt
        call    onespc
        mov     cx,3
outcnt:
        lodsb
        call    out
        loop    outcnt
        ret

twospc:
        call    onespc
onespc:
        mov     al," "
        jmp     out

crlf2:
        mov     al,13
        call    out
        mov     al,10
        jmp     out

dispsize:
        mov     si,word ptr[dirbuf+29]
        mov     di,word ptr[dirbuf+31]
disp32bits:
;prints the 32-bit number di:si on the console in decimal. uses a total
;of 9 digit positions with leading blanks.
        xor     ax,ax
        mov     bx,ax
        mov     bp,ax
        mov     cx,32
convlp:
        shl     si,1
        rcl     di,1
        xchg    ax,bp
        call    convwrd
        xchg    ax,bp
        xchg    ax,bx
        call    convwrd
        xchg    ax,bx
        adc     al,0
        loop    convlp
; conversion complete. print 9-digit number.
        mov     cx,1810h        ;allow leading zero blanking for 8 digits
        xchg    dx,ax
        call    digit
        xchg    ax,bx
        call    outword
        xchg    ax,bp
outword:
        push    ax
        mov     dl,ah
        call    outbyte
        pop     dx
outbyte:
        mov     dh,dl
        shr     dl,1
        shr     dl,1
        shr     dl,1
        shr     dl,1
        call    digit
        mov     dl,dh
digit:
        and     dl,0fh
        jz      blankzer
        mov     cl,0
blankzer:
        dec     ch
        and     cl,ch
        or      dl,30h
        sub     dl,cl
        mov     ah,outch
        int     21h
        ret

convwrd:
        adc     al,al
        daa
        xchg    al,ah
        adc     al,al
        daa
        xchg    al,ah
ret20:  ret

erase:
        mov     cx,11
        mov     si,fcb+1
ambspec:        
        lodsb
        cmp     al,"?"
        jnz     allfil
        loop    ambspec
allfil: 
        cmp     cx,0
        jnz     noprmpt
askagn:         
        mov     dx,offset trangroup:suremes     ;"are you sure (y/n)?"
        mov     ah,printbuf
        int     21h
        mov     ax,0c00h+inchar
        int     21h
        and     al,5fh
        cmp     al,"n"
        jz      ret20
        cmp     al,"y"
        call    crlf2
        jz      noprmpt
        jmp     short askagn
noprmpt:
        mov     ah,delete
        mov     bx,offset trangroup:notfnd
        cmp     byte ptr ds:[fcb+1]," " ;check if parameter exists
        jmp     short opfile
rename:
        mov     ah,renam
        mov     bx,offset trangroup:renerr
        cmp     byte ptr ds:[fcb+16+1]," "  ;check if parameter exists
opfile:
        mov     dx,offset trangroup:misnam
        jz      errj            ;error if missing parameter
        mov     dx,fcb
        int     21h
        inc     al
        jnz     ret20
        mov     dx,bx
errj:   jmp     error

typefil:
        mov     ds,[tpa]
        xor     dx,dx
        mov     ah,setdma
        int     21h
        push    cs
        pop     ds
        mov     dx,fcb
        mov     ah,open
        int     21h
        or      al,al
        mov     dx,offset trangroup:notfnd
        jnz     errj
        xor     ax,ax
        mov     word ptr ds:[fcb+rr],ax ;set rr field
        mov     word ptr ds:[fcb+rr+2],ax
        inc     ax
        mov     word ptr ds:[fcb+reclen],ax     ;set record length
        mov     es,[tpa]
typelp:
        mov     dx,fcb
        mov     cx,[bytcnt]
        mov     ah,rdblk
        int     21h
        jcxz    ret30
        xor     si,si           ;start at 0 in tpa
outlp:
        lods    byte ptr es:[si]                ;in tpa segment
        cmp     al,1ah
        jz      ret30
        mov     ah,outch
        mov     dl,al
        int     21h
        loop    outlp
        jmp     short typelp

ret30:  ret                             ;need a nearby ret

copy:
        xor     ax,ax
        mov     [plus],al               ;will keep track of "+"s
        mov     [filecnt],ax
        mov     si,81h                  ;point to input line
        call    switch                  ;skip over switches on command
        mov     bp,ax
        mov     di,fcb
        call    parsnam                 ;scan first source
        mov     [parm1],dl              ;save ambiguous flag
        mov     [srcpt],si              ;save pointer to command line
;parse each name to find destination and check for /v switch
scannam:
        call    parse
        jnz     scannam
getdest:
        mov     di,offset trangroup:dest
        mov     bx,bp                   ;remeber switches so far
        xor     bp,bp                   ;must have dest. swtiches alone
        call    parsnam
        mov     [arg2s],bp              ;remember switches on destination
        jnz     havdestnam              ;file name present?
        inc     di                      ;point to file name spot
        mov     al,"?"                  ;substitute *.*
        mov     cx,11
        rep     stosb
havdestnam:
        or      bx,bp                   ;bx = all switches combined
        and     bl,vswitch              ;verify requested?
        jz      nover
        mov     ax,46*100h+1            ;set verify
        mov     dl,0
        int     33
nover:
        mov     di,offset trangroup:destname
        mov     si,offset trangroup:dest+1
        mov     bx,fcb+1
        call    buildname               ;see if we can make it unambiguous
        mov     di,offset trangroup:destname
        mov     al,"?"
        mov     cx,11
        repne   scasb                   ;scan for "?" to see if ambiguous
        mov     al,1                    ;flag if ambig.
        jz      ambig
        dec     ax                      ;al=0 if unambig.
ambig:
        mov     dl,al
        mov     ah,[plus]               ;1=found "+"
        xor     al,1                    ;0=ambig, 1=unambig destination
        and     al,[parm1]              ;source ambig. and dest unambig.
        or      al,ah                   ;or found "+" means concatenation
        mov     [ascii],al              ;concatenation implies ascii mode
        mov     [inexact],al            ;ascii implies inexact copy
        shl     al,1
        or      al,dl                   ;combine multiple and concat flags
        mov     [parm2],al
        mov     al,byte ptr[comsw]
        call    setasc                  ;check /a,/b on command
        mov     al,byte ptr[arg1s]
        call    setasc                  ;check for ascii on first filename
        mov     byte ptr[comsw],al              ;save starting switch values
        mov     ah,srchfrst
        call    search                  ;search for first source name
multdest:
        jz      firstsrc                ;find a first source name?
        test    [parm2],1               ;if multiple, we're done
        jnz     endcopy
        xor     ax,ax
        mov     [nxtadd],ax
        mov     [cflag],al              ;flag nothing read yet
nextsng:
        mov     di,fcb
        mov     si,[srcpt]
        call    parsesrc                ;parse next file name into fcb
        mov     [parm1],dl              ;remember if it's ambiguous
        mov     [srcpt],si
        jz      sngclos
        mov     ah,srchfrst
        call    search                  ;search for new file name
        jnz     nextsng                 ;if none, skip it and move to next name
readsng:
        call    checkread
sngloop:
        call    searchnext              ;see if any more of this name
        jz      readsng
        jmp     short nextsng

sngclos:
        call    closefil
endcopy:
        mov     si,[filecnt]
        xor     di,di
        call    disp32bits
        mov     dx,offset trangroup:copied
        mov     ah,printbuf
        int     21h
        jmp     command                 ;stack could be messed up

firstsrc:
        mov     si,offset trangroup:dirbuf+1
        mov     di,offset trangroup:source
        mov     cx,11
        rep     movsb                   ;copy first source name to source
        mov     si,offset trangroup:destname
        mov     di,offset trangroup:dest+1
        mov     bx,offset trangroup:source
        call    buildname               ;build destination name
        xor     ax,ax
        mov     [nxtadd],ax
        mov     [cflag],al
        mov     [append],al
        mov     [nowrite],al
        test    [parm2],1               ;multiple destinations?
        jz      noprt
        mov     si,offset trangroup:dirbuf+1
        call    shoname                 ;if so, show first source
        call    crlf2
noprt:
        call    compname                ;source and dest. the same?
        jnz     doread                  ;if not, read source in
        test    [parm2],2               ;concatenation?
        mov     dx,offset trangroup:overwr
        jz      coperrj                 ;if not, overwrite error
        mov     [append],1              ;set physical append
        mov     ah,open
        mov     dx,offset trangroup:dest
        int     33                      ;open (existing) destination
        cmp     [ascii],0               ;ascii flag set?
        jz      binaryapp
;ascii append. must find logical eof, then seek there with dest. fcb
        mov     [nowrite],1
        call    readin                  ;find eof
        call    flshfil                 ;seek there
        mov     [nowrite],0
        call    flshfil                 ;truncate file
        jmp     short snglchk

sngloopj:jmp    sngloop

coperrj:jmp     coperr

binaryapp:
        mov     word ptr[dest+reclen],1         ;set record length to 1
        mov     si,offset trangroup:dest+16             ;point to file size
        mov     di,offset trangroup:dest+rr
        movsw
        movsw                           ;seek to end of file
        mov     [cflag],1
        jmp     short snglchk
doread:
        call    readin
snglchk:
        test    [parm2],1               ;single or multiple destinations?
        jz      sngloopj
        mov     si,[srcpt]
multapp:
        call    parse
        jz      multclos
        push    si
        mov     si,offset trangroup:dirbuf+1
        mov     di,si
        mov     bx,offset trangroup:source
        call    buildname
        call    checkread
        pop     si
        jmp     short multapp
multclos:
        call    closefil
        mov     al,byte ptr[comsw]
        mov     [ascii],al              ;restore ascii flag
        call    searchnext
        jmp     multdest

parse:
        mov     di,offset trangroup:dirbuf
parsesrc:
        call    scanoff
        cmp     al,"+"
        jnz     retzf
        mov     [plus],1                ;keep track of "+" signs
        inc     si                      ;skip over it
parsnam:
        mov     ax,2901h
        int     33                      ;parse file name
        cmp     al,-1                   ;illegal?
        mov     dx,offset trangroup:baddrv
        jz      coperrj
        xchg    ax,dx                   ;save parse flag in dl
        mov     al,byte ptr[di]         ;get drive number
        or      al,al                   ;is it default?
        jnz     parsw
        mov     al,[curdrv]             ;substitute actual drive
        inc     ax
        mov     byte ptr[di],al
parsw:
        push    bx
        push    di
        call    switch                  ;process switches
        or      bp,ax                   ;combine all switches
        call    setasc                  ;check for /a or /b
        pop     di
        pop     bx
        cmp     byte ptr[di+1]," "              ;did we even get a file name?
        ret

retzf:
        xor     ax,ax
ret35:  ret

searchnext:
        mov     al,[parm1]              ;is name ambiguous?
        dec     al
        jnz     ret35                   ;don't perform search if not
        mov     ah,srchnxt
search:
        push    ax
        mov     ah,setdma
        mov     dx,offset trangroup:dirbuf
        int     33                      ;put result of search in dirbuf
        pop     ax                      ;restore search first/next command
        mov     dx,fcb
        int     33                      ;do the search
        or      al,al
        ret

setasc:
;given switch vector in ax, 
;       set ascii switch if /a is set
;       clear ascii switch if /b is set
;       leave ascii unchanged if neither or both are set
; also sets inexact if ascii is ever set. al = ascii on exit, flags set
        and     al,aswitch+bswitch
        jpe     loadsw                  ;pe means both or neither are set
        and     al,aswitch
        mov     [ascii],al
        or      [inexact],al
loadsw:
        mov     al,[ascii]
        or      al,al
        ret

buildname:
; [si] = ambiguous input file name
; [bx] = source of replacement characters
; [di] = destination
; file name is copied from [si] to [di]. if "?"s are encountered,
; they are replaced with the character in the same position at [bx].
        mov     cx,11
buildnam:
        lodsb
        cmp     al,"?"
        jnz     notambig
        mov     al,byte ptr[bx]
notambig:
        stosb
        inc     bx
        loop    buildnam
        ret

compname:
        mov     si,offset trangroup:dest
        mov     di,offset trangroup:dirbuf
        mov     cx,6
        repe    cmpsw
        ret

checkread:
;read file in (with readin) if not identical to destination
        call    compname                ;see if source and destination the same
        jnz     readin
        cmp     [append],0              ;if physical append, it's ok
        jnz     ret40
        mov     dx,offset trangroup:losterr             ;tell him he's not going to get it
        mov     ah,printbuf
        int     33
ret40:  ret

readin:
;open source file and read it in. if memory fills up, flush it out to
;destination and keep reading. if /a switch set, chop file at first ^z.
; inputs/outputs:
;       [nxtadd] has current pointer in buffer
;       [cflag] <>0 if destination has been created

        mov     dx,offset trangroup:dirbuf
        mov     ah,open
        int     21h
        or      al,al                   ;successful open?
        jnz     ret40                   ;if not, just ignore it
        xor     ax,ax
        mov     word ptr[dirbuf+rr],ax
        mov     word ptr[dirbuf+rr+2],ax
        inc     ax
        mov     word ptr[dirbuf+reclen],ax
copylp:
        mov     dx,[nxtadd]
        mov     ah,setdma
        push    ds
        mov     ds,[tpa]
        int     33
        pop     ds
        mov     cx,[bytcnt]
        sub     cx,dx                   ;compute available space
        mov     dx,offset trangroup:dirbuf
        mov     ah,rdblk                ;read in source file
        int     21h
        jcxz    ret40
        cmp     [ascii],0
        jz      binread
        mov     dx,cx
        mov     di,[nxtadd]
        mov     al,1ah
        push    es
        mov     es,[tpa]
        repne   scasb                   ;scan for eof
        pop     es
        jnz     useall
        inc     cx
useall:
        sub     dx,cx
        mov     cx,dx
binread:
        add     cx,[nxtadd]
        mov     [nxtadd],cx
        cmp     cx,[bytcnt]             ;is buffer full?
        jb      ret40                   ;if not, we must have found eof
        call    flshfil
        jmp     short copylp

closefil:
        mov     ax,[nxtadd]
        mov     bx,ax
        or      al,ah                   ;see if any data is loaded
        or      al,[cflag]              ;   or file was created
        jz      ret50                   ;don't close or count if not created
        mov     al,byte ptr[arg2s]
        call    setasc                  ;check for /b or /a on destination
        jz      binclos
        cmp     bx,[bytcnt]             ;is memory full?
        jnz     putz
        call    flshfil                 ;empty it to make room for 1 lousy byte
        xor     bx,bx
putz:
        push    ds
        mov     ds,[tpa]
        mov     word ptr[bx],1ah                ;add end-of-file mark (ctrl-z)
        pop     ds
        inc     [nxtadd]
binclos:
        call    flshfil
        cmp     [inexact],0             ;copy not exact?
        jnz     nodate                  ;if so, don't copy date & time
        mov     si,offset trangroup:dirbuf+offdate
        mov     di,offset trangroup:dest+offdate        ;make date & time same as original
        movsw                           ;copy date
        movsw                           ;copy time
nodate:
        mov     dx,offset trangroup:dest
        mov     ah,close
        int     21h
        inc     [filecnt]
ret50:  ret

flshfil:
;write out any data remaining in memory.
; inputs:
;       [nxtadd] = no. of bytes to write
;       [cflag] <>0 if file has been created
; outputs:
;       [nxtadd] = 0

        mov     al,1
        xchg    [cflag],al
        or      al,al
        jnz     exists
        cmp     [nowrite],0
        jnz     skpmak                  ;don't actually create if nowrite set
        mov     dx,offset trangroup:dest
        mov     ah,make
        int     21h
        mov     dx,offset trangroup:fuldir
        or      al,al
        jnz     coperr
skpmak:
        xor     ax,ax
        mov     word ptr[dest+rr],ax
        mov     word ptr[dest+rr+2],ax
        inc     ax
        mov     word ptr[dest+reclen],ax
exists:
        xor     cx,cx
        xchg    cx,[nxtadd]
        cmp     [nowrite],0             ;if nowrite set, just seek cx bytes
        jnz     seekend
        xor     dx,dx
        push    ds
        mov     ds,[tpa]
        mov     ah,setdma
        int     33
        pop     ds
        mov     dx,offset trangroup:dest
        mov     ah,wrblk
        int     21h
        or      al,al
        jz      ret60
        mov     dx,offset trangroup:dest
        mov     ah,close
        int     21h
        mov     ah,delete
        int     33
        mov     dx,offset trangroup:nospace
coperr:
        mov     ah,9
        int     21h
        jmp     endcopy

seekend:
        add     word ptr[dest+rr],cx
        adc     word ptr[dest+rr+2],0           ;propagate carry
ret60:  ret

getbatbyt:
;get one byte from the batch file and return it in al. end-of-file
;returns <cr> and ends batch mode. ds must be set to resident segment.
;ah, cx, dx destroyed.
assume  ds:resgroup
        mov     dx,offset resgroup:batfcb
        mov     ah,rdblk
        mov     cx,1
        int     33              ;get one more byte from batch file
        jcxz    bateof
        mov     al,[batbyt]
        cmp     al,1ah
        jnz     ret70
bateof:
        mov     al,0dh          ;if end-of-file, then end of line
        mov     [batch],0       ;and turn off batch mode
ret70:  ret
assume  ds:trangroup

scanoff:
        lodsb
        call    delim
        jz      scanoff
        dec     si              ;point to first non-delimiter
        ret

delim:
        cmp     al," "
        jz      ret80
        cmp     al,"="
        jz      ret80
        cmp     al,","
        jz      ret80
        cmp     al,9            ;check for tab character
ret80:  ret

pause:
        mov     dx,offset trangroup:pausmes
        mov     ah,printbuf
        int     33
        mov     ax,0c00h+inchar ;get character with kb buffer flush
        int     33
ret90:  ret

;date and time are set during initialization and use
;this routines since they need to do a long return

datinit:
        push    es
        push    ds              ;going to use the previous stack
        mov     ax,cs           ;set up the appropriate segment registers
        mov     es,ax
        mov     ds,ax
        mov     word ptr ds:[81h],13    ;want to prompt for date during initialization
        call    date
        call    time
        pop     ds
        pop     es
yyy     proc    far
        ret
yyy     endp

; date - gets and sets the time

date:
        mov     si,81h          ;accepting argument for date inline
        call    scanoff
        cmp     al,13
        jz      prmtdat
        mov     bx,2f00h+"-"    ;"/-"
        call    inline
        jmp     comdat

prmtdat:
        mov     dx,offset trangroup:curdat
        mov     ah,printbuf
        int     33              ;print "current date is "
        mov     ah,getdate
        int     33              ;get date in cx:dx
        cbw
        mov     si,ax
        shl     si,1
        add     si,ax           ;si=ax*3
        add     si,offset trangroup:weektab
        mov     bx,cx
        mov     cx,3
        call    outcnt
        mov     al," "
        call    out
        mov     ax,bx
        mov     cx,dx
        mov     dl,100
        div     dl
        xchg    al,ah
        xchg    ax,dx
        mov     bl,"-"
        call    show
getdat:
        mov     dx,offset trangroup:newdat
        mov     bx,2f00h+"-"    ;"/-" in bx
        call    getbuf
comdat: jz      ret90
        jc      daterr
        lodsb   
        cmp     al,bl
        jz      sepgd
        cmp     al,bh
        jnz     daterr
sepgd:  call    getnum
        jc      daterr
        mov     cx,1900
        cmp     byte ptr[si],13
        jz      bias
        mov     al,100
        mul     ah
        mov     cx,ax
        call    getnum
        jc      daterr
bias:
        mov     al,ah
        mov     ah,0
        add     cx,ax
        lodsb
        cmp     al,13
        jnz     daterr
        mov     ah,setdate
        int     33
        or      al,al
        jnz     daterr
        jmp     ret90
daterr:
        mov     dx,offset trangroup:baddat
        mov     ah,printbuf
        int     33
        jmp     getdat

; time gets and sets the time

time:
        mov     si,81h                  ;accepting argument for time inline
        call    scanoff
        cmp     al,13
        jz      prmttim
        mov     bx,3a00h+":"
        call    inline
        jmp     comtim

prmttim:
        mov     dx,offset trangroup:curtim
        mov     ah,printbuf
        int     33              ;print "current time is "
        mov     ah,gettime
        int     33              ;get time in cx:dx
        mov     bl,":"
        call    show
gettim:
        xor     cx,cx           ;initialize hours and minutes to zero
        mov     dx,offset trangroup:newtim
        mov     bx,3a00h+":"
        call    getbuf
comtim: jz      ret100          ;if no time present, don't change it
        jc      timerr
        mov     cx,dx
        xor     dx,dx
        lodsb
        cmp     al,13
        jz      savtim
        cmp     al,bl
        jnz     timerr
        mov     bl,"."
        call    getnum
        jc      timerr
        mov     dh,ah           ;position seconds
        lodsb
        cmp     al,13
        jz      savtim
        cmp     al,bl
        jnz     timerr  
        call    getnum
        jc      timerr
        mov     dl,ah
        lodsb
        cmp     al,13
        jnz     timerr
savtim:
        mov     ah,settime
        int     33
        or      al,al
        jz      ret100          ;error in time?
timerr:
        mov     dx,offset trangroup:badtim
        mov     ah,printbuf
        int     33              ;print error message
        jmp     gettim          ;try again

getbuf:
        mov     ah,printbuf
        int     33              ;print "enter new date: "
        mov     ah,inbuf
        mov     dx,offset trangroup:combuf
        int     33              ;get input line
        call    crlf2
        mov     si,offset trangroup:combuf+2
        cmp     byte ptr[si],13 ;check if new date entered
        jz      ret100
inline:
        call    getnum          ;get one or two digit number
        jc      ret100
        mov     dh,ah           ;put in position
        lodsb
        cmp     al,bl
        jz      next
        cmp     bl,":"          ;is it a date seperator?
        jnz     datesep
        dec     si
        mov     dl,0
ret100: ret                     ;time may have only an hour specified
datesep:
        cmp     al,bh
        stc
        jnz     ret100
next:   call    getnum
        mov     dl,ah           ;put in position
        ret

getnum:
        call    indig
        jc      ret100
        mov     ah,al           ;save first digit
        call    indig           ;another digit?
        jc      okret
        aad                     ;convert unpacked bcd to decimal
        mov     ah,al
okret:
        or      al,1
ret110: ret

indig:
        mov     al,byte ptr[si]
        sub     al,"0"
        jc      ret110
        cmp     al,10
        cmc
        jc      ret110
        inc     si
        ret

show:
        mov     al,ch
        mov     bh,"0"-" "      ;enable leading zero suppression
        call    out2
        mov     al,bl
        call    out
        mov     al,cl
        call    out2
        mov     al,bl
        call    out
        mov     al,dh
        call    out2
        cmp     bl,":"          ;are we outputting time?
        jnz     skipit
        mov     al,"."
        call    out
skipit: mov     al,dl
out2:   ;output binary number as two ascii digits
        aam                     ;convert binary to unpacked bcd
        xchg    al,ah
        or      ax,3030h        ;add "0" bias to both digits
        cmp     al,"0"          ;is msd zero?
        jnz     nosup
        sub     al,bh           ;suppress leading zero if enabled
nosup:
        mov     bh,0            ;disable zero suppression
        call    out
        mov     al,ah
out:
;print char in al without affecting registers
        xchg    ax,dx
        push    ax
        mov     ah,outch
        int     33
        pop     ax
        xchg    ax,dx
        ret

exeload:
        mov     ax,cs
        add     ax,loadseg
        mov     [exeend],ax     ;store in exeend
        mov     dx,offset trangroup:runvar      ;read header in here
        mov     ah,setdma
        int     33
        mov     cx,runvarsiz    ;amount of header info we need
        mov     dx,offset trangroup:exefcb
        mov     ah,rdblk
        int     33              ;read in header
        or      al,al
        jnz     badexe          ;must not reach eof
        mov     ax,[headsiz]    ;size of header in paragraphs
;convert header size to 512-byte pages by multiplying by 32 & rounding up
        add     ax,31           ;round up first
        mov     cl,5
        shr     ax,cl           ;multiply by 32
        mov     [exefcb+rr],ax  ;position in file of program
        mov     word ptr[exefcb+reclen],512 ;set record size
        add     bx,10h          ;first paragraph above parameter area
        mov     dx,[pages]      ;total size of file in 512-byte pages
        sub     dx,ax           ;size of program in pages
        mov     [psize],dx
        shl     dx,cl           ;convert pages back to paragraphs
        mov     ax,dx
        add     dx,bx           ;size + start = minimum memory (paragr.)
        mov     cx,[exeend]     ;get memory size in paragraphs
        cmp     dx,cx           ;enough memory?
        ja      shrterr
        mov     dx,[initsp]
        add     dx,15
        shr     dx,1
        shr     dx,1
        shr     dx,1
        shr     dx,1
        add     dx,[initss]
        add     dx,bx           ;adjusted value of sp
        cmp     dx,cx           ;is it valid?
        ja      shrterr
        cmp     [loadlow],-1    ;load low or high?
        jz      load            ;if low, load at segment bx
        sub     cx,ax           ;memory size - program size = load addr.
        mov     bx,cx
load:
        mov     bp,bx           ;save load segment
load1:
loadseg equ     (load1-zero)/16
        push    ds
        mov     ds,bx
        xor     dx,dx           ;address 0 in segment
        mov     ah,setdma
        int     33              ;set load address
        pop     ds
        mov     cx,[psize]      ;number of records to read
        mov     dx,offset trangroup:exefcb
        mov     ah,rdblk
        int     33              ;read in up to 64k
        sub     [psize],cx      ;decrement count by amount read
        jz      havexe          ;did we get it all?
        test    al,1            ;check return code if not
        jnz     badexe          ;must be zero if more to come
        add     bx,1000h-20h    ;bump data segment 64k minus one record
        jmp     short load1             ;get next 64k block

badexe:
        mov     dx,offset trangroup:exebad
        jmp     error

shrterr:
        mov     dx,offset trangroup:toobig
        jmp     error

havexe:
        mov     ax,[reltab]     ;get position of table
        mov     [exefcb+rr],ax  ;set in random record field
        mov     word ptr[exefcb+reclen],1  ;set one-byte record
        mov     dx,offset trangroup:relpt       ;4-byte buffer for relocation address
        mov     ah,setdma
        int     33
        cmp     [relcnt],0
        jz      norel
reloc:
        mov     ah,rdblk
        mov     dx,offset trangroup:exefcb
        mov     cx,4
        int     33              ;read in one relocation pointer
        or      al,al           ;check return code
        jnz     badexe
        mov     di,[relpt]      ;get offset of relocation pointer
        mov     ax,[relseg]     ;get segment
        add     ax,bp           ;bias segment with actual load segment
        mov     es,ax
        add     word ptr es:[di],bp             ;relocate
        dec     [relcnt]        ;count off
        jnz     reloc
;set up exit conditions
norel:
        mov     ax,[initss]
        add     ax,bp
        cli
        mov     ss,ax           ;initialize ss
        mov     sp,[initsp]
        sti
        add     [initcs],bp
        mov     ax,[tpa]        ;get pointer to parameter area
        mov     cx,[bytcnt]     ;size of tpa segment
        mov     es,ax
        mov     ds,ax           ;set segment registers to point to it
        call    setup
        jmp     dword ptr cs:[initip]   ;long jump to program

setup:
        and     cl,0f0h         ;adjust to even paragraph boundary
        mov     ax,word ptr ds:[6]              ;get current memory size
        sub     ax,cx           ;find out how much we're changing it
        mov     word ptr ds:[6],cx
        mov     cl,4
        sar     ax,cl           ;convert to a segment address
        add     word ptr ds:[8],ax              ;adjust long jump to go to same place
        mov     dx,80h
        mov     ah,setdma
        int     33              ;set default disk transfer address
        mov     ax,word ptr cs:[parm1]  ;pass on info about fcbs
        xor     cx,cx
        mov     dx,cx           ;assume no batch file
assume  cs:resgroup
        test    cs:[batch],-1   ;batch file in progress?
assume  cs:trangroup
        jz      ret120          ;if not, all set up
        mov     cx,cs:[resseg]
        mov     dx,offset resgroup:batfcb       ;cx:dx points to batch fcb
ret120: ret
trancodesize    equ     $-zero
trancode        ends
comlen  equ     trandatasize+trancodesize-102h          ;end of command load. zero needed to make comlen absolute
trnlen  equ     (pretrlen+trancodesize+trandatasize+15)/16              ;length of transient in paragraphs
        end     progstart
