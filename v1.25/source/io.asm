; i/o system for 86-dos version 1.20 and later. revised 8-02-82.
;
; assumes a cpu support card at f0 hex for character i/o,
; with disk drivers for scp, tarbell, or cromemco controllers.
;
; select whether console input is interrupt-driven or polled.
intinp:		equ	1
;
; select whether the auxiliary port is the support card parallel port
; or on channel 1 of a multiport serial card addressed at 10h.
parallelaux:	equ	1
serialaux:	equ	0
;
; select whether the printer is connected to the support card parallel
; output port (standard) or channel 0 of a multiport serial card
; addressed at 10h.
parallelprn:	equ	1
serialprn:	equ	0
;
; if the multiport serial was chosen for either the auxiliary or the
; printer, select the baud rate here. refer to multiport serial manual
; page 11 to pick the correct value for a given baud rate.
prnbaud:equ	7		; 1200 baud
auxbaud:equ	0fh		; 19200 baud
;
; select disk controller here.
scp:		equ	1
tarbellsd:	equ	0
tarbelldd:	equ	0
cromemco4fdc:	equ	0
cromemco16fdc:	equ	0
;
; select if you want a special conversion version which can read/write
; both the new microsoft format and the old scp format.
; for a two drive system, drives a and b are the new microsoft format,
; and drives c and d are the old scp format (where c is the same physical
; drive as a, and d is the same drive as b).  convert has no effect
; on 5.25-inch drives.
convert:equ	1
;
; select disk configuration:
large:	equ	1		; large drives.
combin:	equ	0		; two 8-inch and one 5.25-inch.
small:	equ	0		; three 5.25-inch drives.
custom:	equ	0		; user defined.
;
; if 8-inch drives are persci, select fastseek here:
; (fastseek with tarbell controllers doesn't work yet).
fastseek:	equ	1
;
; for double-density controllers, select double-sided operation of
; 8-inch disks in double-density mode.
largeds:	equ	0
;
; for double-density controllers, select double-sided operation of
; 5.25-inch disks in double-density mode.
smallds:	equ	0
;
; use table below to select head step speed. step times for 5" drives
; are double that shown in the table. times for fast seek mode (using
; persci drives) is very small - 200-400 microseconds.
;
; step value	1771	1793
;
;     0		 6ms	 3ms
;     1		 6ms	 6ms
;     2		10ms	10ms
;     3		20ms	15ms
;
stpspd:	equ	0
;
; ****** end of selections ********************************************
;
biosseg:equ	40h		; i/o system segment.
bioslen:equ	2048		; maximum length of i/o system.
doslen:	equ	8192		; maximum length of ms-dos.
qsize:	equ	80		; input queue size.
pbufsiz:equ	128		; size of print buffer
base:	equ	0f0h		; cpu support card base port number.
siobase:equ	10h		; base port number of multiport serial card.
stat:	equ	base+7		; serial i/o status port.
data:	equ	base+6		; serial i/o data port.
dav:	equ	2		; data available bit.
tbmt:	equ	1		; transmitter buffer empty bit.
serial:	equ	serialprn+serialaux
stcdata:equ	base+4		; ports for 9513 timer chip.
stccom:	equ	base+5

	if	serialaux
auxstat:equ	siobase+3
auxdata:equ	siobase+2
	endif

	if	parallelaux
auxstat:equ	base+13
auxdata:equ	base+12
	endif

	if	serialprn
prnstat:equ	siobase+1
prndata:equ	siobase+0
	endif

	if	parallelprn
prnstat:equ	base+13
prndata:equ	base+12
	endif

	org	0
	put	100h

	jmp	init
	jmp	status
	jmp	inp
	jmp	outp
	jmp	print
	jmp	auxin
	jmp	auxout
	jmp	read
	jmp	write
	jmp	dskchg
	jmp	setdate
	jmp	settime
	jmp	gettime
	jmp	flush
	jmp	mapdev
mapdev:
	ret	l

init:
	xor	bp,bp		; set up stack just below i/o system.
	mov	ss,bp
	mov	sp,biosseg*16

	if	intinp-1
	mov	al,0ffh		; mask all interrupts.
	outb	base+3
	endif

	if	intinp
	di			; set up keyboard interrupt vector.
	mov	[bp+64h],kbint
	mov	[bp+66h],cs
	ei
	endif

	mov	[bp+4*38h],prnfcb
	mov	[bp+4*38h+2],cs
	push	cs
	pop	ds
;
; initialize time-of-day clock.
;
	mov	si,stctab
	mov	cx,4		;initialize 4 registers
	up
initstc:
	lodb
	out	stccom		;select register to initialize
	lodb
	out	stcdata
	lodb
	out	stcdata
	loop	initstc

	if	serial
	mov	cx,4
serinit:
	lodb
	out	siobase+1
	out	siobase+3
	loop	serinit
	lodb			;baud rate for channel 0
	out	siobase+8
	lodb			;baud rate for channel 1
	out	siobase+9
	endif
;
; move ms-dos down to the first segment just above the i/o system.
;
	mov	si,bioslen	; source points to where ms-dos currently is.
	mov	ax,dosseg	; destination is beginning of dosseg.
	mov	es,ax
	sub	di,di
	mov	cx,doslen/2	; cx is number of words to move.
	rep
	movsw

	mov	si,inittab
	mov	dx,1		; do auto memory scan.
	call	0,dosseg
;
; change disk read and write vectors (int 37 and int 38) to go to
; directread and directwrite rather than read and write.
;
	sub	bp,bp
	mov	w,[bp+37*4],directread
	mov	w,[bp+38*4],directwrite

	mov	dx,100h
	mov	ah,26		;set dma address
	int	33
	mov	cx,[6]		;get size of segment
	mov	bx,ds		;save segment for later
;
; ds must be set to cs so we can point to the fcb.
;
	mov	ax,cs
	mov	ds,ax
	mov	dx,fcb		;file control block for command.com
	mov	ah,15
	int	33		;open command.com
	or	al,al
	jnz	comerr		;error if file not found
	xor	ax,ax
	mov	[fcb+33],ax	; set 4-byte random record field to
	mov	[fcb+35],ax	;  beginning of file.
	inc	ax
	mov	[fcb+14],ax	;set record length field
	mov	ah,39		;block read (cx already set)
	int	33
	jcxz	comerr		;error if no records read
	test	al,1
	jz	comerr		;error if not end-of-file
;
; make all segment registers the same.
;
	mov	ds,bx
	mov	es,bx
	mov	ss,bx
	mov	sp,5ch		;set stack to standard value
	xor	ax,ax
	push	ax		;put zero on top of stack for return
	mov	dx,80h
	mov	ah,26
	int	33		;set default transfer address (ds:0080)
	push	bx		;put segment on stack
	mov	ax,100h
	push	ax		;put address to execute within segment on stack
	ret	l		;jump to command

comerr:
	mov	dx,badcom
	mov	ah,9		;print string
	int	33
	ei
stall:	jp	stall

stctab:	db	17h		;select master mode register
	dw	84f3h		;enable time-of-day
	db	1		;counter 1 mode register
	dw	0138h
	db	2
	dw	0038h
	db	3
	dw	0008h		;set counter 3 to count days

	if	serial
	db	0b7h, 77h, 4eh, 37h, prnbaud, auxbaud
	endif

badcom:	db	13,10,"error in loading command interpreter",13,10,"$"
fcb:	db	1,"command com"
	ds	25
;
; ************ time and date ************
;
gettime:
	mov	al,0a7h		;save counters 1,2,3
	out	stccom
	mov	al,0e0h		;enable data pointer sequencing
	out	stccom
	mov	al,19h		;select hold 1 / hold cycle
	out	stccom
	call	stctime		;get seconds & 1/100's
	xchg	ax,dx
	call	stctime		;get hours & minutes
	xchg	ax,cx
	in	stcdata
	mov	ah,al
	in	stcdata
	xchg	al,ah		;count of days
	jp	pointstat

stctime:
	call	stcbyte
	mov	cl,ah
stcbyte:
	in	stcdata
	mov	ah,al
	shr	ah
	shr	ah
	shr	ah
	shr	ah
	and	al,0fh		;unpack bcd digits
	aad			;convert to binary
	mov	ah,al
	mov	al,cl
	ret

settime:
	push	cx
	push	dx
	call	load0		;put 0 into load registers to condition timer
	mov	al,43h		;load counters 1 & 2
	out	stccom
	pop	dx
	pop	cx
	call	load
	mov	al,43h
	out	stccom		;load counters 1&2
	call	load0
	mov	al,27h		;arm counters 1,2,3
	out	stccom
	jp	pointstat

load0:
	xor	cx,cx
	mov	dx,cx
load:
	mov	al,09		;counter 1 load register
	call	outdx
	mov	al,0ah		;counter 2 load register
	mov	dx,cx
outdx:
	out	stccom		;select a load register
	mov	al,dl
	call	outbcd
	mov	al,dh
outbcd:
	aam			;convert binary to unpacked bcd
	shl	ah
	shl	ah
	shl	ah
	shl	ah
	or	al,ah		;packed bcd
	out	stcdata
	ret

setdate:
	xchg	ax,dx		;put date in dx
	mov	al,0bh		;select counter 3 load register
	out	stccom
	xchg	ax,dx
	out	stcdata
	mov	al,ah
	out	stcdata
	mov	al,44h		;load counter 3
	out	stccom
pointstat:
	push	ax
	mov	al,1fh		;point to status register
	out	stccom		;   so power-off glitches won't hurt
	pop	ax
	ret	l
;
; ************ console input ************
;

	if	intinp-1	; non-interrupt driven input.
status:
	in	stat
	and	al,dav
	jz	nothing		; jump if nothing there.
	pushf			; save z flag.
	inb	data
	and	al,7fh
	seg	cs
	mov	[queue],al	; put new character in buffer.
	popf			; return with z flag clear.
	ret	l
nothing:
	seg	cs
	mov	al,[queue]	; see if there's anything in the buffer.
	not	al		; set up the z flag.
	test	al,80h
	pushf
	not	al
	popf
	ret	l

inp:
	mov	al,-1
	seg	cs
	xchg	al,[queue]	; remove the character from the buffer.
	and	al,al
	jns	inret		; return if we have a character.
inloop:
	in	stat		; wait till a character is available.
	and	al,dav
	jz	inloop
	in	data
	and	al,7fh
inret:
flush:
	ret	l

queue:	db	-1		; for storing characters from status to inp.
	endif

	if	intinp		; interrupt-driven input.
;
; console keyboard interrupt handler.
;
kbint:
	push	ax
	push	si
	mov	al,20h		;end of interrupt command
	out	base+2		;send to slave
	in	data		;get the character
	and	al,7fh
	cmp	al,"c"-"@"
	jz	flsh
	cmp	al,"s"-"@"
	jz	flsh
	cmp	al,"f"-"@"
	jnz	savky
flsh:
	call	13*3,biosseg	; call i/o system keyboard buffer flush.
savky:
	seg	cs
	mov	si,[rear]	;pointer to rear of queue
	call	incq
	seg	cs
	cmp	si,[front]	;any room in queue?
	jz	qfull
	seg	cs
	mov	[si],al		;put character in queue
	seg	cs
	mov	[rear],si	;save pointer
leavint:
	pop	si
	pop	ax
	iret
qfull:
	mov	al,7		; bell character.
	call	3*3,biosseg	; call i/o system console output function.
	jmps	leavint

status:
	push	si
;see if printer ready
	in	prnstat
	and	al,tbmt
	jz	noprn
	seg	cs
	mov	si,[pfront]
	seg	cs
	cmp	si,[prear]	;anything in print queue?
	jnz	sendprn
	seg	cs
	cmp	b,[prnfcb],-1	;print spooling in progress?
	jz	noprn		;if not, nothing to print
;print spooling in progress. get next buffer
	push	ds
	push	cs
	pop	ds
	push	ax
	push	cx
	push	dx
	push	[stksav]
	push	[stksav+2]
	push	[dmaadd]
	push	[dmaadd+2]
	mov	dx,pqueue
	mov	ah,26		;set dma address
	int	33
	mov	dx,prnfcb
	mov	cx,pbufsiz
	mov	ah,39		;read buffer
	int	33
	or	al,al
	jz	noteof
	mov	b,[prnfcb],-1	;turn off print spooling at eof
noteof:
	pop	[dmaadd+2]
	pop	[dmaadd]
	pop	[stksav+2]
	pop	[stksav]
	mov	si,cx
	pop	dx
	pop	cx
	pop	ax
	pop	ds
	or	si,si
	jz	noprn
	add	si,pqueue-1
	seg	cs
	mov	[prear],si
	mov	si,endpq-1
sendprn:
	call	incpq
	seg	cs
	mov	[pfront],si
	seg	cs
	lodsb			;get character to print
	out	prndata
noprn:
	di			; disable interrupts while checking queue.
	seg	cs
	mov	si,[front]
	seg	cs
	cmp	si,[rear]	; anything in queue?
	jz	nochr		; jump if nothing in queue.
	call	incq
	seg	cs
	lodsb			;get character (if there is one)
	or	si,si		;reset zero flag
nochr:
	ei
	pop	si
	ret	l		;zero clear if we have a character

inp:
	call	status,biosseg	; get i/o system console input status.
	jz	inp
	push	si
	di			; disable interrupts while changing queue pointers.
	seg	cs
	mov	si,[front]
	call	incq		; permanently remove char from queue
	seg	cs
	mov	[front],si
	ei
	pop	si
	ret	l

flush:
	di
	seg	cs
	mov	[rear],queue
	seg	cs
	mov	[front],queue
	ei
	ret	l

incq:
	inc	si
	cmp	si,endq		;exceeded length of queue?
	jb	ret
	mov	si,queue
	ret

incpq:
	inc	si
	cmp	si,endpq	;exceeded length of queue?
	jb	ret
	mov	si,pqueue
	ret

front:	dw	queue
rear:	dw	queue
queue:	ds	qsize
endq:	equ	$
pfront:	dw	pqueue
prear:	dw	pqueue
pqueue:	ds	pbufsiz
endpq:	equ	$
prnfcb:	db	-1
	ds	36
	endif

;
; ************ console and printer output ************
;
outp:
	push	ax
outlp:
	in	stat
	and	al,tbmt
	jz	outlp
	pop	ax
	out	data
	ret	l

print:
	push	si
	seg	cs
	mov	si,[prear]
	call	incpq
prinlp:
	seg	cs
	cmp	si,[pfront]
	jnz	prnchr
;print queue is full
	push	ax
	call	status,biosseg	;poll and maybe print something
	pop	ax
	jmps	prinlp
prnchr:
	seg	cs
	mov	[prear],si
	seg	cs
	mov	[si],al
	pop	si
	ret	l
;
; ************ auxiliary i/o ************
;
auxin:
	in	auxstat
	and	al,dav
	jz	auxin
	in	auxdata
	ret	l

auxout:
	push	ax
auxlp:
	in	auxstat
	and	al,tbmt
	jz	auxlp
	pop	ax
	out	auxdata
	ret	l
;
; ************ 1771/1793-type controller disk i/o ************
;
tarbell:equ	tarbellsd+tarbelldd
cromemco:equ	cromemco4fdc+cromemco16fdc

wd1791:	equ	scp+tarbelldd+cromemco16fdc
wd1771:	equ	tarbellsd+cromemco4fdc

	if	wd1791
readcom:equ	80h
writecom:equ	0a0h
	endif

	if	wd1771
readcom:equ	88h
writecom:equ	0a8h
	endif

	if	scp
smallbit:equ	10h
backbit:equ	04h
ddenbit:equ	08h
donebit:equ	01h
disk:	equ	0e0h
	endif

	if	tarbell
backbit:equ	40h
ddenbit:equ	08h
donebit:equ	80h
disk:	equ	78h
	endif

	if	cromemco
smallbit:equ	10h
backbit:equ	0fdh		; send this to port 4 to select back.
ddenbit:equ	40h
donebit:equ	01h
disk:	equ	30h
	endif

	if	smallds-1
smallddsect:	equ	8
	endif

	if	smallds
smallddsect:	equ	16
	endif

	if	largeds-1
largeddsect:	equ	8
	endif

	if	largeds
largeddsect:	equ	16
	endif
;
; disk change function.
; on entry:
;	al = disk drive number.
; on exit:
;	ah = -1 (ff hex) if disk is changed.
;	ah = 0 if don't know.
;	ah = 1 if not changed.
;
;	cf clear if no disk error.
;	al = disk i/o driver number.
;
;	cf set if disk error.
;	al = disk error code (see disk read below).
;
	if	wd1771
dskchg:
	mov	ah,0		; ah = 0 in case we don't know.
	seg	cs
	cmp	al,[curdrv]
	jnz	retl
	push	ax		; save drive number.

	if	cromemco
	inb	disk+4
	endif

	if	tarbell
	inb	disk
	endif

	and	al,20h		; look at head load bit
	pop	ax
	jz	retl
	mov	ah,1		; ah = 1, disk not changed.
retl:
	clc			; no disk error.
	ret	l
	endif			; end of 1771 dskchg.

	if	wd1791
dskchg:
	mov	ah,0		; ah = 0 in case we don't know.
	seg	cs
	cmp	al,[curdrv]
	jnz	denschk		; check density if not same drive.
	push	ax

	if	scp+cromemco
	inb	disk+4
	endif

	if	tarbell
	inb	disk
	endif

	and	al,20h		; look at head load bit
	pop	ax
	jz	denschk		; check density if head not loaded.
	mov	ah,1		; ah = 1, disk not changed.
	mov	bx,prevdens
	seg	cs
	xlat			; get previous density
	clc			; no disk error.
	ret	l
denschk:
	call	chknew		; unload head if selecting new drive.
	cbw
	xchg	ax,si
	add	si,prevdens
	mov	cx,4		; try each density twice
	mov	ah,0		; disk may not have been changed.
chkdens:
	seg	cs
	mov	al,[si]		; get previous disk i/o driver number.
	mov	bx,drvtab
	seg	cs
	xlat			; get drive select byte for previous density

	if	cromemco16fdc
	call	motor		; wait for motor to come up to speed.
	endif

	out	disk+4		; select disk
	mov	al,0c4h		; read address command
	call	dcom
	and	al,98h
	in	disk+3		; eat last byte to reset drq
	jz	havdens		; jump if no error in reading address.
	not	ah		; ah = -1 (disk changed) if new density works.
	seg	cs
	xor	b,[si],1	; try other density
	loop	chkdens
	mov	ax,2		; couldn't read disk at all, ah = 0 for don't 
	stc			;  know if disk changed, al = error code 2 -
	ret	l		;  disk not ready, carry set to indicate error.

havdens:
	seg	cs
	lodsb			; al = disk i/o driver number.
	clc			; no disk error.
	ret	l

prevdens:db	1,3,5,7,9,11,13	; table of previous disk i/o driver numbers.
	endif			; end of 1793 dskchg function.

chknew:
	mov	ah,al		; save disk drive number in ah.
	seg	cs		; al = previous disk drive number,
	xchg	al,[curdrv]	;  make new drive current.
	cmp	al,ah		; changing drives?
	jz	ret
;
; if changing drives, unload head so the head load delay one-shot will
; fire again. do it by seeking to the same track with the h bit reset.
;
	in	disk+1		; get current track number
	out	disk+3		; make it the track to seek to
	mov	al,10h		; seek and unload head
	call	dcom
	mov	al,ah		; restore current drive number
	ret

	if	cromemco16fdc
motor:
	push	ax
	mov	ah,al
	in	disk+4		; see if the motor is on.
	test	al,08h
	mov	al,ah
	outb	disk+4		; select drive & start motor.
	jnz	motorson	; no delay if motors already on.
	push	cx
	mov	cx,43716	; loop count for 1 second.
motordelay:			;  (8 mhz, 16-bit memory).
	aam			; 83 clocks.
	aam			; 83 clocks.
	loop	motordelay	; 17 clocks.
	pop	cx
motorson:
	pop	ax
	ret
	endif
;
; disk read function.
;
; on entry:
;	al = disk i/o driver number
;	bx = disk transfer address in ds
;	cx = number of sectors to transfer
;	dx = logical record number of transfer
; on exit:
;	cf clear if transfer complete
;
;	cf set if hard disk error.
;	cx = number of sectors left to transfer.
;	al = disk error code
;		0 = write protect error
;		2 = not ready error
;		4 = "data" (crc) error
;		6 = seek error
;		8 = sector not found
;	       10 = write fault
;	       12 = "disk" (none of the above) error
;
read:
	call	seek		;position head
	jc	error
	push	es		; make es same as ds.
	mov	bx,ds
	mov	es,bx
rdlp:
	call	readsect	;perform sector read
	jc	popeserror
	inc	dh		;next sector number
	loop	rdlp		;read each sector requested
	clc			; no errors.
	pop	es		; restore es register.
	ret	l
;
; disk write function.
; registers same on entry and exit as read above.
;
write:
	call	seek		;position head
	jc	error
wrtlp:
	call	writesect	;perform sector write
	jc	error
	inc	dh		;bump sector counter
	loop	wrtlp		;write cx sectors
	clc			; no errors.
writeret:
	ret	l

popeserror:
	pop	es		; restore es register.
error:
	mov	bl,-1
	seg	cs
	mov	[di],bl		; indicate we don't know where head is.
	mov	si,errtab
getcod:
	inc	bl		; increment to next error code.
	seg	cs
	lodb
	test	ah,al		; see if error code matches disk status.
	jz	getcod		; try another if not.
	mov	al,bl		; now we've got the code.
	shl	al		; multiply by two.
	stc
	ret	l

errtab:
	db	40h		;write protect error
	db	80h		;not ready error
	db	8		;crc error
	db	2		;seek error
	db	10h		;sector not found
	db	20h		;write fault
	db	7		;"disk" error
;
; direct disk read and write from int 37 and int 38.  subroutine getiodriver
; calls dskchg to convert disk drive number to i/o driver number.
;
; setting curdrv to -1 before calling dskchg forces dskchg to check the disk's
; density before returning the i/o driver number.  this is necessary because
; programs such as format could change the density of a disk and leave the
; head loaded.  if the head is loaded dskchg assumes the disk hasn't been
; changed and returns the old i/o driver number which could be wrong.
;
; curdrv is set to -1 before returning so when dskchg is called by the
; operating system, it will tell the operating system the disk may have
; been changed (because it may have been).
;
directread:

	if	wd1791
	call	getiodriver	; convert drive number to i/o driver number.
	jc	directret	; return if dskchg returned error.
	endif

	call	7*3,biosseg	; call read.
	jmps	directret

directwrite:

	if	wd1791
	call	getiodriver	; convert drive number to i/o driver number.
	jc	directret	; return if dskchg returned error.
	endif

	call	8*3,biosseg	; call write.
directret:
	seg	cs
	mov	b,[curdrv],-1	; force dskchg to do density check.
	ret	l

	if	wd1791
getiodriver:
	seg	cs
	mov	b,[curdrv],-1	; force dskchg to do density check.
	push	bx
	push	cx
	call	9*3,biosseg	; call dskchg.
	pop	cx
	pop	bx
	ret
	endif
;
; function:
;	seeks to proper track.
; on entry:
;	same as for disk read or write above.
; on exit:
;	ah = drive select byte
;	dl = track number
;	dh = sector number
;	si = disk transfer address in ds
;	di = pointer to drive's track counter in cs
;	cx unchanged (number of sectors)
;
seek:
	mov	si,bx		; save transfer address
	cbw
	mov	bx,ax		; prepare to index on drive number

	if	wd1791		; if two disk formats per drive.
	shr	al		; convert to physical disk drive number.
	endif

	call	chknew		; unload head if changing drives.
	seg	cs
	mov	al,[bx+drvtab]	; get drive-select byte.

	if	cromemco16fdc
	call	motor		; wait for the motors to come up to speed.
	endif

	outb	disk+4		; select drive.

	if	cromemco
	or	al,80h		; set auto-wait bit.
	endif

	mov	ah,al		; save drive-select byte in ah.
	xchg	ax,dx		; ax = logical sector number.
	mov	dl,26		; 26 sectors/track unless changed below

	if	scp
	test	dh,smallbit	; check if small disk.
	jz	bigone		; jump if big disk.
	mov	dl,18		; assume 18 sectors on small track.
	test	dh,ddenbit	; check if double-density.
	jz	havsect		; jump if not.
	mov	dl,smallddsect	; number of sectors on small dd track.
	jp	havsect
bigone:
	test	dh,ddenbit	; check if double-density.
	jz	havsect		; jump if not.
	mov	dl,largeddsect	; number of sectors on big dd track.
	endif

	if	tarbelldd	; tarbell dd controller.
	test	dh,ddenbit	; check for double-density.
	jz	havsect
	mov	dl,largeddsect	; number of sectors on dd track.
	endif

	if	cromemco4fdc
	test	dh,smallbit	; check if small disk.
	jnz	havsect		; jump if not.
	mov	dl,18		; 18 sectors on small disk track.
	endif

	if	cromemco16fdc
	test	dh,smallbit	; check if small disk.
	jnz	bigone		; jump if big disk.
	mov	dl,18		; assume 18 sectors on small track.
	test	dh,ddenbit	; check if double-density.
	jz	havsect		; jump if not.
	mov	dl,smallddsect	; number of sectors on small dd track.
	jp	havsect
bigone:
	test	dh,ddenbit	; check if double-density.
	jz	havsect		; jump if not.
	mov	dl,largeddsect	; number of sectors on big dd track.
	endif

havsect:
	div	al,dl		; al = track, ah = sector.
	xchg	ax,dx		; ah has drive-select byte, dx = track & sector.
	inc	dh		; sectors start at one, not zero.
	seg	cs
	mov	bl,[bx+trkpt]	; get this drive's displacement into track table.
	add	bx,trktab	; bx now points to track counter for this drive.
	mov	di,bx
	mov	al,dl		; move new track number into al.
	seg	cs
	xchg	al,[di]		; xchange current track with desired track
	out	disk+1		; inform controller chip of current track
	cmp	al,dl		; see if we're at the right track.
	jz	ret
	mov	bh,2		; seek retry count
	cmp	al,-1		; head position known?
	jnz	nohome		; if not, home head
trysk:
	call	home
	jc	seekerr
nohome:
	mov	al,dl		; al = new track number.
	out	disk+3
	mov	al,1ch+stpspd	; seek command.
	call	movhead
	and	al,98h		; accept not ready, seek, & crc error bits.
	jz	ret
	js	seekerr		; no retries if not ready
	dec	bh
	jnz	trysk
seekerr:
	mov	ah,al		; put status in ah.
	test	al,80h		; see if it was a not ready error.
	stc
	jnz	ret		; status is ok for not ready error.
	mov	ah,2		; everything else is seek error.
	ret

setup:
	mov	bl,dh		; move sector number to bl to play with

	if	scp+cromemco16fdc
	test	ah,ddenbit	; check for double density.
	jz	checksmall	; not dd, check size for sd.
	endif

	if	tarbelldd
	test	ah,ddenbit	; check for double density.
	jz	check26		; not dd.
	endif

	if	wd1791

	if	(scp+tarbell)*largeds+scp*smallds
	mov	al,ah		; select front side of disk.
	out	disk+4
	endif

	if	cromemco*(largeds+smallds)
	mov	al,0ffh		; select front side of disk.
	out	04h
	endif

	cmp	bl,8		; see if legal dd sector number.
	jbe	putsec		; jump if ok.

	if	(largeds-1)*((smallds*(scp+cromemco))-1)
	jp	step		; if only ss drives, we gotta step.
	endif

	if	scp*largeds*(smallds-1)
	test	ah,smallbit	; check for 5.25 inch disk.
	jnz	step		; jump if small because smallds is off.
	endif

	if	scp*smallds*(largeds-1)
	test	ah,smallbit	; check for 8 inch disk.
	jz	step		; jump if large because largeds is off.
	endif

	if	cromemco16fdc*largeds*(smallds-1)
	test	ah,smallbit	; check for 5.25 inch disk.
	jz	step		; jump if small because smallds is off.
	endif

	if	cromemco16fdc*smallds*(largeds-1)
	test	ah,smallbit	; check for 8 inch disk.
	jnz	step		; jump if large because largeds is off.
	endif

	if	largeds+smallds*(scp+cromemco)
	sub	bl,8		; find true sector for back side.
	cmp	bl,8		; see if ok now.
	ja	step		; have to step if still too big.

	if	scp+tarbelldd
	mov	al,ah		; move drive select byte into al.
	or	al,backbit	; select back side.
	out	disk+4
	endif

	if	cromemco16fdc
	mov	al,backbit	; select back side.
	out	04h
	endif

	jp	putsec
	endif

	endif

	if	scp
checksmall:
	test	ah,smallbit	; see if big disk.
	jz	check26		; jump if big.
	endif

	if	cromemco
checksmall:
	test	ah,smallbit	; see if big disk.
	jnz	check26		; jump if big.
	endif

	if 	scp+cromemco
	cmp	bl,18		; see if legal small sd/ss sector.
	ja	step		; jump if not.
	endif

check26:
	cmp	bl,26		; see if legal large sd/ss sector.
	jbe	putsec		; jump if ok.
step:
	inc	dl		; increment track number.
	mov	al,58h		; step in with update.
	call	dcom
	seg	cs
	inc	b,[di]		; increment the track pointer.
	mov	dh,1		; after step, do first sector.
	mov	bl,dh		; fix temporary sector number also.
putsec:
	mov	al,bl		; output sector number to controller.
	out	disk+2
	di			; interrupts not allowed until i/o done

	if	scp+cromemco
	inb	disk+4		; get head-load bit.
	endif

	if	tarbell
	inb	disk
	endif

	not	al
	and	al,20h		; check head load status
	jz	ret
	mov	al,4
	ret

readsect:
	call	setup
	mov	bl,10		; retry count for hard error.
	xchg	di,si		; transfer address to di.
	push	dx		; save track & sector number.
	mov	dl,disk+3	; disk controller data port.
rdagn:
	or	al,readcom
	out	disk

	if	cromemco
	mov	al,ah		; turn on auto-wait.
	out	disk+4
	endif

	mov	bp,di		; save address for retry.
	jmps	rloopentry
rloop:
	stob			; write into memory.
rloopentry:

	if	scp
	in	disk+5		; wait for drq or intrq.
	endif

	if	tarbell+cromemco
	in	disk+4
	endif

	if	tarbell
	shl	al
	inb	dx		; read data from disk controller chip.
	jc	rloop
	endif

	if	scp+cromemco
	shr	al
	inb	dx		; read data from disk controller chip.
	jnc	rloop
	endif

	ei			; interrupts ok now
	call	getstat
	and	al,9ch
	jz	rdpop
	mov	di,bp		; get origainal address back for retry.
	mov	bh,al		; save error status for report
	mov	al,0
	dec	bl
	jnz	rdagn
	mov	ah,bh		; put error status in ah.
	stc
rdpop:
	pop	dx		; get back track & sector number.
	xchg	si,di		; address back to si.

	if	tarbell
forcint:
	mov	al,0d0h		; tarbell controllers need this force interrupt
	out	disk		;  so that type i status is always available
	mov	al,10		;  at the 1771/1793 status port so we can find
intdly:				;  out if the head is loaded.  scp and cromemco
	dec	al		;  controllers have head-load status available
	jnz	intdly		;  at the disk+4 status port.
	endif

	ret

writesect:
	call	setup
	mov	bl,10
	push	dx		; save track & sector number.
	mov	dl,disk+3	; disk controller data port.
wrtagn:
	or	al,writecom
	out	disk

	if	cromemco
	mov	al,ah		; turn on auto-wait.
	out	disk+4
	endif

	mov	bp,si
wrloop:

	if	scp
	inb	disk+5
	endif

	if	tarbell+cromemco
	inb	disk+4
	endif

	if	scp+cromemco
	shr	al
	lodb			; get data from memory.
	outb	dx		; write to disk.
	jnc	wrloop
	endif

	if	tarbell
	shl	al
	lodb			; get data from memory.
	outb	dx		; write to disk.
	jc	wrloop
	endif

	ei			; interrupts ok now.
	dec	si
	call	getstat
	and	al,0fch
	jz	wrpop
	mov	si,bp
	mov	bh,al
	mov	al,0
	dec	bl
	jnz	wrtagn
	mov	ah,bh		; error status to ah.
	stc
wrpop:
	pop	dx		; get back track & sector number.

	if	tarbell
	jmps	forcint
	endif

	if	scp+cromemco
	ret
	endif
;
; subroutine to restore the read/write head to track 0.
;
	if	scp+cromemco+tarbell*(fastseek-1)
home:
	endif

	if	fastseek*cromemco
	test	ah,smallbit	; check for large disk.
	jnz	restore		; big disks are fast seek persci.
	endif

	mov	bl,3
tryhom:

	if	scp*fastseek
	mov	al,ah		; turn on restore to persci.
	or	al,80h
	outb	disk+4
	endif

	mov	al,0ch+stpspd	; restore with verify command.
	call	dcom
	and	al,98h

	if	scp*fastseek
	mov	al,ah		; restore off.
	outb	disk+4
	endif

	jz	ret
	js	homerr		; no retries if not ready
	mov	al,58h+stpspd	; step in with update
	call	dcom
	dec	bl
	jnz	tryhom
homerr:
	stc
	ret
;
; restore for persci drives.
; doesn't exist yet for tarbell controllers.
;
	if	fastseek*tarbell
home:
restore:
	ret
	endif

	if	fastseek*cromemco4fdc
restore:
	mov	al,0c4h		;read address command to keep head loaded
	out	disk
	mov	al,77h
	out	4
chkres:
	in	4
	and	al,40h
	jz	resdone
	in	disk+4
	test	al,donebit
	jz	chkres
	in	disk
	jp	restore		;reload head
resdone:
	mov	al,7fh
	out	4
	call	getstat
	mov	al,0
	out	disk+1		;tell 1771 we're now on track 0
	ret
	endif

	if	fastseek*cromemco16fdc
restore:
	mov	al,0d7h		; turn on drive-select and restore.
	outb	4
	push	ax
	aam			; 10 us delay.
	pop	ax
reswait:
	inb	4		; wait till seek complete is active.
	test	al,40h
	jnz	reswait
	mov	al,0ffh		; turn off drive-select and restore.
	outb	4
	sub	al,al		; tell 1793 we're on track 0.
	outb	disk+1
	ret
	endif
;
; subroutine to move the read/write head to the desired track.
; usually falls through to dcom unless special handling for
; persci drives is required in which case go to fastsk.
;
	if	scp+cromemco+tarbell*(fastseek-1)
movhead:
	endif

	if	cromemco*fastseek
	test	ah,smallbit	; check for persci.
	jnz	fastsk
	endif

dcom:
	out	disk
	push	ax
	aam			;delay 10 microseconds
	pop	ax
getstat:
	in	disk+4
	test	al,donebit

	if	tarbell
	jnz	getstat
	endif

	if	scp+cromemco
	jz	getstat
	endif

	in	disk
	ret
;
; fast seek code for persci drives.
; tarbell not installed yet.
;
	if	fastseek*tarbell
movhead:
fastsk:
	ret
	endif

	if	fastseek*cromemco
fastsk:
	mov	al,6fh
	out	4
	mov	al,18h
	call	dcom
skwait:
	in	4
	test	al,40h
	jnz	skwait
	mov	al,7fh
	out	4
	mov	al,0
	ret
	endif

curdrv:	db	-1
;
; explanation of tables below.
;
; drvtab is a table of bytes which are sent to the disk controller as drive-
; select bytes to choose which physical drive is selected for each disk i/o
; driver.  it also selects whether the disk is 5.25-inch or 8-inch, single-
; density or double-density.  always select side 0 in the drive-select byte if
; a side-select bit is available.  there should be one entry in the drvtab
; table for each disk i/o driver.  exactly which bits in the drive-select byte
; do what depends on which disk controller is used.
;
; trktab is a table of bytes used to store which track the read/write
; head of each drive is on.  each physical drive should have its own
; entry in trktab.
;
; trkpt is a table of bytes which indicates which trktab entry each
; disk i/o driver should use.  since each physical drive may be used for
; more than one disk i/o driver, more than one entry in trkpt may point
; to the same entry in trktab.  drives such as persci 277s which use
; the same head positioner for more than one drive should share entrys
; in trktab.
;
; inittab is the initialization table for 86-dos as described in the
; 86-dos programer's manual under "customizing the i/o system."
;
	if	scp*combin*fastseek
;
; a persci 277 or 299 and one 5.25-inch drive.
;
drvtab:	db	00h,08h,01h,09h,10h,18h,00h,08h,01h,09h
trkpt:	db	0,0,0,0,1,1,0,0,0,0
trktab:	db	-1,-1
inittab:
	if	convert-1
	db	6		; number of disk i/o drivers.
	endif

	if	convert
	db	10
	endif

	db	0		; disk i/o driver 0 uses disk drive 0.
	dw	lsdrive		; disk i/o driver 0 is 8-inch single-density.
	db	0		; disk i/o driver 1 uses disk drive 0.
	dw	lddrive		; disk i/o driver 1 is 8-inch double-density.
	db	1		; etc.
	dw	lsdrive
	db	1
	dw	lddrive
	db	2
	dw	ssdrive
	db	2
	dw	sddrive

	if	convert
	db	3
	dw	oldlsdrive
	db	3
	dw	oldlddrive
	db	4
	dw	oldlsdrive
	db	4
	dw	oldlddrive
	endif
	endif

	if	scp*large*fastseek
;
; persci 277 or 299.
;
drvtab:	db	00h,08h,01h,09h,00h,08h,01h,09h
trkpt:	db	0,0,0,0,0,0,0,0
trktab:	db	-1
inittab:
	if	convert-1
	db	4
	endif

	if	convert
	db	8
	endif

	db	0
	dw	lsdrive
	db	0
	dw	lddrive
	db	1
	dw	lsdrive
	db	1
	dw	lddrive

	if	convert
	db	2
	dw	oldlsdrive
	db	2
	dw	oldlddrive
	db	3
	dw	oldlsdrive
	db	3
	dw	oldlddrive
	endif
	endif

	if	tarbelldd
;
; two 8-inch shugart-type drives.
;
drvtab:	db	0,8,10h,18h,0,8,10h,18h
trkpt:	db	0,0,1,1,0,0,1,1
trktab:	db	-1,-1
inittab:

	if	convert-1
	db	4
	endif

	if	convert
	db	8
	endif

	db	0
	dw	lsdrive
	db	0
	dw	lddrive
	db	1
	dw	lsdrive
	db	1
	dw	lddrive

	if	convert
	db	2
	dw	oldlsdrive
	db	2
	dw	oldlddrive
	db	3
	dw	oldlsdrive
	db	3
	dw	oldlddrive
	endif
	endif

	if	tarbellsd
;
; four 8-inch shugart-type drives.
;
drvtab:	db	0f2h,0e2h,0f2h,0e2h
trkpt:	db	0,1,0,1
trktab:	db	-1,-1
inittab:

	if	convert-1
	db	2
	endif

	if	convert
	db	4
	endif

	db	0
	dw	lsdrive
	db	1
	dw	lsdrive

	if	convert
	db	2
	dw	oldlsdrive
	db	3
	dw	oldlsdrive
	endif
	endif
;
; cromemco drive select byte is derived as follows:
;	bit 7 = 0
;	bit 6 = 1 if double density (if 16fdc)
;	bit 5 = 1 (motor on)
;	bit 4 = 0 for 5", 1 for 8" drives
;	bit 3 = 1 for drive 3
;	bit 2 = 1 for drive 2
;	bit 1 = 1 for drive 1
;	bit 0 = 1 for drive 0
;
	if	cromemco4fdc*large
;
; persci 277 drive.
;
drvtab:	db	31h,32h,31h,32h
trkpt:	db	0,0,0,0
trktab:	db	-1
inittab:

	if	convert-1
	db	2
	endif

	if	convert
	db	4
	endif

	db	0
	dw	lsdrive
	db	1
	dw	lsdrive

	if	convert
	db	2
	dw	oldlsdrive
	db	3
	dw	oldlsdrive
	endif
	endif

	if	cromemco4fdc*combin
;
; a persci 277 and one 5.25-inch drive.
;
drvtab:	db	31h,32h,24h,31h,32h
trkpt:	db	0,0,1,0,0
trktab:	db	-1,-1
inittab:

	if	convert-1
	db	3
	endif

	if	convert
	db	5
	endif

	db	0
	dw	lsdrive
	db	1
	dw	lsdrive
	db	2
	dw	ssdrive

	if	convert
	db	3
	dw	oldlsdrive
	db	4
	dw	oldlsdrive
	endif
	endif

	if	cromemco4fdc*small
;
; three 5.25-inch drives.
;
drvtab:	db	21h,22h,24h
trkpt:	db	0,1,2
trktab:	db	-1,-1,-1
inittab:db	3
	db	0
	dw	ssdrive
	db	1
	dw	ssdrive
	db	2
	dw	ssdrive
	endif

	if	custom
;
; cromemco 4fdc with two 8-inch shugart-type drives.
;
drvtab:	db	31h,32h,31h,32h
trkpt:	db	0,1,0,1
trktab:	db	-1,-1
inittab:
	if	convert-1
	db	2
	endif

	if	convert
	db	4
	endif

	db	0
	dw	lsdrive
	db	1
	dw	lsdrive

	if	convert
	db	2
	dw	oldlsdrive
	db	3
	dw	oldlsdrive
	endif
	endif

	if	cromemco16fdc*small
;
; three 5.25-inch drives.
;
drvtab:	db	21h,61h,22h,62h,24h,64h
trkpt:	db	0,0,1,1,2,2
trktab:	db	-1,-1,-1
inittab:db	6
	db	0
	dw	ssdrive
	db	0
	dw	sddrive
	db	1
	dw	ssdrive
	db	1
	dw	sddrive
	db	2
	dw	ssdrive
	db	2
	dw	sddrive
	endif

	if	cromemco16fdc*combin
;
; a persci 277 or 299 and one 5.25-inch drive.
;
drvtab:	db	31h,71h,32h,72h,24h,64h,31h,71h,32h,72h
trkpt:	db	0,0,0,0,1,1,0,0,0,0
trktab:	db	-1,-1
inittab:
	if	convert-1
	db	6
	endif

	if	convert
	db	10
	endif

	db	0
	dw	lsdrive
	db	0
	dw	lddrive
	db	1
	dw	lsdrive
	db	1
	dw	lddrive
	db	2
	dw	ssdrive
	db	2
	dw	sddrive

	if	convert
	db	3
	dw	oldlsdrive
	db	3
	dw	oldlddrive
	db	4
	dw	oldlsdrive
	db	4
	dw	oldlddrive
	endif
	endif

	if	cromemco16fdc*large
;
; a persci 277 or 299.
;
drvtab:	db	31h,71h,32h,72h,31h,71h,32h,72h
trkpt:	db	0,0,0,0,0,0,0,0
trktab:	db	-1
inittab:
	if	convert-1
	db	4
	endif

	if	convert
	db	8
	endif

	db	0
	dw	lsdrive
	db	0
	dw	lddrive
	db	1
	dw	lsdrive
	db	1
	dw	lddrive

	if	convert
	db	2
	dw	oldlsdrive
	db	2
	dw	oldlddrive
	db	3
	dw	oldlsdrive
	db	3
	dw	oldlddrive
	endif
	endif

	if	small+combin
ssdrive:
	dw	128		; sector size in bytes.
	db	2		; sector per allocation unit.
	dw	54		; reserved sectors.
	db	2		; number of allocation tables.
	dw	64		; number of directory entrys.
	dw	720		; number of sectors on the disk.

	if	smallds-1
sddrive:			; this is the ibm personal computer
	dw	512		; disk format.
	db	1
	dw	1
	db	2
	dw	64
	dw	320
	endif

	if	smallds
sddrive:
	dw	512
	db	2
	dw	1
	db	2
	dw	112
	dw	640
	endif
	endif			; end of small drive dpts.

	if	combin+large
lsdrive:
	dw	128		; size of sector in bytes.
	db	4		; sectors per allocation unit.
	dw	1		; number of reserved sectors.
	db	2		; number of file allocation tables.
	dw	68		; number of directory entrys.
	dw	77*26		; number of sectors on the disk.

	if	convert
oldlsdrive:
	dw	128
	db	4
	dw	52		; old format had two tracks reserved.
	db	2
	dw	64		; 64 directory entrys.
	dw	77*26
	endif

	if	largeds-1
oldlddrive:
lddrive:
	dw	1024
	db	1
	dw	1
	db	2
	dw	96
	dw	77*8
	endif

	if	largeds
lddrive:
	dw	1024
	db	1
	dw	1
	db	2
	dw	192		; 192 directory entrys in new 8-inch dd/ds format.
	dw	77*8*2

	if	convert
oldlddrive:
	dw	1024
	db	1
	dw	1
	db	2
	dw	128		; 128 directory entrys in old 8-inch dd/ds format.
	dw	77*8*2
	endif
	endif

	endif			; end of large drive dpts.

dosseg:	equ	($+15)/16+biosseg	; compute segment to use for 86-dos.
dosdif:	equ	16*(dosseg-biosseg)
stksav:	equ	1701h+dosdif
dmaadd:	equ	15b4h+dosdif
	end
