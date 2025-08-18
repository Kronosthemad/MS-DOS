; hex2bin  version 1.02
; converts intel hex format files to straight binary

fcb:	equ	5ch
read:	equ	20
setdma:	equ	26
open:	equ	15
close:	equ	16
create:	equ	22
delete:	equ	19
blkwrt:	equ	40
getseg:	equ	38
bufsiz:	equ	1024

	org	100h
	put	100h

hex2bin:
	mov	di,fcb+9
	cmp	b,[di]," "
	jnz	havext
	mov	si,hex
	movb
	movw
havext:
;get load offset (default is -100h)
	mov	cl,4		;needed for shifts
	mov	[offset],-100h
	mov	si,fcb+11h	;scan second fcb for offset
	lodb
	cmp	al," "		;check if offset present
	jz	havoff
	mov	b,[sign],0	;assume positive sign for now
	cmp	al,"+"
	jz	getoff		;get a positive offset
	cmp	al,"-"
	jnz	getoff1		;if not + or -, then not signed
	mov	b,[sign],1	;flag as negative offset
getoff:
	lodb			;eat sign
getoff1:
	call	hexchk		;check for valid hex character
	jc	havoff		;no offset if not valid
	xor	bx,bx		;intialize offset sum to 0
convoff:
	shl	bx,cl		;multiply current sum by 16
	or	bl,al		;add in current hex digit
	lodb			;get next digit
	call	hexchk		;and convert it to binary
	jnc	convoff		;loop until all hex digits read
	test	b,[sign],-1	;check if offset was to be negative
	jz	savoff
	neg	bx
savoff:
	mov	[offset],bx
havoff:
	mov	dx,startseg
	mov	ax,ds
	add	dx,ax		;compute load segment
	mov	ah,getseg
	int	33
	mov	es,dx
	seg	es
	mov	cx,[6]		;get size of segment
	mov	[segsiz],cx
	xor	ax,ax
	mov	di,ax
	mov	bp,ax
	shr	cx
	rep
	stow			;fill entire segment with zeros
	mov	ah,open
	mov	dx,fcb
	int	21h
	or	al,al
	jnz	nofil
	mov	b,[fcb+32],0
	mov	[fcb+14],bufsiz	;set record size to buffer size
	mov	dx,buffer
	mov	ah,setdma
	int	33
	mov	ah,read
	mov	dx,fcb		;all set up for sequential reads
	mov	si,buffer+bufsiz ;flag input buffer as empty
readhex:
	call	getch
	cmp	al,":"		;search for : to start line
	jnz	readhex
	call	getbyt		;get byte count
	mov	cl,al
	mov	ch,0
	jcxz	done
	call	getbyt		;get high byte of load address
	mov	bh,al
	call	getbyt		;get low byte of load address
	mov	bl,al
	add	bx,[offset]	;add in offset
	mov	di,bx
	call	getbyt		;throw away type byte
readln:
	cmp	di,[segsiz]
	jae	aderr
	call	getbyt		;get data byte
	stob
	cmp	di,bp		;check if this is the largest address so far
	jbe	havbig
	mov	bp,di		;save new largest
havbig:
	loop	readln
	jp	readhex

nofil:
	mov	dx,nofile
quit:
	mov	ah,9
	int	21h
	int	20h

aderr:
	mov	dx,addr
	jmp	showerr

getch:
	cmp	si,buffer+bufsiz
	jnz	noread
	int	21h
	cmp	al,1
	jz	error
	mov	si,buffer
noread:
	lodb
	cmp	al,1ah
	jz	done
	ret

getbyt:
	call	hexdig
	mov	bl,al
	call	hexdig
	shl	bl
	shl	bl
	shl	bl
	shl	bl
	or	al,bl
	ret

hexchk:
	sub	al,"0"
	jc	ret
	cmp	al,10
	jc	cmcret
	sub	al,"a"-"0"-10
	jc	ret
	cmp	al,16
cmcret:
	cmc
	ret

hexdig:
	call	getch
	call	hexchk
	jnc	ret
error:
	mov	dx,errmes
showerr:
	mov	ah,9
	int	21h
done:
	mov	[fcb+9],4f00h+"c"	;"co"
	mov	b,[fcb+11],"m"
	mov	dx,fcb
	mov	ah,create
	int	21h
	or	al,al
	jnz	noroom
	xor	ax,ax
	mov	[fcb+33],ax
	mov	[fcb+35],ax	;set rr field
	inc	ax
	mov	[fcb+14],ax	;set record size
	xor	dx,dx
	push	ds
	push	es
	pop	ds		;get load segment
	mov	ah,setdma
	int	21h
	pop	ds
	mov	cx,bp
	mov	ah,blkwrt
	mov	dx,fcb
	int	21h
	mov	ah,close
	int	21h
exit:
	int	20h

noroom:
	mov	dx,dirful
	jmp	quit

hex:	db	"hex"
errmes:	db	"error in hex file--conversion aborted$"
nofile:	db	"file not found$"
addr:	db	"address out of range--conversion aborted$"
dirful:	db	"disk directory full$"

offset:	ds	2
segsiz:	ds	2
sign:	ds	1
buffer:	ds	bufsiz

start:
startseg equ	(start+15)/16
