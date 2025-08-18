; seattle computer products 8086 assembler  version 2.44
;   by tim paterson
; runs on the 8086 under ms-dos
.intel_syntax noprefix
;* * * * * * revision history * * * * * *
;
; 12/29/80  2.01  general release with 86-dos version 0.34
; 02/22/81  2.10  increased buffer size from 128 bytes to 1024 bytes
; 03/18/81  2.11  general cleanup and more documentation
; 03/24/81  2.20  modify esc handling for full 8087 operation
; 04/01/81  2.21  fix date in hex and prn files; modify buffer handling
; 04/03/81  2.22  fix 2.21 buffer handling
; 04/13/81  2.23  re-open source file for listing to allow assembling con:
; 04/28/81  2.24  allow nested ifs
; 07/30/81  2.25  add intel string mnemonics; clean up a little
; 08/02/81  2.30  re-write pass 2:
;			always report errors to console
;			exact byte lengths for hex and prn files
; 11/08/81  2.40  add 8087 mnemonics; print full error messages;
;		  allow expressions with *, /, and ()
; 07/04/82  2.41  fix intel's 8087 "reverse-bit" bug; don't copy date
; 08/18/82  2.42  increase stack from 80 to 256 (damn! overflowed again!)
; 01/05/83  2.43  correct over-zealous optimization in 2.42
; 05/09/83  2.44  add memory usage report
;
;* * * * * * * * * * * * * * * * * * * * *

symwid:	equ	5	;5 symbols per line in dump
fcb:	equ	5ch
bufsiz:	equ	1024	;source code buffer
lstbufsiz:equ	bufsiz	;list file buffer
hexbufsiz:equ	70	;hex file buffer (26*2 + 5*2 + 3 + extra)
eol:	equ	13	;ascii carriage return
object:	equ	100h	;default "put" address

;system call function codes
printmes: equ	9
open:	equ	15
close:	equ	16
read:	equ	20
setdma:	equ	26
make:	equ	22
blkwrt:	equ	40

;the following equates define some token values returned by getsym
undefid:equ	0	;undefined identifier (including no nearby ret)
const:	equ	1	;constant (including $)
reg:	equ	2	;8-bit register
xreg:	equ	3	;16-bit register (except segment registers)
sreg:	equ	4	;segment register
freg:	equ	6	;8087 floating point register

;bits to build 8087 opcode table entries
onereg:	equ	40h	;single st register ok as operand
needop:	equ	80h	;must have an operand
integer:equ	20h	;for integer operations
real:	equ	28h	;for real operations
extended equ	10h	;for long integers or temporary real
memory:	equ	18h	;for general memory operations
stackop:equ	10h	;two register arithmetic with pop
arith:	equ	8	;non-pop arithmetic operations

	org	100h
	put	100h

	jmps	begin

header:	db	13,10,'seattle computer products 8086 assembler version 2.44'
	db	13,10,'copyright 1979-1983 by seattle computer products, inc.'
	db	13,10,13,10,'$'

begin:
	mov	sp,stack
	mov	dx,header
	mov	ah,printmes
	int	33
	mov	al,[fcb+17]
	mov	[symflg],al	;save symbol table request flag
	mov	si,fcb+9	;point to file extension
	lodb			;get source drive letter
	call	chkdsk		;valid drive?
	or	al,al
	jz	default		;if no extension, use existing drive spec
	mov	[fcb],al
default:
	lodb			;get hex file drive letter
	cmp	al,'z'		;suppress hex file?
	jz	l0000
	call	chkdsk
l0000:	
	mov	[hexfcb],al
	lodb			;get prn file drive letter
	mov	ah,0		;signal no prn file
	cmp	al,'z'		;suppress prn file?
	jz	noprn
	cmp	al,'y'		;print errors only on console?
	jz	noprn
	mov	ah,2
	cmp	al,'x'		;prn file to console?
	jz	noprn
	mov	ah,4
	cmp	al,'p'		;prn file to printer?
	jz	noprn
	call	chkdsk
	mov	ah,80h
noprn:
	mov	[lstfcb],al
	mov	[lstdev],ah	;flag device for list ouput
	mov	si,extend
	mov	di,fcb+9
	movw
	movb			;set extension to asm
	movw			;zero extent field
	mov	dx,fcb
	mov	ah,open
	int	33
	mov	bx,nofile
	or	al,al
	jz	$+5
	jmp	prerr
	mov	dx,hexfcb
	call	makfil
	mov	dx,lstfcb
	call	makfil
	xor	ax,ax
	mov	[fcb+12],ax	;zero current block field
	mov	[fcb+32],al	;zero next record field
	mov	[fcb+14],bufsiz	;set record size
	mov	[bufpt],srcbuf	;initialize buffer pointer
	mov	[code],start+1	;pointer to next byte of intermediate code
	mov	[iy],start	;pointer to current relocation byte
	xor	ax,ax
	mov	[pc],ax		;default program counter
	mov	[base],ax	;pointer to root of id tree=nil
	mov	[retpt],ax	;pointer to last ret record
	mov	[ifflg],al	;not within if/endif
	mov	[chklab],al	;lookup all labels
	dec	ax
	mov	[lstret],ax	;location of last ret
	mov	ax,[6]		;hl=end of memory
	mov	[heap],ax	;back end of symbol table space
	mov	[bcount],4	;code bytes per relocation byte

;assemble each line of code

loop:
	call	nextchr		;get first character on line
	cmp	al,1ah
	jz	endj
	mov	al,-1		;flag that no tokens have been read yet
	mov	[sym],al
	call	asmlin		;assemble the line
	mov	al,[sym]
	cmp	al,-1		;any tokens found on line?
	jnz	l0002
	call	getsym		;if no tokens read yet, read first one
l0002:	
	cmp	al,';'
	jz	endln
	cmp	al,eol
	jz	endln
	mov	al,14h		;garbage at end of line error
	jp	endlin
endj:	jmp	end

endln:
	xor	al,al		;flag no errors on line
endlin:
;al = error code for line. stack depth unknown
	mov	sp,stack
	call	nexlin
	jp	loop

nexlin:
	mov	ch,0c0h		;put end of line marker and error code (al)
	call	putcd
	call	gen1
	mov	al,[chr]
geteol:
	cmp	al,10
	jz	ret
	cmp	al,1ah
	jz	endj
	call	nextchr		;scan over comments for linefeed
	jp	geteol

abort:
	mov	bx,nomem
prerr:
	mov	dx,bx
	mov	ah,printmes
	int	33
	int	32

makfil:
	mov	si,dx
	lodb			;get drive select byte
	cmp	al,20h		;if not valid, don't make file
	jnc	ret
	mov	cx,4
	mov	di,si
	mov	si,fcb+1
	rep
	movw			;copy source file name
	mov	ah,make
	int	33
	mov	[di-9+14],1	;set record length to 1 byte
	mov	bx,nospac
	or	al,al		;success?
	jnz	prerr
	ret

chkdsk:
	sub	al,' '		;if not present, set zero flag
	jz	ret
	sub	al,20h
	jz	dskerr		;must be in range a-o
	cmp	al,'p'-'@'
	jc	ret
dskerr:
	mov	bx,baddsk
	jp	prerr

error:
	mov	al,cl
	jmp	endlin

nextchr:
	mov	si,[bufpt]
	cmp	si,srcbuf
	jnz	getch
;buffer empty so refill it
	push	dx
	push	ax		;ah must be saved
	mov	dx,si
	mov	ah,setdma
	int	33
	mov	dx,fcb
	mov	ah,read
	int	33
	xchg	ax,dx		;put error code in dl
	pop	ax		;restore ah
	mov	al,dl		;error code back in al
	pop	dx
	cmp	al,1
	mov	al,1ah		;possibly signal end of file
	jz	nomod		;if nothing read
getch:
	lodb
	cmp	si,srcbuf+bufsiz
	jnz	nomod
	mov	si,srcbuf
nomod:
	mov	[bufpt],si
	mov	[chr],al
	ret


mrops:

; get two operands and check for certain types, according to flag byte
; in cl. op code in ch. returns only if immediate operation.

	push	cx		;save type flags
	call	getop
	push	dx		;save first operand
	call	getop2
	pop	bx		;first op in bx, second op in dx
	mov	al,sreg		;check for a segment register
	cmp	al,bh
	jz	segchk
	cmp	al,dh
	jz	segchk
	mov	al,const	;check if the first operand is immediate
	mov	cl,26
	cmp	al,bh
	jz	error		;error if so
	pop	cx		;restore type flags
	cmp	al,dh		;if second operand is immediate, then done
	jz	ret
	mov	al,undefid	;check for memory reference
	cmp	al,bh
	jz	store		;is destination memory?
	cmp	al,dh
	jz	load		;is source memory?
	test	cl,1		;check if register-to-register operation ok
	mov	cl,27
	jz	error
	mov	al,dh
	cmp	al,bh		;registers must be of same length
rr:
	mov	cl,22
	jnz	error
rr1:
	and	al,1		;get register length (1=16 bits)
	or	al,ch		;or in to op code
	call	put		;and write it
	pop	cx		;dump return address
	mov	al,bl
	add	al,al		;rotate register number into middle position
	add	al,al
	add	al,al
	or	al,0c0h		;set register-to-register mode
	or	al,dl		;combine with other register number
	jmp	put

segchk:
;come here if at least one operand is a segment register
	pop	cx		;restore flags
	test	cl,8		;check if segment register ok
	mov	cl,22
	jz	err1
	mov	cx,8e03h	;segment register move op code
	mov	al,undefid
	cmp	al,dh		;check if source is memory
	jz	load
	cmp	al,bh		;check if destination is memory
	jz	store
	mov	al,xreg
	sub	al,dh		;check if source is 16-bit register
	jz	rr		;if so, al must be zero
	mov	ch,8ch		;change direction
	xchg	dx,bx		;flip which operand is first and second
	mov	al,xreg
	sub	al,dh		;let rr perform finish the test
	jp	rr

store:
	test	cl,004h		;check if storing is ok
	jnz	sterr
	xchg	dx,bx		;if so, flip operands
	and	ch,0fdh		;   and zero direction bit
load:
	mov	dh,25
	cmp	al,bh		;check if memory-to-memory
	jz	mrerr
	mov	al,bh
	cmp	al,reg		;check if 8-bit operation
	jnz	xrg
	mov	dh,22
	test	cl,1		;see if 8-bit operation is ok
	jz	mrerr
xrg:
	mov	al,dl
	sub	al,6		;check for r/m mode 6 and register 0
	or	al,bl		;   meaning direct load/store of accumulator
	jnz	notac
	test	cl,8		;see if direct load/store of accumulator
	jz	notac		;   means anything in this case
; process direct load/store of accumulator
	mov	al,ch
	and	al,2		;preserve direction bit only
	xor	al,2		;   but flip it
	or	al,0a0h		;combine with op code
	mov	ch,al
	mov	al,bh		;check byte/word operation
	and	al,1
	or	al,ch
	pop	cx		;dump return address
	jmp	putadd		;write the address

notac:
	mov	al,bh
	and	al,1		;get byte/word bit
	and	al,cl		;but don't use it in word-only operations
	or	al,ch		;combine with op code
	call	put
	mov	al,bl
	add	al,al		;rotate to middle position
	add	al,al
	add	al,al
	or	al,dl		;combine register field
	pop	cx		;dump return address
	jmp	putadd		;write the address

sterr:
	mov	dh,29
mrerr:
	mov	cl,dh

err1:	jmp	error

getop2:
;get the second operand: look for a comma and drop into getop
	mov	al,[sym]
	cmp	al,','
	mov	cl,21
	jnz	err1


getop:

; get one operand. operand may be a memory reference in brackets, a register,
; or a constant. if a flag (such as "b" for byte operation) is encountered,
; it is noted and processing continues to find the operand.
;
; on exit, al (=dh) has the type of operand. other information depends
; on the actual operand:
;
; al=dh=0  memory reference.  dl has the address mode properly prepared in
; the 8086 r/m format (middle bits zero). the constant part of the address
; is in addr. if an undefined label needs to be added to this, a pointer to
; its information fields is in alabel, otherwise alabel is zero.
;
; al=dh=1  value. the constant part is in data. if an undefined label needs
; to be added to this, a pointer to its information fields is in dlabel,
; otherwise dlabel is zero. "$" and "ret" are in this class.
;
; al=dh=2  8-bit register. dl has the register number.
;
; al=dh=3  16-bit register. dl has the register number.
;
; al=dh=4  segment register. dl has the register number.

	call	getsym
getop1:
;enter here if we don't need a getsym first
	cmp	al,'['		;memory reference?
	jz	mem
	cmp	al,5		;flag ("b", "w", etc.)?
	jz	flg
	cmp	al,reg		;8-bit register?
	jz	nreg
	cmp	al,xreg		;16-bit register?
	jz	nreg
	cmp	al,sreg		;segment register?
	jz	nreg
val:				;must be immediate
	xor	al,al		;no addressing modes allowed
val1:
	call	getval
	mov	ax,[con]	;defined part
	mov	[data],ax
	mov	ax,[undef]	;undefined part
	mov	[dlabel],ax
	mov	dl,ch
	mov	dh,const
	mov	al,dh
	ret
nreg:
	push	dx
	call	getsym
	pop	dx
	mov	al,dh
	ret
mem:
	call	getsym
	mov	al,1
	call	getval
	mov	al,[sym]
	cmp	al,']'
	mov	cl,24
	jnz	err1
	call	getsym
	mov	bx,[con]
	mov	[addr],bx
	mov	bx,[undef]
	mov	[alabel],bx
	mov	dl,ch
	mov	dh,undefid
	mov	al,dh
	ret
flg:
	cmp	dl,[maxflg]	;invalid flag for this operation?
	mov	cl,27h
	jg	err1
	call	getsym
	cmp	al,','
	jz	getop
	jp	getop1


getval:

; expression analyzer. on entry, if al=0 then do not allow base or index
; registers. if al=1, we are analyzing a memory reference, so allow base
; and index registers, and compute addressing mode when done. the constant
; part of the expression will be found in con. if an undefined label is to
; be added to this, a pointer to its information fields will be found in
; undef.

	mov	ah,al		;flag is kept in ah
	mov	[undef],0
	mov	al,[sym]
	call	expression
	mov	[con],dx
	mov	al,ah
	mov	ch,0		;initial mode
	test	al,10h		;test index bit
	rcl	al		;base bit (zero flag not affected)
	jz	noind		;jump if not indexed, with base bit in carry
	cmc
	rcl	ch		;rotate in base bit
	rcl	al		;bp bit
	rcl	ch
	rcl	al		;di bit
	rcl	ch		;the low 3 bits now have indexing mode
mode:
	or	ch,080h		;if undefined label, force 16-bit displacement
	test	[undef],-1
	jnz	ret
	mov	bx,[con]
	mov	al,bl
	cbw			;extend sign
	cmp	ax,bx		;is it a signed 8-bit number?
	jnz	ret		;if not, use 16-bit displacement
	and	ch,07fh		;reset 16-bit displacement
	or	ch,040h		;set 8-bit displacement
	or	bx,bx
	jnz	ret		;use it if not zero displacement
	and	ch,7		;specify no displacement
	cmp	ch,6		;check for bp+0 addressing mode
	jnz	ret
	or	ch,040h		;if bp+0, use 8-bit displacement
	ret

noind:
	mov	ch,6		;try direct address mode
	jnc	ret		;if no base register, that's right
	rcl	al		;check bp bit
	jc	mode
	inc	ch		;if not, must be bx
	jp	mode

expression:
;analyze arbitrary expression. flag byte in ah.
;on exit, al has type byte: 0=register or undefined label
	mov	ch,-1		;initial type
	mov	di,dx
	xor	dx,dx		;initial value
	cmp	al,'+'
	jz	plsmns
	cmp	al,'-'
	jz	plsmns
	mov	cl,'+'
	push	dx
	push	cx
	mov	dx,di
	jp	operate
plsmns:
	mov	cl,al
	push	dx
	push	cx
	or	ah,4		;flag that a sign was found
	call	getsym
operate:
	call	term
	pop	cx		;recover operator
	pop	bx		;recover current value
	xchg	dx,bx
	and	ch,al
	or	al,al		;is it register or undefined label?
	jz	nocon		;if so, then no constant part
	cmp	cl,"-"		;subtract it?
	jnz	add
	neg	bx
add:
	add	dx,bx
nexterm:
	mov	al,[sym]
	cmp	al,'+'
	jz	plsmns
	cmp	al,'-'
	jz	plsmns
	mov	al,ch
	ret
nocon:
	cmp	cl,"-"
	jnz	nexterm
badop:
	mov	cl,5
	jmp	error

term:
	call	factor
mulop:
	push	dx		;save value
	push	ax		;save type
	call	getsym
	pop	cx
	cmp	al,"*"
	jz	getfact
	cmp	al,"/"
	jnz	endterm
getfact:
	or	cl,cl		;can we operate on this type?
	jz	badop
	push	ax		;save operator
	call	getsym		;get past operator
	call	factor
	or	al,al
	jz	badop
	pop	cx		;recover operator
	pop	bp		;and current value
	xchg	ax,bp		;save ah in bp
	cmp	cl,"/"		;do we divide?
	jnz	domul
	or	dx,dx		;dividing by zero?
	mov	cl,29h
	jz	err2
	mov	bx,dx
	xor	dx,dx		;make 32-bit dividend
	div	ax,bx
	jmps	nexfact
domul:
	mul	ax,dx
nexfact:
	mov	dx,ax		;result in dx
	xchg	ax,bp		;restore flags to ah
	mov	al,-1		;indicate a number
	jmps	mulop
endterm:
	pop	dx
	mov	al,cl
	ret

factor:
	mov	al,[sym]
	cmp	al,const
	jz	ret
	cmp	al,undefid
	jz	uval
	cmp	al,"("
	jz	paren
	cmp	al,'"'
	jz	string
	cmp	al,"'"
	jz	string
	cmp	al,xreg		;only 16-bit register may index
	mov	cl,20
	jnz	err2
	test	ah,1		;check to see if indexing is ok
	mov	cl,1
	jz	err2
	mov	al,dl
	mov	cl,3
	sub	al,3		;check for bx
	jz	bxj
	sub	al,2		;check for bp
	jz	bpj
	dec	al		;check for si
	mov	cl,4
	jz	sij
	dec	al		;check for di
	jz	dij
	mov	cl,2		;invalid base/index register
err2:	jmp	error

dij:
	or	ah,20h		;flag seeing index register di
sij:
	test	ah,10h		;check if already seen index register
	jnz	err2
	or	ah,10h		;flag seeing index register
	ret

bpj:
	or	ah,40h		;flag seeing base register bp
bxj:
	test	ah,80h		;check if already seen base register
	jnz	err2
	or	ah,80h		;flag seeing base register
	ret

paren:
	call	getsym		;eat the "("
	call	expression
	cmp	b,[sym],")"	;better have closing paren
	mov	cl,20
	jnz	err30
	ret

uval:
	mov	cl,6
	test	ah,8		;check if undefined label has been seen
	jnz	err30
	or	ah,8		;flag seeing undefined label
	mov	[undef],bx
	ret

err30:	jmp	error

string:
	mov	ch,al
	mov	al,[chr]
	cmp	al,ch
	mov	cl,35
	mov	dl,al
	mov	dh,0
	jnz	l0003
	call	zerlen
l0003:
	call	getchr
	mov	cl,37
	test	ah,2
	jz	err30
	test	ah,4
	mov	cl,38
	jnz	err30
strgdat:
	mov	al,dl
	cmp	al,eol
	mov	cl,39
	jz	err30
	call	put
	mov	al,[datsiz]
	or	al,al
	jnz	bytsiz
	mov	al,dh
	call	put
bytsiz:
	mov	al,[chr]
	mov	dl,al
	call	getchr
	jp	strgdat

zerlen:
	call	nextchr
	cmp	al,ch
	jnz	err30
	ret

getchr:
	call	nextchr
	cmp	al,ch
	jnz	ret
	call	nextchr
	cmp	al,ch
	jz	ret
	pop	bx		;kill return address to strgdat loop
	mov	al,-1		;flag type as constant
	ret


getsym:

; the lexical scanner. used only in the operand field. returns with the token
; in sym and al, sometimes with additional info in bx or dx.
;
; al=sym=0  undefined label. bx has pointer to information fields.
;
; al=sym=1  constant (or defined label). dx has value.
;
; al=sym=2,3,4  8-bit register, 16-bit register, or segment register,
; respectively. dl has register number.
;
; al=sym=5  a mode flag (such as "b" for byte operation). type of flag in dl
; and also stored in flag: -1=no flags, 0=b, 1=w, 2=s, 3=l, 4=t.
;
; al=sym=6  8087 floating point register, st(n) or st. dl has register number.
;
; all other values are the ascii code of the character. note that this may
; never be a letter or number.

	push	ax		;save ah
	call	getsy
	pop	ax
	mov	al,[sym]
	ret

scanb:
	mov	al,[chr]
scant:
	cmp	al,' '
	jz	nexb
	cmp	al,9
	jnz	ret
nexb:
	call	nextchr
	jp	scant

dollar:
	mov	dx,[oldpc]
	mov	al,const
	mov	[sym],al
nextchj:
	jmp	nextchr

getsy:
	call	scanb
	cmp	al,'$'
	jz	dollar
	mov	[sym],al
	or	al,20h
	cmp	al,'z'+1
	jnc	nextchj
	cmp	al,'a'
	jc	$+5
	jmp	letter
	cmp	al,'9'+1
	jnc	nextchj
	cmp	al,'0'
	jc	nextchj
	mov	bx,sym
	mov	b,[bx],const
	call	readid
	dec	bx
	mov	al,[bx]
	mov	cl,7
	mov	bx,0
	cmp	al,'h'
	jnz	$+5
	jmp	hex
	inc	cl
	mov	[ix],id
dec:
	mov	si,[ix]
	mov	al,[si]
	inc	[ix]
	cmp	al,'9'+1
	jc	$+5
	jmp	error
	sub	al,'0'
	mov	dx,bx
	shl	bx
	shl	bx
	add	bx,dx
	shl	bx
	mov	dl,al
	mov	dh,0
	add	bx,dx
	dec	ch
	jnz	dec
	xchg	dx,bx
	ret

hex:
	mov	dx,id
	dec	ch
hex1:
	mov	si,dx
	lodb
	inc	dx
	sub	al,'0'
	cmp	al,10
	jc	gotit
	cmp	al,'g'-'0'
	jnc	err4
	sub	al,'a'-10-'0'
gotit:
	shl	bx
	shl	bx
	shl	bx
	shl	bx
	add	bl,al
	dec	ch
	jnz	hex1
	xchg	dx,bx
	ret

err4:	jmp	error

getlet:
	call	scanb
	cmp	al,eol
	stc
	jz	ret
	cmp	al,';'
	stc
	jz	ret
	mov	cl,10
	or	al,20h
	cmp	al,'a'
	jc	err4
	cmp	al,'z'+1
	jnc	err4
readid:
	mov	bx,id
	mov	ch,0
moreid:
	mov	[bx],al
	inc	ch
	inc	bx
	call	nextchr
	cmp	al,'0'
	jc	nomore
	or	al,20h
	cmp	al,'z'+1
	jnc	nomore
	cmp	al,'9'+1
	jc	moreid
	cmp	al,'a'
	jnc	moreid
nomore:
	mov	cl,al
	mov	al,ch
	mov	[lenid],al
	or	al,al
	mov	al,cl
	ret

letter:
	call	readid
	mov	al,ch
	dec	al
	jnz	noflg
	mov	al,[id]
	mov	cx,5
	mov	di,flgtab
	up
	repne
	scab			;see if one of b,w,s,l,t
	jz	savflg		;go save flag
	xor	al,al
	mov	ch,[lenid]
noflg:
	dec	al
	push	bx
	jnz	l0004
	call	regchk
l0004:	
	pop	bx
	mov	al,dh
	jz	symsav
	call	lookret
symsav:
	mov	[sym],al
	ret

savflg:
	mov	dl,cl		;need flag type in dl
	xchg	[flag],cl
	cmp	cl,-1
	mov	cl,32
	mov	al,5
	jz	symsav
errj3:	jmp	error

flgtab:	db	"tlswb"

fpreg:
;have detected "st" for 8087 floating point stack register
	mov	dl,0		;default is st(0)
	call	scanb		;get next character
	cmp	al,"("		;specifying register number?
	jnz	havreg
;get register number
	call	nextchr		;skip over the "("
	call	getop		;a little recursion never hurt anybody
	cmp	al,const	;better have found a constant
	mov	cl,20		;operand error if not
	jnz	errj3
	cmp	[dlabel],0	;constant must be defined
	mov	cl,30
	jnz	errj3
	mov	dx,[data]	;get constant
	cmp	dx,7		;constant must be in range 0-7
	mov	cl,31
	ja	errj3
	mov	al,[sym]
	cmp	al,")"
	mov	cl,24
	jnz	errj3
havreg:
	mov	dh,freg
	xor	al,al		;zero set means register found
	ret

regchk:
	mov	bx,id
	cmp	[bx],"s"+7400h	;"st"
	jz	fpreg
	mov	cl,[bx]
	inc	bx
	mov	al,[bx]
	mov	bx,regtab
	mov	dh,xreg
	mov	dl,0
	cmp	al,'x'
	jz	scanreg
	mov	dh,reg
	cmp	al,'l'
	jz	scanreg
	mov	dl,4
	cmp	al,'h'
	jz	scanreg
	mov	dh,sreg
	mov	dl,0
	mov	bx,segtab
	cmp	al,'s'
	jz	scanreg
	mov	dh,xreg
	cmp	al,'p'
	jz	preg
	cmp	al,'i'
	jnz	ret
	mov	dl,6
	mov	al,cl
	cmp	al,'s'
	jz	ret
	inc	dl
	cmp	al,'d'
	ret
preg:
	mov	dl,4
	mov	al,cl
	cmp	al,'s'
	jz	ret
	inc	dl
	cmp	al,'b'
	ret
scanreg:
	mov	al,cl
	mov	cx,4
	up
	mov	di,bx
	repnz
	scab
	mov	bx,di
	jnz	ret
	mov	al,cl
	add	al,dl
	mov	dl,al
	xor	al,al
	ret

regtab:	db	'bdca'

segtab:	db	'dsce'

look:
	mov	ch,[bx]
	inc	bx
	mov	dx,id
	call	cpslp
	jz	ret
	xor	al,80h
	rol	al		;make end-of-symbol bit least significant
	mov	cl,al
	dec	bx
	mov	al,[bx]
	xor	al,80h
	rol	al
	cmp	al,cl
	jnc	small
	inc	ch
	inc	ch
small:
	mov	dl,ch
	mov	dh,0
	add	bx,dx
	mov	dx,[bx]
	inc	bx
	mov	al,dl
	or	al,dh
	stc
	jz	ret
	xchg	dx,bx
	jp	look

lookret:
	mov	al,ch
	cmp	al,3	;ret has 3 letters
	jnz	lookup
	dec	bx
	or	b,[bx],080h
	mov	dx,retstr+2
chkret:
	mov	si,dx
	lodb
	cmp	al,[bx]
	jnz	lookit
	dec	bx
	dec	dx
	dec	ch
	jnz	chkret
	mov	dx,[lstret]
	mov	al,dl
	and	al,dh
	inc	al
	jz	allret
	mov	bx,[pc]
	sub	bx,dx
	mov	al,bl
	cbw
	cmp	ax,bx		;signed 8-bit number?
	mov	al,1
	jz	ret
allret:
	mov	bx,[retpt]
	mov	al,bh
	or	al,bl
	mov	al,0
	jnz	ret
	mov	bx,[heap]
	dec	bx
	dec	bx
	dec	bx
	mov	[heap],bx
	xor	al,al
	mov	[bx],al
	mov	[retpt],bx
	ret

lookup:
	dec	bx
	or	b,[bx],080h
lookit:
	mov	bx,[base]
	mov	al,bh
	or	al,bl
	jz	empty
	call	look
	jc	enter
	mov	dx,4
	add	bx,dx
	mov	al,[bx]
	or	al,al
	jz	ret
	inc	bx
	mov	dx,[bx]
	inc	bx
	ret

enter:
	push	bx		;save pointer to link field
	call	create		;add the node
	pop	si
	mov	[si-1],dx	;link new node
	ret			;zero was set by create

empty:
	call	create
	mov	[base],dx
	ret


create:

; add a new node to the identifier tree. the identifier is at id with
; bit 7 of the last character set to one. the length of the identifier is
; in lenid, which is id-1.
;
; node format:
;	1. length of identifier (1 byte)
;	2. identifier (1-80 bytes)
;	3. left link (2-byte pointer to alphabetically smaller identifiers)
;	4. right link (0 if none larger)
;	5. data field:
;	   a. defined flag (0=undefined, 1=defined)
;	   b. value (2 bytes)
;
; this routine returns with al=zero and zero flag set (which indicates
; on return from lookup that it has not yet been defined), dx points
; to start of new node, and bx points to data field of new node.

	mov	al,[lenid]
	add	al,8		;storage needed for the node
	mov	bx,[heap]
	mov	dl,al
	mov	dh,0
	sub	bx,dx		;heap grows downward
	mov	[heap],bx
	xchg	dx,bx
	mov	bx,[code]	;check to make sure there's enough
	cmp	bx,dx
	jb	$+5
	jmp	abort
	push	dx
	mov	bx,lenid
	mov	cl,[bx]
	inc	cl
	mov	ch,0
	up
	mov	si,bx
	mov	di,dx
	rep
	movb			;move identifier and length into node
	mov	dx,di
	mov	bx,si
	mov	ch,4
	xchg	dx,bx
nilify:
	mov	[bx],cl		;zero left and right links
	inc	bx
	dec	ch
	jnz	nilify
	xor	al,al		;set zero flag
	mov	[bx],al		;zero defined flag
	pop	dx		;restore pointer to node
	ret

cpslp:
	mov	si,dx
	lodb
	cmp	al,[bx]
	lahf
	inc	dx
	inc	bx
	sahf
	jnz	ret
	dec	ch
	jnz	cpslp
	ret

getlab:
	mov	bx,0
	mov	[labpt],bx
	mov	b,[flag],-1
	mov	dh,0
	mov	al,[chr]
	cmp	al,' '+1
	jc	not1
	or	dh,001h
not1:
	call	getlet
	jc	ret
	cmp	al,':'
	jnz	labchk
	call	nextchr
	jp	label
labchk:
	or	al,al
	test	dh,001h
	jz	ret
label:
	mov	al,[chklab]
	or	al,al
	jz	$+5
	jmp	getlet
	call	lookup
	mov	cl,11
	jnz	err5
	mov	dx,[pc]
	mov	b,[bx],1
	inc	bx
	mov	[bx],dx
	mov	[labpt],bx
	jmp	getlet

err5:	jmp	error

asmlin:
	mov	b,[maxflg],1	;allow only b and w flags normally
	mov	bx,[pc]
	mov	[oldpc],bx
	call	getlab
	jnc	$+5
	jmp	endln
	mov	bx,lenid
	mov	al,[bx]
	mov	cl,12
	sub	al,2
	mov	ch,al
	jc	err5
	inc	bx
	cmp	b,[bx],"f"	;see if an 8087 mnemonic
	jz	ndpop
	cmp	al,5
	jnc	err5
	mov	al,[bx]
	sub	al,'a'
	mov	cl,al
	add	al,al
	add	al,al
	add	al,cl
	add	al,ch
	add	al,al
	mov	bx,optab
	mov	dl,al
	mov	dh,0
	add	bx,dx
	mov	bx,[bx]
	inc	ch
	mov	cl,ch
	mov	ah,[bx]
	inc	bx
	or	ah,ah
	jz	operr
findop:
	mov	ch,cl
	mov	dx,id+1
	xchg	ax,bp		;save count of opcodes in bp
	call	cpslp
	jz	havop
	xchg	ax,bp
	mov	dh,0
	mov	dl,ch
	inc	dx
	inc	dx
	add	bx,dx
	dec	ah
	jnz	findop
operr:
	mov	cl,12
	jmp	error

havop:
	mov	al,[bx+2]	;get opcode
	jmp	[bx]

ndpop:	;first letter is "f" so must be 8087 opcode ("numeric data processor")
	mov	b,[maxflg],4	;allow all type flags
	inc	bx
	cmp	b,[bx],"n"	;"no-wait" form?
	mov	ah,0
	jnz	savnflg
	mov	ah,1
	dec	al
	inc	bx		;skip over the "n"
savnflg:
	mov	[nowait],ah	;0 for wait, 1 for no wait
	cmp	al,1
	jb	operr		;not enough char left for valid opcode?
	cmp	al,5
	ja	operr		;too many?
	cbw
	xchg	ax,dx		;save length in dx
	mov	si,dx
	or	b,[si+bx],80h	;set high bit of last character
	mov	al,[bx]		;get first char of opcode
	inc	bx
	sub	al,"a"
	jb	try2xm1		;go see if opcode starts with "2"
	cmp	al,"z"-"a"
	ja	operr
	cbw
	shl	ax		;double to index into address table
	xchg	ax,si		;put in index register
	mov	di,[si+ndptab]	;get start of opcode table for this letter
lookndp:
	mov	ah,[di]		;number of opcodes starting with this letter
	or	ah,ah
	jz	operr		;any start with this letter?
fndndp:
	inc	di
	mov	si,bx		;pointer to start of opcode
	mov	cx,dx		;get length of opcode
	repe
	cmpb			;compare opcode to table entry
	jz	havndp
	dec	di		;back up in case that was last letter
	mov	al,80h		;look for char with high bit set
endop:
	scasb
	ja	endop
	inc	di		;skip over info about opcode
	dec	ah
	jnz	fndndp
operrj:	jp	operr

try2xm1:
	cmp	al,"2"-"a"
	jnz	operr
	mov	di,xm1
	jp	lookndp

specialop:
	and	al,7		;mask to special op number
	jz	fwait		;if zero, go handle fwait
;handle fnop
	cmp	b,[nowait],0	;was "n" present (if not opcode was "fop")
	jz	operr
	mov	al,9bh		;need wait opcode after all
	call	put
	mov	al,0d9h
	call	put
	mov	al,0d0h
	jmp	put

fwait:
	cmp	b,[nowait],0	;"fnwait" not legal
	jnz	operrj
	ret			;nothing to do - "wait" already sent

havndp:
	mov	si,di
	cmp	b,[nowait],0
	jnz	nwait
	mov	al,9bh		;wait opcode
	call	put
nwait:
	lodw			;get opcode info
	test	al,0f8h		;any operand bits set?
	jz	noops		;if no operands, output code
	test	al,78h		;special case?
	jz	specialop
	push	ax
	call	getsym		;see if any operands
	pop	cx
	cmp	al,";"
	jz	noopchk
	cmp	al,eol
	jz	noopchk
	cmp	al,freg		;is it 8087 register?
	jnz	memop
	xchg	ax,cx
	test	al,onereg	;one register ok as operand?
	jnz	putreg		;yes - save it
	test	al,20h		;memory-only operation?
	mov	cl,20
	jnz	errj4
	test	al,18h		;two-register operation?
	jpe	errj4		;must be exactly one bit set
	push	dx		;save register number
	push	ax		;save opcode
	call	getsym
	cmp	al,","
	mov	cl,15h
	jnz	errj4
	call	getsym
	mov	cl,20
	cmp	al,freg
	jnz	errj4
	pop	ax
	pop	bx
	xor	al,2		;flip "pop" bit
	and	al,0fbh		;reset direction bit to st(0)
	or	bl,bl		;is first register st(0)?
	jz	st0dest
	xchg	dx,bx
	or	bl,bl		;one of these must be st(0)
	jnz	errj4
	xor	al,4		;flip direction
	jmps	putreg
st0dest:
	test	al,2		;is pop bit set?
	jnz	errj4		;don't allow destination st(0) then pop
putreg:
	and	ah,0f8h		;zero out register field
	or	ah,dl
	or	ah,0c0h
	push	ax
	call	getsym		;get to next symbol
	pop	ax
	jmps	noops

noopchk:
	xchg	ax,cx
	test	al,80h		;is no operands ok?
	mov	cl,20
	jnz	errj4
noops:
;first test for fdiv or fsub and reverse "r" bit if "d" bit is set
	push	ax
	and	ax,0e005h
	cmp	ax,0e004h
	pop	ax
	jnz	norev
	xor	ah,8		;reverse "r" bit
norev:
	and	al,7
	or	al,0d8h		;esc hook
	call	put
	mov	al,ah
	jmp	put

badflag:
	mov	cl,20h
errj4:	jmp	error

memop:
	push	cx		;save opcode
	call	getop1		;get memory operand
	cmp	al,undefid	;is it?
	mov	cl,20
	jnz	errj4
	pop	ax
	test	al,20h		;does it have memory format field?
	jnz	getformat
	test	al,8		;check if any memory operand legal
	jz	errj4
	test	al,10h		;check for 2-op arithmetic
	jnz	putmem		;if not, just use as plain memory op
getformat:
	and	al,0f9h		;zero memory format bits
	mov	cl,[flag]
	dec	cl		;must now be in range 0-3
	jl	badflag
	mov	ch,al		;save opcode byte
	shr	al		;put format bits in bits 2 & 3
	and	al,0ch
	or	al,cl		;combine format bits with flag
	mov	bx,formattab
	xlat
	or	al,al		;valid combination?
	js	badflag
	or	ah,al		;possibly set new bits in second byte
	or	al,ch		;set memory format bits
putmem:
	and	al,7
	or	al,0d8h
	call	put
	mov	al,ah
	and	al,38h
	or	al,dl		;combine addressing mode
	jmp	putadd

formattab:
;there are 16 entries in this table. the 4-bit index is built like this:
;	bit 3		0 for normal memory ops, 1 if extended is ok
;	bit 2		0 for integer, 1 for real
;	bit 0 & 1	flag: 00=w, 01=s, 10=l, 11=t
;
;the entries in the table are used as two 3-bit fields. bits 0-2 are ored
;into the first byte of the opcode for the memory format field. bits 3-6
;are ored into the second byte to modify the opcode for extended operands.
;if bit 7 is set, then that combination is illegal.

	db	6,2,80h,80h	;normal integers
	db	80h,0,4,80h	;normal reals
	db	6,2,2eh,80h	;extended integers
	db	80h,0,4,2bh	;extended reals

grp1:
	mov	cx,8a09h
	call	mrops
	mov	cx,0c6h
	mov	al,bh
	cmp	al,undefid
	jnz	l0006
	call	stimm
l0006:	
	and	al,1
	jz	bytimm
	mov	al,0b8h
	or	al,bl
	call	put
	jmp	putwor

bytimm:
	mov	al,0b0h
	or	al,bl
	call	put
putbj:	jmp	putbyt

immed:
	mov	al,bh
	cmp	al,undefid
	jz	stimm
	mov	al,bl
	or	al,al
	jz	ret
	mov	al,bh
	call	imm
	or	al,0c0h
	call	put
finimm:
	mov	al,cl
	pop	cx
	test	al,1
	jz	putbj
	cmp	al,83h
	jz	putbj
	jmp	putwor

stimm:
	mov	al,[flag]
	call	imm
	call	putadd
	jp	finimm

imm:
	and	al,1
	or	al,cl
	mov	cl,al
	call	put
	mov	al,ch
	and	al,38h
	or	al,bl
	ret

put:
;save byte in al as pure code, with intermediate code bits 00. al and
;di destroyed, no other registers affected.
	push	bx
	push	cx
	mov	ch,0		;flag as pure code
	call	gen
	pop	cx
	pop	bx
	ret

gen:
;save byte of code in al, given intermediate code bits in bits 7&8 of ch.
	call	putinc		;save it and bump code pointer
gen1:
	mov	al,[reloc]
	rcl	ch
	rcl	al
	rcl	ch
	rcl	al
	mov	[reloc],al
	mov	bx,bcount
	dec	b,[bx]
	jnz	ret
	mov	b,[bx],4
	mov	bx,reloc
	mov	al,[bx]
	mov	b,[bx],0
	mov	di,[iy]
	mov	[di],al
	mov	bx,[code]
	mov	[iy],bx
	inc	bx
	mov	[code],bx
	ret

putinc:
	inc	[pc]
putcd:
	mov	di,[code]
	stob
	mov	[code],di
	ret

putwor:
;save the word value described by [dlabel] and [data] as code. if defined,
;two bytes of pure code will be produced. otherwise, appropriate intermediate
;code will be generated.
	push	cx
	mov	ch,80h
	push	dx
	push	bx
	jp	putbw

putbyt:
;same as putwor, above, but for byte value.
	push	cx
	mov	ch,40h
	push	dx
	push	bx
	mov	bx,[dlabel]
	mov	al,bh
	or	al,bl
	jnz	putbw
	mov	bx,[data]
	or	al,bh
	jz	putbw
	inc	bh
	jz	putbw
	mov	cl,31
	jmp	error
putbw:
	mov	dx,[dlabel]
	mov	bx,[data]
putchk:
	or	dx,dx
	jz	noundef
	mov	al,dl
	call	putcd
	mov	al,dh
	call	putcd
	mov	al,bl
	call	putinc
	mov	al,bh
	test	ch,080h
	jz	smput
	call	gen
	jp	pret
smput:
	call	putcd
	call	gen1
pret:
	pop	bx
	pop	dx
	pop	cx
	ret

noundef:
	mov	al,bl
	mov	cl,bh
	push	cx
	mov	ch,0
	call	gen
	pop	cx
	mov	al,cl
	test	ch,080h
	mov	ch,0
	jz	pret
	call	gen
	jp	pret

putadd:
;save complete addressing mode. addressing mode is in al; if this is a register
;operation (>=c0), then the one byte will be saved as pure code. otherwise,
;the details of the addressing mode will be investigated and the optional one-
;or two-byte displacement will be added, as described by [addr] and [alabel].
	push	cx
	push	dx
	push	bx
	mov	ch,0
	mov	cl,al
	call	gen		;save the addressing mode as pure code
	mov	al,cl
	mov	ch,80h
	and	al,0c7h
	cmp	al,6
	jz	twobt		;direct address?
	and	al,0c0h
	jz	pret		;indirect through reg, no displacement?
	cmp	al,0c0h
	jz	pret		;register to register operation?
	mov	ch,al		;save whether one- or two-byte displacement
twobt:
	mov	bx,[addr]
	mov	dx,[alabel]
	jp	putchk

grp2:
	call	getop
	mov	cx,0ff30h
	cmp	al,undefid
	jz	pmem
	mov	ch,50h
	cmp	al,xreg
	jz	pxreg
	mov	ch,6
	cmp	al,sreg
	jnz	$+5
	jmp	packreg
	mov	cl,20
	jmp	error

pmem:
	mov	al,ch
	call	put
	mov	al,cl
	or	al,dl
	jmp	putadd

pxreg:
	mov	al,ch
	or	al,dl
	jmp	put

grp3:
	call	getop
	push	dx
	call	getop2
	pop	bx
	mov	cx,8614h
	mov	al,sreg
	cmp	al,bh
	jz	err6
	cmp	al,dh
	jz	err6
	mov	al,const
	cmp	al,bh
	jz	err6
	cmp	al,dh
	jz	err6
	mov	al,undefid
	cmp	al,bh
	jz	exmem
	cmp	al,dh
	jz	exmem1
	mov	al,bh
	cmp	al,dh
	mov	cl,22
	jnz	err6
	cmp	al,xreg
	jz	l0008
	call	rr1
l0008:			;rr1 never returns
	mov	al,bl
	or	al,al
	jz	exacc
	xchg	dx,bx
	mov	al,bl
	or	al,al
	mov	al,bh
	jz	exacc
	call	rr1
exacc:
	mov	al,90h
	or	al,dl
	jmp	put

exmem:
	xchg	dx,bx
exmem1:
	cmp	al,bh
	jz	err6
	mov	cl,1	;flag word as ok
	call	notac	;notac never returns
err6:	jmp	error

grp4:
	push	ax
	call	getop
	pop	cx
	xchg	cl,ch
	cmp	al,const
	jz	fixed
	sub	al,xreg
	dec	dl
	dec	dl
	or	al,dl
	mov	cl,20
	jnz	err6
	mov	al,ch
	or	al,8
	jmp	put
fixed:
	mov	al,ch
	call	put
	jmp	putbyt

grp5:
	push	ax
	call	getop
	mov	cl,20
	cmp	al,const
	jnz	err6
	mov	bx,[dlabel]
	mov	al,bh
	or	al,bl
	mov	cl,30
	jnz	err6
	mov	bx,[data]
	pop	ax
	or	al,al
	jz	org
	dec	al
	jz	dsj
	dec	al
	jz	equ
	dec	al
	jz	$+5
	jmp	if
putop:
	mov	al,-3
	jp	newloc
align:
	mov	al,[pc]
	and	al,1
	jz	ret
	mov	bx,1
dsj:
	xchg	dx,bx
	mov	bx,[pc]
	add	bx,dx
	mov	[pc],bx
	xchg	dx,bx
	mov	al,-4
	jp	newloc
equ:
	xchg	dx,bx
	mov	bx,[labpt]
	mov	al,bh
	or	al,bl
	mov	cl,34
	jz	err7
	mov	[bx],dl
	inc	bx
	mov	[bx],dh
	ret
org:
	mov	[pc],bx
	mov	al,-2
newloc:
	call	putcd
	mov	al,bl
	call	putcd
	mov	al,bh
	call	putcd
	mov	ch,0c0h
	jmp	gen1
grp6:
	mov	ch,al
	mov	cl,4
	call	mrops
	mov	cl,23
err7:	jmp	error
grp7:
	mov	ch,al
	mov	cl,1
	call	mrops
	mov	cl,80h
	mov	dx,[dlabel]
	mov	al,dh
	or	al,dl
	jnz	accj
	xchg	dx,bx
	mov	bx,[data]
	mov	al,bl
	cbw
	cmp	ax,bx
	xchg	dx,bx
	jnz	accj
	or	cl,002h
accj:	jmp	accimm
grp8:
	mov	cl,al
	mov	ch,0feh
	jp	oneop
grp9:
	mov	cl,al
	mov	ch,0f6h
oneop:
	push	cx
	call	getop
one:
	mov	cl,26
	cmp	al,const
	jz	err7
	cmp	al,sreg
	mov	cl,22
	jz	err7
	pop	cx
	cmp	al,undefid
	jz	mop
	and	al,1
	jz	rop
	test	cl,001h
	jz	rop
	mov	al,cl
	and	al,0f8h
	or	al,dl
	jmp	put
mop:
	mov	al,[flag]
	and	al,1
	or	al,ch
	call	put
	mov	al,cl
	and	al,38h
	or	al,dl
	jmp	putadd
rop:
	or	al,ch
	call	put
	mov	al,cl
	and	al,38h
	or	al,0c0h
	or	al,dl
	jmp	put
grp10:
	mov	cl,al
	mov	ch,0f6h
	push	cx
	call	getop
	mov	cl,20
	mov	al,dl
	or	al,al
	jnz	errj1
	mov	al,dh
	cmp	al,xreg
	jz	g10
	cmp	al,reg
errj1:	jnz	err8
g10:
	push	ax
	call	getop
	pop	ax
	and	al,1
	mov	[flag],al
	mov	al,dh
onej:	jp	one
grp11:
	call	put
	mov	al,0ah
	jmp	put
grp12:
	mov	cl,al
	mov	ch,0d0h
	push	cx
	call	getop
	mov	al,[sym]
	cmp	al,','
	mov	al,dh
	jnz	onej
	push	dx
	call	getop
	sub	al,reg
	mov	cl,20
	dec	dl
	or	al,dl
	jnz	err8
	pop	dx
	mov	al,dh
	pop	cx
	or	ch,002h
	push	cx
	jmp	one
grp13:
	mov	ch,al
	mov	cl,1
	call	mrops
	mov	cl,80h
accimm:
	call	immed
	or	ch,004h
	and	ch,0fdh
aimm:
	mov	al,bh
	and	al,1
	lahf
	push	ax
	or	al,ch
	call	put
	pop	ax
	sahf
	jnz	$+5
	jmp	putbyt
	jmp	putwor

err8:	jmp	error

grp14:
;jmp and call mnemonics
	lahf
	xchg	ah,al
	push	ax
	xchg	ah,al
	mov	b,[maxflg],3	;allow "l" flag
	call	getop
	cmp	al,const
	jz	direct
	mov	cl,20
	cmp	al,reg
	jz	err8
	cmp	al,sreg
	jz	err8
	cmp	al,xreg
	jnz	notrg
	or	dl,0c0h
notrg:
;indirect jump. dl has addressing mode.
	mov	al,0ffh
	call	put
	pop	ax
	xchg	ah,al
	sahf
	and	al,38h
	or	al,dl
	mov	ch,[flag]
	cmp	ch,3		;flag "l" present?
	jz	putaddj		;if so, do inter-segment
	mov	cl,27h
	cmp	ch,-1		;better not be a flag
	jnz	err8
	and	al,0f7h		;convert to intra-segment
putaddj:
	jmp	putadd
direct:
	mov	al,[sym]
	cmp	al,','
	jz	longj
	pop	ax
	xchg	ah,al
	sahf
	dec	al
	cmp	al,0e9h
	jz	gotop
	mov	al,0e8h
gotop:
	call	put
	mov	dx,[pc]
	inc	dx
	inc	dx
	sub	[data],dx
	jmp	putwor
longj:
	pop	ax
	xchg	ah,al
	sahf
	call	put
	call	putwor
	call	getop
	mov	cl,20
	cmp	al,const
	jnz	err8
	jmp	putwor

grp16:
;ret mnemonic
	lahf
	xchg	ah,al
	push	ax
	xchg	ah,al
	call	getsym
	cmp	al,5
	jz	longr
	cmp	al,eol
	jz	nodec
	cmp	al,';'
	jz	nodec
getsp:
	call	getop1
	pop	cx
	cmp	al,const
	mov	cl,20
	jnz	err9
	mov	al,ch
	and	al,0feh
	call	put
	jmp	putwor
longr:
	cmp	dl,3		;is flag "l"?
	mov	cl,27h
	jnz	err10		;if not, bad flag
	pop	ax
	xchg	ah,al
	sahf
	or	al,8
	lahf
	xchg	ah,al
	push	ax
	xchg	ah,al
notlon:
	call	getsym
	cmp	al,eol
	jz	doret
	cmp	al,';'
	jz	doret
	cmp	al,','
	jnz	l0011
	call	getsym
l0011:	
	jp	getsp
nodec:
;return is intra-segment (short) without add to sp. 
;record position for ret symbol.
	mov	bx,[pc]
	mov	[lstret],bx
	xchg	dx,bx
	mov	bx,[retpt]
	mov	al,bh
	or	al,bl
	jz	doret
	mov	b,[bx],1
	inc	bx
	mov	[bx],dx
	mov	bx,0
	mov	[retpt],bx
doret:
	pop	ax
	xchg	ah,al
	sahf
	jmp	put

grp17:
	call	put
	call	getop
	cmp	al,const
	mov	cl,20
err9:	jnz	err10
	mov	bx,[data]
	mov	dx,[pc]
	inc	dx
	sub	bx,dx
	mov	[data],bx
	call	putbyt
	mov	bx,[dlabel]
	mov	al,bh
	or	al,bl
	jnz	ret
	mov	bx,[data]
	mov	al,bl
	cbw
	cmp	ax,bx		;signed 8-bit number?
	jz	ret
	mov	cl,31
err10:	jmp	error
	ret
grp18:
	call	getop
	cmp	al,const
	mov	cl,20
	jnz	err10
	mov	bx,[dlabel]
	mov	al,bh
	or	al,bl
	jnz	genint
	mov	bx,[data]
	mov	dx,3
	sbb	bx,dx
	jnz	genint
	mov	al,0cch
	jmp	put
genint:
	mov	al,0cdh
	call	put
	jmp	putbyt

grp19:	;esc opcode
	call	getop
	mov	cl,20
	cmp	al,const
	jnz	errj		;first operand must be immediate
	mov	cl,1eh
	test	[dlabel],-1	;see if all labels have been defined
	jnz	errj
	mov	ax,[data]
	cmp	ax,64		;must only be 6 bits
	mov	cl,1fh
	jnb	errj
	mov	bl,al		;save for second byte
	shr	al
	shr	al
	shr	al
	or	al,0d8h		;esc opcode
	call	put
	push	bx
	call	getop2
	pop	bx
	and	bl,7		;low 3 bits of first operand
	shl	bl
	shl	bl
	shl	bl
	cmp	al,undefid	;check for memory operand
	jz	escmem
	cmp	al,const	;check for another immediate
	jz	escimm
	mov	cl,20
errj:	jmp	error

escmem:
	or	bl,dl		;combine mode with first operand
	mov	al,bl
	jmp	putadd

escimm:
	mov	cl,1eh
	test	[dlabel],-1	;see if second operand is fully defined
	jnz	errj
	mov	ax,[data]
	mov	cl,1fh
	cmp	ax,8		;must only be 3 bit value
	jnb	errj
	or	al,bl		;combine first and second operands
	or	al,0c0h		;force "register" mode
	jmp	put

grp20:
	mov	ch,al
	mov	cl,1
	call	mrops
	mov	cl,0f6h
	call	immed
	mov	ch,0a8h
	jmp	aimm
grp21:
	call	getop
	cmp	al,sreg
	mov	cl,28
	jnz	errj
	mov	ch,26h
packreg:
	mov	al,dl
	add	al,al
	add	al,al
	add	al,al
	or	al,ch
	jmp	put
grp22:
	call	getop
	mov	cx,8f00h
	cmp	al,undefid
	jnz	$+5
	jmp	pmem
	mov	ch,58h
	cmp	al,xreg
	jnz	$+5
	jmp	pxreg
	mov	ch,7
	cmp	al,sreg
	jz	packreg
	mov	cl,20
err11:	jmp	error
grp23:
	mov	[datsiz],al
getdat:
	call	getsym
	mov	al,2
	call	val1
	mov	al,[sym]
	cmp	al,','
	mov	al,[datsiz]
	jnz	enddat
	call	savdat
	jp	getdat
enddat:
	cmp	al,2
	jnz	savdat
	mov	bx,[data]
	lahf
	or	bl,080h
	sahf
	mov	[data],bx
savdat:
	or	al,al
	jz	$+5
	jmp	putbyt
	jmp	putwor
if:
	or	bx,bx
	jz	skipcd
	inc	b,[ifflg]
	ret

skipcd:
	inc	b,[chklab]
skiplp:
	xor	al,al
	call	nexlin
	call	nextchr
	cmp	al,1ah
	jz	end
	call	getlab
	jc	skiplp
	mov	di,lenid
	mov	si,ifend
	mov	ch,0
	mov	cl,[di]
	inc	cl
	repe
	cmpb
	jz	endcond
	mov	di,lenid
	mov	si,ifnest
	mov	cl,[di]
	inc	cl
	repe
	cmpb
	jnz	skiplp
	inc	b,[chklab]
	jp	skiplp

endcond:
	dec	b,[chklab]
	jnz	skiplp
	ret

endif:
	mov	al,[ifflg]
	mov	cl,36
	dec	al
	js	errjmp
	mov	[ifflg],al
	ret

errjmp:	jmp	error

;*********************************************************************
;
;	pass 2
;
;*********************************************************************

end:
	mov	dl,4
wrend:
	mov	ch,0ffh
	mov	al,ch
	call	gen
	dec	dl
	jnz	wrend
	mov	[bufpt],srcbuf
	mov	b,[hexcnt],-5	;flag hex buffer as empty
	mov	[lstpnt],lstbuf
	mov	[hexpnt],hexbuf
	xor	ax,ax
	mov	[errcnt],ax
	mov	[pc],ax
	mov	[line],ax	;current line number
	mov	[hexadd],object
	mov	dx,fcb
	mov	ah,open
	int	33		;re-open source file
	xor	ax,ax
	mov	[fcb+12],ax	;set current block to zero
	mov	[fcb+20h],al	;set next record field to zero
	mov	[fcb+14],bufsiz
	mov	[count],al
	mov	ch,1
	mov	si,start
fixline:
	mov	di,start	;store code over used up intermediate code
	xor	al,al
	mov	[spc],al	;no "special" yet (org, put, ds)
	mov	[err],al	;no second pass errors yet
nexbt:
	shl	cl		;shift out last bit of previous code
	dec	ch		;still have codes left?
	jnz	testtyp
	lodb			;get next flag byte
	mov	cl,al
	mov	ch,4
testtyp:
	shl	cl		;set flags based on two bits
	jo	fixup
	lodb
	jc	emark
objbt:
	stob
	jp	nexbt

fixup:
;either a word or byte fixup is needed from a forward reference
	lodw			;get pointer to symbol
	xchg	ax,bx
	lodw			;get constant part
	add	ax,[bx+1]	;add symbol value to constant part
	cmp	b,[bx],0	;see if symbol got defined
	jnz	havdef
	mov	b,[err],100	;undefined - flag error
	xor	ax,ax
havdef:
	or	cl,cl		;see if word or byte fixup
	js	defbyt
	stow
	jp	nexbt

defbyt:
	mov	dx,ax
	cbw			;extend sign
	cmp	ax,dx		;see if in range +127 to -128
	jz	objbt		;if so, it's always ok
	not	ah		;check for range +255 to -256
	cmp	ah,dh
	jnz	rngerr		;must always be in this range
;check for short jump. if so, we're out of range; otherwise we're ok
	cmp	di,start+1	;only one other byte on line?
	jnz	objbt		;can't be short jump if not
	mov	al,[start]	;get the first byte of this line
	cmp	al,0ebh		;direct short jump?
	jz	rngerr
	and	al,0fch
	cmp	al,0e0h		;loop or jcxz instruction?
	jz	rngerr
	and	al,0f0h
	cmp	al,70h		;conditional jump?
	mov	al,dl		;get code byte in al
	jnz	objbt		;if not, we're ok
rngerr:
	mov	b,[err],101	;value out of range
	jp	objbt

finij:	jmp	fini

emark:
	cmp	al,-1		;end of file?
	jz	finij
	cmp	al,-10		;special item?
	ja	spend
	push	cx
	push	si
	push	ax		;save error code
	mov	ah,[lstdev]
	and	ah,0feh		;reset error indicator
	or	al,[err]	;see if any errors on this line
	jz	noerr
	or	ah,1		;send line to console if error occured
noerr:
	mov	[lstdev],ah
	mov	cx,di
	call	strtlin		;print address of line
	mov	si,start
	sub	cx,si		;get count of bytes of code
	jz	sholin
codlp:
	lodb
	call	savcd		;ouput code to hex and prn files
	loop	codlp
sholin:
	mov	al,0
	xchg	al,[count]
	mov	cx,7		;allow 7 bytes of code per line
	sub	cl,al
	mov	al,' '
	jz	nofil
blnk:				;put in 3 blanks for each byte not present
	call	list
	call	list
	call	list
	loop	blnk
nofil:
	call	outlin
	pop	ax		;restore error code
	call	reperr
	mov	al,[err]
	call	reperr
	pop	si
	pop	cx
	mov	al,[spc]	;any special funtion?
	or	al,al
	jnz	spcfun
	jmp	fixline

spend:
	mov	[spc],al	;record special function
	lodw			;get it's data
	mov	[data],ax
	jmp	nexbt

spcfun:
	mov	dx,[data]
	cmp	al,-2
	jz	dorg
	cmp	al,-3
	jz	dput
dds:
;handle ds pseudo-op
	add	[pc],dx
	add	[hexadd],dx
	jmp	fixline

dorg:
;handle org pseudo-op
	mov	[pc],dx
	jmp	fixline

dput:
;handle put pseudo-op
	mov	[hexadd],dx
	jmp	fixline

outlin:
;copy the source line to the ouput device. line will be preceded by
;assembler-generated line number. this routine may be called several times
;on one line (once for each line of object code bytes), so it sets a flag
;so the line will only be output on the first call.
	mov	al,-1
	xchg	al,[linflg]
	or	al,al
	jnz	crlf		;output line only if first time
	mov	ax,[line]
	inc	ax
	mov	[line],ax
	mov	bh,0		;no leading zero suppression
	call	out10
	mov	al," "
	call	list
	mov	al,[lstfcb]
	cmp	al,'z'
	jz	crlf		;don't call nextchr if listing suppressed
	push	si		;save the only register destroyed by nextchr
outln:
	call	nextchr
	call	list
	cmp	al,10		;output until linefeed found
	jnz	outln
	pop	si
	ret

prtcnt:
	mov	ax,[errcnt]
	mov	bx,ercntm
prnt10:
	push	ax
	call	print
	pop	ax
	mov	bh,"0"-" "	;enable leading zero suppression
	call	out10
crlf:
	mov	al,13
	call	list
	mov	al,10
	jp	list

out10:
	xor	dx,dx
	mov	di,10000
	div	ax,di
	or	al,al		;>10,000?
	jnz	lead
	sub	al,"0"-" "	;convert leading zero to blank
lead:
	add	al,"0"
	call	list
	xchg	ax,dx
	mov	bl,100
	div	al,bl
	mov	bl,ah
	call	hidig		;convert to decimal and print 1000s digit
	call	digit		;print 100s digit
	mov	al,bl
	call	hidig		;convert to decimal and print 10s digit
	mov	bh,0		;ensure leading zero suppression is off
	jp	digit

hidig:
	aam			;convert binary to unpacked bcd
	or	ax,3030h	;add "0" bias
digit:
	xchg	al,ah
	cmp	al,"0"
	jz	supz
	mov	bh,0		;turn off zero suppression if not zero
supz:
	sub	al,bh		;convert leading zeros to blanks
	jp	list

strtlin:
	mov	b,[linflg],0
	mov	bx,[pc]
	mov	al,bh
	call	phex
	mov	al,bl
phexb:
	call	phex
	mov	al,' '
list:
	push	ax
	push	dx
	and	al,7fh
	mov	dl,al
	test	b,[lstdev],3	;see if output goes to console
	jz	prnchk
	mov	ah,2
	int	33		;output to console
prnchk:
	test	b,[lstdev],4	;see if output goes to printer
	jz	filchk
	mov	ah,5
	int	33		;output to printer
filchk:
	mov	al,dl
	pop	dx
	test	b,[lstdev],80h	;see if output goes to a file
	jz	listret
	call	wrtbuf
listret:
	pop	ax
	ret

wrtbuf:
	push	di
	mov	di,[lstpnt]
	stob
	cmp	di,lstbuf+lstbufsiz
	jnz	savpt
	push	ax
	push	cx
	push	dx
	call	flushbuf
	pop	dx
	pop	cx
	pop	ax
savpt:
	mov	[lstpnt],di
	pop	di
	ret

phex:
	push	ax
	call	uhalf
	call	list
	pop	ax
	call	lhalf
	jp	list

fini:
	or	b,[lstdev],1
	call	prtcnt
	mov	bx,symsize
	mov	ax,[6]
	sub	ax,[heap]		;size of symbol table
	call	prnt10
	mov	bx,fresize
	mov	ax,[heap]
	sub	ax,[code]		;free space remaining
	call	prnt10
	and	b,[lstdev],0feh
	mov	al,[hexfcb]
	cmp	al,'z'
	jz	symdmp
	mov	al,[hexcnt]
	cmp	al,-5
	jz	l0012
	call	enhexl
l0012:	
	mov	al,':'
	call	putchr
	mov	ch,10
hexend:
	push	cx
	mov	al,'0'
	call	putchr
	pop	cx
	dec	ch
	jnz	hexend
	mov	al,13
	call	putchr
	mov	al,10
	call	putchr
	mov	al,1ah
	call	putchr
	call	wrthex		;flush hex file buffer
	mov	dx,hexfcb
	mov	ah,close
	int	33
symdmp:
	mov	al,[symflg]
	cmp	al,'s'
	jnz	endsym
	mov	al,[lstdev]
	or	al,al		;any output device for symbol table dump?
	jnz	dosymtab
	or	al,1		;if not, send it to console
	mov	[lstdev],al
dosymtab:
	mov	bx,symmes
	call	print
	mov	dx,[base]
	mov	al,dh
	or	al,dl
	jz	endsym
	mov	b,[symlin],symwid  ;no symbols on this line yet
	mov	bx,[heap]
	mov	sp,bx		;need maximum stack for recursive tree walk
	call	node
endsym:
	test	b,[lstdev],80h	;print listing to file?
	jz	exit
	mov	al,1ah
	call	wrtbuf		;write end-of-file mark
	mov	di,[lstpnt]
	call	flushbuf
	mov	ah,close
	int	33
exit:	jmp	0

node:
	xchg	dx,bx
	push	bx
	mov	dl,[bx]
	mov	dh,0
	inc	bx
	add	bx,dx
	mov	dx,[bx]
	or	dx,dx
	jz	l0014
	call	node
l0014:	
	pop	bx
	mov	al,[bx]
	inc	bx
	mov	ch,al
	add	al,24
	shr	al
	shr	al
	shr	al
	mov	cl,al
	inc	cl		;invert last bit
	and	cl,1		;number of extra tabs needed (0 or 1)
	shr	al		;number of positions wide this symbol needs
	sub	[symlin],al
	jnc	wrtsym		;will it fit?
	sub	al,symwid
	neg	al
	mov	[symlin],al
	call	crlf		;start new line if not
wrtsym:
	mov	al,[bx]
	inc	bx
	call	list
	dec	ch
	jnz	wrtsym
	inc	cl
tabval:
	mov	al,9
	call	list
	loop	tabval
	inc	bx
	inc	bx
	push	bx
	mov	al,[bx+4]
	call	phex
	mov	al,[bx+3]
	call	phex
	cmp	b,[symlin],0	;will any more fit on line?
	jz	nexsymlin
	mov	al,9
	call	list
	jp	rightson
nexsymlin:
	call	crlf
	mov	b,[symlin],symwid
rightson:
	pop	bx
	mov	dx,[bx]
	or	dx,dx
	jnz	node
	ret

savcd:
	mov	[prev],al
	push	bx
	push	cx
	push	ax
	push	dx
	call	codbyt
	pop	dx
	mov	bx,count
	inc	b,[bx]
	mov	al,[bx]
	cmp	al,8
	jnz	noext
	mov	b,[bx],1
	call	outlin
	mov	al,' '
	mov	ch,5
tab:
	call	list
	dec	ch
	jnz	tab
noext:
	pop	ax
	call	phexb
	pop	cx
	inc	[pc]
	inc	[hexadd]
	pop	bx
	ret

reperr:
	or	al,al		;did an error occur?
	jz	ret
	inc	[errcnt]
	push	ax
	mov	bx,errmes	;print "error"
	call	print
	pop	ax
;we have error number in al. see if there's an error message for it
	mov	di,errtab
	mov	bl,80h
errlook:
	scasb			;do we have the error message
	jbe	havmes		;quit looking if we have it or passed it
	xchg	ax,bx		;put 80h in al to look for end of this message
nextmes:
	scasb			;look for high bit set in message
	ja	nextmes		;   which means we've reached the end
	xchg	ax,bx		;restore error number to al
	jmps	errlook		;keep looking

havmes:
	mov	bx,di		;put address of message in bx
	jz	prnerr		;do we have a message for this error?
	call	phex		;if not, just print error number
	jmp	crlf

prnerr:
	call	print
	jmp	crlf

print:
	mov	al,[bx]
	call	list
	or	al,al
	js	ret
	inc	bx
	jp	print

outa:
	mov	dl,al
out:
	and	dl,7fh
	mov	cl,2
system:
	call	5
	ret

codbyt:
	cmp	b,[hexfcb],"z"
	jz	ret
	push	ax
	mov	dx,[lastad]
	mov	bx,[hexadd]
	mov	[lastad],bx
	inc	dx
	mov	al,[hexcnt]
	cmp	al,-5
	jz	newlin
	cmp	bx,dx
	jz	afhex
	call	enhexl
newlin:
	mov	al,':'
	call	putchr
	mov	al,-4
	mov	[hexcnt],al
	xor	al,al
	mov	[chksum],al
	mov	bx,[hexpnt]
	mov	[hexlen],bx
	call	hexbyt
	mov	al,[hexadd+1]
	call	hexbyt
	mov	al,[hexadd]
	call	hexbyt
	xor	al,al
	call	hexbyt
afhex:
	pop	ax
hexbyt:
	mov	ch,al
	mov	bx,chksum
	add	al,[bx]
	mov	[bx],al
	mov	al,ch
	call	uhalf
	call	putchr
	mov	al,ch
	call	lhalf
	call	putchr
	mov	bx,hexcnt
	inc	b,[bx]
	mov	al,[bx]
	cmp	al,26
	jnz	ret
enhexl:
	mov	di,[hexlen]
	mov	ch,al
	call	uhalf
	stob
	mov	al,ch
	call	lhalf
	stob
	mov	al,-6
	mov	[hexcnt],al
	mov	al,[chksum]
	add	al,ch
	neg	al
	call	hexbyt
	mov	al,13
	call	putchr
	mov	al,10
	call	putchr
wrthex:
;write out the line
	mov	dx,hexbuf
	mov	[hexpnt],dx
	mov	ah,setdma
	int	33
	sub	di,dx		;length of buffer
	mov	cx,di
	mov	dx,hexfcb
	mov	ah,blkwrt
	int	33
	or	al,al
	jnz	dskful
	ret

putchr:
	mov	di,[hexpnt]
	stob
	mov	[hexpnt],di
	ret

flushbuf:
	mov	cx,di
	mov	dx,lstbuf
	mov	di,dx
	sub	cx,dx
	jz	ret		;buffer empty?
	mov	ah,setdma
	int	33
	mov	dx,lstfcb
	mov	ah,blkwrt
	int	33
	or	al,al
	jz	ret
dskful:
	mov	bx,wrterr
	jmp	prerr

uhalf:
	rcr	al
	rcr	al
	rcr	al
	rcr	al
lhalf:
	and	al,0fh
	or	al,30h
	cmp	al,'9'+1
	jc	ret
	add	al,7
	ret

none:	db	0

; 8086 mnemonic table

; this table is actually a sequence of subtables, each starting with a label.
; the label signifies which mnemonics the subtable applies to--a3, for example,
; means all 3-letter mnemonics beginning with a.

a3:
	db	7
	db	'dd'
	dw	grp7
	db	2
	db	'nd'
	dw	grp13
	db	22h
	db	'dc'
	dw	grp7
	db	12h
	db	'aa'
	dw	put
	db	37h
	db	'as'
	dw	put
	db	3fh
	db	'am'
	dw	grp11
	db	0d4h
	db	'ad'
	dw	grp11
	db	0d5h
a5:
	db	1
	db	'lign'
	dw	align
	db	0
c3:
	db	7
	db	'mp'
	dw	grp7
	db	3ah
	db	'lc'
	dw	put
	db	0f8h
	db	'ld'
	dw	put
	db	0fch
	db	'li'
	dw	put
	db	0fah
	db	'mc'
	dw	put
	db	0f5h
	db	'bw'
	dw	put
	db	98h
	db	'wd'
	dw	put
	db	99h
c4:
	db	3
	db	'all'
	dw	grp14
	db	9ah
	db	'mpb'
	dw	put
	db	0a6h
	db	'mpw'
	dw	put
	db	0a7h
c5:
	db	2
	db	'mpsb'
	dw	put
	db	0a6h
	db	'mpsw'
	dw	put
	db	0a7h
d2:
	db	5
	db	'b'
	dw	grp23
	db	1
	db	'w'
	dw	grp23
	db	0
	db	'm'
	dw	grp23
	db	2
	db	's'
	dw	grp5
	db	1
	db	'i'
	dw	put
	db	0fah
d3:
	db	4
	db	'ec'
	dw	grp8
	db	49h
	db	'iv'
	dw	grp10
	db	30h
	db	'aa'
	dw	put
	db	27h
	db	'as'
	dw	put
	db	2fh
d4:
	db	1
	db	'own'
	dw	put
	db	0fdh
e2:
	db	1
	db	'i'
	dw	put
	db	0fbh
e3:
	db	3
	db	'qu'
	dw	grp5
	db	2
	db	'sc'
	dw	grp19
	db	0d8h
	db	'nd'
	dw	end
	db	0
e5:
	db	1
	db	'ndif'
	dw	endif
	db	0
h3:
	db	1
	db	'lt'
	dw	put
	db	0f4h
h4:
	db	1
	db	'alt'
	dw	put
	db	0f4h
i2:
	db	2
	db	'n'
	dw	grp4
	db	0e4h
	db	'f'
	dw	grp5
	db	4
i3:
	db	4
	db	'nc'
	dw	grp8
	db	41h
	db	'nb'
	dw	grp4
	db	0e4h
	db	'nw'
	dw	grp4
	db	0e5h
	db	'nt'
	dw	grp18
	db	0cch
i4:
	db	4
	db	'mul'
	dw	grp10
	db	28h
	db	'div'
	dw	grp10
	db	38h
	db	'ret'
	dw	put
	db	0cfh
	db	'nto'
	dw	put
	db	0ceh
j2:
	db	10
	db	'p'
	dw	grp17
	db	0ebh
	db	'z'
	dw	grp17
	db	74h
	db	'e'
	dw	grp17
	db	74h
	db	'l'
	dw	grp17
	db	7ch
	db	'b'
	dw	grp17
	db	72h
	db	'a'
	dw	grp17
	db	77h
	db	'g'
	dw	grp17
	db	7fh
	db	'o'
	dw	grp17
	db	70h
	db	's'
	dw	grp17
	db	78h
	db	'c'
	dw	grp17
	db	72h
j3:
	db	17
	db	'mp'
	dw	grp14
	db	0eah
	db	'nz'
	dw	grp17
	db	75h
	db	'ne'
	dw	grp17
	db	75h
	db	'nl'
	dw	grp17
	db	7dh
	db	'ge'
	dw	grp17
	db	7dh
	db	'nb'
	dw	grp17
	db	73h
	db	'ae'
	dw	grp17
	db	73h
	db	'nc'
	dw	grp17
	db	73h
	db	'ng'
	dw	grp17
	db	7eh
	db	'le'
	dw	grp17
	db	7eh
	db	'na'
	dw	grp17
	db	76h
	db	'be'
	dw	grp17
	db	76h
	db	'pe'
	dw	grp17
	db	7ah
	db	'np'
	dw	grp17
	db	7bh
	db	'po'
	dw	grp17
	db	7bh
	db	'no'
	dw	grp17
	db	71h
	db	'ns'
	dw	grp17
	db	79h
j4:
	db	6
	db	'mps'
	dw	grp17
	db	0ebh
	db	'cxz'
	dw	grp17
	db	0e3h
	db	'nge'
	dw	grp17
	db	7ch
	db	'nae'
	dw	grp17
	db	72h
	db	'nbe'
	dw	grp17
	db	77h
	db	'nle'
	dw	grp17
	db	7fh
l3:
	db	3
	db	'ea'
	dw	grp6
	db	8dh
	db	'ds'
	dw	grp6
	db	0c5h
	db	'es'
	dw	grp6
	db	0c4h
l4:
	db	5
	db	'oop'
	dw	grp17
	db	0e2h
	db	'odb'
	dw	put
	db	0ach
	db	'odw'
	dw	put
	db	0adh
	db	'ahf'
	dw	put
	db	9fh
	db	'ock'
	dw	put
	db	0f0h
l5:
	db	4
	db	'oope'
	dw	grp17
	db	0e1h
	db	'oopz'
	dw	grp17
	db	0e1h
	db	'odsb'
	dw	put
	db	0ach
	db	'odsw'
	dw	put
	db	0adh
l6:
	db	2
	db	'oopne'
	dw	grp17
	db	0e0h
	db	'oopnz'
	dw	grp17
	db	0e0h
m3:
	db	2
	db	'ov'
	dw	grp1
	db	88h
	db	'ul'
	dw	grp10
	db	20h
m4:
	db	2
	db	'ovb'
	dw	put
	db	0a4h
	db	'ovw'
	dw	put
	db	0a5h
m5:
	db	2
	db	'ovsb'
	dw	put
	db	0a4h
	db	'ovsw'
	dw	put
	db	0a5h
n3:
	db	3
	db	'ot'
	dw	grp9
	db	10h
	db	'eg'
	dw	grp9
	db	18h
	db	'op'
	dw	put
	db	90h
o2:
	db	1
	db	'r'
	dw	grp13
	db	0ah
o3:
	db	2
	db	'ut'
	dw	grp4
	db	0e6h
	db	'rg'
	dw	grp5
	db	0
o4:
	db	2
	db	'utb'
	dw	grp4
	db	0e6h
	db	'utw'
	dw	grp4
	db	0e7h
p3:
	db	2
	db	'op'
	dw	grp22
	db	8fh
	db	'ut'
	dw	grp5
	db	3
p4:
	db	2
	db	'ush'
	dw	grp2
	db	0ffh
	db	'opf'
	dw	put
	db	9dh
p5:
	db	1
	db	'ushf'
	dw	put
	db	9ch
r3:
	db	6
	db	'et'
	dw	grp16
	db	0c3h
	db	'ep'
	dw	put
	db	0f3h
	db	'ol'
	dw	grp12
	db	0
	db	'or'
	dw	grp12
	db	8
	db	'cl'
	dw	grp12
	db	10h
	db	'cr'
	dw	grp12
	db	18h
r4:
	db	2
	db	'epz'
	dw	put
	db	0f3h
	db	'epe'
	dw	put
	db	0f3h
r5:
	db	2
	db	'epnz'
	dw	put
	db	0f2h
	db	'epne'
	dw	put
	db	0f2h
s3:
	db	11
	db	'ub'
	dw	grp7
	db	2ah
	db	'bb'
	dw	grp7
	db	1ah
	db	'bc'
	dw	grp7
	db	1ah
	db	'tc'
	dw	put
	db	0f9h
	db	'td'
	dw	put
	db	0fdh
	db	'ti'
	dw	put
	db	0fbh
	db	'hl'
	dw	grp12
	db	20h
	db	'hr'
	dw	grp12
	db	28h
	db	'al'
	dw	grp12
	db	20h
	db	'ar'
	dw	grp12
	db	38h
	db	'eg'
	dw	grp21
	db	26h
s4:
	db	5
	db	'cab'
	dw	put
	db	0aeh
	db	'caw'
	dw	put
	db	0afh
	db	'tob'
	dw	put
	db	0aah
	db	'tow'
	dw	put
	db	0abh
	db	'ahf'
	dw	put
	db	9eh
s5:
	db	4
	db	'casb'
	dw	put
	db	0aeh
	db	'casw'
	dw	put
	db	0afh
	db	'tosb'
	dw	put
	db	0aah
	db	'tosw'
	dw	put
	db	0abh
t4:
	db	1
	db	'est'
	dw	grp20
	db	84h
u2:
	db	1
	db	'p'
	dw	put
	db	0fch
w4:
	db	1
	db	'ait'
	dw	put
	db	9bh
x3:
	db	1
	db	'or'
	dw	grp13
	db	32h
x4:
	db	2
	db	'chg'
	dw	grp3
	db	86h
	db	'lat'
	dw	put
	db	0d7h


; 8087 mnemonic table
; similar to 8086 table above, except not distinguished by opcode length

xm1:	;f2xm1
	db	1		;one opcode
	dm	"xm1"
	db	1,0f0h

ndpa:
	db	3
	dm	"dd"
	db	6+arith,0c1h
	dm	"ddp"
	db	needop+stackop,0
	dm	"bs"
	db	1,0e1h

ndpb:
	db	2
	dm	"ld"
	db	7+needop+memory,20h
	dm	"stp"
	db	7+needop+memory,30h

ndpc:
	db	5
	dm	"om"
	db	0+onereg+real,0d1h
	dm	"omp"
	db	0+onereg+real,0d9h
	dm	"hs"
	db	1,0e0h
	dm	"ompp"
	db	6,0d9h
	dm	"lex"
	db	3,0e2h

ndpd:
	db	6
	dm	"iv"
	db	6+arith,0f1h
	dm	"ivp"
	db	needop+stackop,30h
	dm	"ivr"
	db	6+arith,0f9h
	dm	"ivrp"
	db	needop+stackop,38h
	dm	"ecstp"
	db	1,0f6h
	dm	"isi"
	db	3,0e1h

ndpe:
	db	1
	dm	"ni"
	db	3,0e0h

ndpf:
	db	1
	dm	"ree"
	db	5+needop+onereg,0

ndpi:
	db	13
	dm	"add"
	db	2+needop+integer,0
	dm	"ld"
	db	3+needop+integer+extended,0
	dm	"sub"
	db	2+needop+integer,20h
	dm	"stp"
	db	3+needop+integer+extended,18h
	dm	"st"
	db	3+needop+integer,10h
	dm	"mul"
	db	2+needop+integer,8
	dm	"div"
	db	2+needop+integer,30h
	dm	"subr"
	db	2+needop+integer,28h
	dm	"divr"
	db	2+needop+integer,38h
	dm	"com"
	db	2+needop+integer,10h
	dm	"comp"
	db	2+needop+integer,18h
	dm	"ncstp"
	db	1,0f7h
	dm	"nit"
	db	3,0e3h

ndpl:
	db	10
	dm	"d"
	db	1+needop+onereg+real+extended,0
	dm	"dz"
	db	1,0eeh
	dm	"d1"
	db	1,0e8h
	dm	"dpi"
	db	1,0ebh
	dm	"dl2t"
	db	1,0e9h
	dm	"dl2e"
	db	1,0eah
	dm	"dlg2"
	db	1,0ech
	dm	"dln2"
	db	1,0edh
	dm	"dcw"
	db	1+needop+memory,28h
	dm	"denv"
	db	1+needop+memory,20h

ndpm:
	db	2
	dm	"ul"
	db	6+arith,0c9h
	dm	"ulp"
	db	needop+stackop,8

ndpo:
	db	1
	dm	"p"
	db	needop+1,0	;flag special handling

ndpn:
	db	1
	dm	"op"
	db	1,0d0h

ndpp:
	db	3
	dm	"rem"
	db	1,0f8h
	dm	"tan"
	db	1,0f2h
	dm	"atan"
	db	1,0f3h

ndpr:
	db	2
	dm	"ndint"
	db	1,0fch
	dm	"stor"
	db	5+needop+memory,20h

ndps:
	db	12
	dm	"t"
	db	5+needop+onereg+real,0d0h
	dm	"tp"
	db	7+needop+onereg+real+extended,0d8h
	dm	"ub"
	db	6+arith,0e1h
	dm	"ubp"
	db	needop+stackop,0e0h
	dm	"ubr"
	db	6+arith,0e9h
	dm	"ubrp"
	db	needop+stackop,0e8h
	dm	"qrt"
	db	1,0fah
	dm	"cale"
	db	1,0fdh
	dm	"ave"
	db	5+needop+memory,30h
	dm	"tcw"
	db	1+needop+memory,38h
	dm	"tenv"
	db	1+needop+memory,30h
	dm	"tsw"
	db	5+needop+memory,38h

ndpt:
	db	1
	dm	"st"
	db	1,0e4h

ndpw:
	db	1
	dm	"ait"
	db	needop,0	;flag special handling

ndpx:
	db	3
	dm	"ch"
	db	1+onereg,0c9h
	dm	"am"
	db	1,0e5h
	dm	"tract"
	db	1,0f4h

ndpy:
	db	2
	dm	"l2x"
	db	1,0f1h
	dm	"l2xp1"
	db	1,0f9h


optab:
; table of pointers  to mnemonics. for each letter of the alphabet (the
; starting letter of the mnemonic), there are 5 entries. each entry
; corresponds to a mnemonic whose length is 2, 3, 4, 5, and 6 characters
; long, respectively. if there are no mnemonics for a given combination
; of first letter and length (such as a-2), then the corresponding entry
; points to none. otherwise, it points to a place in the mnemonic table
; for that type.

; this table only needs to be modified if a mnemonic is added to a group
; previously marked none. change the none to a label made up of the first
; letter of the mnemonic and its length, then add a new subsection to
; the mnemonic table in alphabetical order.

	dw	none
	dw	a3
	dw	none
	dw	a5
	dw	none
	dw	none	;b
	dw	none
	dw	none
	dw	none
	dw	none
	dw	none	;c
	dw	c3
	dw	c4
	dw	c5
	dw	none
	dw	d2	;d
	dw	d3
	dw	d4
	dw	none
	dw	none
	dw	e2	;e
	dw	e3
	dw	none
	dw	e5
	dw	none
	dw	none	;f
	dw	none
	dw	none
	dw	none
	dw	none
	dw	none	;g
	dw	none
	dw	none
	dw	none
	dw	none
	dw	none	;h
	dw	h3
	dw	h4
	dw	none
	dw	none
	dw	i2	;i
	dw	i3
	dw	i4
	dw	none
	dw	none
	dw	j2	;j
	dw	j3
	dw	j4
	dw	none
	dw	none
	dw	none	;k
	dw	none
	dw	none
	dw	none
	dw	none
	dw	none	;l
	dw	l3
	dw	l4
	dw	l5
	dw	l6
	dw	none	;m
	dw	m3
	dw	m4
	dw	m5
	dw	none
	dw	none	;n
	dw	n3
	dw	none
	dw	none
	dw	none
	dw	o2	;o
	dw	o3
	dw	o4
	dw	none
	dw	none
	dw	none	;p
	dw	p3
	dw	p4
	dw	p5
	dw	none
	dw	none	;q
	dw	none
	dw	none
	dw	none
	dw	none
	dw	none	;r
	dw	r3
	dw	r4
	dw	r5
	dw	none
	dw	none	;s
	dw	s3
	dw	s4
	dw	s5
	dw	none
	dw	none	;t
	dw	none
	dw	t4
	dw	none
	dw	none
	dw	u2	;u
	dw	none
	dw	none
	dw	none
	dw	none
	dw	none	;v
	dw	none
	dw	none
	dw	none
	dw	none
	dw	none	;w
	dw	none
	dw	w4
	dw	none
	dw	none
	dw	none	;x
	dw	x3
	dw	x4
	dw	none
	dw	none
	dw	none	;y
	dw	none
	dw	none
	dw	none
	dw	none
	dw	none	;z
	dw	none
	dw	none
	dw	none
	dw	none

ndptab:
;lookup table for 8087 mnemonics. there is one entry for each letter of the
;alphabet
	dw	ndpa
	dw	ndpb
	dw	ndpc
	dw	ndpd
	dw	ndpe
	dw	ndpf
	dw	none	;g
	dw	none	;h
	dw	ndpi
	dw	none	;j
	dw	none	;k
	dw	ndpl
	dw	ndpm
	dw	ndpn
	dw	ndpo
	dw	ndpp
	dw	none	;q
	dw	ndpr
	dw	ndps
	dw	ndpt
	dw	none	;u
	dw	none	;v
	dw	ndpw
	dw	ndpx
	dw	ndpy
	dw	none	;z

;error message table

errtab:
	dm	1,"register not allowed in immediate value"
	dm	2,"index or base register must be bp, bx, si, or di"
	dm	3,"only one base register (bx, bp) allowed"
	dm	4,"only one index register (si or di) allowed"
	dm	5,"only addition allowed on register or undefined label"
	dm	6,"only one undefined label per expression allowed"
	dm	7,"illegal digit in hexadecimal number"
	dm	8,"illegal digit in decimal number"
	dm	10,"illegal character in label or opcode"
	dm	11,"label defined twice"
	dm	12,"opcode not recognized"
	dm	20,"invalid operand"
	dm	21,'"," and second operand expected'
	dm	22,"register mismatch"
	dm	23,"immediate operand not allowed"
	dm	24,'"]" expected'
	dm	25,"two memory operands not allowed"
	dm	26,"destination must not be immediate value"
	dm	27,"both operands must not be registers"
	dm	28,"operand must be segment register"
	dm	29,"first operand must be register"
	dm	30,"undefined label not allowed"
	dm	31,"value out of range"
	dm	32,"missing or illegal operand size flag"
	dm	33,"must have label on same line"
	dm	35,"zero-length string illegal"
	dm	36,"endif without if"
	dm	37,"one-character strings only"
	dm	38,"illegal expression"
	dm	39,"end of string not found"
	dm	100,"undefined label"
	dm	101,"value out of range (forward)"
	db	255

errmes:	dm	'***** error:  '
nospac:	db	13,10,'file creation error',13,10,"$"
nomem:	db	13,10,'insufficient memory',13,10,'$'
nofile:	db	13,10,'file not found',13,10,'$'
wrterr:	db	13,10,'disk full',13,10,'$'
baddsk:	db	13,10,'bad disk specifier',13,10,'$'
ercntm:	dm	13,10,13,10,'error count ='
symsize	dm	13,10,'symbol table size = '
fresize	dm	      'free space =        '
symmes:	dm	13,10,'symbol table',13,10,13,10
extend:	db	'asm',0,0
ifend:	db	5,'endif'
ifnest:	db	2,'if'
retstr:	dm	'ret'
hexfcb:	db	0,'        hex',0,0,0,0
	ds	16
	db	0,0,0,0,0
lstfcb:	db	0,'        prn',0,0,0,0
	ds	16
	db	0,0,0,0,0
pc:	ds	2
oldpc:	ds	2
labpt:	ds	2
flag:	ds	1
maxflg:	ds	1
addr:	ds	2
alabel:	ds	2
data:	ds	2
dlabel:	ds	2
con:	ds	2
undef:	ds	2
lenid:	ds	1
id:	ds	80
chr:	ds	1
sym:	ds	1
base:	ds	2
heap:	ds	2
symflg:	ds	1
symlin:	ds	1
code:	ds	2
datsiz:	ds	1
reloc:	ds	1
bcount:	ds	1
count:	ds	1
err:	ds	1
line:	ds	2
hexlen:	ds	2
hexadd:	ds	2
lastad:	ds	2
hexcnt:	ds	1
chksum:	ds	1
linflg:	ds	1
prev:	ds	1
ifflg:	ds	1
chklab:	ds	1
errcnt:	ds	2
lstret:	ds	2
retpt:	ds	2
lstdev:	ds	2
spc:	ds	1
nowait:	ds	1
ix:	ds	2
iy:	ds	2
hexpnt:	ds	2
lstpnt:	ds	2
hexbuf:	ds	hexbufsiz
lstbuf:	ds	lstbufsiz
bufpt:	ds	2
srcbuf:	ds	bufsiz
	ds	100h
	align
stack:	equ	$
start:	equ	$
