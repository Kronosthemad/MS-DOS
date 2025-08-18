	title	ms-dos version 1.25 by tim paterson     march 3, 1982
	page	60,132
; use the following booleans to set the switches 
false	equ	0
true	equ	not false

; use the switches below to produce the standard microsoft version of the ibm
; version of the operating system
msver	equ	true
ibm	equ	false

; set this switch to cause dos to move itself to the end of memory
highmem	equ	true

; turn on switch below to allow testing disk code with debug. it sets
; up a different stack for disk i/o (functions > 11) than that used for
; character i/o which effectively makes the dos re-entrant.

dsktest	equ	false

	include	msdos.asm

