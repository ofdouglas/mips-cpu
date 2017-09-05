	.section .text
	.global  _start
	.set noreorder
	.set noat

start:
	ori	$1, 0x0008
loop:	addi	$1, $1, -1
	bne	$1, $0, loop
	nop
	ori	$2, 0xffff





	
	
