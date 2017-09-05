	.set noreorder
	.set noat

	.section .text
	.global  start
start:
reset_exc:
	nop
	b 	begin
	nop
	
tlb_miss_exc:
	nop
	b	start
	nop

general_exc:
	nop
	b	start
	nop

begin:	
	ori	$1, 0x00ff
	ori	$2, 0xffff
	la	$3, 0xbfc12000		# address of LEDs

loop:	sw	$1, 0($3)
	nop
	sw	$2, 0($3)
	nop
	j	loop
	nop

