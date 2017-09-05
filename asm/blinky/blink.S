	.set noreorder
	.set noat

	.section .text
	.global  start
start:
reset_exc:	
	b 	begin
	nop
	
tlb_miss_exc:	
	b	reset_exc
	nop

general_exc:
	addiu	$3, $3, 1
	mfc0	$k0, $14
	jr	$k0
	rfe

begin:	
	li	$1, 100000		# 1e5 for 1ms interrupt
#	li 	$1, 100			# 100 for 1us interrupt
	mtc0 	$1, $11
	mfc0	$1, $12
	ori	$1, 0x410		# StatusReg: enable interrupts
	mtc0	$1, $12			# and unmask timer interrupt
	li	$3, 0
	la	$4, 0xbfc12000		# address of our 'led'

loop:	slti	$5, $3, 500
	bne	$5, $0, loop
	nop
	
	addu	$3, $0, $0
	lw	$6, 0($4)
	xori	$6, $6, 1
	sw	$6, 0($4)
	j	loop


