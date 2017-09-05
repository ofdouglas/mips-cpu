	.section .text
	.global  start
	.set     noreorder
	.set     noat

	
start:  jal	funct
	addiu	$1, $0, 1
	addiu	$3, $0, 3
	j	exit
	nop
	
	
funct:	jr	$ra
	addiu	$2, $0, 2



exit:	beq	$0, $0, exit
	nop
	
	
