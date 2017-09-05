	.section .text
	.global  start
	.set     noreorder
#	.set     noat		

	
start:	addiu	$t0, $zero, 27
	addiu	$t1, $zero, 4
	divu	$t0, $t1
	mflo	$s0
	mfhi	$s1
	
	addiu	$t0, $zero, -27
	addiu	$t1, $zero, 4
	div	$t0, $t1
	mflo	$s2
	mfhi	$s3

	lui	$t0 0x0003
	ori	$t0, $t0, 1
	addiu	$t1, $zero, 0x8001
	multu	$t0, $t1
	mflo	$s4
	mfhi	$s5
	
	addiu	$t0, $zero, -2
	addiu	$t1, $zero, 3
	mult	$t0, $t1
	mflo	$s6
	mfhi	$s7
	
	j	exit
	nop
	
	

exit:	beq	$zero, $zero, exit
	nop
	
	
