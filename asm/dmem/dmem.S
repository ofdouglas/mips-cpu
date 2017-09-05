	.section .text
	.global  _start
	.set noreorder
	.set noat

start:
	lui	$1, 0xbfc0
	ori	$1, 0x0400
	lui	$2, 0xdead
	ori	$2, 0xbeef
	
	sw	$2, 0($1)		
	sw	$2, 4($1)		
	sh	$2, 8($1)		
	sh	$2, 10($1)		
	sb	$2, 13($1)		
	sb	$2, 14($1)		
	
	lw	$3, 0($1)		# 0xdeadbeef
	lw	$4, 4($1)		# 0xdeadbeef
	lw	$5, 8($1)		# 0xbeefbeef
	lh	$6, 8($1)		# 0xffffbeef
	lhu	$7, 8($1)		# 0x0000beef
	lb	$8, 13($1)		# 0xffffffef
	lbu	$9, 13($1)		# 0x000000ef
#	lwl	$10, 
#	lwr	$10, 

	
