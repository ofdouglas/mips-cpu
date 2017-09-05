

	.section .text
	.global _start
	.set noreorder
	.set noat
	
start:	ori	$2, $0, 6
	lui	$1, 0xdead
	ori	$1, 0xbeef
	
	sll	$3, $1, 7		# 0x56df7780
	srl	$4, $1, 7		# 0x01bd5b7d
	sra	$5, $1, 7		# 0xffbd5b7d
	sllv	$6, $5, $2		# 0xef56df40
	srlv	$7, $5, $2		# 0x03fef56d
	srav	$8, $5, $2		# 0xfffef56d

	
	

	
	
