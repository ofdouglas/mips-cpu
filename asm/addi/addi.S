	.section .text
	.global  _start
	.set noreorder
	.set noat

start:
	addi	$1, $0, 1
	addi	$1, $1, 2
	addi	$1, $1, 4
	addi	$1, $1, 8
	addi	$1, $1, 16
	addi	$1, $1, 32
	
	
