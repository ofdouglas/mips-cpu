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
	lw	$3, 0($1)
	
