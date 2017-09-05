	.section .text
	.global  start
	.set noreorder
	.set noat

	
start:
	lui	$1, 0xdead
	ori	$1, 0xbeef
	lui	$2, 0xabad
	ori	$2, 0xc0de

	nor	$3, $2, $1	   # 0x00520100 
	xor	$4, $2, $1	   # 0x75007e31
	or	$5, $2, $1	   # 0xffadfeff
	and	$6, $2, $1	   # 0x8aad80ce
	addu	$7, $2, $1	   # 0x8a5b7fcd
	subu	$8, $2, $1	   # 0xcd0001ef
	subu	$9, $1, $2	   # 0x32fffe11

	slt	$10, $1, $2 	   # 0	!
	slt	$11, $2, $1 	   # 1
	slt	$12, $2, $2 	   # 0	!
	sltu	$13, $1, $2 	   # 0
	sltu	$14, $2, $1 	   # 1
	sltu	$15, $2, $2 	   # 0

	ori	$16, $1, 0xf00f    # 0xdeadfeef
	andi	$17, $1, 0xf00f    # 0x0000b00f
	xori	$18, $1, 0xfeeb    # 0xdead4004
	addiu	$19, $1, 1	   # 0xdeadbef0
	addiu	$20, $1, -1	   # 0xdeadbeee

	addiu	$21, $0, -1	   # 0xffffffff
	addiu	$22, $0, 1	   # 0x00000001

	slti	$23, $21, 0x0001   # 1
	slti	$24, $0,  0x7fff   # 1	!
	slti	$25, $22, 0xffff   # 0
	
	sltiu	$26, $22, 0xffff   # 1	
	sltiu	$27, $0,  0x0001   # 1
	sltiu	$28, $22, 0xffff   # 1
