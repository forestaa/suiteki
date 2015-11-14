	.text
ack.15:
	li	$v0, 0
	sub	$at, $a0, $v0
	bgtz	$at, blez_else.34
	li	$v0, 1
	add	$v0, $a1, $v0
	jr	$ra
blez_else.34:
	li	$v0, 0
	sub	$at, $a1, $v0
	bgtz	$at, blez_else.35
	li	$v0, 1
	sub	$v0, $a0, $v0
	li	$v1, 1
	move	$a1, $v1
	move	$a0, $v0
	j	ack.15
blez_else.35:
	li	$v0, 1
	sub	$v0, $a0, $v0
	li	$v1, 1
	sub	$v1, $a1, $v1
	sw	$v0, 0($sp)
	move	$t8, $ra
	move	$a1, $v1
	sw	$t8, 4($sp)
	addi	$sp, $sp, 8
	jal	ack.15
	addi	$sp, $sp, -8
	lw	$t8, 4($sp)
	move	$v1, $v0
	move	$ra, $t8
	lw	$v0, 0($sp)
	move	$a1, $v1
	move	$a0, $v0
	j	ack.15
_min_caml_start: # main entry point
   # main program start
	li	$v0, 3
	li	$v1, 10
	move	$t8, $ra
	move	$a1, $v1
	move	$a0, $v0
	sw	$t8, 4($sp)
	addi	$sp, $sp, 8
	jal	ack.15
	addi	$sp, $sp, -8
	lw	$t8, 4($sp)
	move	$ra, $t8
	move	$t8, $ra
	move	$a0, $v0
	sw	$t8, 4($sp)
	addi	$sp, $sp, 8
	jal	min_caml_print_int
	addi	$sp, $sp, -8
	lw	$t8, 4($sp)
	move	$ra, $t8
   # main program end
	jr	$ra
