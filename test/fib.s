	.text
fib.10:
	li	$v0, 1
	sub	$at, $a0, $v0
	bgtz	$at, blez_else.24
	move	$v0, $a0
	jr	$ra
blez_else.24:
	li	$v0, 1
	sub	$v0, $a0, $v0
	sw	$a0, 0($sp)
	move	$t8, $ra
	move	$a0, $v0
	sw	$t8, 4($sp)
	addi	$sp, $sp, 8
	jal	fib.10
	addi	$sp, $sp, -8
	lw	$t8, 4($sp)
	move	$ra, $t8
	li	$v1, 2
	lw	$a0, 0($sp)
	sub	$v1, $a0, $v1
	sw	$v0, 4($sp)
	move	$t8, $ra
	move	$a0, $v1
	sw	$t8, 12($sp)
	addi	$sp, $sp, 16
	jal	fib.10
	addi	$sp, $sp, -16
	lw	$t8, 12($sp)
	move	$ra, $t8
	lw	$v1, 4($sp)
	add	$v0, $v1, $v0
	jr	$ra
_min_caml_start: # main entry point
   # main program start
	li	$v0, 10
	move	$t8, $ra
	move	$a0, $v0
	sw	$t8, 4($sp)
	addi	$sp, $sp, 8
	jal	fib.10
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
