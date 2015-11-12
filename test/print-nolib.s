	.text
min_caml_print_int:
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	jal	div10
	addi    $sp, $sp, -4
	lw	$ra, 0($sp)
	bgtz	$v0, min_caml_print_int_rec
	j	min_caml_print_int_final
min_caml_print_int_rec:
	sw	$ra, 0($sp)
	sw	$v1, 4($sp)
	move	$a0, $v0
	addi    $sp, $sp, 8
	jal	min_caml_print_int
	addi    $sp, $sp, -8
	lw	$ra, 0($sp)
	lw	$v1, 4($sp)
min_caml_print_int_final:
	addi	$a0, $v1, 48
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	jal	min_caml_print_byte
	addi    $sp, $sp, -4
	lw	$ra, 0($sp)
	jr	$ra
min_caml_print_byte:
	li	$v0, 11
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
	sw	$ra, 0($sp)
	jr	$ra
div10:
	move	$t8, $a0
	li	$t0, 10 # de = 10
	li	$t1, 1 # temp = 1
	li	$t2, 0 # quotient = 0
div10_shift:
	sub	$t3, $a0, $t0 # t3 = a0 - t0 = nu - de
	bltz	$t3, div10_calc # nu - de > 0 <=> not (nu-de <= 0)
	sll	$t0, $t0, 1 # de <<= 1;
	sll	$t1, $t1, 1 # temp <<= 1
	j	div10_shift
div10_calc:
	li	$t3, 1
	sub	$t3, $t1, $t3 # t3 = t1 - 1
	blez	$t3, div10_return # temp - 1 <= 0 <=> not (temp - 1 > 0)
	srl	$t0, $t0, 1
	srl	$t1, $t1, 1
	sub	$t3, $a0, $t0 # t3 = nu - de
	bgez	$t3, div10_calc_if # nu - de >= 0 <=> nu >= de
	j	div10_calc
div10_calc_if:
	sub	$a0, $a0, $t0
	add	$t2, $t2, $t1
	j	div10_calc
div10_return:
	move	$a0, $t2
	li	$a1, 10
	sw	$t2, 4($sp)
	sw	$ra, 8($sp)
	addi	$sp, $sp, 12
	jal	mul
	sub	$v1, $t8, $v0
	addi	$sp, $sp, -12
	lw	$ra, 8($sp)
	lw	$v0, 4($sp)
	jr	$ra
mul:
	li	$v0, 0
	li	$t0, 1
mul_calc:
	blez	$a1, mul_return
	add	$v0, $v0, $a0
	sub	$a1, $a1, $t0
	j	mul_calc
mul_return:
	jr	$ra
_min_caml_start: # main entry point
   # main program start
	li	$a0, 123
	sw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jal	min_caml_print_int
	addi	$sp, $sp, -4
	lw	$ra, 0($sp)
   # main program end
	jr	$ra
