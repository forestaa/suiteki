	.text
min_caml_print_newline:
	li	$a0, 10
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	jal	min_caml_print_byte
	addi    $sp, $sp, 4
	lw	$ra, 0($sp)
	jr	$ra
min_caml_print_int:
	bltz	$a0, min_caml_print_int_minus
	j	min_caml_print_int_cont
min_caml_print_int_minus:
	# print '-'
	sw	$a0, 0($sp)
	li	$a0, 45
	sw	$ra, -1($sp)
	addi    $sp, $sp, -4
	jal	min_caml_print_byte
	addi    $sp, $sp, 4
	lw	$ra, -1($sp)
	lw	$a0, 0($sp)
	li	$t0, 0
	sub	$a0, $t0, $a0 # negate a0
min_caml_print_int_cont:
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	jal	div10
# この時点でv0が商、v1があまり。
	addi    $sp, $sp, 4
	lw	$ra, 0($sp)
	bgtz	$v0, min_caml_print_int_rec
	j	min_caml_print_int_final
min_caml_print_int_rec:
	sw	$ra, 0($sp)
	sw	$v1, -4($sp)
	move	$a0, $v0
	addi    $sp, $sp, -8
	jal	min_caml_print_int_cont
	addi    $sp, $sp, 8
	lw	$ra, 0($sp)
	lw	$v1, -4($sp)
min_caml_print_int_final:
	addi	$a0, $v1, 48
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	jal	min_caml_print_byte
	addi    $sp, $sp, 4
	lw	$ra, 0($sp)
	jr	$ra
min_caml_print_byte:
	li	$v0, 11
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	syscall
	addi    $sp, $sp, 4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_prerr_int:
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	jal	div10
	addi    $sp, $sp, 4
	lw	$ra, 0($sp)
	bgtz	$v0, min_caml_prerr_int_rec
	j	min_caml_prerr_int_final
min_caml_prerr_int_rec:
	sw	$ra, 0($sp)
	sw	$v1, -4($sp)
	move	$a0, $v0
	addi    $sp, $sp, -8
	jal	min_caml_prerr_int
	addi    $sp, $sp, 8
	lw	$ra, 0($sp)
	lw	$v1, -4($sp)
min_caml_prerr_int_final:
	addi	$a0, $v1, 48
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	jal	min_caml_prerr_byte
	addi    $sp, $sp, 4
	lw	$ra, 0($sp)
	jr	$ra
min_caml_prerr_byte:
	li	$v0, 103
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	syscall
	addi    $sp, $sp, 4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_prerr_float:
min_caml_read_char:
	li	$v0, 12
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	syscall
	addi    $sp, $sp, 4
	lw	$ra, 0($sp)
	jr	$ra
min_caml_read_int:
	li	$t0, 0 # 文字列のカウンタ
	move	$t6, $hp # heap pointerの初期位置を覚えておく
min_caml_read_int_read_input:
	sw	$t0, 0($sp) # save counter
	addi	$sp, $sp, -4
	jal	min_caml_read_char
	addi	$sp, $sp, 4 # back
	lw	$t0, 0($sp)
	addi	$t2, $v0, -32  # space?
	beq	$t2, $r0, min_caml_read_int_calc
	sw	$v0, 0($hp)
	addi	$hp, $hp, 4
	addi	$t0, $t0, 1
	j	min_caml_read_int_read_input
min_caml_read_int_calc:
	# この時点で$t0には文字列の長さが入っている。
	# また$t6+4*iにi文字目の文字が入っている。
	move	$t2, $hp # 文字列を後ろからたどるためのポインタ
	addi	$t6, $t6, -4 # ループ終了の判定に使う。
	li	$t1, 1 # ループごとに10倍される値。
	li	$v0, 0
	# 文字を後ろから読んでいく。
	addi	$t2, $t2, -4
min_caml_read_int_for_loop:
	beq	$t2, $t6, min_caml_read_int_finalize
	# 1文字読む
	lw	$t3, 0($t2)
	addi	$t3, $t3, -48 # Char -> Int
	# 「$t3 * (ループごとに10倍される値)」の計算
	sw	$ra, 0($sp)
	sw	$t0, -4($sp)
	sw	$t1, -8($sp)
	sw	$t2, -12($sp)
	sw	$t3, -16($sp)
	sw	$v0, -24($sp)
	move	$a0, $t3
	move	$a1, $t1
	addi	$sp, $sp, -28
	jal	mul
	addi	$sp, -sp, 28
	sw	$v0, -24($sp)
	# $t1を10倍する
	lw	$a0, -8($sp) # t1をロードしてa0に代入
	li	$a1, 10
	addi	$sp, $sp, -28
	jal	mul
	addi	$sp, $sp, 28
	move	$t1, $v0 # t1を10倍された値で更新
	# 値を$spから再取得
	lw	$ra, 0($sp)
	lw	$t0, -4($sp)
	lw	$t2, -12($sp)
	lw	$t3, -16($sp)
	lw	$v0, -24($sp)
	addi	$t2, $t2, -4
	j	min_caml_read_int_for_loop
min_caml_read_int_finalize:
	jr	$ra
min_caml_read_float:
	li	$t0, 0 # 文字列のカウンタ
	move	$t6, $hp # heap pointerの初期位置を覚えておく
min_caml_read_float_read_input:
	sw	$t0, 0($sp)
	addi	$sp, $sp, -4
	jal	min_caml_read_char
	addi	$sp, $sp, 4
	lw	$t0, 0($sp)
	addi	$t2, $v0, -32  # space?
	beq	$t2, $r0, min_caml_read_float_calc
	sw	$v0, 0($hp)
	addi	$hp, $hp, 4
	addi	$t0, $t0, 1
	j	min_caml_read_float_read_input
min_caml_read_float_calc:
	move	$t2, $hp # 文字列を後ろからたどるためのポインタ
	addi	$t6, $t6, -4 # ループ終了の判定に使う。
	li	$t1, 1 # ループごとに10倍する値
	li	$v0, 0 # result * 10 (int)
	# 文字を後ろから読んでいく。
	addi	$t7, $t0, -1 # ループにおけるi (jの算出に必要)
	addi	$t2, $t2, -4
min_caml_read_float_for_loop:
	beq	$t2, $t6, min_caml_read_float_finalize
	addi	$t4, $t0, -2
	sub	$t4, $t4, $t7
	bgez	$t4, min_caml_read_float_question_true
	move	$t5, $t7
	j	min_caml_read_float_question_after
min_caml_read_float_question_true:
	addi	$t5, $t7, -4 # j = i - 1
min_caml_read_float_question_after:
	sub	$t4, $t6, $t5 # 元のポインタ - 読もうとするポインタ
	bltz	$t4, min_caml_read_float_finalize
	# 後ろから1文字読む
	lw	$t3, 0($t5)
	addi	$t3, $t3, -48
	sw	$ra, 0($sp)
	sw	$t0, -4($sp)
	sw	$t1, -8($sp)
	sw	$t2, -12($sp)
	sw	$t3, -16($sp)
	sw	$t8, -20($sp)
	sw	$v0, -24($sp)
	move	$a0, $t3
	move	$a1, $t1
	addi	$sp, $sp, -28
	jal	mul # (str[j] - 48) * dの計算
	addi	$sp, $sp, 28
	sw	$v0, -24($sp)
	lw	$a0, -8($sp)
	li	$a1, 10
	addi	$sp, $sp, -28
	jal	mul # d * 10の計算
	addi	$sp, $sp, 28
	move	$t1, $v0
	lw	$ra, 0($sp)
	lw	$t0, -4($sp)
	lw	$t2, -12($sp)
	lw	$t3, -16($sp)
	lw	$t8, -20($sp)
	lw	$v0, -24($sp)
	addi	$t7, $t7, -1 # iをデクリメント
	addi	$t2, $t2, -4 # 1文字前にポインタを移動
	j	min_caml_read_float_for_loop
min_caml_read_float_finalize:
	mtc1	$v0, $f0
	li	$t8, 10
	mtc1	$t8, $f1
	cvt.s.w	$f0, $f0
	cvt.s.w	$f1, $f1
	div.s	$f0, $f0, $f1
	jr	$ra
min_caml_create_array:
	move	$v0, $hp # (return value)
create_array_loop:
	bne	$a0, $zero, create_array_cont
	jr	$ra
create_array_cont:
	sw	$a1, 0($hp)
	addi	$a0, $a0, -1
	addi	$hp, $hp, 1
	j	create_array_loop
min_caml_create_float_array:
	move	$v0, $hp
create_float_array_loop:
	bne	$a0, $zero, create_float_array_cont
	jr	$ra
create_float_array_cont:
	swc1	$f12, 0($hp)
	addi	$a0, $a0, -1
	addi	$hp, $hp, 1
	j	create_float_array_loop
min_caml_abs_float:
	abs.s	$f0, $f12
	jr	$ra
min_caml_sqrt:
	sqrt.s	$f0, $f12
	jr	$ra
min_caml_floor:
	li	$v0, 100
	sw	$ra, 0($sp)
	addi    $sp, $sp, -4
	syscall
	addi    $sp, $sp, 4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_int_of_float:
min_caml_truncate:
	trunc.w.s	$f12, $f12
	mfc1	$v0, $f12
	jr	$ra
min_caml_float_of_int:
	mtc1	$v0, $f0
	cvt.s.w	$f0, $f0
	jr	$ra
min_caml_cos:
	lwc1	$f4, 0(x1)
	c.olt.s	0, $f12, $f4
	bc1t	0, cos_branch1
	lwc1	$f4, 0(x2)
	c.olt.s	0, $f12, $f4
	bc1t	0, cos_branch2
	lwc1	$f4, 0(x3)
	c.olt.s	0, $f12, $f4
	bc1t	0, cos_branch3
        li	$f0, 1
	sub.s	$f12, $f12, $f4
	j	calc
cos_branch3:
        li	$f0, -1
	sub.s	$f12, $f4, $f12
	j	calc
cos_branch2:
	lwc1	$f4, 0(x1)
        li	$f0, -1
	sub.s	$f12, $f12, $f4
	j	calc
cos_branch1:
        li	$f0, 1
	sub.s	$f12, $f4, $f12
	j	calc
min_caml_sin:
	lwc1	$f4, 0(x1)
	c.olt.s	0, $f12, $f4
	bc1t	0, sin_branch1
	lwc1	$f4, 0(x2)
	c.olt.s	0, $f12, $f4
	bc1t	0, sin_branch2
	lwc1	$f4, 0(x3)
	c.olt.s	0, $f12, $f4
	bc1t	0, sin_branch3
	lwc1	$f4, 0(x4)
        li	$f0, -1
	sub.s	$f12, $f4, $f12
	j	calc
sin_branch3:
        li	$f0, -1
	sub.s	$f12, $f12, $f4
	j	calc
sin_branch2:
        li	$f0, 1
	sub.s	$f12, $f4, $f12
	j	calc
sin_branch1:
        li	$f0, 1
calc:
	lwc1	$f16, 0(a1)
	lwc1	$f17, 0(a2)
	lwc1	$f18, 0(a3)
	lwc1	$f19, 0(a4)
	mul.s	$f19, $f19, $f12
	add.s	$f19, $f19, $f18
	mul.s   $f19, $f19, $f12
	add.s	$f19, $f19, $f17
	mul.s	$f19, $f19, $f12
	add.s	$f19, $f19, $f16
	mul.s	$f19, $f19, $f12
	mul.s	$f0, $f19, $f0
	jr	$r31
min_caml_atan:
	lwc1	$f16, 0(b1)
	lwc1	$f17, 0(b2)
	lwc1	$f18, 0(b3)
	lwc1	$f19, 0(b4)
	mul.s	$f19, $f19, $f12
	add.s	$f19, $f19, $f18
	mul.s   $f19, $f19, $f12
	add.s	$f19, $f19, $f17
	mul.s	$f19, $f19, $f12
	add.s	$f19, $f19, $f16
	mul.s	$f0, $f19, $f12
	jr	$ra
mul:
# calculate a0 * a1.
	li	$v0, 0
	beq	$a0, $zero, mul_return_b
	j	mul_loop
mul_return_b:
	move	$v0, $a1
	jr	$ra
mul_loop:
	beq	$a0, $zero, mul_final
	li	$s0, 1
	and	$s0, $a0, $s0
	bne	$s0, $zero, mul_if_true
	j	mul_loop_cont
mul_if_true: # if a0 is odd
	add	$v0, $v0, $a1
mul_loop_cont:
	srl	$a0, $a0, 1
	sll	$a1, $a1, 1
	j	mul_loop
mul_final:
	jr	$ra
div10:
	move	$t8, $a0 # assume a0 = 4
	li	$t0, 10
	addi	$t0, $a0, -10
	bltz	$t0, div10_smaller_than_10
	j	div10_cont
div10_smaller_than_10:
	li	$t0, 0
	move	$v0, 0
	move	$v1, $a0
	jr	$ra
div10_cont:
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
	sw	$t2, -4($sp)
	sw	$ra, -8($sp)
	addi	$sp, $sp, -12
	jal	mul
	sub	$v1, $t8, $v0
	addi	$sp, $sp, 12
	lw	$ra, -8($sp)
	lw	$v0, -4($sp)
	jr	$ra
	.data
min_caml_objects:
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
min_caml_size:
	.word	128
	.word	128
min_caml_dbg:
	.word	1
min_caml_screen:
	.word	0
	.word	0
	.word	0
min_caml_vp:
	.word	0
	.word	0
	.word	0
min_caml_view:
	.word	0
	.word	0
	.word	0
min_caml_light:
	.word	0
	.word	0
	.word	0
min_caml_cos_v:
	.word	0
	.word	0
min_caml_sin_v:
	.word	0
	.word	0
min_caml_beam:
	.word	1132396544 # 255.0
min_caml_and_net:
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
	.word	-1
min_caml_or_net:
	.word	-1
min_caml_temp:
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
min_caml_cs_temp:
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
min_caml_solver_dist:
	.word	0
min_caml_vscan:
	.word	0
	.word	0
	.word	0
min_caml_intsec_rectside:
	.word	0
min_caml_tmin:
	.word	1315859240 # 1.0 * 10 ^ 9
min_caml_crashed_point:
	.word	0
	.word	0
	.word	0
min_caml_crashed_object:
	.word	0
min_caml_end_flag:
	.word	0
min_caml_viewpoint:
	.word	0
	.word	0
	.word	0
min_caml_nvector:
	.word	0
	.word	0
	.word	0
min_caml_rgb:
	.word	0
	.word	0
	.word	0
min_caml_texture_color:
	.word	0
	.word	0
	.word	0
min_caml_solver_w_vec:
	.word	0
	.word	0
	.word	0
min_caml_chkinside_p:
	.word	0
	.word	0
	.word	0
min_caml_isoutside_q:
	.word	0
	.word	0
	.word	0
min_caml_nvector_w:
	.word	0
	.word	0
	.word	0
min_caml_scan_d:
	.word	0
min_caml_scan_offset:
	.word	0
min_caml_scan_sscany:
	.word	0
min_caml_scan_met1:
	.word	0
min_caml_wscan:
	.word	0
	.word	0
	.word	0
x4:
	.word	1086911939
x3:
	.word	1083619410
x2:
	.word	1078523331
x1:
	.word	1070134723
a4:
	.word	1019893672
a3:
	.word	3192858555
a2:
	.word	1023136372
a1:
	.word	1065213965
b4:
	.word	1034410097
b3:
	.word	3195372722
b2:
	.word	3179091843
b1:
	.word	1065411936
