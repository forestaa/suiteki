	.text
min_caml_fhalf:
	li	$t0, 2
	mtc1	$t0, $f5
	cvt.s.w	$f5, $f5
	inv.s	$f5, $f5
	mul.s	$f0, $f12, $f5
	jr	$ra
min_caml_fequal:
	c.eq.s	0, $f12, $f13
	bc1t	0, min_caml_fequal_true
	li	$v0, 0
	jr	$ra
min_caml_fless_true:
	li	$v0, 1
	jr	$ra
min_caml_fless:
	c.olt.s	0, $f12, $f13
	bc1t	0, min_caml_fless_true
	li	$v0, 0
	jr	$ra
min_caml_fless_true:
	li	$v0, 1
	jr	$ra
min_caml_fabs:
	j	min_caml_abs_float
min_caml_fiszero:
	mtc1	$r0, $f5
	cvt.s.w	$f5, $f5
	c.eq.s	0, $f5, $f12
	bc1t	0, min_caml_fiszero_true
	li	$v0, 0
	jr	$ra
min_caml_fiszero_true:
	li	$v0, 1
	jr	$ra
min_caml_fispos:
	mtc1	$r0, $f5
	cvt.s.w	$f5, $f5
	c.olt.s	0, $f5, $f12
	bc1t	0, min_caml_fispos_true
	li	$v0, 0
	jr	$ra
min_caml_fispos_true:
	li	$v0, 1
	jr	$ra
min_caml_fisneg:
	mtc1	$r0, $f5
	cvt.s.w	$f5, $f5
	c.olt.s	0, $f12, $f5
	bc1t	0, min_caml_fisneg_true
	li	$v0, 0
	jr	$ra
min_caml_fisneg_true:
	li	$v0, 1
	jr	$ra
min_caml_fneg:
	neg.s	$f0, $f12
	jr	$ra
min_caml_fsqr:
	sqrt.s	$f0, $f12
	jr	$ra
min_caml_print_newline:
	li	$a0, 10
	sw	$ra, 0($sp)
	addi    $sp, $sp, -1
	jal	min_caml_print_byte
	addi    $sp, $sp, 1
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
	addi    $sp, $sp, -2
	jal	min_caml_print_byte
	addi    $sp, $sp, 2
	lw	$ra, -1($sp)
	lw	$a0, 0($sp)
	li	$t0, 0
	sub	$a0, $t0, $a0 # negate a0
min_caml_print_int_cont:
	sw	$ra, 0($sp)
	addi    $sp, $sp, -1
	jal	div10
# この時点でv0が商、v1があまり。
	addi    $sp, $sp, 1
	lw	$ra, 0($sp)
	bgtz	$v0, min_caml_print_int_rec
	j	min_caml_print_int_final
min_caml_print_int_rec:
	sw	$ra, 0($sp)
	sw	$v1, -1($sp)
	move	$a0, $v0
	addi    $sp, $sp, -2
	jal	min_caml_print_int_cont
	addi    $sp, $sp, 2
	lw	$ra, 0($sp)
	lw	$v1, -1($sp)
min_caml_print_int_final:
	addi	$a0, $v1, 48
	sw	$ra, 0($sp)
	addi    $sp, $sp, -1
	jal	min_caml_print_byte
	addi    $sp, $sp, 1
	lw	$ra, 0($sp)
	jr	$ra
min_caml_print_byte:
	li	$v0, 11
	sw	$ra, 0($sp)
	addi    $sp, $sp, -1
	syscall
	addi    $sp, $sp, 1
	sw	$ra, 0($sp)
	jr	$ra
min_caml_prerr_int:
	sw	$ra, 0($sp)
	addi    $sp, $sp, -1
	jal	div10
	addi    $sp, $sp, 1
	lw	$ra, 0($sp)
	bgtz	$v0, min_caml_prerr_int_rec
	j	min_caml_prerr_int_final
min_caml_prerr_int_rec:
	sw	$ra, 0($sp)
	sw	$v1, -1($sp)
	move	$a0, $v0
	addi    $sp, $sp, -2
	jal	min_caml_prerr_int
	addi    $sp, $sp, 2
	lw	$ra, 0($sp)
	lw	$v1, -1($sp)
min_caml_prerr_int_final:
	addi	$a0, $v1, 48
	sw	$ra, 0($sp)
	addi    $sp, $sp, -1
	jal	min_caml_prerr_byte
	addi    $sp, $sp, 1
	lw	$ra, 0($sp)
	jr	$ra
min_caml_prerr_byte:
	li	$v0, 103
	sw	$ra, 0($sp)
	addi    $sp, $sp, -1
	syscall
	addi    $sp, $sp, 1
	sw	$ra, 0($sp)
	jr	$ra
min_caml_prerr_float:
min_caml_read_char:
	li	$v0, 12
	sw	$ra, 0($sp)
	addi    $sp, $sp, -1
	syscall
	addi    $sp, $sp, 1
	lw	$ra, 0($sp)
	jr	$ra
min_caml_read_token:
# call read_char repeatedly until the newly read char is not a space-char (that is,
# ' ' or '\t' or '\r' or '\n'), storing them in heap, and return the head address of
# the stored characters.
# also, return the length of the string using $v1.
	li	$s0, 0
	move	$t0, $hp
min_caml_read_token_loop:
	sw	$ra, 0($sp)
	sw	$t0, -1($sp)
	sw	$s0, -2($sp)
	addi	$sp, $sp, -3
	jal	min_caml_read_char
	addi	$sp, $sp, 3
	lw	$s0, -2($sp)
	lw	$t0, -1($sp)
	lw	$ra, 0($sp)
	addi	$t2, $v0, -32  # space?
	beq	$t2, $r0, min_caml_read_token_l1
	addi	$t2, $v0, -9  # tab?
	beq	$t2, $r0, min_caml_read_token_l1
	addi	$t2, $v0, -10  # newline?
	beq	$t2, $r0, min_caml_read_token_l1
	addi	$t2, $v0, -11  # tab?
	beq	$t2, $r0, min_caml_read_token_l1
	addi	$t2, $v0, -12  # newline?
	beq	$t2, $r0, min_caml_read_token_l1
	addi	$t2, $v0, -13  # newline?
	beq	$t2, $r0, min_caml_read_token_l1
	sw	$v0, 0($hp)
	addi	$hp, $hp, 1
	addi	$s0, $s0, 1
	j	min_caml_read_token_loop
min_caml_read_token_l1:
	bne	$s0, $zero, min_caml_read_token_finish
	j	min_caml_read_token_loop
min_caml_read_token_finish:
	move	$v0, $t0
	move	$v1, $s0
	jr	$ra
min_caml_read_int:
	sw	$ra, 0($sp)
	addi	$sp, $sp, -1
	jal	min_caml_read_token
	addi	$sp, $sp, 1
	lw	$ra, 0($sp)
# この時点で$v0に文字列の先頭のアドレスが、$v1にはその長さがある。
# まず先頭文字が'-'かどうかを確認する。もし'-'なら負の数なので$v0をインクリメントして
# $v1をデクリメントし、符号を表すレジスタを-1にする。
	li	$s0, 1 # 符号を表すレジスタ
	lw	$t0, 0($v0)
	addi	$t2, $t0, -45  # hyphen?
	beq	$t2, $r0, min_caml_read_int_negative
	j	min_caml_read_int_cont
min_caml_read_int_negative:
	addi	$v0, $v0, 1
	addi	$v1, $v1, -1
	li	$s0, -1
min_caml_read_int_cont:
# あとは自然数だと思って読めばオーケー。
	li	$t0, 1 # ループごとに10倍される数
	li	$s1, 0 # 最終的には返り値となる数
	addi	$s2, $v1, -1 # 文字列を後ろから読んでいくための変数(iと呼ぶ)
# コードを読みやすくするためs3に文字列の先頭のアドレスを移動する
# (v0だとreturn valueに衝突して色々面倒)
	move	$s3, $v0
min_caml_read_int_loop:
	bltz	$s2, min_caml_read_int_finalize
# まずstr[i]を取得する
	add	$t8, $s3, $s2
	lw	$t1, 0($t8)
# str[i]を整数に変換する
	addi	$t1, $t1, -48
# t1 * t0を計算したい。ここでmulを使う。
	move	$a0, $t0
	move	$a1, $t1
	sw	$ra, 0($sp)
	sw	$s3, -1($sp) # 文字列の先頭のアドレス
	sw	$t0, -2($sp) # ループごとに10倍される数
	sw	$s0, -3($sp) # sign
	sw	$s1, -4($sp) # return value
	addi	$sp, $sp, -5
	jal	mul
	addi	$sp, $sp, 5
	lw	$s1, -4($sp)
	lw	$s0, -3($sp)
	lw	$t0, -2($sp)
	lw	$s3, -1($sp)
	lw	$ra, 0($sp)
# t1 * t0をresultに追加:
	add	$s1, $s1, $v0
# つぎにt0 * 10を計算する。
	move	$a0, $t0
	li	$a1, 10
	sw	$ra, 0($sp)
	sw	$s3, -1($sp)
	sw	$t0, -2($sp)
	sw	$s0, -3($sp)
	sw	$s1, -4($sp)
	addi	$sp, $sp, -5
	jal	mul
	addi	$sp, $sp, 5
	lw	$s1, -4($sp)
	lw	$s0, -3($sp)
	lw	$t0, -2($sp)
	lw	$s3, -1($sp)
	lw	$ra, 0($sp)
# t0を10倍した値で更新:
	move	$t0, $v0
	addi	$s2, $s2, -1 # i--
	j	min_caml_read_int_loop
min_caml_read_int_finalize:
# s1に入っている値に符号の処理をしてv0に結果を入れて返すだけ。
	bltz	$s0, min_caml_read_int_finalize_negative
	move	$v0, $s1
	jr	$ra
min_caml_read_int_finalize_negative:
	sub	$v0, $zero, $s1
	jr	$ra
min_caml_str_has_point_p:
# a0: the head address of a string
# a1: the length of the string
# return value: 1 if and only if the string contains '.', otherwise 0.
	li	$t0, 0
	addi	$t1, $a1, -1
min_caml_str_has_point_p_loop:
	beq	$t0, $t1, min_caml_str_has_point_p_false
	add	$t2, $a0, $t0
	lw	$t3, 0($t2)
	addi	$t3, $t3, -46  # dot?
	beq	$t3, $r0, min_caml_str_has_point_p_true
	addi	$t0, $t0, 1
	j	min_caml_str_has_point_p_loop
min_caml_str_has_point_p_false:
	li	$v0, 0
	jr	$ra
min_caml_str_has_point_p_true:
	li	$v0, 1
	jr	$ra
min_caml_str_with_no_point:
# a0: the head address of a string
# a1: the length of the string
# return value: the head address of a newly-created string. the new string is
# the string constructed by eliminating all dots ('.') from the given string.
# e.g., if the given string is "255.0", then the returned string is "2550"
# note that this functions assumes that the given string contains '.'.
	li	$t0, 0
	li	$t4, 0
	addi	$t1, $a1, -1 # assuming that the str contains '.'
	move	$v0, $hp
min_caml_str_with_no_point_loop:
	beq	$t0, $t1, min_caml_str_with_no_point_finalize
	add	$t2, $a0, $t4
	lw	$t2, 0($t2)
	addi	$t3, $t2, -46  # dot?
	beq	$t3, $r0, min_caml_str_with_no_point_ignore_dot
	sw	$t2, 0($hp)
	addi	$hp, $hp, 1
	addi	$t0, $t0, 1
	addi	$t4, $t4, 1
	j	min_caml_str_with_no_point_loop
min_caml_str_with_no_point_ignore_dot:
	addi	$t4, $t4, 1
	j	min_caml_str_with_no_point_loop
min_caml_str_with_no_point_finalize:
	jr	$ra
min_caml_str_point_at:
# a0: the head address of a string
# a1: the length of the string
# return value: the location of '.', counted from tail.
# e.g., if input string is "255.0", then the result is 1. if input is "22.00", then 2.
# note that this function assumes that the given string contains '.'.
	li	$t0, 0 # counter
min_caml_str_point_at_loop:
	add	$t2, $a0, $t0
	lw	$t2, 0($t2)
	addi	$t3, $t2, -46  # dot?
	beq	$t3, $r0, min_caml_str_point_at_found_dot
	addi	$t0, $t0, 1
	j	min_caml_str_point_at_loop
min_caml_str_point_at_found_dot:
	addi	$t0, $t0, 1
	sub	$v0, $a1, $t0
	jr	$ra
min_caml_read_float:
	sw	$ra, 0($sp)
	addi	$sp, $sp, -1
	jal	min_caml_read_token
	addi	$sp, $sp, 1
	lw	$ra, 0($sp)
# この時点で$v0に文字列の先頭のアドレスが、$v1にはその長さがある。
# まず先頭文字が'-'かどうかを確認する。もし'-'なら負の数なので$v0をインクリメントして
# $v1をデクリメントし、符号を表すレジスタを-1にする。
	li	$s0, 1 # 符号を表すレジスタ
	lw	$t0, 0($v0)
	addi	$t2, $t0, -45  # hyphen?
	beq	$t2, $r0, min_caml_read_float_negative
	j	min_caml_read_float_cont
min_caml_read_float_negative:
	addi	$v0, $v0, 1
	addi	$v1, $v1, -1
	li	$s0, -1
min_caml_read_float_cont:
# あとは正の浮動小数点数だと思って読むことができる。
# 値がv0, v1にあるといろいろ面倒なのでs1, s2に移動しておく
	move	$s1, $v0 # address
	move	$s2, $v1 # length
# まず小数点が存在するか否かをチェックする。存在しない場合はたとえば"1"を1.0にして返すことになる。
	move	$a0, $s1
	move	$a1, $s2
	sw	$ra, 0($sp)
	sw	$s0, -1($sp)
	sw	$s1, -2($sp)
	sw	$s2, -3($sp)
	addi	$sp, $sp, -4
	jal	min_caml_str_has_point_p
	addi	$sp, $sp, 4
	lw	$s2, -3($sp)
	lw	$s1, -2($sp)
	lw	$s0, -1($sp)
	lw	$ra, 0($sp)
	beq	$v0, $zero, min_caml_read_float_no_point
	j	min_caml_read_float_point_exists
min_caml_read_float_no_point:
# 小数点が存在しない場合。これはread_intの処理が使える。
# 'v0'に文字の先頭のアドレス、'v1'に文字列の長さ、's1'に符号を入れる。
	move	$v0, $s1
	move	$v1, $s2
	move	$s1, $s0
	sw	$ra, 0($sp)
	addi	$sp, $sp, -1
	jal	min_caml_read_int_cont
	addi	$sp, $sp, 1
	lw	$ra, 0($sp)
# あとは読んだ結果を浮動小数点数に変換するだけ
	mtc1	$v0, $f0
	cvt.s.w	$f0, $f0
	jr	$ra
min_caml_read_float_point_exists:
# 1. 浮動小数点を除いた文字列を構成する(sとする)
# 3. 小数点が後ろから何文字目に存在するのかを調べる(k=(0,1,..)文字目とする)
# 2. sを整数として読む(nとする)
# 4. n/(10^k)を浮動小数点数として計算する
	move	$a0, $s1
	move	$a1, $s2
	sw	$ra, 0($sp)
	sw	$s0, -1($sp)
	sw	$s1, -2($sp)
	sw	$s2, -3($sp)
	addi	$sp, $sp, -4
	jal	min_caml_str_with_no_point
	addi	$sp, $sp, 4
	lw	$s2, -3($sp)
	lw	$s1, -2($sp)
	lw	$s0, -1($sp)
	lw	$ra, 0($sp)
# これで$v0に浮動小数点なしの文字列の先頭のアドレスが入る。
# つぎに小数点の位置を調べる。
	sw	$ra, 0($sp)
	sw	$s0, -1($sp)
	sw	$s1, -2($sp)
	sw	$s2, -3($sp)
	sw	$v0, -4($sp)
	addi	$sp, $sp, -5
	jal	min_caml_str_point_at
	addi	$sp, $sp, 5
	lw	$s3, -4($sp) # s3に浮動小数点なしの文字列の先頭を格納
	lw	$s2, -3($sp)
	lw	$s1, -2($sp)
	lw	$s0, -1($sp)
	lw	$ra, 0($sp)
# これで$v0に浮動小数点の位置が入る。
# つぎにs3を整数として読む。
	sw	$ra, 0($sp)
	sw	$v0, -1($sp)
	sw	$s2, -2($sp)
	addi	$sp, $sp, -3
	addi	$v1, $s2, -1 # v1には文字列の長さを入れなければならない
	move	$s1, $s0 # s1は符号
	move	$v0, $s3 # v0は文字列の先頭のアドレス
	jal	min_caml_read_int_cont
	addi	$sp, $sp, 3 # 315
	lw	$s2, -2($sp)
	lw	$s1, -1($sp) # s1に浮動小数点数の位置を格納
	lw	$ra, 0($sp)
# つぎに整数として読んだs3を浮動小数点数に変換する
	mtc1	$v0, $f0
	cvt.s.w	$f0, $f0
# 10*kでf0を割るために10*kを計算する。
	move	$a0, $s1
	li	$a1, 10
	sw	$ra, 0($sp)
	swc1	$f0, -1($sp)
	jal	mul
	lwc1	$f0, -1($sp)
	lw	$ra, 0($sp)
# 10*kを浮動小数点数に変換する。
	mtc1	$v0, $f1
	cvt.s.w	$f1, $f1
# あとは割るだけ
	inv.s	$f1, $f1
	mul.s	$f0, $f0, $f1
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
	li	$t0, 0
	mtc1	$t0, $f3
	c.olt.s	0, $f12, $f3
	bc1t	0, min_caml_abs_negate
	mov.s	$f0, $f12
	jr	$ra
min_caml_abs_negate:
	neg.s	$f0, $f12
	jr	$ra
min_caml_sqrt:
	sqrt.s	$f0, $f12
	jr	$ra
min_caml_floor:
	li	$t0, 0
	mtc1	$t0, $f3
	cvt.s.w	$f3, $f3
	c.olt.s	0, $f12, $f3 # if $f12 < 0
	bc1t	0, min_caml_floor_neg
	trunc.w.s	$f12, $f12
	cvt.s.w	$f0, $f12
	jr	$ra
min_caml_floor_neg:
	trunc.w.s	$f12, $f12
	cvt.s.w	$f0, $f12
	li	$t0, 1
	mtc1	$t0, $f3
	cvt.s.w	$f3, $f3
	sub.s	$f0, $f0, $f3
	jr	$ra
min_caml_int_of_float:
min_caml_truncate:
	trunc.w.s	$f12, $f12
	mfc1	$v0, $f12
	jr	$ra
min_caml_float_of_int:
	mtc1	$a0, $f0
	cvt.s.w	$f0, $f0
	jr	$ra
calc:
	lwc1	$f16, 0(s01)
	lwc1	$f17, 0(s03)
	lwc1	$f18, 0(s05)
	lwc1	$f19, 0(s07)
	lwc1	$f20, 0(s09)
	lwc1	$f21, 0(s11)
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f20
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f19
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f18
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f17
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f16
	mul.s	$f0, $f21, $f12
	jr	$ra
sin_v0:
	j	calc
sin_v1:
	lwc1	$f4, 0(x2)
	sub.s	$f12, $f4, $f12
	j	calc
sin_v2:
	lwc1	$f4, 0(x2)
	sub.s	$f12, $f12, $f4
	sw	$ra, 0($sp)
	addi	$sp, $sp, -1
	jal	calc
	addi	$sp, $sp, 1
	lw	$ra, 0($sp)
	li	$t0, -1
	mtc1	$t0, $f2
	cvt.s.w	$f2, $f2
	mul.s	$f0, $f0, $f2
	jr	$ra
sin_v3:
	lwc1	$f4, 0(pi2)
	sub.s	$f12, $f4, $f12
	sw	$ra, 0($sp)
	addi	$sp, $sp, -1
	jal	calc
	addi	$sp, $sp, 1
	lw	$ra, 0($sp)
	li	$t0, -1
	mtc1	$t0, $f2
	cvt.s.w	$f2, $f2
	mul.s	$f0, $f0, $f2
	jr	$ra
mysin_pos:
# f12: x (float)
# note: assuming x >= 0
	li	$s0, 0
	li	$s1, 0 # n / 4 (as integer)
	li	$s2, 0 # n % 4
	li	$t1, 0 # counter for s1
	li	$t2, 4 # for comparison
	lwc1	$f9, 0(pidiv2)
category_loop:
	addi	$t0, $s0, 1
	mtc1	$t0, $f10
	cvt.s.w	$f10, $f10
	mul.s	$f10, $f10, $f9
	c.olt.s	0, $f12, $f10
	bc1t	0, category_finalize
	addi	$s0, $s0, 1 # n++
	addi	$s2, $s2, 1
	addi	$t1, $t1, 1
	beq	$t1, $t2, category_4
	j	category_loop
category_4:
	li	$t1, 0
	li	$s2, 0
	addi	$s1, $s1, 1
	j	category_loop
category_finalize:
	move	$v0, $s0
# x -= (2 * pi) * m
	mtc1	$s1, $f10
	cvt.s.w	$f10, $f10
	lwc1	$f11, 0(pi2)
	mul.s	$f11, $f10, $f11
	sub.s	$f12, $f12, $f11
# switch by n % 4
	addi	$t0, $s2, 0
	beq	$t0, $r0, sin_v0
	addi	$t0, $s2, -1
	beq	$t0, $r0, sin_v1
	addi	$t0, $s2, -2
	beq	$t0, $r0, sin_v2
	j	sin_v3
mysin_neg:
# add 2*pi to x again and again until x >= 0 holds.
	lwc1	$f11, 0(pi2)
	mtc1	$zero, $f13
mysin_neg_loop:
	c.olt.s	0, $f13, $f12 # 0 < x ?
	bc1t	0, mysin_pos
	add.s	$f12, $f12, $f11 # x += 2*pi
	j	mysin_neg_loop
min_caml_sin:
	mtc1	$zero, $f13
	c.olt.s	0, $f12, $f13
	bc1t	0, mysin_neg
	j	mysin_pos
_min_caml_start:
min_caml_cos:
	lwc1	$f13, 0(pidiv2)
	add.s	$f12, $f12, $f13
	j	min_caml_sin
atan_calc:
	lwc1	$f16, 0(t01)
	lwc1	$f17, 0(t03)
	lwc1	$f18, 0(t05)
	lwc1	$f19, 0(t07)
	lwc1	$f20, 0(t09)
	lwc1	$f21, 0(t11)
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f20
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f19
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f18
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f17
	mul.s	$f21, $f21, $f12
	mul.s	$f21, $f21, $f12
	add.s	$f21, $f21, $f16
	mul.s	$f0, $f21, $f12
	jr	$ra
min_caml_atan:
	li	$t0, 1
	mtc1	$t0, $f5
	cvt.s.w	$f5, $f5
	c.olt.s	0, $f5, $f12 # 1 < x?
	bc1t	0, min_caml_atan_bigger
	li	$t0, -1
	mtc1	$t0, $f5
	cvt.s.w	$f5, $f5
	c.olt.s	0, $f12, $f5 # x < -1?
	bc1t	0, min_caml_atan_smaller
	j	atan_calc
min_caml_atan_bigger:
	inv.s	$f12, $f12
	sw	$ra, 0($sp)
	addi	$sp, $sp, -1
	jal	atan_calc
	addi	$sp, $sp, 1
	lw	$ra, 0($sp)
	lwc1	$f2, 0(pidiv2)
	sub.s	$f0, $f2, $f0
	jr	$ra
min_caml_atan_smaller:
	inv.s	$f12, $f12
	sw	$ra, 0($sp)
	addi	$sp, $sp, -1
	jal	atan_calc
	addi	$sp, $sp, 1
	lw	$ra, 0($sp)
	lwc1	$f2, 0(pidiv2)
	neg.s	$f2, $f2
	sub.s	$f0, $f2, $f0
	jr	$ra
mul:
# calculate a0 * a1.
	li	$v0, 0
	beq	$a0, $zero, mul_return_zero
	j	mul_loop
mul_return_zero:
	li	$v0, 0
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
# divide a0 by 10, storing the quotient in v0 and the remaining v1.
	move	$t8, $a0 # assume a0 = 4
	li	$t0, 10
	addi	$t0, $a0, -10
	bltz	$t0, div10_smaller_than_10
	j	div10_cont
div10_smaller_than_10:
	li	$t0, 0
	move	$v0, $r0
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
	sw	$t2, -1($sp)
	sw	$ra, -2($sp)
	addi	$sp, $sp, -3
	jal	mul
	sub	$v1, $t8, $v0
	addi	$sp, $sp, 3
	lw	$ra, -2($sp)
	lw	$v0, -1($sp)
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
min_caml_size: # address of array
	.word	128
	.word	128
min_caml_dbg:
	.word	1
min_caml_screen:
	.word	0
	.word	0
	.word	0
min_caml_screenx_dir:
	.word	0
	.word	0
	.word	0
min_caml_screeny_dir:
	.word	0
	.word	0
	.word	0
min_caml_screenz_dir:
	.word	0
	.word	0
	.word	0
min_caml_n_objects:
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
min_caml_or_net:
	.word	-1
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
min_caml_startp:
	.word	0
	.word	0
	.word	0
min_caml_startp_fast:
	.word	0
	.word	0
	.word	0
min_caml_light_dirvec:
	.word	0
	.word	0
min_caml_intersection_point:
	.word	0
	.word	0
	.word	0
min_caml_intersected_object_id:
	.word	0
min_caml_reflections:
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
min_caml_n_reflections:
	.word	0
min_caml_diffuse_ray:
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
pi:
	.word	1078530011 # 3.1415927
pi2:
	.word	1086918619 # pi * 2
pidiv2:
	.word	1070141403 # pi / 2
pivot:
	.word	1056964608 # 0.5
s01:
	.word	1065353216 # 1.0
s03:
	.word	3190467243 # -0.16...
s05:
	.word	1007192201
s07:
	.word	3109031169
s09:
	.word	909700893
s11:
	.word	3000447531
t01:
	.word	1065353216 # 1.0
t03:
	.word	3198855851
t05:
	.word	1045220557
t07:
	.word	3188869413
t09:
	.word	1038323257
t11:
	.word	3183095436
