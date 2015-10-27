	.text
min_caml_print_newline:
	addi $a0, $0, 10
	li	$v0, 11
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_print_int:
	li	$v0, 1
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_print_byte:
	mov	%i2, %o0
	st	%o7, [%i0]
	call	putchar
	nop
	ld	[%i0], %o7
	retl
	nop
min_caml_prerr_int:
	set	min_caml_stderr, %o0
	set	format_int, %o1
	mov	%i2, %o2
	st	%o7, [%i0]
	call	fprintf
	nop
	ld	[%i0], %o7
	retl
	nop
min_caml_prerr_byte:
	mov	%i2, %o0
	set	min_caml_stderr, %o1
	st	%o7, [%i0]
	call	fputc
	nop
	ld	[%i0], %o7
	retl
	nop
min_caml_prerr_float:
	set	min_caml_stderr, %o0
	set	format_float, %o1
	std	%f0, [%i0]
	ldd	[%i0], %o2
	st	%o7, [%i0]
	call	fprintf
	nop
	ld	[%i0], %o7
	retl
	nop
min_caml_read_int:
	li	$v0, 5
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_read_float:
	li	$v0, 6
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_create_array:
	move	$v1, $a0
	move	$t0, $hp
create_array_loop:
	bne	$v1, $zero, create_array_cont
	move	$v0, $hp
	addi	$hp, $t0, 4
	jr	$ra
create_array_cont:
	sw	$a1, 0($t0)
	addi	$v1, $v1, -1
	addi	$t0, $t0, 4
	b	create_array_loop
min_caml_create_float_array:
	move	$t1, $f12
	move	$t0, $hp
create_float_array_loop:
	bne	$t1, $zero, create_array_cont
	move	$f0, $hp
	addi	$hp, $t0, 4
	jr	$ra
create_float_array_cont:
	sw	$f13, 0($t0)
	addi	$t1, $t1, -1
	addi	$t0, $t0, 4
	b	create_array_loop
min_caml_abs_float:
	abs.s	$f0, $f12
	jr	$ra
min_caml_sqrt:
	sqrt.s	$f0, $f12
	jr	$ra
min_caml_floor:
	li	$v0, 100
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
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
	li	$v0, 101
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_sin:
	li	$v0, 102
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
	sw	$ra, 0($sp)
	jr	$ra
min_caml_atan:
	li	$v0, 103
	sw	$ra, 0($sp)
	addi    $sp, $sp, 4
	syscall
	addi    $sp, $sp, -4
	sw	$ra, 0($sp)
	jr	$ra
.section ".rodata"
format_int:
	.asciz	"%d"
format_float:
	.asciz	"%lf"
.align 8
float_0:
	.long	0x0
	.long	0x0
float_1:
	.long	0x3ff00000
	.long	0x0
