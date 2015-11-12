	.text
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
