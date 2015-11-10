	.data
x4:
	.word	65535
x3:
	.word	1023
x2:
	.word	127
x1:
	.word	63
	.text
_min_caml_start: # main entry point
   # main program start
	lwc1	$f16, 0(x1)
	lwc1	$f17, 0(x2)
	lwc1	$f18, 0(x3)
	lwc1	$f19, 0(x4)
   # main program end
	jr	$ra
