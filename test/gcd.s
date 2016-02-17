        .text 
_min_caml_start:
        li  $v0, 21600
        li  $v1, 337500
        move  $a0, $v0
        move  $a1, $v1
        sw  $ra, 0($sp)
        addi  $sp, $sp, -1
        jal gcd
        addi  $sp, $sp, 1
        lw  $ra, 0($sp)
        move  $a0, $v0
        sw  $ra, 0($sp)
        addi  $sp, $sp, -1
        jal  min_caml_print_int
        addi  $sp, $sp, 1
        lw  $ra, 0($sp)
        jr  $ra
gcd:
        blez  $a0, gcd_ret
        sub $t0, $a1, $a0
        bgez  $t0, gcd_rec
        move  $t1, $a0
        move  $a0, $a1
        sub $a1, $t1, $a1
        sw  $ra, 0($sp)
        addi  $sp, $sp, -1
        jal gcd
        addi  $sp, $sp, 1
        lw  $ra, 0($sp)
        jr  $ra
gcd_ret:
        move  $v0, $a1
        jr  $ra
gcd_rec:
        move  $a1, $t0
        sw  $ra, 0($sp)
        addi  $sp, $sp, -1
        jal gcd
        addi  $sp, $sp, 1
        lw  $ra, 0($sp)
        jr  $ra
