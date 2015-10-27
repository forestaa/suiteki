     li     $r1, 8
     blez   $r0, .fib1
     add    $r2, $r1, $r0
.fib1:
     add    $r3, $r1, $r0
     sw     $r1, 1($r1)
     lw     $r4, 1($r1)
