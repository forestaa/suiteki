# nonsense assembly instructions (just for tests)
     lw     $t0, 4($gp)       # fetch N
     add    $t1, $t0, $zero   # copy N to $t1
     addi   $t1, $t1, 3       # N+3
     sw     $t1, 0($gp)       # i = ...
.Label1:
     li     %t3, 10
     lw     $t0, 4($gp)       # fetch N
     add    $t1, $t0, $zero   # copy N to $t1
     addi   $t1, $t1, 3       # N+3
     sw     $t1, 0($gp)       # i = ...
     blez   $t3, .Label1
