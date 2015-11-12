## build
```
$ git clone http://github.com/rikatze/suiteki.git

$ cd suiteki

# Install dependencies
$ cabal install

# Build the assembler
$ cabal build

$ ./dist/build/suiteki/suiteki --no-externals ./test/print-nolib.s

$ ../sim/sim_machine ./a.out
(...)
f30  0.000000  0x00000000
f31  0.000000  0x00000000
123

$ ./dist/build/suiteki/suiteki ./test/print.s

$ ../sim/sim_machine ./a.out
(...)
f30  0.000000  0x00000000
f31  0.000000  0x00000000
123

$ ./dist/build/suiteki/suiteki ./test/fib.s

$ ../sim/sim_machine ./a.out
(...)
f30  0.000000  0x00000000
f31  0.000000  0x00000000
832040


# Show output
$ xxd -b -c 4 output  # or whatever you want
```
