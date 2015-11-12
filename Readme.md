## how to build & examples
```
$ git clone http://github.com/rikatze/suiteki.git

$ cd suiteki

# Install dependencies
$ cabal install

# Build the assembler
$ cabal build

$ ./dist/build/suiteki/suiteki ./test/print.s

# assuming that you've cloned the simulator (https://github.com/forestaa/cpu_simulator) at '..'
$ ../cpu_simulator/sim_machine ./a.out
(...)
f30  0.000000  0x00000000
f31  0.000000  0x00000000
123

# assemble without library (for debugging)
$ ./dist/build/suiteki/suiteki --no-externals ./test/print-nolib.s

$ ../cpu_simulator/sim_machine ./a.out
(...)
f30  0.000000  0x00000000
f31  0.000000  0x00000000
123


$ ./dist/build/suiteki/suiteki ./test/fib.s

$ ../cpu_simulator/sim_machine ./a.out
(...)
f30  0.000000  0x00000000
f31  0.000000  0x00000000
832040

# Show output
$ xxd -b -c 4 output  # or whatever you want
```
