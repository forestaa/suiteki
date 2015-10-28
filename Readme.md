## build
```
$ git clone http://github.com/rikatze/suiteki.git
$ cd suiteki

# Install dependencies
$ cabal install

# Build the assembler
$ cabal build

# Note that you can replace "./dist/build/suiteki/suiteki" with "cabal run -- "
$ ./dist/build/suiteki/suiteki ./test/fib.s -o ./output -l ./lib/libmincaml.S

# Show output
$ xxd -b -c 4 output  # or whatever you want
```
