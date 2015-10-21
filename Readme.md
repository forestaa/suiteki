## build
```
$ git clone http://github.com/rikatze/suiteki.git
$ cd suiteki
$ cabal install # install dependencies
$ cabal build
$ ./dist/build/suiteki/suiteki ./sample/sample.S -o ./output -l path/to/lib
$ xxd -b -c 4 output  # or whatever you want
```
