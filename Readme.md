## build
```
$ git clone http://github.com/rikatze/suiteki.git
$ cd suiteki
$ cabal install
$ cabal build
$ ./dist/build/suiteki/suiteki ./sample/sample.S ./output
$ xxd -b -c 4 output  # or whatever you want
```
