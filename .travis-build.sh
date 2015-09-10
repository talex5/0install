#!/bin/bash -eux
eval `opam config env`
make
sudo make install
0install add-feed http://0install.net/tools/0install.xml dist/0install/feed.xml
git clone https://github.com/0install/0compile.git -b use-ocaml
0install run -c 0compile/0compile.xml autocompile http://0install.net/tests/GNU-Hello.xml
0install run -c http://0install.net/tests/GNU-Hello.xml
