#!/bin/bash -eux
eval `opam config env`
make
cd ocaml
./0install run -c http://0install.net/2006/interfaces/0compile.xml autocompile http://0install.net/tests/GNU-Hello.xml
./0install run -c http://0install.net/tests/GNU-Hello.xml
