#!/bin/bash

docker run -it --rm -v /Users/hs/code/MOOC/Peking_Compiler/compiler:/root/compiler maxxing/compiler-dev \
    autotest -riscv -s lv1 /root/compiler
