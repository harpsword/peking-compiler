
# Introduction

This repo is an implementation of Peking University Compiler course, related link: https://pku-minic.github.io/online-doc. 


# Experience of LALRPOP

lexer match precedence
1. longest match
2. same length, prefers the fixed string

# Experience of RiscV

指令的规则
1. 后缀
   1. 有i 表示指 令用于对寄存器值与一个立即数进行按位操作
   2. 有u 表示操作中读取的数据都当作无符号数


2. slt t0, t1, t2
   1. t0 = t1 < t2
3. sltiu t0, t1, t2
   1. t0 = t1 < t2
   2. 特点：比较t1和t2的无符号值
   3. 常用范式
      1. sltiu t1, t1, 1
         1. t1 < 1, set t1 = 1, else 0
         2. so when t1 >= 1, t1 will be 0
         3. when t1 < 1, t1 will be 1
         4. meaning, when t1=0, set t1 to 1
4. sltu t0, t1, t2
   1. t0 = t1 < t2
5. xori t0, t1, t2， 
   1. i表示 指令用于对寄存器值与一个立即数进行按位异或操作
   2. t0 = t1 xor t2, 
   3. xori a0, a0, 1
      1. when a0 = 1 or 0, meaning a0 = !a0
