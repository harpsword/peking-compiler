  .data
  .global var2
var2:
  .zero 4

  .data
  .global var4
var4:
  .zero 4

  .text
  .globl t
t:
  addi sp, sp, -16
  la t0, var2
  lw t0, 0(t0)
  sw t0, 0(sp)
  lw t0, 0(sp)
  li t1, 1
  add t2, t0, t1
  sw t2, 4(sp)
  lw t0, 4(sp)
  la t1, var2
  sw t0, 0(t1)
  li t0, 1
  mv a0, t0
  addi sp, sp, 16
  ret

  .text
  .globl f
f:
  addi sp, sp, -16
  la t2, var4
  lw t2, 0(t2)
  sw t2, 0(sp)
  lw t2, 0(sp)
  li t3, 1
  add t4, t2, t3
  sw t4, 4(sp)
  lw t2, 4(sp)
  la t3, var4
  sw t2, 0(t3)
  mv a0, x0
  addi sp, sp, 16
  ret

  .text
  .globl main
main:
  addi sp, sp, -448
  sw ra, 444(sp)
  sw x0, 0(sp)
  lw t2, 0(sp)
  sw t2, 8(sp)
  call f
  sw a0, 12(sp)
  call f
  sw a0, 16(sp)
  lw t2, 12(sp)
  lw t4, 16(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 20(sp)
  lw t2, 8(sp)
  lw t4, 20(sp)
  add t5, t2, t4
  sw t5, 24(sp)
  lw t2, 24(sp)
  sw t2, 0(sp)
  lw t2, 0(sp)
  sw t2, 28(sp)
  call f
  sw a0, 32(sp)
  call t
  sw a0, 36(sp)
  lw t2, 32(sp)
  lw t4, 36(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 40(sp)
  lw t2, 28(sp)
  lw t4, 40(sp)
  add t5, t2, t4
  sw t5, 44(sp)
  lw t2, 44(sp)
  sw t2, 0(sp)
  lw t2, 0(sp)
  sw t2, 48(sp)
  call t
  sw a0, 52(sp)
  call f
  sw a0, 56(sp)
  lw t2, 52(sp)
  lw t4, 56(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 60(sp)
  lw t2, 48(sp)
  lw t4, 60(sp)
  add t5, t2, t4
  sw t5, 64(sp)
  lw t2, 64(sp)
  sw t2, 0(sp)
  lw t2, 0(sp)
  sw t2, 68(sp)
  call t
  sw a0, 72(sp)
  call t
  sw a0, 76(sp)
  lw t2, 72(sp)
  lw t4, 76(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 80(sp)
  lw t2, 68(sp)
  lw t4, 80(sp)
  add t5, t2, t4
  sw t5, 84(sp)
  lw t2, 84(sp)
  sw t2, 0(sp)
  lw t2, 0(sp)
  sw t2, 88(sp)
  call f
  sw a0, 92(sp)
  call f
  sw a0, 96(sp)
  lw t2, 92(sp)
  lw t4, 96(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 100(sp)
  lw t2, 88(sp)
  lw t4, 100(sp)
  add t5, t2, t4
  sw t5, 104(sp)
  lw t2, 104(sp)
  sw t2, 0(sp)
  lw t2, 0(sp)
  sw t2, 108(sp)
  call f
  sw a0, 112(sp)
  call t
  sw a0, 116(sp)
  lw t2, 112(sp)
  lw t4, 116(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 120(sp)
  lw t2, 108(sp)
  lw t4, 120(sp)
  add t5, t2, t4
  sw t5, 124(sp)
  lw t2, 124(sp)
  sw t2, 0(sp)
  lw t2, 0(sp)
  sw t2, 128(sp)
  call t
  sw a0, 132(sp)
  call f
  sw a0, 136(sp)
  lw t2, 132(sp)
  lw t4, 136(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 140(sp)
  lw t2, 128(sp)
  lw t4, 140(sp)
  add t5, t2, t4
  sw t5, 144(sp)
  lw t2, 144(sp)
  sw t2, 0(sp)
  lw t2, 0(sp)
  sw t2, 148(sp)
  call t
  sw a0, 152(sp)
  call t
  sw a0, 156(sp)
  lw t2, 152(sp)
  lw t4, 156(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 160(sp)
  lw t2, 148(sp)
  lw t4, 160(sp)
  add t5, t2, t4
  sw t5, 164(sp)
  lw t2, 164(sp)
  sw t2, 0(sp)
  call t
  sw a0, 168(sp)
  call t
  sw a0, 172(sp)
  call t
  sw a0, 176(sp)
  lw t2, 172(sp)
  lw t4, 176(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 180(sp)
  lw t2, 168(sp)
  lw t4, 180(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 184(sp)
  call f
  sw a0, 188(sp)
  call t
  sw a0, 192(sp)
  call t
  sw a0, 196(sp)
  lw t2, 192(sp)
  lw t4, 196(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 200(sp)
  lw t2, 188(sp)
  lw t4, 200(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 204(sp)
  call f
  sw a0, 208(sp)
  call f
  sw a0, 212(sp)
  call t
  sw a0, 216(sp)
  lw t2, 212(sp)
  lw t4, 216(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 220(sp)
  lw t2, 208(sp)
  lw t4, 220(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 224(sp)
  call t
  sw a0, 228(sp)
  call t
  sw a0, 232(sp)
  lw t2, 228(sp)
  lw t4, 232(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 236(sp)
  call t
  sw a0, 240(sp)
  lw t2, 236(sp)
  lw t4, 240(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 244(sp)
  call f
  sw a0, 248(sp)
  call t
  sw a0, 252(sp)
  lw t2, 248(sp)
  lw t4, 252(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 256(sp)
  call t
  sw a0, 260(sp)
  lw t2, 256(sp)
  lw t4, 260(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 264(sp)
  call f
  sw a0, 268(sp)
  call f
  sw a0, 272(sp)
  lw t2, 268(sp)
  lw t4, 272(sp)
  sltu t2, x0, t2
  sltu t4, x0, t4
  and t5, t2, t4
  sw t5, 276(sp)
  call f
  sw a0, 280(sp)
  lw t2, 276(sp)
  lw t4, 280(sp)
  or t5, t2, t4
  sltu t5, x0, t5
  sw t5, 284(sp)
  la t2, var2
  lw t2, 0(t2)
  sw t2, 288(sp)
  lw t2, 288(sp)
  mv a0, t2
  call putint
  li t2, 32
  mv a0, t2
  call putch
  la t2, var4
  lw t2, 0(t2)
  sw t2, 292(sp)
  lw t2, 292(sp)
  mv a0, t2
  call putint
  li t2, 10
  mv a0, t2
  call putch
  lw t2, 0(sp)
  sw t2, 296(sp)
  lw a0, 296(sp)
  lw ra, 444(sp)
  addi sp, sp, 448
  ret