int var;

const int one = 1;

int main() {
  return var + one;
}

/*
global @var = alloc i32, zeroinit

fun @main(): i32 {
%entry:
  %0 = load @var
  %1 = add %0, 1
  ret %1
}


*/

/*
  .data
  .globl var
var:
  .zero 4

  .text
  .globl main
main:
  addi sp, sp, -16
  la t0, var
  lw t0, 0(t0)
  sw t0, 0(sp)

  lw t0, 0(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 4(sp)
  
  lw a0, 4(sp)
  addi sp, sp, 16
  ret

*/