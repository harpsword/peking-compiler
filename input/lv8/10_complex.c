int a = 10;

int inc() {
  a = a + 1;
  return a;
}

/*
 global %0 = alloc i32, 10
 
fun @inc(): i32 {
@entry:

  %1 = load %0
  %2 = add %1, 1
  store %2, %0
  %3 = load %0
  ret %3
}
*/

void print_a() {
  putint(a);
  putch(10);
}

int main() {
  int i = 0;
  while (i < 10) {
    inc();
    int a = 1;
    a = a + 2;
    putint(a);
    putch(10);
    print_a();
    i = i + 1;
  }
  return 0;
}