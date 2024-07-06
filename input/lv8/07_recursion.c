int fib(int n) {
  if (n < 2) {
    return n;
  }
  return fib(n - 1) + fib(n - 2);
}

int main() {
  return fib(20);
}

/*
fun @fib(@n: i32): i32 {
@entry:
  %0 = alloc i32
  store @n, %0
  %1 = load %0
  %2 = lt %1, 2
  br %2, @then0, @else0

@then0:
  %3 = load %0
  ret %3
  jump @end0

@else0:

@end0:
  %4 = load %0
  %5 = sub %4, 1
  %6 = call @fib(%5)

  %7 = load %0
  %8 = sub %7, 2
  %9 = call @fib(%8)
  
  %10 = add %6, %9
  ret %10
}
*/