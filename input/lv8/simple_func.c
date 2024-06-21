int half(int x) {
  return x / 2;
  // lhs = {0:1073741824}
  // lhs alloc = {0:1073741825}
  
}

void f() {}

int main() {
  f();
  return half(10);
}

/*
fun @half(@x: i32): i32 {
%entry:
  %x = alloc i32
  store @x, %x
  %0 = load %x
  %1 = div %0, 2
  ret %1
}

fun @f() {
%entry:
  ret
}

fun @main(): i32 {
%entry:
  call @f()
  %0 = call @half(10)
  ret %0
}


*/
