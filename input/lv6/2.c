int main() {
  int a = 2;
  if (a) {

    if (a) {
        a = a + 5;
    } else {
        a = a + 6;
    }
    a = a + 1;
  } else a = 0;  // 在实际写 C/C++ 程序的时候别这样, 建议 if 的分支全部带大括号
  return a;
}

/*
fun @main(): i32 {
%entry:
  @a = alloc i32
  store 2, @a
  // if 的条件判断部分
  %0 = load @a
  br %0, %then, %else

// if 语句的 if 分支
%then:
  %1 = load @a
  %2 = add %1, 1
  store %2, @a
  jump %end

// if 语句的 else 分支
%else:
  store 0, @a
  jump %end

// if 语句之后的内容, if/else 分支的交汇处
%end:
  %3 = load @a
  ret %3
}

*/