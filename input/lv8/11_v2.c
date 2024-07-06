#include <stdio.h>

int x, y;

int t() {
  x = x + 1;
  return 1;
}

int f() {
  y = y + 1;
  return 0;
}

int main()
{
   /*  Write C code in this online editor and run it. */
	  int sum = 0;
  sum = sum + (f() || f());
  sum = sum + (f() || t());
  sum = sum + (t() || f());
  sum = sum + (t() || t());
  sum = sum + (f() && f());
  sum = sum + (f() && t());
  sum = sum + (t() && f());
  sum = sum + (t() && t());
//   t() || t() && t();
//   f() || t() && t();
//   f() || f() && t();
//   t() && t() || t();
//   f() && t() || t();
//   f() && f() || f();
   printf("x = %d \n", x);
   printf("y = %d \n", y);
   printf("sum = %d \n", sum);
   return 0;
}