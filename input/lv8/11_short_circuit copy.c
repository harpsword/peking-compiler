int x, y;

int t() {
  x = x + 1;
  return 1;
}

int f() {
  y = y + 1;
  return 0;
}

int main() {
  int sum = 0;
  sum = sum + (f() || f()); // sum = 0, y = 2, x = 0
  sum = sum + (f() || t()); // sum = 1, y = 3, x = 1 // right (1, 3, 1)
  // sum = sum + (t() || f()); // sum = 2, y = 3, x = 2
  // sum = sum + (t() || t()); // sum = 3, y = 3, x = 3, // wrong error (3, 4, 4)
  // sum = sum + (f() && f()); // sum = 3, y = 4, x = 3
  // sum = sum + (f() && t()); // sum = 3, y = 5, x = 3
  // sum = sum + (t() && f()); // sum = 3, y = 6, x = 4
  // sum = sum + (t() && t()); // sum = 4, y = 6, x = 6 // wrong error (4, 8, 8)
  // t() || t() && t(); // x = 7, y = 6
  // f() || t() && t(); // x = 9, 6 = 7
  // f() || f() && t(); // x = 9, y = 9
  // t() && t() || t(); // x = 11, y = 9
  // f() && t() || t(); // x = 12, y = 10
  // f() && f() || f(); // x = 12, y = 12
  putint(x);
  putch(32);
  putint(y);
  putch(10);
  return sum;
}