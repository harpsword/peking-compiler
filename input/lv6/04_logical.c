int main() {
  int a = 1, b = 0;
  if (a || b) {
    b = 1;
    a = 0; 
  } // a = 0, b = 1
  if (a && b) {
    return 0;
  }
  return 77;
}