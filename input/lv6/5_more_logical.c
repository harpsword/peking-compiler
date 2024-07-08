int main() {
  int a = 1 || 2 && 3; // a  = 1, right
  int b = 0 && 1 || 0; // b = 0, right
  int c = (1 && 0 || 1) * 4; // c = 4
  int d = 5;
  const int e = 6 || 7 && 8; // e = 1
  if (a == 1 || a == 2);
  if (b == 0 || b == 1) d = d + 1; else; // d = 6
  if (a && b || c && d) d = d + e; // d = 6 + 1 = 7
  return d || !c; // 7 || !4 = 1
  return d || e; // 7 || 1 = 1
return d;
}