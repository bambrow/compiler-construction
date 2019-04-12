function int main() {
  var int a;
  a = 1 + (-2); // STR -4
  var int b;
  b = 4 - a; // STR -8
  var int c;
  c = null(int); // STR -12
  var int d;
  d = sizeof(char); // STR -16
  var int e;
  e = !c; // STR -20
  var int f;
  f = -e; // STR -24
  var int g;
  g = +f; // STR -28
  var *int h;
  h = &g; // STR -32
  *h = 5;
  var int i;
  i = a * b; // STR -36
  var int j;
  j = 1 < 2; // STR -40
  var int k;
  k = 2 > 3; // STR -44
  var int l;
  l = 1 <= 2; // STR -48
  var int m;
  m = 2 >= 2; // STR -52
  var int n;
  n = 1 == 1; // STR -56
  var int o;
  o = 1 != 1; // STR -60
  var int p;
  p = 1 && 1; // STR -64
  var int q;
  q = 0 || 1; // STR -68
  var *int r;
  *r = *h; // STR -72
  return a + b - c + d - e;
}