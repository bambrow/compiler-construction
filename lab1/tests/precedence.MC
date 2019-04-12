// This tests if the precedence of expressions behaves properly.

function int main() { 
  
  a1 = a || b && c;
  a2 = a && b || c;
  a3 = a == b || c;
  a4 = b >= a == c;
  a5 = a + b < c;
  a6 = a + b * c;
  a7 = a * !b;
  a8 = !a(1+1, 2-2, 3>3, 4<4, 5&&5, 6||6, !7);
  a9 = (a + b) * c;
  a10 = (a || b) && c;

  return 0;

}
