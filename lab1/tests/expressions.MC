// This tests if the expressions can be parsed correctly with proper associates.

function int main() { 
  
  a1 = a || b || c;
  a2 = a && b && c;
  a3 = a + b + c;
  a4 = a * b * c;
  a5 = *a;
  a6 = &a;
  a7 = !a;
  a8 = sizeof(char);
  a9 = null(int);
  a10 = a(1+1, 2-2, 3>3, 4<4, 5&&5, 6||6, !7);
  a11 = (a+b)*c;
  a12 = "a" + "b" + "c";

  return 0;

}
