// This tests if statements can be parsed properly.

function int main() { 

  var int a;
  var int b;
  var char c;
  a = b + c;
  if (a == 0) a = 1;
  if (a == 1) {
    a = 2;
  }
  if (a == 3) {
    a = 4;
  } else {
    a = 5;
  }
  if (a == 4) {
    if (b == 0) {
      c = 1;
    } else if (b == 1) {
      c = 2;
    } else {
      c = 3;
    }
  } else if (a == 5) if (b == 0) c = 1;
  while (a < 10) {
    if (a % 2 == 1) {
      a = a + 1;
    } else {
      a = a + 3;
    }
  }
  while (a < 20) if (a < 25) a = a + 10;

  return 0;

}
