// This tests if the declarations can be parsed correctly.

function char generateChar() {
  return 1;
}

function int generateInt() {
  return 2;
}

function int add(int a, int b) {
  return a + b;
}

function int hypot(int a, int b) {
  return a * a + b * b;
}

function *int getPointer(int a) {
  return *a;
}

function int getInt(*int a) {
  return &a;
}

function int main() { 

  return 0;

}

function int extra(int a, char b, int c) {
  return 1;
}
