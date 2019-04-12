function int func(int a, int b) {
  return a + b;
}

function int func2(int a, int b) {
  return a - b;
}

function int func3() {
  return 2;
}

function int main() {
  return func(1, 2) * func(2, 3) + func3();
}