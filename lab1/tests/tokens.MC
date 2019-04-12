// This tests if the tokens can be parsed correctly.

function int main() { 

  a = 1;
  $a = 123;
  _a = "";
  $a_1 = "abc";
  _a$1 = "    ";
  b = "\"\\\n\t\1\11\111\x11";
  c = "\
      ";
  // comments
  /* comments */
  /*** *** ***/
  /* // */
  // /**/

  return 0;

}
