module xxx.xxx.cs.cc.Bool {

  // Syntax.
  sort B
    | ⟦ T ⟧@4 | ⟦ F ⟧@4 // constants
    | sugar ⟦ ( ⟨B#⟩ ) ⟧@4 → B# // parenthesis
    | ⟦ ⟨B@2⟩ | ⟨B@1⟩ ⟧@1 // disjunction
    | ⟦ ⟨B@3⟩ & ⟨B@2⟩ ⟧@2 // conjunction
    | ⟦ ! ⟨B@3⟩ ⟧@3 // negation
    ;

  // Semantic operations.
  sort B;
  | scheme And(B,B) ;
  And(⟦T⟧, #2) → #2 ;
  And(⟦F⟧, #2) → ⟦F⟧ ;

  | scheme Or(B,B) ;
  Or(⟦T⟧, #2) → ⟦T⟧ ;
  Or(⟦F⟧, #2) → #2 ;

  | scheme Not(B) ;
  Not(⟦F⟧) → ⟦T⟧ ;
  Not(⟦T⟧) → ⟦F⟧ ;

  // Recursive Evaluation scheme
  sort B;
  | scheme Evaluate(B) ;
  Evaluate(⟦ T ⟧) → ⟦ T ⟧ ;
  Evaluate(⟦ F ⟧) → ⟦ F ⟧ ;
  Evaluate(⟦ ⟨B#1⟩ | ⟨B#2⟩ ⟧) → Or(Evaluate(#1), Evaluate(#2)) ;
  Evaluate(⟦ ⟨B#1⟩ & ⟨B#2⟩ ⟧) → And(Evaluate(#1), Evaluate(#2)) ;
  Evaluate(⟦ ! ⟨B#⟩ ⟧) → Not(Evaluate(#)) ;

}
