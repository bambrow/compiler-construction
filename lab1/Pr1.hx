
module xxx.xxx.xxx.compiler.Pr1 {

/* LEXICAL ANALYSIS. */
  
// white space

space [ \t\f\r\n] | "//" .* | "/*" ([^*] | "*" [^/])* "*/" ;  // nested is not permitted

// token fragments

token fragment Letter | [a-zA-z] ;
token fragment Digit | [0-9] ;
token fragment Hexadecimal | [0-9a-fA-F] ;
token fragment Escape | "\n" | [\"\\] | [nt] | ⟨Digit⟩ ⟨Digit⟩? ⟨Digit⟩? | "x" ⟨Hexadecimal⟩ ⟨Hexadecimal⟩ ;

// tokens

token ID | (⟨Letter⟩ | [$_]) (⟨Letter⟩ | [$_] | ⟨Digit⟩)* ; // identifier
token INT | ⟨Digit⟩+ ; // integer
token STR | \" ([^"\\\n] | \\ ⟨Escape⟩)* \" ; // string

/* SYNTAX ANALYSIS. */

// expressions

sort Expr 
    | ⟦ ⟨Expr@1⟩ || ⟨Expr⟩ ⟧
    | ⟦ ⟨Expr@2⟩ && ⟨Expr@1⟩ ⟧@1
    | ⟦ ⟨Expr@2⟩ == ⟨Expr@3⟩ ⟧@2
    | ⟦ ⟨Expr@2⟩ != ⟨Expr@3⟩ ⟧@2
    | ⟦ ⟨Expr@3⟩ < ⟨Expr@4⟩ ⟧@3
    | ⟦ ⟨Expr@3⟩ > ⟨Expr@4⟩ ⟧@3
    | ⟦ ⟨Expr@3⟩ <= ⟨Expr@4⟩ ⟧@3
    | ⟦ ⟨Expr@3⟩ >= ⟨Expr@4⟩ ⟧@3
    | ⟦ ⟨Expr@4⟩ + ⟨Expr@5⟩ ⟧@4
    | ⟦ ⟨Expr@4⟩ - ⟨Expr@5⟩ ⟧@4
    | ⟦ ⟨Expr@5⟩ * ⟨Expr@6⟩ ⟧@5
    | ⟦ ⟨Expr@5⟩ / ⟨Expr@6⟩ ⟧@5
    | ⟦ ⟨Expr@5⟩ % ⟨Expr@6⟩ ⟧@5
    | ⟦ ! ⟨Expr@6⟩ ⟧@6
    | ⟦ - ⟨Expr@6⟩ ⟧@6
    | ⟦ + ⟨Expr@6⟩ ⟧@6
    | ⟦ * ⟨Expr@6⟩ ⟧@6
    | ⟦ & ⟨Expr@6⟩ ⟧@6
    | ⟦ ⟨Expr@7⟩ ( ⟨Exprlist⟩ ) ⟧@7
    | ⟦ null ( ⟨Type⟩ ) ⟧@7
    | ⟦ sizeof ( ⟨Type⟩ ) ⟧@7
    | ⟦ ⟨ID⟩ ⟧@8
    | ⟦ ⟨STR⟩ ⟧@8
    | ⟦ ⟨INT⟩ ⟧@8
    | sugar ⟦ ( ⟨Expr#⟩ ) ⟧@8 → Expr# // parenthesis
    ;

sort Exprlist   
    | ⟦ ⟨Expr⟩ , ⟨Exprlist⟩ ⟧
    | ⟦ ⟨Expr⟩  ⟧
    | ⟦⟧
    ;

// types

sort Type   
    | ⟦ * ⟨Type⟩ ⟧
    | ⟦ ⟨Type@1⟩ ( ⟨Typelist⟩ ) ⟧@1
    | ⟦ int ⟧@2
    | ⟦ char ⟧@2
    | sugar ⟦ ( ⟨Type#⟩ ) ⟧@2 → Type# // parenthesis
    ;


sort Typelist   
    | ⟦ ⟨Type⟩ , ⟨Typelist⟩ ⟧
    | ⟦ ⟨Type⟩  ⟧
    | ⟦⟧
    ;

// statements

sort Stat 
    | ⟦ { ⟨Statlist⟩ }⟧@1
    | ⟦ return ⟨Expr⟩ ;⟧@1
    | ⟦ while ( ⟨Expr⟩ ) ⟨Stat⟩ ⟧@1
    | ⟦ if ( ⟨Expr⟩ ) ⟨Stat⟩ ⟧@1
    | ⟦ if ( ⟨Expr⟩ ) ⟨Stat⟩ else ⟨Stat⟩ ⟧@1
    | ⟦ ⟨ID⟩ = ⟨Expr⟩ ;⟧@2
    | ⟦ * ⟨Expr⟩ = ⟨Expr⟩ ;⟧@2
    | ⟦ var ⟨Type⟩ ⟨ID⟩ ;⟧@3
    ;


sort Statlist   
    | ⟦ ⟨Stat⟩ ⟨Statlist⟩ ⟧
    | ⟦ ⟨Stat⟩  ⟧
    | ⟦⟧
    ;

// declarations

sort Dec 
    | ⟦ function ⟨Type⟩ ⟨ID⟩ ( ⟨Arglist⟩ )  { ⟨Statlist⟩ }⟧
    ;

sort Arglist   
    | ⟦ ⟨Type⟩ ⟨ID⟩ , ⟨Arglist⟩ ⟧
    | ⟦ ⟨Type⟩ ⟨ID⟩ ⟧
    | ⟦⟧
    ;

// program

main sort Pro 
    | ⟦ ⟨Dec⟩ ⟨Pro⟩ ⟧
    | ⟦ ⟨Dec⟩  ⟧
    ;

}



