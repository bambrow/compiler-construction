
module xxx.xxx.cs.cc.Hw6 {

    space [ \n\t] | "//" .* ;

    token fragment Letter | [a-zA-z] ;
    token fragment Digit | [0-9] ;
    token ID | (⟨Letter⟩ | [$_]) (⟨Letter⟩ | [$_] | ⟨Digit⟩)* ;

    sort BProg | ⟦ ⟨ BBlock ⟩ ⟧ ;
    sort BBlock | ⟦ { ⟨ BStat ⟩ } ⟧ ;
    sort BStat | ⟦ def ⟨ ID ⟩ ; ⟨ BStat ⟩ ⟧ 
               | ⟦ use ⟨ ID ⟩ ; ⟨ BStat ⟩ ⟧ 
               | ⟦ ⟨ BBlock ⟩ ⟨ BStat ⟩ ⟧
               | ⟦⟧ ;
    sort B | True | False ;
    
    attribute ↑ok(B) ;
    attribute ↓e{ID} ;

    sort BProg ;
    | scheme BProgE(BProg) ↓e ;

    BProgE(⟦ ⟨ BBlock#1 ⟩ ⟧)
    → ⟦ ⟨ BBlock BBlockE(#1) ⟩ ⟧ ;

    sort BBlock ;
    | scheme BBlockE(BBlock) ↓e ;

    BBlockE(⟦ { ⟨ BStat#1 ⟩ } ⟧)
    → ⟦ { ⟨ BStat BStatE(#1) ⟩ } ⟧ ;

    sort BStat ;
    | ↑ok ;

    ⟦ def ⟨ ID#1 ⟩ ; ⟨ BStat#2 ⟩ ⟧↑ok(True) ;
    ⟦ ⟨ BBlock#1 ⟩ ⟨ BStat#2 ⟩ ⟧↑ok(True) ;

    | scheme BStatE(BStat) ↓e ;

    BStatE(⟦ def ⟨ ID#1 ⟩ ; ⟨ BStat#2 ⟩ ⟧)
    → ⟦ def ⟨ ID#1 ⟩ ; ⟨ BStat BStatE(#2)↓e{#1} ⟩ ⟧ ;
    BStatE(⟦ use ⟨ ID#1 ⟩ ; ⟨ BStat#2 ⟩ ⟧)↓e{#1}
    → ⟦ use ⟨ ID#1 ⟩ ; ⟨ BStat BStatE(#2) ⟩ ⟧↑ok(True) ;
    BStatE(⟦ use ⟨ ID#1 ⟩ ; ⟨ BStat#2 ⟩ ⟧)↓e{¬#1}
    → ⟦ use ⟨ ID#1 ⟩ ; ⟨ BStat BStatE(#2) ⟩ ⟧↑ok(False) ;
    BStatE(⟦ ⟨ BBlock#1 ⟩ ⟨ BStat#2 ⟩ ⟧)
    → ⟦ ⟨ BBlock BBlockE(#1) ⟩ ⟨ BStat BStatE(#2) ⟩ ⟧ ;

    sort BProg | scheme Eval(BBlock) ;
    Eval(⟦ { ⟨ BStat#1↑ok(#b) ⟩ } ⟧) → #b ;


}