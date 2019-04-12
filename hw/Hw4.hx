
module xxx.xxx.cs.cc.Hw4 {
	
	space [ \n\t] | "//" .* ;

	token NUM | [0-9]+ ;

	sort L 
	  | ⟦ ⟨ NUM ⟩ ⟨ L ⟩ ⟧ 
	  | ⟦ { ⟨ L ⟩ } ⟨ L ⟩ ⟧ 
	  | ⟦ ⟧ 
      ;

    sort Computed;

    | scheme Prod(L) ;
    Prod ( # ) → ProdHelper(#, ⟦ 1 ⟧) ;

    | scheme ProdHelper(L, Computed);
    ProdHelper ( ⟦ ⟧, #n ) → #n ;
    ProdHelper ( ⟦ ⟨ NUM#1 ⟩ ⟨ L#2 ⟩ ⟧, #n ) → ProdHelper(#2, ⟦ #n * #1 ⟧) ;
    ProdHelper ( ⟦ { ⟨ L#1 ⟩ } ⟨ L#2 ⟩ ⟧, #n ) → ProdHelper(#2, ProdHelper(#1, #n)) ;

    sort L;

    | scheme Flat(L) ;
    Flat ( ⟦ ⟧ ) → ⟦ ⟧ ;
    Flat ( ⟦ ⟨ NUM#1 ⟩ ⟨ L#2 ⟩ ⟧ ) → ⟦ ⟨ NUM#1 ⟩ ⟨ L Flat(#2) ⟩ ⟧ ;
    Flat ( ⟦ { ⟨ L#1 ⟩ } ⟨ L#2 ⟩ ⟧ ) → App(Flat(#1), Flat(#2)) ;

    | scheme App(L, L) ;
    App ( ⟦ ⟧, # ) → # ;
    App ( ⟦ ⟨ NUM#1 ⟩ ⟨ L#2 ⟩ ⟧, #3 ) → ⟦ ⟨ NUM#1 ⟩ ⟨ L App(#2, #3) ⟩ ⟧ ;

    sort L;

    | scheme Rev(L) ;
    Rev ( # ) → RevHelper(#, ⟦ ⟧) ;

    | scheme RevHelper(L, L) ;
    RevHelper ( ⟦ ⟧, # ) → # ;
    RevHelper ( ⟦ ⟨ NUM#1 ⟩ ⟨ L#2 ⟩ ⟧, #3 ) → RevHelper(#2, ⟦ ⟨ NUM#1 ⟩ ⟨ L#3 ⟩ ⟧) ;
    RevHelper ( ⟦ { ⟨ L#1 ⟩ } ⟨ L#2 ⟩ ⟧, #3 ) → RevHelper(#2, ⟦ { ⟨ L Rev(#1) ⟩ } ⟨ L#3 ⟩ ⟧) ;

}