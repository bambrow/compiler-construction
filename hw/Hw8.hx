
module xxx.xxx.cs.cc.Hw8 {
	
	space [ \n\t] | "//" .* ;

	token INT | [0-9]+ ;

	sort List
	  | ⟦ ⟨ INT ⟩ ⟨ List ⟩ ⟧ 
	  | ⟦ ⟧ 
      ;

    sort Computed | scheme Add(Computed, Computed) ;
    Add(#1, #2) → ⟦ #1 + #2 ⟧ ; 

    sort Computed | scheme LenS(List) ;
    LenS( ⟦ ⟧ ) → ⟦ 0 ⟧ ;
    LenS( ⟦ ⟨ INT#1 ⟩ ⟨ List#2 ⟩ ⟧ ) → Add( ⟦ 1 ⟧, LenS(#2) );

    attribute ↑len(Computed) ;
    sort List | ↑len ;
    ⟦ ⟧ ↑len(⟦ 0 ⟧) ;
    ⟦ ⟨ INT#1 ⟩ ⟨ List#2 ↑len(#len2) ⟩ ⟧ ↑len(Add(⟦ 1 ⟧, #len2)) ;

    sort Computed | scheme LenA(List) ;
    LenA(# ↑len(#len)) → #len ;

    sort Bool | ⟦ TRUE ⟧ | ⟦ FALSE ⟧ ;

    sort List | scheme Append(INT, List) ;
    Append(#1, ⟦ ⟧) → ⟦ ⟨ INT#1 ⟩ ⟧ ;
    Append(#1, ⟦ ⟨ INT#2 ⟩ ⟨ List#3 ⟩ ⟧) → ⟦ ⟨ INT#2 ⟩ ⟨ List Append(#1, #3) ⟩ ⟧ ;
    | scheme Append0(List) ;
    Append0(#1) → Append(⟦ 0 ⟧, #1) ;

    sort List | scheme Rev(List) ;
    Rev ( # ) → RevHelper(#, ⟦ ⟧) ;
    | scheme RevHelper(List, List) ;
    RevHelper ( ⟦ ⟧, # ) → # ;
    RevHelper ( ⟦ ⟨ INT#1 ⟩ ⟨ List#2 ⟩ ⟧, #3 ) → RevHelper(#2, ⟦ ⟨ INT#1 ⟩ ⟨ List#3 ⟩ ⟧) ;

/*
    sort Bool | scheme Match(List, List) ;
    Match(⟦ ⟧, ⟦ ⟧) → ⟦ TRUE ⟧ ;
    Match(⟦ ⟧, #) → ⟦ FALSE ⟧ ;
    Match(#, ⟦ ⟧) → ⟦ FALSE ⟧ ;
    Match(⟦ ⟨ INT#1 ⟩ ⟨ List#2 ⟩ ⟧, ⟦ ⟨ INT#3 ⟩ ⟨ List#4 ⟩ ⟧)
      → ⟦ $#1=$#3 ? ⟨ Bool Match(#2, #4) ⟩ : FALSE ⟧ ;
*/

/*
    sort Bool | scheme PalS(List) ;
    PalS(⟦ ⟧) → ⟦ TRUE ⟧ ;
    PalS(⟦ ⟨ INT#1 ⟩ ⟨ List#2 ⟩ ⟧) → PalS1(#1, #2) ;
    | scheme PalS1(INT, List) ;
    PalS1(#, ⟦ ⟧) → ⟦ FALSE ⟧ ;
    PalS1(#1, #2) → PalS2(#1, Rev(#2)) ;
    | scheme PalS2(INT, List) ;
    PalS2(#1, ⟦ ⟨ INT#2 ⟩ ⟨ List#3 ⟩ ⟧) → $#1=$#2 ? PalS(#3) : ⟦ FALSE ⟧ ;
*/

    sort Bool | scheme PalA(List) ;
    // PalA(# ↑rev(#rev)) → Match(#, #rev) ;

    attribute ↑rev(List) ;
    sort List | ↑rev ;
    ⟦ ⟧ ↑rev(⟦ ⟧) ;
    ⟦ ⟨ INT#1 ⟩ ⟨ List#2 ↑rev(#rev2) ⟩ ⟧↑rev(Append(#1, #rev2)) ;

}












