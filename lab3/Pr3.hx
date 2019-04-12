// Compiler Construction/Fall 2016/Project Milestone 3 -*-hacs-*-
//
// Contents.
// 1. MiniC Lexical analysis and grammar
// 2. MinARM32 assembler grammar
// 3. Compiler from MiniC to MinARM32
//
// Author: 
// Weiqiang Li 
// Xianglong Hu 

module xxx.xxx.cs.cc.Pr3 {

  
  ////////////////////////////////////////////////////////////////////////
  // 1. MiniC LEXICAL ANALYSIS AND GRAMMAR
  ////////////////////////////////////////////////////////////////////////

  space [ \t\n\r] | '//' [^\n]* | '/*' ( [^*] | '*' [^/] )* '*/'  ; // Inner /* ignored

  token ID  	| ⟨LetterEtc⟩ (⟨LetterEtc⟩ | ⟨Digit⟩)* ;
  token INT	| ⟨Digit⟩+ ;
  token STR | "\"" ( [^\"\\\n] | \\ ⟨Escape⟩ )* "\"";

  token fragment Letter     | [A-Za-z] ;
  token fragment LetterEtc  | ⟨Letter⟩ | [$_] ;
  token fragment Digit      | [0-9] ;
  
  token fragment Escape  | [\n\\nt"] | "x" ⟨Hex⟩ ⟨Hex⟩ | ⟨Octal⟩;
  token fragment Hex     | [0-9A-Fa-f] ;
  token fragment Octal   | [0-7] | [0-7][0-7] | [0-7][0-7][0-7];
 
  main sort Program  |  ⟦ ⟨Declarations⟩ ⟧ ;

  sort Declarations | ⟦ ⟨Declaration⟩ ⟨Declarations⟩ ⟧ | ⟦⟧ ;

  sort Declaration
    |  ⟦ function ⟨Type⟩ ⟨Identifier⟩ ⟨ArgumentSignature⟩ { ⟨Statements⟩ } ⟧
    ;

  sort ArgumentSignature
    |  ⟦ ( ) ⟧
    |  ⟦ ( ⟨Type⟩ ⟨Identifier⟩ ⟨TypeIdentifierTail⟩ ) ⟧
    ;
  sort TypeIdentifierTail |  ⟦ , ⟨Type⟩ ⟨Identifier⟩ ⟨TypeIdentifierTail⟩ ⟧  |  ⟦ ⟧ ;

  sort Statements | ⟦ ⟨Statement⟩ ⟨Statements⟩ ⟧ | ⟦⟧ ;

  sort Statement
    |  ⟦ { ⟨Statements⟩ } ⟧
    |  ⟦ var ⟨Type⟩ ⟨Identifier⟩ ; ⟧
    |  ⟦ ⟨Expression⟩ = ⟨Expression⟩ ; ⟧
    |  ⟦ if ( ⟨Expression⟩ ) ⟨IfTail⟩ ⟧
    |  ⟦ while ( ⟨Expression⟩ ) ⟨Statement⟩ ⟧
    |  ⟦ return ⟨Expression⟩ ; ⟧
    ;

  sort IfTail | ⟦ ⟨Statement⟩ else ⟨Statement⟩ ⟧ | ⟦ ⟨Statement⟩ ⟧ ;

  sort Type
    |  ⟦ int ⟧@3
    |  ⟦ char ⟧@3
    |  ⟦ ( ⟨Type⟩ )⟧@3
    |  ⟦ ⟨Type@2⟩ ( ⟨TypeList⟩ )⟧@2
    |  ⟦ * ⟨Type@1⟩ ⟧@1
    ;
    
  sort TypeList | ⟦ ⟨Type⟩ ⟨TypeListTail⟩ ⟧ | ⟦⟧;
  sort TypeListTail | ⟦ , ⟨Type⟩ ⟨TypeListTail⟩ ⟧ | ⟦⟧;  

  sort Expression

    |  sugar ⟦ ( ⟨Expression#e⟩ ) ⟧@10 → #e

    |  ⟦ ⟨Integer⟩ ⟧@10
    |  ⟦ ⟨String⟩ ⟧@10
    |  ⟦ ⟨Identifier⟩ ⟧@10

    |  ⟦ ⟨Expression@9⟩ ( ⟨ExpressionList⟩ ) ⟧@9
    |  ⟦ null ( ⟨Type⟩ ) ⟧@9
    |  ⟦ sizeof ( ⟨Type⟩ )⟧@9

    |  ⟦ ! ⟨Expression@8⟩ ⟧@8
    |  ⟦ - ⟨Expression@8⟩ ⟧@8
    |  ⟦ + ⟨Expression@8⟩ ⟧@8
    |  ⟦ * ⟨Expression@8⟩ ⟧@8
    |  ⟦ & ⟨Expression@8⟩ ⟧@8

    |  ⟦ ⟨Expression@7⟩ * ⟨Expression@8⟩ ⟧@7

    |  ⟦ ⟨Expression@6⟩ + ⟨Expression@7⟩ ⟧@6
    |  ⟦ ⟨Expression@6⟩ - ⟨Expression@7⟩ ⟧@6

    |  ⟦ ⟨Expression@6⟩ < ⟨Expression@6⟩ ⟧@5
    |  ⟦ ⟨Expression@6⟩ > ⟨Expression@6⟩ ⟧@5
    |  ⟦ ⟨Expression@6⟩ <= ⟨Expression@6⟩ ⟧@5
    |  ⟦ ⟨Expression@6⟩ >= ⟨Expression@6⟩ ⟧@5

    |  ⟦ ⟨Expression@5⟩ == ⟨Expression@5⟩ ⟧@4
    |  ⟦ ⟨Expression@5⟩ != ⟨Expression@5⟩ ⟧@4

    |  ⟦ ⟨Expression@3⟩ && ⟨Expression@4⟩ ⟧@3

    |  ⟦ ⟨Expression@2⟩ || ⟨Expression@3⟩ ⟧@2
    ;
    
  // Helper to describe actual list of arguments of function call.

  sort ExpressionList | ⟦ ⟨Expression⟩ ⟨ExpressionListTail⟩ ⟧  |  ⟦⟧ ;
  sort ExpressionListTail | ⟦ , ⟨Expression⟩ ⟨ExpressionListTail⟩ ⟧  |  ⟦⟧ ;  

  sort Integer		| ⟦ ⟨INT⟩ ⟧ ;
  sort String		| ⟦ ⟨STR⟩ ⟧ ;
  sort Identifier	| symbol ⟦⟨ID⟩⟧ ;

 
  ////////////////////////////////////////////////////////////////////////
  // 2. MinARM32 ASSEMBLER GRAMMAR
  ////////////////////////////////////////////////////////////////////////
 
  sort Instructions | ⟦ ⟨Instruction⟩ ⟨Instructions⟩ ⟧ | ⟦⟧ ;

  sort Instruction
    | ⟦DEF ⟨Identifier⟩ = ⟨Addr⟩ ¶⟧  // define identifier
    | ⟦¶⟨Label⟩ ⟧               // define address label
    | ⟦DCI ⟨Integers⟩ ¶⟧        // allocate integers
    | ⟦DCS ⟨String⟩ ¶⟧          // allocate strings
    | ⟦⟨Op⟩ ¶⟧                  // machine instruction
    ;

  sort Addr | ⟦⟨Integer⟩⟧ ;

  sort Integers | ⟦ ⟨Integer⟩, ⟨Integers⟩ ⟧ | ⟦ ⟨Integer⟩ ⟧ ;

  sort Label | symbol ⟦⟨ID⟩⟧ ;
 
  sort Op

    | ⟦MOV ⟨Reg⟩, ⟨Arg⟩ ⟧		// move
    | ⟦MVN ⟨Reg⟩, ⟨Arg⟩ ⟧		// move not
    | ⟦ADD ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧	// add
    | ⟦SUB ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧	// subtract
    | ⟦RSB ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧	// reverse subtract
    | ⟦AND ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧	// bitwise and
    | ⟦ORR ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧	// bitwise or
    | ⟦EOR ⟨Reg⟩, ⟨Reg⟩, ⟨Arg⟩ ⟧	// bitwise exclusive or
    | ⟦CMP ⟨Reg⟩, ⟨Arg⟩ ⟧	    	// compare
    | ⟦MUL ⟨Reg⟩, ⟨Reg⟩, ⟨Reg⟩ ⟧	// multiply

    | ⟦B ⟨Label⟩ ⟧			// branch always
    | ⟦BEQ ⟨Label⟩ ⟧			// branch if equal
    | ⟦BNE ⟨Label⟩ ⟧			// branch if not equal
    | ⟦BGT ⟨Label⟩ ⟧			// branch if greater than
    | ⟦BLT ⟨Label⟩ ⟧			// branch if less than
    | ⟦BGE ⟨Label⟩ ⟧			// branch if greater than or equal
    | ⟦BLE ⟨Label⟩ ⟧			// branch if less than or equal
    | ⟦BL ⟨Label⟩ ⟧			// branch and link

    | ⟦LDR ⟨Reg⟩, ⟨Mem⟩ ⟧		// load register from memory
    | ⟦STR ⟨Reg⟩, ⟨Mem⟩ ⟧		// store register to memory

    | ⟦LDMFD ⟨Reg⟩! , {⟨Regs⟩} ⟧ 	// load multiple fully descending (pop)
    | ⟦STMFD ⟨Reg⟩! , {⟨Regs⟩} ⟧	// store multiple fully descending (push)
    ;

  sort Reg	| ⟦R0⟧ | ⟦R1⟧ | ⟦R2⟧ | ⟦R3⟧ | ⟦R4⟧ | ⟦R5⟧ | ⟦R6⟧ | ⟦R7⟧
		| ⟦R8⟧ | ⟦R9⟧ | ⟦R10⟧ | ⟦R11⟧ | ⟦R12⟧ | ⟦SP⟧ | ⟦LR⟧ | ⟦PC⟧ ;

  sort Arg | ⟦⟨Constant⟩⟧ | ⟦⟨Reg⟩⟧ | ⟦⟨Reg⟩, LSL ⟨Constant⟩⟧ | ⟦⟨Reg⟩, LSR ⟨Constant⟩⟧ ;

  sort Mem | ⟦[⟨Reg⟩, ⟨Sign⟩⟨Arg⟩]⟧ ;
  sort Sign | ⟦+⟧ | ⟦-⟧ | ⟦⟧ ;

  sort Regs | ⟦⟨Reg⟩⟧ | ⟦⟨Reg⟩-⟨Reg⟩⟧ | ⟦⟨Reg⟩, ⟨Regs⟩⟧ | ⟦⟨Reg⟩-⟨Reg⟩, ⟨Regs⟩⟧ ;

  sort Constant | ⟦#⟨Integer⟩⟧ | ⟦&⟨Identifier⟩⟧ ;

  // Helper concatenation/flattening of Instructions.

  sort Instructions | scheme ⟦ { ⟨Instructions⟩ } ⟨Instructions⟩ ⟧ ;
  ⟦ {} ⟨Instructions#⟩ ⟧ → # ;
  ⟦ {⟨Instruction#1⟩ ⟨Instructions#2⟩} ⟨Instructions#3⟩ ⟧
    → ⟦ ⟨Instruction#1⟩ {⟨Instructions#2⟩} ⟨Instructions#3⟩ ⟧ ;

  // Helper data structure for list of registers.

  sort Rs | NoRs | MoRs(Reg, Rs) | scheme AppendRs(Rs, Rs) ;
  AppendRs(NoRs, #Rs) → #Rs ;
  AppendRs(MoRs(#Rn, #Rs1), #Rs2) → MoRs(#Rn, AppendRs(#Rs1, #Rs2)) ;

  // Helper conversion from Regs syntax to register list.

  | scheme XRegs(Regs) ;
  XRegs(⟦⟨Reg#r⟩⟧) → MoRs(#r, NoRs) ;
  XRegs(⟦⟨Reg#r1⟩-⟨Reg#r2⟩⟧) → XRegs1(#r1, #r2) ;
  XRegs(⟦⟨Reg#r⟩, ⟨Regs#Rs⟩⟧) → MoRs(#r, XRegs(#Rs)) ;
  XRegs(⟦⟨Reg#r1⟩-⟨Reg#r2⟩, ⟨Regs#Rs⟩⟧) → AppendRs(XRegs1(#r1, #r2), XRegs(#Rs)) ;

  | scheme XRegs1(Reg, Reg) ;
  XRegs1(#r, #r) → MoRs(#r, NoRs) ;
  default XRegs1(#r1, #r2) → XRegs2(#r1, #r2) ;

  | scheme XRegs2(Reg, Reg) ;
  XRegs2(⟦R0⟧, #r2) → MoRs(⟦R0⟧, XRegs1(⟦R1⟧, #r2)) ;
  XRegs2(⟦R1⟧, #r2) → MoRs(⟦R1⟧, XRegs1(⟦R2⟧, #r2)) ;
  XRegs2(⟦R2⟧, #r2) → MoRs(⟦R2⟧, XRegs1(⟦R3⟧, #r2)) ;
  XRegs2(⟦R3⟧, #r2) → MoRs(⟦R3⟧, XRegs1(⟦R4⟧, #r2)) ;
  XRegs2(⟦R4⟧, #r2) → MoRs(⟦R4⟧, XRegs1(⟦R5⟧, #r2)) ;
  XRegs2(⟦R5⟧, #r2) → MoRs(⟦R5⟧, XRegs1(⟦R6⟧, #r2)) ;
  XRegs2(⟦R6⟧, #r2) → MoRs(⟦R6⟧, XRegs1(⟦R7⟧, #r2)) ;
  XRegs2(⟦R7⟧, #r2) → MoRs(⟦R7⟧, XRegs1(⟦R8⟧, #r2)) ;
  XRegs2(⟦R8⟧, #r2) → MoRs(⟦R8⟧, XRegs1(⟦R9⟧, #r2)) ;
  XRegs2(⟦R9⟧, #r2) → MoRs(⟦R9⟧, XRegs1(⟦R10⟧, #r2)) ;
  XRegs2(⟦R10⟧, #r2) → MoRs(⟦R10⟧, XRegs1(⟦R11⟧, #r2)) ;
  XRegs2(⟦R11⟧, #r2) → MoRs(⟦R11⟧, XRegs1(⟦R12⟧, #r2)) ;
  XRegs2(⟦R12⟧, #r2) → MoRs(⟦R12⟧, NoRs) ;
  XRegs1(⟦SP⟧, #r2) → error⟦MinARM32 error: Cannot use SP in Regs range.⟧ ;
  XRegs1(⟦LR⟧, #r2) → error⟦MinARM32 error: Cannot use LR in Regs range.⟧ ;
  XRegs1(⟦PC⟧, #r2) → error⟦MinARM32 error: Cannot use PC in Regs range.⟧ ;
  
  // Helpers to insert computed assembly constants.

  sort Constant | scheme Immediate(Computed) | scheme Reference(Computed) ;
  Immediate(#x) → ⟦#⟨Integer#x⟩⟧ ;
  Reference(#id) → ⟦&⟨Identifier#id⟩⟧ ;

  sort Mem | scheme FrameAccess(Computed) ;
  FrameAccess(#x)
    → FrameAccess1(#x, ⟦ [R12, ⟨Constant Immediate(#x)⟩] ⟧, ⟦ [R12, -⟨Constant Immediate(⟦0-#x⟧)⟩] ⟧) ;
  | scheme FrameAccess1(Computed, Mem, Mem) ;
  FrameAccess1(#x, #pos, #neg) → FrameAccess2(⟦ #x ≥ 0 ? #pos : #neg ⟧) ;
  | scheme FrameAccess2(Computed) ;
  FrameAccess2(#mem) → #mem ;

  sort Instruction | scheme AddConstant(Reg, Reg, Computed) ;
  AddConstant(#Rd, #Rn, #x)
    → AddConstant1(#x,
		   ⟦ ADD ⟨Reg#Rd⟩, ⟨Reg#Rn⟩, ⟨Constant Immediate(#x)⟩ ⟧,
		   ⟦ SUB ⟨Reg#Rd⟩, ⟨Reg#Rn⟩, ⟨Constant Immediate(⟦0-#x⟧)⟩ ⟧) ;
  | scheme AddConstant1(Computed, Instruction, Instruction) ;
  AddConstant1(#x, #pos, #neg) → AddConstant2(⟦ #x ≥ 0 ? #pos : #neg ⟧) ;
  | scheme AddConstant2(Computed) ;
  AddConstant2(#add) → #add ;
  

  ////////////////////////////////////////////////////////////////////////
  // 3. COMPILER FROM MiniC TO MinARM32
  ////////////////////////////////////////////////////////////////////////

  // HACS doesn't like to compile with Computed sort
  // unless there exists a scheme that can generate Computed

  sort Computed | scheme Dummy ;
  Dummy → ⟦ 0 ⟧;

  // MAIN SCHEME

  sort Instructions  |  scheme Compile(Program) ;
  Compile(#1) → P2(P1(#1), #1) ;

  // PASS 1

  // Result sort for first pass, with join operation.

  sort After1 | Data1(Instructions, FT) | scheme Join1(After1, After1) ;
  Join1(Data1(#1, #ft1), Data1(#2, #ft2))
    → Data1(⟦ { ⟨Instructions#1⟩ } ⟨Instructions#2⟩ ⟧, AppendFT(#ft1, #ft2)) ;

  // Function to return type environment (list of pairs with append).

  sort FT | NoFT | MoFT(Identifier, Type, FT) | scheme AppendFT(FT, FT) ;
  AppendFT(NoFT, #ft2) → #ft2 ;
  AppendFT(MoFT(#id1, #T1, #ft1), #ft2) → MoFT(#id1, #T1, AppendFT(#ft1, #ft2)) ;

  sort After1 | scheme P1(Program) ;
  P1(⟦⟨Declarations#Ds⟩⟧) → P1Ds(#Ds) ;

  sort After1 | scheme P1Ds(Declarations); 
  P1Ds(⟦⟨Declaration#D⟩ ⟨Declarations#Ds⟩⟧) → Join1(D(#D), P1Ds(#Ds)) ;
  P1Ds(⟦⟧) → Data1(⟦⟧, NoFT) ;

  
  sort After1 | scheme D(Declaration) ;
  D(⟦ function ⟨Type#T⟩ f ⟨ArgumentSignature#As⟩ { ⟨Statements#S⟩ } ⟧)
    → Data1(⟦⟧, MoFT(⟦f⟧, #T, NoFT)) ;

  // PASS 2

  sort Instructions | scheme P2(After1, Program) ;
  P2(Data1(#1, #ft1), #P) → P2Load(#1, #ft1, #P) ;

  // Type environment ($ρ$) is split in two components (by used sorts).

  attribute ↓ft{Identifier : Type} ;	// map from function name to return type
  attribute ↓vt{Identifier : Local} ;	// map from local variable name to type\&location
  sort Local | RegLocal(Type, Reg) | FrameLocal(Type, Computed) ;  // type\&location

  // Other inherited attributes.

  attribute ↓return(Label) ;		// label of return code
  attribute ↓true(Label) ;		// label to jump for true result
  attribute ↓false(Label) ;		// label to jump for false result
  attribute ↓value(Reg) ;		// register for expression result
  attribute ↓offset(Computed) ;		// frame offset for first unused local
  attribute ↓unused(Rs) ;		// list of unused registers
  
  sort Instructions | scheme P2Load(Instructions, FT, Program) ↓ft ↓vt ;
  P2Load(#is, MoFT(⟦f⟧, #T, #ft), #P) → P2Load(#is, #ft, #P) ↓ft{⟦f⟧ : #T} ;
  P2Load(#is, NoFT, #P) → ⟦ { ⟨Instructions#is⟩ } ⟨Instructions P(#P)⟩ ⟧ ;

  // Pass 2 recursion.

  sort Instructions | scheme P(Program) ↓ft ↓vt ;
  P(⟦ ⟨Declarations#Ds⟩ ⟧) → Ds(#Ds) ;

  sort Instructions | scheme Ds(Declarations) ↓ft ↓vt ;
  Ds(⟦ ⟨Declaration#D⟩ ⟨Declarations#Ds⟩ ⟧) → ⟦ { ⟨Instructions F(#D)⟩ } ⟨Instructions Ds(#Ds)⟩ ⟧ ;
  Ds(⟦⟧) → ⟦⟧ ;
  
  sort Instructions | scheme F(Declaration) ↓ft ↓vt ;
  F(⟦ function ⟨Type#T⟩ f ⟨ArgumentSignature#AS⟩ { ⟨Statements#S⟩ } ⟧) → ⟦
	f	STMFD SP!, {R4-R11,LR}
		MOV R12, SP
		{ ⟨Instructions AS(#AS, XRegs(⟦R0-R3⟧), #S) ↓return(⟦L⟧)⟩ }
	L	MOV SP, R12
		LDMFD SP!, {R4-R11,PC}
  ⟧ ;
  
  sort Instructions | scheme AS(ArgumentSignature, Rs, Statements) ↓ft ↓vt ↓return ;
  AS(⟦ () ⟧, #Rs, #S) → S(#S) ↓offset(⟦0-4⟧) ↓unused(XRegs(⟦R4-R11⟧)) ;
  AS(⟦ ( ⟨Type#T⟩ a ⟨TypeIdentifierTail#TIT⟩ ) ⟧, MoRs(#r, #Rs), #S)
    → AS2(#TIT, #Rs, #S) ↓vt{⟦a⟧ : RegLocal(#T, #r)} ;

  sort Instructions | scheme AS2(TypeIdentifierTail, Rs, Statements) ↓ft ↓vt ↓return ;
  AS2(⟦ ⟧, #Rs, #S) → S(#S) ↓offset(⟦0-4⟧) ↓unused(XRegs(⟦R4-R11⟧)) ;
  AS2(⟦ , ⟨Type#T⟩ a ⟨TypeIdentifierTail#TIT⟩ ⟧, MoRs(#r, #Rs), #S)
    → AS2(#TIT, #Rs, #S) ↓vt{⟦a⟧ : RegLocal(#T, #r)} ;
  AS2(⟦ , ⟨Type#T⟩ a ⟨TypeIdentifierTail#TIT⟩ ⟧, NoRs, #S)
    → error⟦More than four arguments to function not allowed.⟧ ;

  // helper functions for transfer access

  sort Instructions | scheme Str(Reg, Identifier) ; 
  Str(#reg, #id) → ⟦ STR ⟨ Reg#reg ⟩, [R12, ⟨ Constant Reference(#id) ⟩] ⟧ ;
  | scheme Ldr(Reg, Identifier) ; 
  Ldr(#reg, #id) → ⟦ LDR ⟨ Reg#reg ⟩, [R12, ⟨ Constant Reference(#id) ⟩] ⟧ ;

  // helper function for next register

  sort Reg | scheme NextArgReg(Reg) ;
  NextArgReg( ⟦ R0 ⟧ ) → ⟦ R1 ⟧ ;
  NextArgReg( ⟦ R1 ⟧ ) → ⟦ R2 ⟧ ;
  NextArgReg( ⟦ R2 ⟧ ) → ⟦ R3 ⟧ ;
  NextArgReg( ⟦ R3 ⟧ ) → ⟦ R4 ⟧ ;
  NextArgReg( ⟦ R4 ⟧ ) → error⟦MinARM32 error: register range exceeded!⟧ ;
  
  sort Reg | scheme NextReg(Reg) ;
  NextReg( ⟦ R4 ⟧ ) → ⟦ R5 ⟧ ;
  NextReg( ⟦ R5 ⟧ ) → ⟦ R6 ⟧ ;
  NextReg( ⟦ R6 ⟧ ) → ⟦ R7 ⟧ ;
  NextReg( ⟦ R7 ⟧ ) → ⟦ R8 ⟧ ;
  NextReg( ⟦ R8 ⟧ ) → ⟦ R9 ⟧ ;
  NextReg( ⟦ R9 ⟧ ) → ⟦ R10 ⟧ ;
  NextReg( ⟦ R10 ⟧ ) → ⟦ R11 ⟧ ;
  NextReg( ⟦ R11 ⟧ ) → error⟦MinARM32 error: register range exceeded!⟧ ;

  sort Reg | scheme Head(Rs) ;
  Head(MoRs(#Rn, #Rs)) → ⟦ R4 ⟧;
  Head(NoRs) → error⟦MinARM32 error: register range exceeded!⟧ ;
  sort Rs | scheme Tail(Rs) ;
  Head(MoRs(#Rn, #Rs)) → #Rs ;
  Tail(NoRs) → error⟦MinARM32 error: register range exceeded!⟧ ;

  // Problem with line 368:
  // Head(MoRs(#Rn, #Rs)) →  ⟦ R4 ⟧;
  // Originally it was written as: Head(MoRs(#Rn, #Rs)) → #Rn;
  // But this will cause a very strange bug - heisenbug
  // which means that around 50% - 60% chances, the output will not be correct
  // To be more specific, the register will not be parsed correctly
  // and the other 40% - 50% cases, the output becomes magically correct
  // and we did not change any code in this process
  // only sometimes delete the all the files in folder and re-generate Pr3.hx
  // sometimes just run hacs Pr3.hx again
  // and this bug magically disappears
  // we do not know why but just in case, we changed this to R4
  // which is perfectly fine because for Statements the registers start with R4
  // and our register allocation guarantee that there will be no conflict

  // attribute for next label after true/false

  attribute ↓next(Label) ;  

  sort Constant | scheme Immediate2(Local) ;
  Immediate2(FrameLocal(#T, #x)) → Immediate3(#x) ;
  | scheme Immediate3(Computed) ;
  Immediate3(#x) → ⟦#⟨ INT#x ⟩⟧ ;

  // handle Expression

  sort Instructions | scheme E(Expression) ↓vt ↓value ↓true ↓false ↓next ;
  E(⟦ ⟨ Integer#int ⟩ ⟧) ↓value(#reg) → 
    ⟦ MOV ⟨ Reg#reg ⟩, #⟨ Integer#int ⟩ ⟧ ;
  E(⟦ ⟨ String#str ⟩ ⟧) → 
    ⟦ DCS ⟨ String#str ⟩ ⟧ ; 
  E(⟦ ⟨ Identifier#id ⟩ ⟧) ↓vt{#id:#loc} ↓value(#reg) → 
    ⟦ ⟨ Instructions Ldr(#reg, #id) ⟩ ⟧ ;
  E(⟦ ⟨ Identifier#E ⟩ ( ⟨ ExpressionList#EL ⟩ ) ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → 
    FC(#E, #EL) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ;
  E(⟦ null ( ⟨ Type#T ⟩ ) ⟧) ↓value(#reg) → 
    ⟦ MOV ⟨ Reg#reg ⟩, #0 ⟧ ;
  E(⟦ sizeof ( ⟨ Type#T ⟩ ) ⟧) ↓value(#reg) → 
    ⟦ MOV ⟨ Reg#reg ⟩, #4 ⟧ ; // all have size of 4
  E(⟦ ! ⟨ Expression#E ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    MVN ⟨ Reg#reg ⟩, ⟨ Reg#reg ⟩
  ⟧ ;
  E(⟦ - ⟨ Expression#E ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    RSB ⟨ Reg#reg ⟩, ⟨ Reg#reg ⟩, #0
  ⟧ ;
  E(⟦ + ⟨ Expression#E ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
  ⟧ ;
  E(⟦ * ⟨ Expression#E ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    LDR ⟨ Reg#reg ⟩, [R12, ⟨ Reg#reg ⟩] 
  ⟧ ; // dereference
  E(⟦ & ⟨ Identifier#E ⟩ ⟧) ↓vt{#E:#loc} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦
    MOV ⟨ Reg#reg ⟩, ⟨ Constant Immediate2(#loc) ⟩
  ⟧ ; // address-of identifier
  E(⟦ & * ⟨ Expression#E ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
  ⟧ ; // address-of pointer
  E(⟦ ⟨ Expression#E1 ⟩ * ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    MUL ⟨ Reg#reg ⟩, ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ + ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    ADD ⟨ Reg#reg ⟩, ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ - ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    SUB ⟨ Reg#reg ⟩, ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ < ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    CMP ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
    BLT ⟨ Label#t ⟩
    MOV ⟨ Reg#reg ⟩, #0
    B ⟨ Label#f ⟩
  ⟨ Label#t ⟩  
    MOV ⟨ Reg#reg ⟩, #1
    B ⟨ Label#n ⟩
  ⟨ Label#f ⟩
  ⟨ Label#n ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ > ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    CMP ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
    BGT ⟨ Label#t ⟩
    MOV ⟨ Reg#reg ⟩, #0
    B ⟨ Label#f ⟩
  ⟨ Label#t ⟩ 
    MOV ⟨ Reg#reg ⟩, #1
    B ⟨ Label#n ⟩
  ⟨ Label#f ⟩
  ⟨ Label#n ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ <= ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    CMP ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
    BLE ⟨ Label#t ⟩
    MOV ⟨ Reg#reg ⟩, #0
    B ⟨ Label#f ⟩
  ⟨ Label#t ⟩  
    MOV ⟨ Reg#reg ⟩, #1
    B ⟨ Label#n ⟩
  ⟨ Label#f ⟩
  ⟨ Label#n ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ >= ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    CMP ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
    BGE ⟨ Label#t ⟩
    MOV ⟨ Reg#reg ⟩, #0
    B ⟨ Label#f ⟩
  ⟨ Label#t ⟩  
    MOV ⟨ Reg#reg ⟩, #1
    B ⟨ Label#n ⟩
  ⟨ Label#f ⟩
  ⟨ Label#n ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ == ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    CMP ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
    BEQ ⟨ Label#t ⟩
    MOV ⟨ Reg#reg ⟩, #0
    B ⟨ Label#f ⟩
  ⟨ Label#t ⟩  
    MOV ⟨ Reg#reg ⟩, #1
    B ⟨ Label#n ⟩
  ⟨ Label#f ⟩
  ⟨ Label#n ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ != ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    CMP ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
    BNE ⟨ Label#t ⟩
    MOV ⟨ Reg#reg ⟩, #0
    B ⟨ Label#f ⟩
  ⟨ Label#t ⟩  
    MOV ⟨ Reg#reg ⟩, #1
    B ⟨ Label#n ⟩
  ⟨ Label#f ⟩
  ⟨ Label#n ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ && ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    AND ⟨ Reg#reg ⟩, ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
  ⟧ ;
  E(⟦ ⟨ Expression#E1 ⟩ || ⟨ Expression#E2 ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(#reg)) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    ORR ⟨ Reg#reg ⟩, ⟨ Reg#reg ⟩, ⟨ Reg NextReg(#reg) ⟩
  ⟧ ;

  // handle FunctionCall

  sort Instructions | scheme FC(Identifier, ExpressionList) ↓vt ↓value ↓true ↓false ↓next ;
  FC(f, ⟦ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦
    B f
    MOV ⟨ Reg#reg ⟩, R0
  ⟧ ;
  FC(f, ⟦ ⟨ Expression#E2 ⟩ ⟨ ExpressionListTail#ELT ⟩ ⟧) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    MOV R0, ⟨ Reg#reg ⟩
    { ⟨ Instructions FC2(#ELT, NextArgReg(⟦ R0 ⟧)) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    BL f
    MOV ⟨ Reg#reg ⟩, R0
  ⟧ ;
  | scheme FC2(ExpressionListTail, Reg) ↓vt ↓value ↓true ↓false ↓next ;
  FC2(⟦ ⟧, #) → ⟦ ⟧ ;
  FC2(⟦ , ⟨ Expression#E ⟩ ⟨ ExpressionListTail#ELT ⟩ ⟧, #r) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
    MOV ⟨Reg #r⟩, ⟨Reg #reg⟩
    { ⟨ Instructions FC2(#ELT, NextArgReg(#r)) ↓vt{:#vt} ↓value(#reg) ↓true(#t) ↓false(#f) ↓next(#n) ⟩ }
  ⟧ ;

  // helper for DEF

  sort Addr | scheme Address(Computed) ;
  Address(#x) → ⟦⟨ INT#x ⟩⟧ ;

  // handle Statements

  sort Instructions | scheme S(Statements) ↓ft ↓vt ↓return ↓unused ↓offset ;
  S(⟦ ⟧) → ⟦ ⟧ ;
  S(⟦ var ⟨ Type#T ⟩ ⟨ Identifier#I ⟩ ; ⟨ Statements#S ⟩ ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦
    DEF ⟨ Identifier#I ⟩ = ⟨ Addr Address(#o) ⟩
    { ⟨ Instructions S(#S) ↓ft{:#ft} ↓vt{:#vt} ↓vt{#I:FrameLocal(#T, #o)} ↓return(#r) ↓unused(#Rs) ↓offset(⟦ #o - 4 ⟧) ⟩ }
  ⟧ ; // 4 bytes each
  S(⟦ ⟨ Statement#Stmt ⟩ ⟨ Statements#S ⟩ ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦
    { ⟨ Instructions STMT(#Stmt) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ⟩ }
    { ⟨ Instructions S(#S) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ⟩ }
  ⟧ ;

  // handle Statement

  sort Instructions | scheme STMT(Statement) ↓ft ↓vt ↓return ↓unused ↓offset ;
  STMT(⟦ { ⟨ Statements#S ⟩ } ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) →
    S(#S) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o)
  ;
  STMT(⟦ ⟨ Identifier#E1 ⟩ = ⟨ Expression#E2 ⟩ ; ⟧) ↓ft{:#ft} ↓vt{#E1:#loc} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦
    { ⟨ Instructions E(#E2) ↓vt{#E1:#loc} ↓value(Head(#Rs)) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    { ⟨ Instructions Str(Head(#Rs), #E1) ⟩ }
  ⟧ ; // identifier assignment
  STMT(⟦ * ⟨ Expression#E1 ⟩ = ⟨ Expression#E2 ⟩ ; ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(Head(#Rs)) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(Head(#Rs))) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    LDR ⟨ Reg NextReg(NextReg(Head(#Rs))) ⟩, [R12, ⟨ Reg Head(#Rs) ⟩]
    MOV ⟨ Reg NextReg(NextReg(Head(#Rs))) ⟩, ⟨ Reg NextReg(Head(#Rs)) ⟩
    STR ⟨ Reg NextReg(NextReg(Head(#Rs))) ⟩, [R12, ⟨ Reg Head(#Rs) ⟩]
  ⟧ ; // pointer assignment
  STMT(⟦ & ⟨ Identifier#E1 ⟩ = ⟨ Expression#E2 ⟩ ; ⟧) ↓ft{:#ft} ↓vt{#E1:#loc} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦
    { ⟨ Instructions E(#E2) ↓vt{#E1:#loc} ↓value(Head(#Rs)) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    { ⟨ Instructions Str(Head(#Rs), #E1) ⟩ }
  ⟧ ; // reference assignment
  STMT(⟦ & * ⟨ Expression#E1 ⟩ = ⟨ Expression#E2 ⟩ ; ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦
    { ⟨ Instructions E(#E1) ↓vt{:#vt} ↓value(Head(#Rs)) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    { ⟨ Instructions E(#E2) ↓vt{:#vt} ↓value(NextReg(Head(#Rs))) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    LDR ⟨ Reg NextReg(NextReg(Head(#Rs))) ⟩, [R12, ⟨ Reg Head(#Rs) ⟩]
    MOV ⟨ Reg NextReg(NextReg(Head(#Rs))) ⟩, ⟨ Reg NextReg(Head(#Rs)) ⟩
    STR ⟨ Reg NextReg(NextReg(Head(#Rs))) ⟩, [R12, ⟨ Reg Head(#Rs) ⟩]
  ⟧ ; // reference-pointer assignment
  STMT(⟦ if ( ⟨ Expression#E ⟩ ) ⟨ IfTail#IT ⟩ ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦ 
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(Head(#Rs)) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    CMP ⟨ Reg Head(#Rs) ⟩, #0
    BNE IF_TRUE
    B IF_FALSE
    { ⟨ Instructions IT(#IT) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ↓true(⟦ IF_TRUE ⟧) ↓false(⟦ IF_FALSE ⟧) ↓next(⟦ IF_NEXT ⟧) ⟩ }
  ⟧ ;
  STMT(⟦ while ( ⟨ Expression#E ⟩ ) ⟨ Statement#Stmt ⟩ ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦ 
  WHILE_REPEAT
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(Head(#Rs)) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    CMP ⟨ Reg Head(#Rs) ⟩, #0
    BEQ WHILE_NEXT
    { ⟨ Instructions STMT(#Stmt) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ⟩ }
    B WHILE_REPEAT
  WHILE_NEXT
  ⟧ ;
  STMT(⟦ return ⟨ Expression#E ⟩ ; ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) → ⟦ 
    { ⟨ Instructions E(#E) ↓vt{:#vt} ↓value(Head(#Rs)) ↓true(⟦ E_TRUE ⟧) ↓false(⟦ E_FALSE ⟧) ↓next(⟦ E_NEXT ⟧) ⟩ }
    MOV R0, ⟨ Reg Head(#Rs) ⟩
    B ⟨ Label#r ⟩
  ⟧ ;

  // handle IfTail

  sort Instructions | scheme IT(IfTail) ↓ft ↓vt ↓return ↓unused ↓offset ↓true ↓false ↓next ;
  IT(⟦ ⟨ Statement#S1 ⟩ else ⟨ Statement#S2 ⟩ ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
  ⟨ Label#t ⟩
    { ⟨ Instructions STMT(#S1) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ⟩ }
  B ⟨ Label#n ⟩
  ⟨ Label#f ⟩
    { ⟨ Instructions STMT(#S2) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ⟩ }
  ⟨ Label#n ⟩
  ⟧ ;
  IT(⟦ ⟨ Statement#S ⟩ ⟧) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ↓true(#t) ↓false(#f) ↓next(#n) → ⟦ 
  ⟨ Label#t ⟩
    { ⟨ Instructions STMT(#S) ↓ft{:#ft} ↓vt{:#vt} ↓return(#r) ↓unused(#Rs) ↓offset(#o) ⟩ }
  B ⟨ Label#n ⟩
  ⟨ Label#f ⟩
  ⟨ Label#n ⟩
  ⟧ ;

}

