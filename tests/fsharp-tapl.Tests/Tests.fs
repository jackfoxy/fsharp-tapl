
namespace FsharpTapl.Tests.Regression

open NUnit.Framework

type InputSource =
    | File
    | Console of string

module Tests =

    let regression name inputSource (expected : string list) =

        let exe =  sprintf @"..\..\..\..\bin\Debug\%s" name

        let args =
            match inputSource with
            | File ->
                sprintf  @"-i ..\..\..\..\src\%s\test.f" name
            | Console consoleInput ->
                sprintf "-s \"%s\"" consoleInput

        let (outputs, errors) = 
            RunProcess.runProc exe args None

        Assert.AreEqual((Seq.length errors), 0)

        let output =
            outputs
            |> Seq.filter (fun t -> t |> (isNull >> not))
            |> List.ofSeq

        Assert.AreEqual(expected, output)

    [<Test>]
    let ``arith file`` () =

        regression "arith" File ["true"; "false"; "0"; "1"; "false"]

    [<Test>]
    let ``arith string`` () =

        regression "arith" (Console "iszero (pred (succ 0));") ["true"]

    [<Test>]
    let ``bot file`` () =

        regression "bot" File ["(lambda x:Top. x) : Top -> Top";
                            "(lambda x:Top. x) : Top";
                            "(lambda x:Top. x) : Top -> Top";
                            "(lambda x:Bot. x) : Bot -> Bot";
                            "(lambda x:Bot. x x) : Bot -> Bot";]

    [<Test>]
    let ``bot string`` () =

        regression "bot" (Console "(lambda x:Bot->Top. x) (lambda x:Top. x);") ["(lambda x:Top. x) : Bot -> Top";]

    [<Test>]
    let ``equirec file`` () =

        regression "equirec" File ["(lambda x:A. x) : A -> A";
                                "(lambda f:Rec X. A->A. lambda x:A. f x) : (Rec X. A->A) -> A -> A";]

    [<Test>]
    let ``equirec string`` () =

        regression "equirec" (Console "lambda x:A->B. x;") ["(lambda x:A->B. x) : (A->B) -> A -> B";]

    [<Test>]
    let ``fomega file`` () =

        regression "fomega" File ["(lambda X. lambda x:X. x) : All X. X -> X";
                            "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";]

    [<Test>]
    let ``fomega string`` () =

        regression "fomega" (Console "lambda X. lambda x:X->X. x;") ["(lambda X. lambda x:X->X. x) : All X. (X->X) -> X -> X";]

    [<Test>]
    let ``fomsub file`` () =

        regression "fomsub" File ["(lambda X. lambda x:X. x) : All X. X -> X";
                            "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                            "(lambda x:Top. x) : Top -> Top";
                            "(lambda x:Top. x) : Top";
                            "(lambda x:Top. x) : Top -> Top";
                            "(lambda X<:Top->Top. lambda x:X. x x) : All X<:Top->Top. X -> Top";]

    [<Test>]
    let ``fomsub string`` () =

        regression "fomsub" (Console "(lambda x:Top->Top. x x) (lambda x:Top. x);") ["(lambda x:Top. x) : Top";]

    [<Test>]
    let ``fullequirec file`` () =

        regression "fullequirec" File ["\"hello\" : String";
                                    "(lambda x:A. x) : A -> A";
                                    "6.28318 : Float";
                                    "(lambda x:Bool. x) : Bool -> Bool";
                                    "true : Bool";
                                    "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                    "3 : Nat";
                                    "T :: *";
                                    "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";
                                    "(lambda f:Rec X. A->A. lambda x:A. f x) : (Rec X. A->A) -> A -> A";
                                    "{x=true, y=false} : {x:Bool, y:Bool}";
                                    "true : Bool";
                                    "{true, false} : {Bool, Bool}";
                                    "true : Bool";
                                    "(lambda x:<a:Bool,b:Bool>. x)";
                                    "  : <a:Bool,b:Bool> -> <a:Bool, b:Bool>";
                                    "Counter :: *";
                                    "p : {get:Nat, inc:Unit->Counter}";
                                    "p1 : Counter";
                                    "1 : Nat";
                                    "get : Counter -> Nat";
                                    "inc : Counter -> Unit -> (Rec P. {get:Nat, inc:Unit->P})";
                                    "Hungry :: *";
                                    "f0 : Nat -> Nat -> Hungry";
                                    "f1 : Nat -> Hungry";
                                    "f2 : Hungry";
                                    "T :: *";
                                    "fix_T : (T->T) -> T";
                                    "D :: *";
                                    "fix_D : (D->D) -> D";
                                    "diverge_D : Unit -> D";
                                    "lam : (D->D) -> D -> D";
                                    "ap : D -> D -> (Rec X. X -> X)";
                                    "myfix : D -> D";
                                    "true : Bool";
                                    "unit : Unit";
                                    "NatList :: *";
                                    "nil : NatList";
                                    "cons : Nat -> NatList -> NatList";
                                    "isnil : NatList -> Bool";
                                    "hd : NatList -> Nat";
                                    "tl : NatList -> NatList";
                                    "plus : Nat -> Nat -> Nat";
                                    "sumlist : NatList -> Nat";
                                    "mylist : NatList";
                                    "10 : Nat";]

    [<Test>]
    let ``fullequirec string`` () =

        regression "fullequirec" (Console "(lambda x:Nat. pred (succ (succ x))) (pred (succ 0));") ["1 : Nat";]

    [<Test>]
    let ``fullerror file`` () =

        regression "fullerror" File ["(lambda x:Bot. x) : Bot -> Bot";
                                "(lambda x:Bot. x x) : Bot -> Bot";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "error : Bool";
                                "error : Bot";
                                "error : Bool";]

    [<Test>]
    let ``fullerror string`` () =

        regression "fullerror" (Console "(lambda x:Bool. x) (if error then true else false);") ["error : Bool";]

    [<Test>]
    let ``fullfomsub file`` () =

        regression "fullfomsub" File ["(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda z:Top. z) : Top";
                                "\"hello\" : String";
                                "unit : Unit";
                                "(lambda x:A. x) : A -> A";
                                "true : Bool";
                                "{x=true, y=false} : {x:Bool, y:Bool}";
                                "true : Bool";
                                "{true, false} : {Bool, Bool}";
                                "true : Bool";
                                "{x=true, y=false, a=false} : {x:Top, y:Bool}";
                                "6.28318 : Float";
                                "(lambda X. lambda x:X. x) : All X. X -> X";
                                "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                                "(lambda X<:Top->Top. lambda x:X. x x) : All X<:Top->Top. X -> Top";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";
                                "T :: *";
                                "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";
                                "{*All Y. Y, lambda x:All Y. Y.x} as {Some X, X->X}";
                                "  : {Some X, X->X}";
                                "{*Nat, {c=0,f=lambda x:Nat. (succ x)}} as {Some X, {c:X,f:X->Nat}}";
                                "  : {Some X, {c:X,f:X->Nat}}";
                                "1 : Nat";
                                "Pair :: * => * => *";
                                "pair : All X. All Y. X -> Y -> (All R. (X->Y->R) -> R)";
                                "f : All X. All Y. Pair X Y -> Pair X Y";
                                "fst : All X. All Y. Pair X Y -> X";
                                "snd : All X. All Y. Pair X Y -> Y";
                                "pr : All R. (Nat->Bool->R) -> R";
                                "0 : Nat";
                                "false : Bool";
                                "List :: * => *";
                                "diverge : All X. Unit -> X";
                                "nil : All X. List X";
                                "cons : All X. X -> List X -> List X";
                                "isnil : All X. List X -> Bool";
                                "head : All X. List X -> X";
                                "tail : All X. List X -> List X";]

    [<Test>]
    let ``fullfomsub string`` () =

        regression "fullfomsub" (Console "if true then {x=true,y=false,a=false} else {y=false,x={},b=false};") ["{x=true, y=false, a=false} : {x:Top, y:Bool}";]

    [<Test>]
    let ``fullfomsubref file`` () =

        regression "fullfomsubref" File ["(lambda x:Bot. x) : Bot -> Bot";
                                    "(lambda x:Bot. x x) : Bot -> Bot";
                                    "(lambda x:<a:Bool,b:Bool>. x)";
                                    "  : <a:Bool,b:Bool> -> <a:Bool, b:Bool>";
                                    "(lambda x:Top. x) : Top -> Top";
                                    "(lambda x:Top. x) : Top";
                                    "(lambda x:Top. x) : Top -> Top";
                                    "(lambda z:Top. z) : Top";
                                    "\"hello\" : String";
                                    "unit : Unit";
                                    "(lambda x:A. x) : A -> A";
                                    "true : Bool";
                                    "{x=true, y=false} : {x:Bool, y:Bool}";
                                    "true : Bool";
                                    "{true, false} : {Bool, Bool}";
                                    "true : Bool";
                                    "{x=true, y=false, a=false} : {x:Top, y:Bool}";
                                    "6.28318 : Float";
                                    "(lambda X. lambda x:X. x) : All X. X -> X";
                                    "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                                    "(lambda X<:Top->Top. lambda x:X. x x) : All X<:Top->Top. X -> Top";
                                    "(lambda x:Bool. x) : Bool -> Bool";
                                    "true : Bool";
                                    "error : Bool";
                                    "error : Bot";
                                    "error : Bool";
                                    "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                    "3 : Nat";
                                    "T :: *";
                                    "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";
                                    "CounterRep :: *";
                                    "SetCounter :: *";
                                    "setCounterClass : CounterRep ->";
                                    "                  (Unit->SetCounter) -> Unit -> SetCounter";
                                    "newSetCounter : Unit -> SetCounter";
                                    "c : SetCounter";
                                    "1 : Nat";
                                    "InstrCounter :: *";
                                    "InstrCounterRep :: *";
                                    "instrCounterClass : InstrCounterRep ->";
                                    "                    (Unit->InstrCounter) -> Unit -> InstrCounter";
                                    "newInstrCounter : Unit -> InstrCounter";
                                    "ic : InstrCounter";
                                    "1 : Nat";
                                    "0 : Nat";
                                    "unit : Unit";
                                    "2 : Nat";
                                    "1 : Nat";
                                    "instrCounterClass : InstrCounterRep ->";
                                    "                    (Unit->InstrCounter) -> Unit -> InstrCounter";
                                    "ResetInstrCounter :: *";
                                    "resetInstrCounterClass : InstrCounterRep ->";
                                    "                         (Unit->ResetInstrCounter) ->";
                                    "                         Unit -> ResetInstrCounter";
                                    "BackupInstrCounter :: *";
                                    "BackupInstrCounterRep :: *";
                                    "backupInstrCounterClass : BackupInstrCounterRep ->";
                                    "                          (Unit->BackupInstrCounter) ->";
                                    "                          Unit -> BackupInstrCounter";
                                    "newBackupInstrCounter : Unit -> BackupInstrCounter";
                                    "ic : BackupInstrCounter";
                                    "2 : Nat";
                                    "2 : Nat";
                                    "3 : Nat";
                                    "2 : Nat";
                                    "8 : Nat";
                                    "Counter :: *";
                                    "inc3 : Counter -> Unit";
                                    "SetCounter :: *";
                                    "InstrCounter :: *";
                                    "CounterRep :: *";
                                    "InstrCounterRep :: *";
                                    "dummySetCounter : SetCounter";
                                    "dummyInstrCounter : InstrCounter";
                                    "setCounterClass : CounterRep -> (Source SetCounter) -> SetCounter";
                                    "newSetCounter : Unit -> SetCounter";
                                    "instrCounterClass : InstrCounterRep ->";
                                    "                    (Source InstrCounter) -> InstrCounter";
                                    "newInstrCounter : Unit -> InstrCounter";
                                    "c : InstrCounter";
                                    "4 : Nat";
                                    "54 : Nat";
                                    "4 : Nat";]

    [<Test>]
    let ``fullfomsubref string`` () =

        regression "fullfomsubref" (Console "if true then {x=true,y=false,a=false} else {y=false,x={},b=false};") ["{x=true, y=false, a=false} : {x:Top, y:Bool}";]

    [<Test>]
    let ``fullisorec file`` () =

        regression "fullisorec" File ["\"hello\" : String";
                                "unit : Unit";
                                "(lambda x:A. x) : A -> A";
                                "true : Bool";
                                "6.28318 : Float";
                                "{x=true, y=false} : {x:Bool, y:Bool}";
                                "true : Bool";
                                "{true, false} : {Bool, Bool}";
                                "true : Bool";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";
                                "(lambda x:<a:Bool,b:Bool>. x)";
                                "  : <a:Bool,b:Bool> -> <a:Bool, b:Bool>";
                                "Counter :: *";
                                "p : Counter";
                                "p1 : Counter";
                                "1 : Nat";
                                "T :: *";
                                "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";]

    [<Test>]
    let ``fullisorec string`` () =

        regression "fullisorec" (Console "(lambda x:Nat. pred(succ (succ x))) (succ 0);") ["2 : Nat";]

    [<Test>]
    let ``fullomega file`` () =

        regression "fullomega" File ["\"hello\" : String";
                                "unit : Unit";
                                "(lambda x:A. x) : A -> A";
                                "true : Bool";
                                "6.28318 : Float";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";
                                "T :: *";
                                "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";
                                "(lambda X. lambda x:X. x) : All X. X -> X";
                                "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                                "{*All Y. Y, lambda x:All Y. Y.x} as {Some X, X->X}";
                                "  : {Some X, X->X}";
                                "{x=true, y=false} : {x:Bool, y:Bool}";
                                "true : Bool";
                                "{true, false} : {Bool, Bool}";
                                "true : Bool";
                                "{*Nat, {c=0,f=lambda x:Nat. (succ x)}} as {Some X, {c:X,f:X->Nat}}";
                                "  : {Some X, {c:X,f:X->Nat}}";
                                "1 : Nat";
                                "Pair :: * => * => *";
                                "pair : All X. All Y. X -> Y -> (All R. (X->Y->R) -> R)";
                                "f : All X. All Y. Pair X Y -> Pair X Y";
                                "fst : All X. All Y. Pair X Y -> X";
                                "snd : All X. All Y. Pair X Y -> Y";
                                "pr : All R. (Nat->Bool->R) -> R";
                                "0 : Nat";
                                "false : Bool";
                                "List :: * => *";
                                "diverge : All X. Unit -> X";
                                "nil : All X. List X";
                                "cons : All X. X -> List X -> List X";
                                "isnil : All X. List X -> Bool";
                                "head : All X. List X -> X";
                                "tail : All X. List X -> List X";]

    [<Test>]
    let ``fullomega string`` () =

        regression "fullomega" (Console "List = lambda X. All R. (X->R->R) -> R -> R;") ["List :: * => *";]

    [<Test>]
    let ``fullpoly file`` () =

        regression "fullpoly" File ["\"hello\" : String";
                                "unit : Unit";
                                "(lambda x:A. x) : A -> A";
                                "true : Bool";
                                "6.28318 : Float";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";
                                "T :: *";
                                "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";
                                "(lambda X. lambda x:X. x) : All X. X -> X";
                                "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                                "{*All Y. Y, lambda x:All Y. Y.x} as {Some X, X->X}";
                                "  : {Some X, X->X}";
                                "{x=true, y=false} : {x:Bool, y:Bool}";
                                "true : Bool";
                                "{true, false} : {Bool, Bool}";
                                "true : Bool";
                                "{*Nat, {c=0,f=lambda x:Nat. (succ x)}} as {Some X, {c:X,f:X->Nat}}";
                                "  : {Some X, {c:X,f:X->Nat}}";
                                "1 : Nat";]

    [<Test>]
    let ``fullpoly string`` () =

        regression "fullpoly" (Console "T = Nat->Nat;") ["T :: *";]

    [<Test>]
    let ``fullrecon file`` () =

        regression "fullrecon" File ["true : Bool";
                                "       (lambda x:Bool. x) : Bool -> Bool";
                                "                     true : Bool";
                                "       (lambda x:Nat. (succ x)) : Nat -> Nat";
                                "                           3 : Nat";
                                "    (lambda x:A. x) : A -> A";
                                "                  (lambda x:X. lambda y:X->X. y x) : X -> (X->X) -> X";
                                "                                   0 : Nat";
                                "    (lambda x. x 0) : (Nat->?X7) -> ?X7";
                                "                  0 : Nat";
                                "    1 : Nat";
                                "    ";]

    [<Test>]
    let ``fullrecon string`` () =

        regression "fullrecon" (Console "lambda X. lambda x:X. x;") ["(lambda X. lambda x:X. x) : All X. X -> X";]

    [<Test>]
    let ``fullref file`` () =

        regression "fullref" File ["(lambda x:Bot. x) : Bot -> Bot";
                                "(lambda x:Bot. x x) : Bot -> Bot";
                                "(lambda x:<a:Bool,b:Bool>. x)";
                                "  : <a:Bool,b:Bool> -> <a:Bool, b:Bool>";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda z:Top. z) : Top";
                                "\"hello\" : String";
                                "unit : Unit";
                                "(lambda x:A. x) : A -> A";
                                "true : Bool";
                                "{x=true, y=false} : {x:Bool, y:Bool}";
                                "true : Bool";
                                "{true, false} : {Bool, Bool}";
                                "true : Bool";
                                "{x=true, y=false, a=false} : {x:Top, y:Bool}";
                                "6.28318 : Float";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";
                                "T :: *";
                                "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";]

    [<Test>]
    let ``fullref string`` () =

        regression "fullref" (Console "(lambda x:Top->Top. x) (lambda x:Top. x);") ["(lambda x:Top. x) : Top -> Top";]

    [<Test>]
    let ``fullsimple file`` () =

        regression "fullsimple" File ["(lambda x:<a:Bool,b:Bool>. x)";
                                "  : <a:Bool,b:Bool> -> <a:Bool, b:Bool>";
                                "\"hello\" : String";
                                "unit : Unit";
                                "(lambda x:A. x) : A -> A";
                                "true : Bool";
                                "6.28318 : Float";
                                "{x=true, y=false} : {x:Bool, y:Bool}";
                                "true : Bool";
                                "{true, false} : {Bool, Bool}";
                                "true : Bool";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";
                                "T :: *";
                                "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";]

    [<Test>]
    let ``fullsimple string`` () =

        regression "fullsimple" (Console "(lambda x:Nat. pred(succ (succ x))) (succ 0);") ["2 : Nat";]

    [<Test>]
    let ``fullsub file`` () =

        regression "fullsub" File ["(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda z:Top. z) : Top";
                                "\"hello\" : String";
                                "unit : Unit";
                                "(lambda x:A. x) : A -> A";
                                "true : Bool";
                                "{x=true, y=false} : {x:Bool, y:Bool}";
                                "true : Bool";
                                "{true, false} : {Bool, Bool}";
                                "true : Bool";
                                "{x=true, y=false, a=false} : {x:Top, y:Bool}";
                                "6.28318 : Float";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";
                                "T :: *";
                                "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";]

    [<Test>]
    let ``fullsub string`` () =

        regression "fullsub" (Console "(lambda x:Nat. pred (succ (succ x))) (succ 0);") ["2 : Nat";]

    [<Test>]
    let ``fulluntyped file`` () =

        regression "fulluntyped" File ["true";
                                    "false";
                                    "x ";
                                    "x";
                                    "x = true";
                                    "true";
                                    "false";
                                    "(lambda x'. x')";
                                    "(lambda x'. x' x')";
                                    "{x=lambda x'.x', y=lambda x'.x'}";
                                    "(lambda x'. x')";
                                    "\"hello\"";
                                    "120.";
                                    "0";
                                    "1";
                                    "false";
                                    "true";]

    [<Test>]
    let ``fulluntyped string`` () =

        regression "fulluntyped" (Console "iszero (pred (succ (succ (succ 0))));") ["false";]

    [<Test>]
    let ``fullupdate file`` () =

        regression "fullupdate" File ["(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda z:Top. z) : Top";
                                "\"hello\" : String";
                                "unit : Unit";
                                "(lambda x:A. x) : A -> A";
                                "true : Bool";
                                "{x=true, y=false} : {x:Bool, y:Bool}";
                                "true : Bool";
                                "{true, false} : {Bool, Bool}";
                                "true : Bool";
                                "{x=true, y=false, a=false} : {x:Top, y:Bool}";
                                "6.28318 : Float";
                                "(lambda X. lambda x:X. x) : All X. X -> X";
                                "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                                "(lambda X<:Top->Top. lambda x:X. x x) : All X<:Top->Top. X -> Top";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";
                                "T :: *";
                                "(lambda f:T. lambda x:Nat. f (f x)) : T -> Nat -> Nat";
                                "{*All Y. Y, lambda x:All Y. Y.x} as {Some X, X->X}";
                                "  : {Some X, X->X}";
                                "{*Nat, {c=0,f=lambda x:Nat. (succ x)}} as {Some X, {c:X,f:X->Nat}}";
                                "  : {Some X, {c:X,f:X->Nat}}";
                                "1 : Nat";
                                "Pair :: * => * => *";
                                "pair : All X. All Y. X -> Y -> (All R. (X->Y->R) -> R)";
                                "f : All X. All Y. Pair X Y -> Pair X Y";
                                "fst : All X. All Y. Pair X Y -> X";
                                "snd : All X. All Y. Pair X Y -> Y";
                                "pr : All R. (Nat->Bool->R) -> R";
                                "0 : Nat";
                                "false : Bool";
                                "List :: * => *";
                                "diverge : All X. Unit -> X";
                                "nil : All X. List X";
                                "cons : All X. X -> List X -> List X";
                                "isnil : All X. List X -> Bool";
                                "head : All X. List X -> X";
                                "tail : All X. List X -> List X";]
                
    [<Test>]
    let ``fullupdate string`` () =

        regression "fullupdate" (Console "(lambda X. lambda x:X. x) [All X.X->X];") ["(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";]
                         
//    [<Test>]
//    let ``joinexercise file`` () =
//
//        regression "joinexercise" []
//
//    [<Test>]
//    let ``letexercise file`` () =
//
//        regression "letexercise" []

    [<Test>]
    let ``purefsub file`` () =

        regression "purefsub" File ["(lambda X. lambda x:X. x) : All X. X -> X";
                                "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda X<:Top->Top. lambda x:X. x x) : All X<:Top->Top. X -> Top";]

    [<Test>]
    let ``purefsub string`` () =

        regression "purefsub" (Console "lambda X<:Top->Top. lambda x:X. x x;") ["(lambda X<:Top->Top. lambda x:X. x x) : All X<:Top->Top. X -> Top";]

    [<Test>]
    let ``rcdsubbot file`` () =

        regression "rcdsubbot" File ["(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda z:Top. z) : Top";
                                "(lambda x:Bot. x) : Bot -> Bot";
                                "(lambda x:Bot. x x) : Bot -> Bot";]

    [<Test>]
    let ``rcdsubbot string`` () =

        regression "rcdsubbot" (Console "(lambda x:Top->Top. x) (lambda x:Top. x);") ["(lambda x:Top. x) : Top -> Top";]

    [<Test>]
    let ``recon file`` () =

        regression "recon" File ["(lambda x:Bool. x) : Bool -> Bool";
                            "                     true : Bool";
                            "       (lambda x:Nat. (succ x)) : Nat -> Nat";
                            "                           3 : Nat";
                            "    (lambda x:A. x) : A -> A";
                            "                  (lambda x:X. lambda y:X->X. y x) : X -> (X->X) -> X";
                            "                                   0 : Nat";
                            "    "]

    [<Test>]
    let ``recon string`` () =

        regression "recon" (Console "(lambda x:Nat. pred (succ (succ x))) (succ 0);") ["2 : Nat";]

    [<Test>]
    let ``reconbase file`` () =

        regression "reconbase" File ["(lambda x:A. x) : A -> A";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";]

    [<Test>]
    let ``reconbase string`` () =

        regression "reconbase" (Console "(lambda x:Nat. pred (succ (succ x))) (succ 0);") ["2 : Nat";]

    [<Test>]
    let ``simplebool file`` () =

        regression "simplebool" File ["(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";]

    [<Test>]
    let ``simplebool string`` () =

        regression "simplebool" (Console "(lambda x:Bool. if x then false else true);") ["(lambda x:Bool. if x then false else true) : Bool -> Bool";]

//    [<Test>]
//    let ``tyarith file`` () =
//
//        regression "tyarith" []
//
//    [<Test>]
//    let ``tyarith string`` () =
//
//        regression "tyarith" (Console "") ["";]

    [<Test>]
    let ``untyped file`` () =

        regression "untyped" File ["x ";
                                "x";
                                "(lambda x'. x')";
                                "(lambda x'. x' x')";]

    [<Test>]
    let ``untyped string`` () =

        regression "untyped" (Console "(lambda x. x) (lambda x. x x);") ["(lambda x. x x)";]