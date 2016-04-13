
namespace FsharpTapl.Tests.Regression

//open fsharp-tapl

open NUnit.Framework

module Tests =

    let regression name (expected : string list) =

        let exe =  sprintf @"E:\BitSync\GitRepos\fsharp-tapl\bin\Debug\%s" name
        let testFile = sprintf  @"-i E:\BitSync\GitRepos\fsharp-tapl\src\%s\test.f" name

        let (outputs, errors) = 
            RunProcess.runProc exe testFile None

        Assert.AreEqual((Seq.length errors), 0)

        let output =
            outputs
            |> Seq.filter (fun t -> t |> (isNull >> not))
            |> List.ofSeq

        Assert.AreEqual(expected, output)

    [<Test>]
    let ``arith`` () =

        regression "arith" ["true"; "false"; "0"; "1"; "false"]

    [<Test>]
    let ``bot`` () =

        regression "bot" ["(lambda x:Top. x) : Top -> Top";
                            "(lambda x:Top. x) : Top";
                            "(lambda x:Top. x) : Top -> Top";
                            "(lambda x:Bot. x) : Bot -> Bot";
                            "(lambda x:Bot. x x) : Bot -> Bot";]

    [<Test>]
    let ``equirec`` () =

        regression "equirec" ["(lambda x:A. x) : A -> A";
                                "(lambda f:Rec X. A->A. lambda x:A. f x) : (Rec X. A->A) -> A -> A";]

    [<Test>]
    let ``fomega`` () =

        regression "fomega" ["(lambda X. lambda x:X. x) : All X. X -> X";
                            "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";]

    [<Test>]
    let ``fomsub`` () =

        regression "fomsub" ["(lambda X. lambda x:X. x) : All X. X -> X";
                            "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                            "(lambda x:Top. x) : Top -> Top";
                            "(lambda x:Top. x) : Top";
                            "(lambda x:Top. x) : Top -> Top";
                            "(lambda X<:Top->Top. lambda x:X. x x) : All X<:Top->Top. X -> Top";]

    [<Test>]
    let ``fullequirec`` () =

        regression "fullequirec" ["\"hello\" : String";
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
    let ``fullerror`` () =

        regression "fullerror" ["(lambda x:Bot. x) : Bot -> Bot";
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
    let ``fullfomsub`` () =

        regression "fullfomsub" ["(lambda x:Top. x) : Top -> Top";
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
    let ``fullfomsubref`` () =

        regression "fullfomsubref" ["(lambda x:Bot. x) : Bot -> Bot";
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
    let ``fullisorec`` () =

        regression "fullisorec" ["\"hello\" : String";
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
    let ``fullomega`` () =

        regression "fullomega" ["\"hello\" : String";
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
    let ``fullpoly`` () =

        regression "fullpoly" ["\"hello\" : String";
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
    let ``fullrecon`` () =

        regression "fullrecon" ["true : Bool";
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
    let ``fullref`` () =

        regression "fullref" ["(lambda x:Bot. x) : Bot -> Bot";
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
    let ``fullsimple`` () =

        regression "fullsimple" ["(lambda x:<a:Bool,b:Bool>. x)";
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
    let ``fullsub`` () =

        regression "fullsub" ["(lambda x:Top. x) : Top -> Top";
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
    let ``fulluntyped`` () =

        regression "fulluntyped" ["true";
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
    let ``fullupdate`` () =

        regression "fullupdate" ["(lambda x:Top. x) : Top -> Top";
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

//    [<Test>]
//    let ``joinexercise`` () =
//
//        regression "joinexercise" []
//
//    [<Test>]
//    let ``letexercise`` () =
//
//        regression "letexercise" []

    [<Test>]
    let ``purefsub`` () =

        regression "purefsub" ["(lambda X. lambda x:X. x) : All X. X -> X";
                                "(lambda x:All X. X->X. x) : (All X. X->X) -> (All X. X -> X)";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda X<:Top->Top. lambda x:X. x x) : All X<:Top->Top. X -> Top";]

    [<Test>]
    let ``rcdsubbot`` () =

        regression "rcdsubbot" ["(lambda x:Top. x) : Top -> Top";
                                "(lambda x:Top. x) : Top";
                                "(lambda x:Top. x) : Top -> Top";
                                "(lambda z:Top. z) : Top";
                                "(lambda x:Bot. x) : Bot -> Bot";
                                "(lambda x:Bot. x x) : Bot -> Bot";]

    [<Test>]
    let ``recon`` () =

        regression "recon" ["(lambda x:Bool. x) : Bool -> Bool";
                            "                     true : Bool";
                            "       (lambda x:Nat. (succ x)) : Nat -> Nat";
                            "                           3 : Nat";
                            "    (lambda x:A. x) : A -> A";
                            "                  (lambda x:X. lambda y:X->X. y x) : X -> (X->X) -> X";
                            "                                   0 : Nat";
                            "    "]

    [<Test>]
    let ``reconbase`` () =

        regression "reconbase" ["(lambda x:A. x) : A -> A";
                                "(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";
                                "(lambda x:Nat. (succ x)) : Nat -> Nat";
                                "3 : Nat";]

    [<Test>]
    let ``simplebool`` () =

        regression "simplebool" ["(lambda x:Bool. x) : Bool -> Bool";
                                "true : Bool";]

//    [<Test>]
//    let ``tyarith`` () =
//
//        regression "tyarith" []

    [<Test>]
    let ``untyped`` () =

        regression "untyped" ["x ";
                                "x";
                                "(lambda x'. x')";
                                "(lambda x'. x' x')";]
