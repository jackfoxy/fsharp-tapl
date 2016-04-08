// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.




// Define your library scripting code here

let input=[8,10; 1,3; 15,18; 7,8; 4,5; 2,6]

let overlap (a,b) (c,d) = a<=c && b>=c || a<=d && b>=d

let lap (a,b) (c,d) = if overlap (a,b) (c,d) then (min a c, max b d) else (c,d)

let laple l e = 
 let  l1 = List.filter ((<>) e) l
 if   List.exists (overlap e) l1  then l1 |> List.map (lap e) else l

let rec f3 l =
 let l1 = List.fold (fun a e -> laple a e) l l
 if l1 <> l then l1 |> f3  else l

let f4 l = l |> List.sort |> f3

[(8,10);(1,3);(15,18);(7,8);(4,5);(2,6)] |> f4

///////

input
|> List.sort
|> List.fold (fun (s : (int * int) list ) (t : int * int) ->
    match s, t  with
    | ((headStart, headEnd) :: tl), (itemStart, itemEnd) when (itemStart <= headEnd && itemEnd > headEnd) ->
        (headStart, itemEnd)::tl
    | ((_, headEnd) :: _), (_, itemEnd) when itemEnd <= headEnd ->
        s
    | _, x -> 
        x::s
    ) []
|> List.rev


let rec f = function    
    | 0 -> 0    
    | n -> n % 2 + f (n/2)

[uint8 255; uint8 0xE0; byte 1] 
|> List.map int 
|> List.fold (fun s t -> s + f t) 0

(*
to do:
1) .\bin\Debug\fulluntyped src\fulluntyped\test.f --relsults not quite right, check Jack's original output
2) .\bin\Debug\letexercise src\letexercise\test.f --throws
3) other w/o tests


*)