open System.IO

let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

input |> List.map (printfn "%A")

type Elem =
    | Group of Elem list
    | Item of int

let digit c = c >= '0' && c <= '9'

let rec getInt (i:int)(chars : char list) =
    match chars with
    | a::b::rest when (digit a) && (digit b) ->
        let d = (a - '0') |> int
        let i = i * 10 + d
        getInt i (b::rest)
    | a::rest when (digit a) -> (i*10) + (a-'0' |> int),rest
    
let parse (s:string) =
    let s = s.ToCharArray () |> Array.toList
    let rec parse (Group group:Elem) (s:char list) : Elem*(char list)=
        match s with
        | [] -> (Group group),[]
        | '[' :: rest ->
            let inner,rest = (parse (Group []) rest)
            let group = Group (inner::group)
            parse group rest
        | ']' :: rest -> (Group (group |> List.rev)),rest
        | ',' :: rest -> parse (Group group) rest
        | d :: rest when digit d ->
            let i,rest = getInt 0 (d::rest)
            parse (Group ((Item i)::group)) rest
    parse (Group []) s 

'4'::'x'::[] |> getInt 0 |> printfn "%A"  
'1'::'4'::'7'::'x'::[] |> getInt 0 |> printfn "%A"

printfn "X"


let rec split3 (l:(Elem*char list) list) =
    match l with
    | (Group [a],[]) :: (Group [b],[]) :: (Group [],[]) ::rest -> (a,b) :: split3 rest
    | [] -> []
    | [(Group [a],[]);(Group [b],[])] -> (a,b)::[]

let pairs = input |> List.map parse |> split3

pairs |> printfn "%A"

type Ordered =
    | OK
    | WRONG
    | SAME
    | UNHANDLED of Elem*Elem

let rec compare (a:Elem) (b:Elem) =
    match (a,b) with
    | Item a,Item b when a = b -> SAME
    | Item a,Item b when a < b -> OK
    | Item a,Item b when a > b -> WRONG
    | Group [],Group [] -> SAME
    | Group (_::_), Group [] -> WRONG
    | Group [],Group (_::_) -> OK
    | Group (a::ra), Group (b::rb) ->
        match compare a b with
        | SAME -> compare (Group ra) (Group rb)
        | res -> res
    | Group a,Item b -> compare (Group a) (Group [Item b])
    | Item a,Group b -> compare (Group [Item a]) (Group b)
    | a,b -> UNHANDLED (a,b)

let comparePair (a,b) = compare a b
printfn "COMPARING***"
let res = pairs |> List.map comparePair |> List.mapi (fun i res -> (i+1,res))
          |> List.filter (fun (a,b) -> b = OK)
          |> List.map fst
          |> List.sum 

res |> printfn "%A"

printfn "##### TASK 2 "

let rec unpair l =
    match l with
    | [] -> []
    | (a,b)::rest -> a::b::(unpair rest)

let allFromInput = pairs |> unpair 
let div1 = Group [Group [Item 2]]
let div2 = Group [Group [Item 6]]

let all = div1::div2::allFromInput 

all |> List.map (printfn "%A")
printfn $"LENGTH: {all.Length}"

let rec qsort (div:Elem) (front:Elem list) (back:Elem list) (input:Elem list) =
    match input with
    | [] ->
        let front = match front with
                    | [] -> []
                    | a::rest -> qsort a [] [] rest
        let back = match back with
                    | [] -> []
                    | b::rest -> qsort b [] [] rest 
        [front;[div];back] |> List.concat 
    | a :: rest when compare a div = OK -> qsort div (a::front) back rest
    | a :: rest -> qsort div front (a::back) rest

let sorted = qsort all.Head [] [] all.Tail

let indexed = sorted |> List.mapi (fun i elem -> (i+1,elem))

let pos1 = indexed |> List.find (fun (i,a) -> a = div1) |> fst 
let pos2 = indexed |> List.find (fun (i,a) -> a = div2) |> fst

printfn $"pos1={pos1} pos2={pos2} res={pos1*pos2}"
    