module LowCode.Logic.Eval

open AST

let eval<'a when 'a : comparison> : AST<'a> -> unit =
    let rec eval symbols =
        function
        | Start l -> eval symbols l
        | Var ((k, v), l) -> eval (Map.add k v symbols) l
        | If (p, t, f) ->
            let valueType =
                function
                | Variable s -> Map.find s symbols
                | Constant c -> c
            let inline cmp op a b =
                if op (valueType a) (valueType b) then t else f
            match p with
            | IsEqual (a, b) -> eval symbols (cmp (=) a b)
            | IsDifferent (a, b) -> eval symbols (cmp (<>) a b)
            | IsGreaterThan (a, b) -> eval symbols (cmp (>) a b)
            | IsLessThan (a, b) -> eval symbols (cmp (<) a b)
            | IsGreaterOrEqualTo (a, b) -> eval symbols (cmp (>=) a b)
            | IsLessOrEqualTo (a, b) -> eval symbols (cmp (<=) a b)
        | Print (s, l) -> printf "%s" s; eval symbols l
        | End -> ()
    in eval Map.empty
