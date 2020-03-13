module LowCode.Utility

let curry f = fun a b -> f (a, b)
let curry2 f = fun a b c -> f (a, b, c)
let curry3 f = fun a b c d -> f (a, b, c, d)
let uncurry f = fun (a, b) -> f a b
let uncurry2 f = fun (a, b, c) -> f a b c
let uncurry3 f = fun (a, b, c, d) -> f a b c d

type State<'s, 'a> = State of ('s -> 's * 'a)

module Function =
    let map = (<<)
    let (<+>) = map
    let unit f = fun _ -> f
    let (<*>) f g x = f x (g x)
    let (>>=) f k = fun r -> k (f r) r

module State =
    let map f (State s) = State (fun s' -> (s', f >> s))
    let (<+>) = map
    let unit a = State (fun s -> (s, a))
    let (<*>) (State f) (State s) = State <| fun s' ->
        let (s'', f') = f s'
        let (s''', a) = s s''
        in (s''', f' a)
    let (>>=) (State s) f = State (fun s' -> let (s'', a) = s s' in f a s'')
