[<AutoOpen>]
module Extensions

type Deferred<'t> =
    | NotStarted
    | InProgress
    | Resolved of 't

// cf. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise for naming
type PromiseStatus<'t> =
    | Pending
    | Completed of 't

let uncurry f (a, b) = f a b

let uncurry3 f (a, b, c) = f a b c
