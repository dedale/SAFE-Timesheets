[<AutoOpen>]
module AsyncExtensions

module Async =
    let map f a = async {
        let! x = a
        return f x
    }
