open ETF
open System

[<EntryPoint>]
let main argv =
    let term = fun i -> Term.Tuple [Term.String "n2o"; Atom "nitro"; Int i]

    let count = 100000000

    let τ1 = DateTimeOffset.Now.ToUnixTimeSeconds ()

    for i = 1 to count do
        let τ = decodeTerm (encodeTerm (term i))
        ()

    let τ2 = DateTimeOffset.Now.ToUnixTimeSeconds ()
    let Δτ = τ2 - τ1

    printfn "%d second(s) to encode-decode %d terms" Δτ count
    printfn "%f terms per second" (float count / float Δτ)

    0