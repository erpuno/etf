open BERT

open System.Numerics

[<EntryPoint>]
let main argv =
    let xs = [|131uy;116uy;0uy;0uy;0uy;2uy;109uy;0uy;0uy;0uy;4uy;114uy;101uy;110uy;
               116uy;70uy;63uy;243uy;51uy;51uy;51uy;51uy;51uy;51uy;100uy;0uy;2uy;
               111uy;107uy;108uy;0uy;0uy;0uy;3uy;97uy;1uy;70uy;63uy;240uy;0uy;0uy;0uy;
               0uy;0uy;0uy;109uy;0uy;0uy;0uy;1uy;49uy;106uy|]

    let τ = decodeTerm xs
    let ys = encodeTerm τ
    let ρ = decodeTerm ys

    printfn "Bytes: %A" xs
    printfn "Term: %A" τ
    printfn "Encoded: %A" ys
    printfn "Decoded: %A" ρ

    0