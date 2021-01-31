namespace ETF

open System
open System.Text
open System.Numerics
open System.Buffers.Binary

open type ETF.Types.Term

[<AutoOpen>]
module Decoder =

    let getWord (xs : Read<byte>) : UInt16 =
        BinaryPrimitives.ReadUInt16BigEndian (xs.Getn 2)

    let getDword (xs : Read<byte>) : UInt32 =
        BinaryPrimitives.ReadUInt32BigEndian (xs.Getn 4)

    let getInt (xs : Read<byte>) : Int32 =
        BinaryPrimitives.ReadInt32BigEndian (xs.Getn 4)

    let getString (enc : Encoding) (xs : Read<byte>) (n : int) : string =
        let res = enc.GetString (xs.Content, xs.Idx, n)
        xs.Shift n
        res

    let getBignum (xs : Read<byte>) (n : int) : BigInteger =
        let sign = xs.Get()
        let res = BigInteger (Array.init (n + 1) (fun idx -> if idx < n then xs.Get () else 0uy))
        if sign <> 0uy then -res else res

    let getUTF8   = getString Encoding.UTF8
    let getLatin1 = getString Encoding.Latin1

    let rec parseRead (xs : Read<byte>) : Term =
        // TODO: getList should receive uint
        let getList n = List.init n (fun _ -> parseRead xs)

        match xs.Get () with
        |  70uy -> Float (BinaryPrimitives.ReadDoubleBigEndian (xs.Getn 8))
        |  97uy -> Byte (xs.Get ())
        |  98uy -> Int (getInt xs)
        |  99uy ->
            let mutable res = double 0
            match Double.TryParse (getLatin1 xs 26, &res) with
            | true  -> Float res
            | false -> Error "Float?"
        | 100uy -> Atom (getWord xs |> int |> getLatin1 xs)
        | 104uy -> Tuple (xs.Get() |> int |> getList)
        | 105uy -> Tuple (getInt xs |> getList)
        | 106uy -> Nil
        | 107uy -> String (getWord xs |> int |> getUTF8 xs)
        | 108uy -> List (getList (getInt xs), parseRead xs)
        | 109uy -> Binary (Array.init (getInt xs) (fun _ -> xs.Get ()))
        | 110uy -> Bigint (xs.Get () |> int |> getBignum xs)
        | 111uy -> Bigint (getInt xs |> getBignum xs)
        | 115uy -> Atom (xs.Get () |> int |> getLatin1 xs)
        | 116uy -> fun _ -> (parseRead xs, parseRead xs)
                   |> Seq.init (getInt xs)
                   |> fun res -> Dict (Map res)
        | 118uy -> Atom (getWord xs |> int |> getUTF8 xs)
        | 119uy -> Atom (xs.Get () |> int |> getUTF8 xs)
        | _     -> Error "Term?"

    let decodeTerm (arr : byte array) =
        let xs = Read arr
        let magic = xs.Get ()
        if magic <> 131uy then Error "BERT?"
        else parseRead xs
