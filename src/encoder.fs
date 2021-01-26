namespace BERT

open System
open System.Text
open System.Numerics
open System.Buffers.Binary

open type BERT.Types.Term

[<AutoOpen>]
module Encoder =

    let byteSize  = int Byte.MaxValue
    let wordSize  = int UInt16.MaxValue
    let dwordSize = Int32.MaxValue

    let stringSize (s : string) =
        let bytes = Encoding.UTF8.GetByteCount s
        if bytes <= wordSize then bytes + 2
        else failwith "string too long (>65535)"

    let rec listSize (xs : Term list) =
        let total = List.sum (List.map termSize xs)
        let length = xs.Length
        if length <= dwordSize then total + 4
        else failwith "term too long (>2147483647)"

    and termSize : Term -> int = function
        | Byte _       -> 2
        | Int _        -> 5
        | Float _      -> 9
        | Atom s       -> 1 + stringSize s
        | Tuple xs     -> 1 + listSize xs
        | String s     -> 1 + stringSize s
        | List (x, xs) -> 1 + listSize x + termSize xs
        | Binary xs    -> 1 + 4 + xs.Length
        | Bigint x     ->
            let bytes = x.GetByteCount ()
            if bytes <= dwordSize then 1 + 4 + 1 + bytes
            else failwith "Bigint too big (more than 2147483647 bytes)"
        | Dict dict    ->
            1 + 4 + Seq.sumBy (fun (k, v) -> termSize k + termSize v)
                              (Map.toSeq dict)
        | Nil          -> 1

    let writeWord (xs : Write<byte>) (x : UInt16) : unit =
        BinaryPrimitives.WriteUInt16BigEndian (xs.Span 2, x)

    let writeInt (xs : Write<byte>) (x : Int32) : unit =
        BinaryPrimitives.WriteInt32BigEndian (xs.Span 4, x)

    let writeUTF8 (xs : Write<byte>) (s : string) =
        let bytes = Encoding.UTF8.GetByteCount s
        let chars : char array = s.ToCharArray ()

        writeWord xs (uint16 bytes)
        Encoding.UTF8.GetBytes (ReadOnlySpan chars, xs.Span bytes)
        |> ignore

    let rec writeTerm (xs : Write<byte>) : Term -> unit = function
        | Byte x -> xs.Write 97uy; xs.Write x
        | Int x  -> xs.Write 98uy; writeInt xs x
        | Float x ->
            xs.Write 70uy
            BinaryPrimitives.WriteDoubleBigEndian (xs.Span 8, x)
        | Atom s       -> xs.Write 118uy; writeUTF8 xs s
        | Tuple ys     -> xs.Write 105uy; writeInt xs ys.Length;
                          List.iter (writeTerm xs) ys
        | String s     -> xs.Write 107uy; writeUTF8 xs s
        | List (y, ys) -> xs.Write 108uy; writeInt xs y.Length;
                          List.iter (writeTerm xs) y; writeTerm xs ys
        | Binary ys    -> xs.Write 109uy; writeInt xs ys.Length;
                          Array.iter xs.Write ys
        | Dict dict    -> xs.Write 116uy; writeInt xs (Map.count dict);
                          Map.iter (fun k v -> writeTerm xs k; writeTerm xs v) dict
        | Bigint x     ->
            let bytes = (BigInteger.Abs x).ToByteArray ()
            xs.Write 111uy; writeInt xs bytes.Length;
            xs.Write (if x.Sign >= 0 then 0uy else 1uy)
            Array.iter xs.Write bytes
        | Nil -> xs.Write 106uy

    let encodeTerm (τ : Term) : byte array =
        let xs = Write (0uy, termSize τ + 1)
        xs.Write 131uy
        writeTerm xs τ
        xs.Content