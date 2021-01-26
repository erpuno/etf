namespace ETF

open System
open System.Numerics

[<AutoOpen>]
module Types =

    type Term =
        | Byte   of byte
        | Int    of Int32
        | Float  of float
        | Atom   of string
        | Tuple  of Term list
        | String of string
        | List   of Term list * Term
        | Binary of byte array
        | Bigint of BigInteger
        | Dict   of Map<Term, Term>
        | Error  of string
        | Nil

    type Read<'a> (arr : 'a array) =
        member this.Content = arr
        member val Idx = 0 with get, set

        member this.Get (star : unit) : 'a =
            let res = arr.[this.Idx]
            this.Idx <- this.Idx + 1
            res

        member this.Getn (n : int) : ReadOnlySpan<'a> =
            if n <= 0 then failwith "Getn expects positive integer"
            let span = ReadOnlySpan (arr, this.Idx, n)
            this.Shift n
            span

        member this.Shift (n : int) =
            this.Idx <- this.Idx + n

    type Write<'a> (inh : 'a, size : int) =
        member val Content = Array.init size (fun _ -> inh)
        member val Idx = 0 with get, set

        member this.Write (value : 'a) : unit =
            this.Content.[this.Idx] <- value
            this.Idx <- this.Idx + 1

        member this.Span (n : int) : Span<'a> =
            if n <= 0 then failwith "Span expects positive integer"
            let span = Span (this.Content, this.Idx, n)
            this.Shift n
            span

        member this.Shift (n : int) =
            this.Idx <- this.Idx + n