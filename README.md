F# ETF Serializer
=================

[![NuGet version (etf)](https://img.shields.io/nuget/v/etf.svg?style=flat-square)](https://www.nuget.org/packages/etf/)

This is tiny (170 LOC) but robust Erlang Term Format (ETF) encoder/decoder.

Usage
-----

Add library to your project:

```
$ dotnet add package etf
```

Open `ETF` namespace:

```fsharp
open ETF
```

Use `decodeTerm` and `encodeTerm`:

```fsharp
let τ = Term.String "hello world"
let bytes = encodeTerm τ
let ρ = decodeTerm bytes
```

Credits
-------

* Maxim Sokhatsky
* Siegmentation Fault
