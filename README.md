# purescript-binary

[![Build Status](https://travis-ci.org/Unisay/purescript-binary.svg?branch=master)](https://travis-ci.org/Unisay/purescript-binary)

The idea behind this library is to provide a bit-manipulation primitives together with type classes
that capture isomorphism between your custom types and arrays of bits.

Additionally, library provides [Un]signed integer types of a size fixed at the type level:
- [x] Data.Binary.UnsignedInt
- [x] Data.Binary.SignedInt

Example usage:

```console
> pulp psci

> import Prelude
> import Data.Binary (toBits)
> import Data.Binary.UnsignedInt
> import Data.Typelevel.Num 
> uint32 = fromInt d32
> bits = uint32 >>> toBits
> uint32 100
UnsignedInt32#1100100

> :t uint32 100
UnsignedInt (NumCons D3 D2)

> bits 100
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0]

> two = uint32 2
> five = uint32 5
> toBits (two * five)
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0]

> import Data.Bounded 
> (top :: UnsignedInt D17)
UnsignedInt17#11111111111111111

> (bottom :: UnsignedInt D17)
UnsignedInt17#0

```

`Data.Binary.SignedInt` works in the two's complement for negative values:

```console
> import Data.Binary.SignedInt
> import Data.Typelevel.Num 
> import Data.Bounded 
> (bottom :: SignedInt D5)
SignedInt5#10000

> (top :: SignedInt D5)
SignedInt5#01111
```
