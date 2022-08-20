# Playground frontend form generation

In order to handle arbitrary datatypes and their values, Plutus uses
classes `ToSchema` and `ToArgument` and the `FormArgument` datatype.
`FormArgument` is based on the `Fix` type from `recursion-schemes` which is used
by some of the frontend code.

Minimal example on the Haskell side (uses `LockArgs` from `GameStateMachine.hs` use case):

```
nix-shell shell.nix
cd plutus-use-cases/
cabal repl
```

```
import Schema
import Ledger.Value as V
import Ledger.Ada as Ada
import Plutus.Contracts.GameStateMachineargs = LockArgs "hello" (Ada.lovelaceValueOf 10000000)
toArgument args

Fix (FormObjectF [("lockArgsSecret",Fix (FormStringF (Just "hello"))),("lockArgsValue",Fix (FormValueF (Value (Map [(,Map [("",10000000)])]))))])
```

This is the code responsible for generating endpoint argument forms
in Plutus Playground on the PureScript side:

https://github.com/input-output-hk/plutus/blob/74cb849b6580d937a97aff42636d4ddc6a140ed6/plutus-playground-client/src/Action/View.purs#L88
https://github.com/input-output-hk/plutus/blob/74cb849b6580d937a97aff42636d4ddc6a140ed6/web-common-plutus/src/Schema/View.purs#L32-L307

(the commit is fixed here for convenience)

The PureScript frontend uses Halogen to generate HTML, here is the example:

```
nix-shell shell.nix
cd plutus-playground-client/
spago repl
```

```
import Schema.View
import Data.BigInteger as BigInteger
import Data.Functor.Foldable (Fix(..))
import Data.Int as Int
import Data.Json.JsonTuple (JsonTuple(..))
import Data.Tuple
import Data.Maybe
import Halogen.HTML
import Schema
import Schema.Types
import Plutus.V1.Ledger.Value
import PlutusTx.AssocMap as AssocMap
import Data.Unit

v = Fix (FormValueF (Value { getValue: AssocMap.fromTuples [ ( Tuple (CurrencySymbol { unCurrencySymbol: "" }) (AssocMap.fromTuples [ Tuple (TokenName { unTokenName: "" }) (BigInteger.fromInt 10000000) ])) ] }))
s = Fix (FormStringF (Just "hello"))
f = Fix (FormObjectF [JsonTuple (Tuple "lockArgsSecret" s), JsonTuple (Tuple "lockArgsValue" v)])
html = actionArgumentForm 0 (\_ -> unit) f :: HTML Unit Unit
```
