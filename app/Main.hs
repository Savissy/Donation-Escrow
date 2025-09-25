{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}



module Main where

import Prelude (IO, putStrLn)
import PlutusTx (compile)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude

-- We use BuiltinData so the validator stays "untyped" (portable)
-- Datum: BuiltinByteString (hash)
-- Redeemer: BuiltinByteString (preimage)
{-# INLINABLE mkHashlock #-}
mkHashlock :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkHashlock rawDatum rawRedeemer _rawCtx =
    let
        -- decode BuiltinData to BuiltinByteString
        datumBS    = unsafeFromBuiltinData @BuiltinByteString rawDatum
        redeemerBS = unsafeFromBuiltinData @BuiltinByteString rawRedeemer

        -- compute hash of redeemer
        computedHash = sha2_256 redeemerBS

        ok = computedHash == datumBS
    in
        if ok
           then ()                       -- succeed
           else traceError ("Hash mismatch" :: BuiltinString)


-- compiled Plutus Core
validatorCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCompiled = $$(compile [|| mkHashlock ||])

main :: IO ()
main = putStrLn "Hash-lock validator compiled (validatorCompiled :: CompiledCode ...)."
