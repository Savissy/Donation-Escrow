{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module AuctionValidator where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 (CurrencySymbol, Datum (..), Lovelace, OutputDatum (..), 
                           POSIXTime, PubKeyHash, ScriptContext (..), TokenName, TxInfo (..), 
                           TxOut (..), from, to, ScriptInfo (..), Redeemer (..), getRedeemer)
import PlutusLedgerApi.V3.Contexts (getContinuingOutputs)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.List qualified as List

data AuctionParams = AuctionParams
  { apSeller         :: PubKeyHash
  -- ^ Seller's public key hash. The highest bid (if exists) will be sent to the seller.
  -- If there is no bid, the asset auctioned will be sent to the seller.
  , apCurrencySymbol :: CurrencySymbol
  -- ^ The currency symbol of the token being auctioned.
  , apTokenName      :: TokenName
  -- ^ The name of the token being auctioned.
  -- These can all be encoded as a `Value`.
  , apMinBid         :: Lovelace
  -- ^ The minimum bid in Lovelace.
  , apEndTime        :: POSIXTime
  -- ^ The deadline for placing a bid. This is the earliest time the auction can be closed.
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''AuctionParams
PlutusTx.makeIsDataSchemaIndexed ''AuctionParams [('AuctionParams, 0)]

data Bid = Bid
  { bAddr   :: PlutusTx.BuiltinByteString
  -- ^ Bodder's wallet address
  , bPkh    :: PubKeyHash
  -- ^ Bidder's public key hash.
  , bAmount :: Lovelace
  -- ^ Bid amount in Lovelace.
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.deriveShow ''Bid
PlutusTx.makeIsDataSchemaIndexed ''Bid [('Bid, 0)]

instance PlutusTx.Eq Bid where
  {-# INLINEABLE (==) #-}
  bid == bid' =
    bPkh bid
      PlutusTx.== bPkh bid'
      PlutusTx.&& bAmount bid
      PlutusTx.== bAmount bid'

{- | Datum represents the state of a smart contract. In this case
it contains the highest bid so far (if exists).
-}
newtype AuctionDatum = AuctionDatum {adHighestBid :: Maybe Bid}
  deriving stock (Generic)
  deriving newtype
    ( HasBlueprintDefinition
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    )

{- | Redeemer is the input that changes the state of a smart contract.
In this case it is either a new bid, or a request to close the auction
and pay out the seller and the highest bidder.
-}
data AuctionRedeemer = NewBid Bid | Payout
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''AuctionRedeemer [('NewBid, 0), ('Payout, 1)]

{-# INLINEABLE auctionTypedValidator #-}

{- | Given the auction parameters, determines whether the transaction is allowed to
spend the UTXO. V3 validator extracts datum and redeemer from ScriptContext.
-}
auctionTypedValidator ::
  AuctionParams ->
  ScriptContext ->
  Bool
auctionTypedValidator params ctx@(ScriptContext txInfo scriptRedeemer scriptInfo) =
  List.and conditions
  where
    -- Extract redeemer from script context
    redeemer :: AuctionRedeemer
    redeemer = case PlutusTx.fromBuiltinData (getRedeemer scriptRedeemer) of
      Nothing -> PlutusTx.traceError "Failed to parse AuctionRedeemer"
      Just r  -> r
      
    -- Extract datum from script context  
    highestBid :: Maybe Bid
    highestBid = case scriptInfo of
      SpendingScript _ (Just (Datum datum)) -> 
        case PlutusTx.fromBuiltinData datum of
          Just (AuctionDatum bid) -> bid
          Nothing -> PlutusTx.traceError "Failed to parse AuctionDatum"
      _ -> PlutusTx.traceError "Expected SpendingScript with datum"
      
    conditions :: [Bool]
    conditions = case redeemer of
      NewBid bid ->
        [ -- The new bid must be higher than the highest bid.
          -- If this is the first bid, it must be at least as high as the minimum bid.
          sufficientBid bid
        , -- The bid is not too late.
          validBidTime
        , -- The previous highest bid should be refunded.
          refundsPreviousHighestBid
        , -- A correct new datum is produced, containing the new highest bid.
          correctOutput bid
        ]
      Payout ->
        [ -- The payout is not too early.
          validPayoutTime
        , -- The seller gets the highest bid.
          sellerGetsHighestBid
        , -- The highest bidder gets the asset.
          highestBidderGetsAsset
        ]
    sufficientBid :: Bid -> Bool
    sufficientBid (Bid _ _ amt) = case highestBid of
      Just (Bid _ _ amt') -> amt PlutusTx.> amt'
      Nothing             -> amt PlutusTx.>= apMinBid params
    validBidTime :: Bool
    ~validBidTime = to (apEndTime params) `contains` txInfoValidRange txInfo
    refundsPreviousHighestBid :: Bool
    ~refundsPreviousHighestBid = case highestBid of
      Nothing -> True
      Just (Bid _ bidderPkh amt) ->
        case List.find
          ( \o ->
              (toPubKeyHash (txOutAddress o) PlutusTx.== Just bidderPkh)
                PlutusTx.&& (lovelaceValueOf (txOutValue o) PlutusTx.== amt)
          )
          (txInfoOutputs txInfo) of
          Just _  -> True
          Nothing -> PlutusTx.traceError "Not found: refund output"
    currencySymbol :: CurrencySymbol
    currencySymbol = apCurrencySymbol params

    tokenName :: TokenName
    tokenName = apTokenName params

    correctOutput :: Bid -> Bool
    correctOutput bid = case getContinuingOutputs ctx of
      [o] ->
        let correctOutputDatum = case txOutDatum o of
              OutputDatum (Datum newDatum) -> case PlutusTx.fromBuiltinData newDatum of
                Just (AuctionDatum (Just bid')) ->
                  PlutusTx.traceIfFalse
                    "Invalid output datum: contains a different Bid than expected"
                    (bid PlutusTx.== bid')
                Just (AuctionDatum Nothing) ->
                  PlutusTx.traceError "Invalid output datum: expected Just Bid, got Nothing"
                Nothing ->
                  PlutusTx.traceError "Failed to decode output datum"
              OutputDatumHash _ ->
                PlutusTx.traceError "Expected OutputDatum, got OutputDatumHash"
              NoOutputDatum ->
                PlutusTx.traceError "Expected OutputDatum, got NoOutputDatum"

            outValue = txOutValue o

            correctOutputValue =
              (lovelaceValueOf outValue PlutusTx.== bAmount bid)
                PlutusTx.&& (valueOf outValue currencySymbol tokenName PlutusTx.== 1)
         in correctOutputDatum PlutusTx.&& correctOutputValue
      os ->
        PlutusTx.traceError
          ( "Expected exactly one continuing output, got "
              PlutusTx.<> PlutusTx.show (List.length os)
          )
    validPayoutTime :: Bool
    ~validPayoutTime = from (apEndTime params) `contains` txInfoValidRange txInfo

    sellerGetsHighestBid :: Bool
    ~sellerGetsHighestBid = case highestBid of
      Nothing -> True
      Just bid ->
        case List.find
          ( \o ->
              (toPubKeyHash (txOutAddress o) PlutusTx.== Just (apSeller params))
                PlutusTx.&& (lovelaceValueOf (txOutValue o) PlutusTx.== bAmount bid)
          )
          (txInfoOutputs txInfo) of
          Just _  -> True
          Nothing -> PlutusTx.traceError "Not found: Output paid to seller"

    highestBidderGetsAsset :: Bool
    ~highestBidderGetsAsset =
      let highestBidder = case highestBid of
            -- If there are no bids, the asset should go back to the seller
            Nothing  -> apSeller params
            Just bid -> bPkh bid
       in case List.find
            ( \o ->
                (toPubKeyHash (txOutAddress o) PlutusTx.== Just highestBidder)
                  PlutusTx.&& (valueOf (txOutValue o) currencySymbol tokenName PlutusTx.== 1)
            )
            (txInfoOutputs txInfo) of
            Just _  -> True
            Nothing -> PlutusTx.traceError "Not found: Output paid to highest bidder"

{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator ::
  AuctionParams ->
  BuiltinData ->
  PlutusTx.BuiltinUnit
auctionUntypedValidator params ctx =
  PlutusTx.check
    ( auctionTypedValidator
        params
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

auctionValidatorScript ::
  AuctionParams ->
  CompiledCode (BuiltinData -> PlutusTx.BuiltinUnit)
auctionValidatorScript params =
  $$(PlutusTx.compile [||auctionUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 params

PlutusTx.asData
  [d|
    data Bid' = Bid'
      { bPkh' :: PubKeyHash
      , -- \^ Bidder's wallet address.
        bAmount' :: Lovelace
      }
      -- \^ Bid amount in Lovelace.

      -- We can derive instances with the newtype strategy, and they
      -- will be based on the instances for 'Data'
      deriving newtype (Eq, Ord, PlutusTx.ToData, FromData, UnsafeFromData)

    -- don't do this for the datum, since it's just a newtype so
    -- simply delegates to the underlying type

    -- \| Redeemer is the input that changes the state of a smart contract.
    -- In this case it is either a new bid, or a request to close the auction
    -- and pay out the seller and the highest bidder.
    data AuctionRedeemer' = NewBid' Bid | Payout'
      deriving newtype (Eq, Ord, PlutusTx.ToData, FromData, UnsafeFromData)
    |]

