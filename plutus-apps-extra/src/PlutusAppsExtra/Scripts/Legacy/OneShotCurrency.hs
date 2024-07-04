{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry"            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

-- | Due to the transition to a new version of the GHC and updating versions
-- of the Plutus libraries, some Plutus scripts began to be compiled a little
-- differently, which led to a change in their hash and a breakdown in backward
-- compatibility in encoins-core. This module contains the initial version of `oneShotCurrencyPolicy`.
module PlutusAppsExtra.Scripts.Legacy.OneShotCurrency where

import           Cardano.Ledger.Babbage               (Babbage)
import           Cardano.Ledger.Core                  (eraProtVerLow)
import           Cardano.Ledger.Plutus                (Plutus (..), PlutusBinary (..), PlutusLanguage (..), PlutusRunnable (..))
import qualified Data.ByteString.Short                as SBS
import           Data.Maybe                           (fromJust)
import           Ledger                               (Language (..), MintingPolicyHash, Versioned (..), mkMintingPolicyScript)
import           Ledger.Tx                            (DecoratedTxOut)
import           Plutus.Script.Utils.V2.Scripts       (MintingPolicy, mintingPolicyHash, scriptCurrencySymbol)
import           PlutusAppsExtra.Constraints.OffChain (tokensMintedTx, utxoSpentPublicKeyTx)
import           PlutusAppsExtra.Types.Tx             (TransactionBuilder)
import           PlutusCore                           (NamedDeBruijn (..))
import qualified PlutusCore                           as PLC
import           PlutusCore.Default                   (DefaultUni (..))
import           PlutusCore.Version                   (plcVersion100)
import           PlutusLedgerApi.Common               (ScriptNamedDeBruijn (..))
import           PlutusLedgerApi.V2                   (CurrencySymbol, TokenName, TxOutRef (..), singleton)
import qualified PlutusLedgerApi.V2                   as P
import           PlutusTx.Code                        (CompiledCodeIn (..))
import           PlutusTx.Prelude                     hiding (Monoid (..), Semigroup (..))
import qualified Prelude                              as Haskell
import           Text.Hex                             (Text, decodeHex)
import           Universe                             (Some (..), ValueOf (..))
import           Unsafe.Coerce                        (unsafeCoerce)
import qualified UntypedPlutusCore                    as UPLC
import           UntypedPlutusCore                    (Term (..))

data LegacyOneShotCurrencyParams = LegacyOneShotCurrencyParams TxOutRef TokenName Integer

-------------------------------- On-Chain ------------------------------------

legacyOneShotCurrencyValue :: CurrencySymbol -> LegacyOneShotCurrencyParams -> P.Value
legacyOneShotCurrencyValue s (LegacyOneShotCurrencyParams _ name amt) = singleton s name amt

liftLegacyOneShotCurrencyParams :: LegacyOneShotCurrencyParams -> UPLC.Program UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
liftLegacyOneShotCurrencyParams (LegacyOneShotCurrencyParams (TxOutRef (P.TxId refBs) refInt) (P.TokenName name) amt) = UPLC.Program () plcVersion100 $
    Apply () (Apply () (Force () (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "Plutus_V1_Ledger_Tx_TxOutRef", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "match_Plutus_V1_Ledger_Tx_TxOutRef", ndbnIndex = 0}) (Apply () (Apply () (Force () (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "GHC_Tuple_Tuple2", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "match_GHC_Tuple_Tuple2", ndbnIndex = 0}) (Apply () (Apply () (Apply () (Force () (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "GHC_Types_Nil", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "GHC_Types_Cons", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "match_GHC_Types_Nil", ndbnIndex = 0}) (Apply () (Apply () (Force () (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "PlutusAppsExtra_Scripts_OneShotCurrency_OneShotCurrencyParams", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "match_PlutusAppsExtra_Scripts_OneShotCurrency_OneShotCurrencyParams", ndbnIndex = 0}) (Apply () (Apply () (Var () (NamedDeBruijn {ndbnString = "PlutusAppsExtra_Scripts_OneShotCurrency_OneShotCurrencyParams", ndbnIndex = 2})) (Apply () (Apply () (Var () (NamedDeBruijn {ndbnString = "Plutus_V1_Ledger_Tx_TxOutRef", ndbnIndex = 9})) (Constant () (Some (ValueOf DefaultUniByteString (fromBuiltin refBs))))) (Constant () (Some (ValueOf DefaultUniInteger (refInt)))))) (Apply () (Apply () (Force () (Var () (NamedDeBruijn {ndbnString = "GHC_Types_Cons", ndbnIndex = 4}))) (Apply () (Apply () (Force () (Force () (Var () (NamedDeBruijn {ndbnString = "GHC_Tuple_Tuple2", ndbnIndex = 7})))) (Constant () (Some (ValueOf DefaultUniByteString (fromBuiltin name))))) (Constant () (Some (ValueOf DefaultUniInteger amt))))) (Force () (Var () (NamedDeBruijn {ndbnString = "GHC_Types_Nil", ndbnIndex = 5}))))))))) (LamAbs () (NamedDeBruijn {ndbnString = "arg_0", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "arg_1", ndbnIndex = 0}) (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "case_PlutusAppsExtra_Scripts_OneShotCurrency_OneShotCurrencyParams", ndbnIndex = 0}) (Apply () (Apply () (Var () (NamedDeBruijn {ndbnString = "case_PlutusAppsExtra_Scripts_OneShotCurrency_OneShotCurrencyParams", ndbnIndex = 1})) (Var () (NamedDeBruijn {ndbnString = "arg_0", ndbnIndex = 3}))) (Var () (NamedDeBruijn {ndbnString = "arg_1", ndbnIndex = 2})))))))) (LamAbs () (NamedDeBruijn {ndbnString = "x", ndbnIndex = 0}) (Var () (NamedDeBruijn {ndbnString = "x", ndbnIndex = 1}))))))))) (Delay () (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "case_GHC_Types_Nil", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "case_GHC_Types_Cons", ndbnIndex = 0}) (Var () (NamedDeBruijn {ndbnString = "case_GHC_Types_Nil", ndbnIndex = 2}))))))) (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "arg_0", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "arg_1", ndbnIndex = 0}) (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "case_GHC_Types_Nil", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "case_GHC_Types_Cons", ndbnIndex = 0}) (Apply () (Apply () (Var () (NamedDeBruijn {ndbnString = "case_GHC_Types_Cons", ndbnIndex = 1})) (Var () (NamedDeBruijn {ndbnString = "arg_0", ndbnIndex = 4}))) (Var () (NamedDeBruijn {ndbnString = "arg_1", ndbnIndex = 3})))))))))) (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "x", ndbnIndex = 0}) (Var () (NamedDeBruijn {ndbnString = "x", ndbnIndex = 1}))))))))) (Delay () (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "arg_0", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "arg_1", ndbnIndex = 0}) (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "case_GHC_Tuple_Tuple2", ndbnIndex = 0}) (Apply () (Apply () (Var () (NamedDeBruijn {ndbnString = "case_GHC_Tuple_Tuple2", ndbnIndex = 1})) (Var () (NamedDeBruijn {ndbnString = "arg_0", ndbnIndex = 3}))) (Var () (NamedDeBruijn {ndbnString = "arg_1", ndbnIndex = 2})))))))))) (Delay () (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "x", ndbnIndex = 0}) (Var () (NamedDeBruijn {ndbnString = "x", ndbnIndex = 1})))))))))) (LamAbs () (NamedDeBruijn {ndbnString = "arg_0", ndbnIndex = 0}) (LamAbs () (NamedDeBruijn {ndbnString = "arg_1", ndbnIndex = 0}) (Delay () (LamAbs () (NamedDeBruijn {ndbnString = "case_Plutus_V1_Ledger_Tx_TxOutRef", ndbnIndex = 0}) (Apply () (Apply () (Var () (NamedDeBruijn {ndbnString = "case_Plutus_V1_Ledger_Tx_TxOutRef", ndbnIndex = 1})) (Var () (NamedDeBruijn {ndbnString = "arg_0", ndbnIndex = 3}))) (Var () (NamedDeBruijn {ndbnString = "arg_1", ndbnIndex = 2})))))))) (LamAbs () (NamedDeBruijn {ndbnString = "x", ndbnIndex = 0}) (Var () (NamedDeBruijn {ndbnString = "x", ndbnIndex = 1})))

legacyOneShotCurrencyPolicy :: LegacyOneShotCurrencyParams -> MintingPolicy
legacyOneShotCurrencyPolicy currencyParam = mkMintingPolicyScript $
  let bs = fromJust $ decodeHex checkPolicyUplc
      Right (PlutusRunnable r) = decodePlutusRunnable @PlutusV2 (eraProtVerLow @Babbage) $ Plutus $ PlutusBinary $ SBS.toShort bs
      ScriptNamedDeBruijn progFun = P.deserialisedScript r
      progParam = liftLegacyOneShotCurrencyParams currencyParam
      Right prog = UPLC.applyProgram progFun progParam
  in DeserializedCode (unsafeCoerce prog) Nothing Haskell.mempty

checkPolicyUplc :: Text
checkPolicyUplc = "590b2801000032323322323232323232323232323232323233223232323232323232323232323232323233223232322223232533533223500722350022235005223301b330184901024330003232333553017120013501e501c2350012233355301a1200135021501f23500122333500123303b4800000488cc0f00080048cc0ec00520000013301000200135003222222222222008323500e22333223355002001200130181200133502022335530191200123500122335502d0023355301c1200123500122335503000233350012330414800000488cc1080080048cc10400520000013301300200150273355301c1200123500122335029335502b006335029335502b002001502a502a0013500722333350012326320333357389201024c680003320012326320333357389201024c68000332326320333357389201024c68000333301849010243310035002222222222222333553021120013501f502225335333573466e3c044d400488d4008880081041004ccd5cd19b8701035001223500222001041040104000c33573892011d52656465656d6572206465636f646564207375636365737366756c6c79003333573466e1cd55ce9baa0044800080a48c98c80a4cd5ce01501481399ab9c4912353637269707420636f6e74657874206465636f646564207375636365737366756c6c79003333573466e1cd55cea80124000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4094098d5d0a80619a8128131aba1500b33502502735742a014666aa052eb940a0d5d0a804999aa814bae502835742a01066a04a0626ae85401cccd540a40c9d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40f1d69aba15002303d357426ae8940088c98c80fccd5ce02001f81e89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a81e3ad35742a004607a6ae84d5d1280111931901f99ab9c04003f03d135573ca00226ea8004d5d09aba2500223263203b33573807807607226aae7940044dd50009aba1500533502575c6ae854010ccd540a40b88004d5d0a801999aa814bae200135742a00460606ae84d5d1280111931901b99ab9c038037035135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00460406ae84d5d1280111931901499ab9c02a02902710281326320283357389210350543500028135573ca00226ea800488cd54c020480048d400488cd54070008ccd40048cd54c030480048d400488cd54080008d5403400400488ccd5540200400080048cd54c030480048d400488cd54080008d54030004004ccd55400c02c008004444888ccd54c01048005405ccd54c020480048d400488cd54070008d54024004ccd54c0104800488d4008894cd4ccd54c03448004d402d40388d400488cc028008014018400c4cd406c01000d4060004cd54c020480048d400488c8cd5407400cc004014c8004d540b4894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d5409888448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c00401048cd402c88ccd400c88008008004d400488004c8004d540888844894cd400454050884cd4054c010008cd54c01848004010004c8004d5408488448894cd40044d400c88004884ccd401488008c010008ccd54c01c48004014010004448cc004894cd40084084400407888ccd5cd19b8f00200101f01e225335001101e133573800403a640026aa03a442244a66a0022a01e44266a020600800466aa600c240020080022246600200403644a66a004200220342466a00444666a006440040040026a0024400224424660020060042464460046eb0004c8004d5406488cccd55cf80092805119a80498021aba100230033574400402a464646666ae68cdc39aab9d5002480008cc8848cc00400c008c034d5d0a80118029aba135744a004464c6402a66ae7005805404c4d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a0080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c058d5d0a80119a80800a9aba135744a004464c6403466ae7006c0680604d55cf280089baa00135742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263201c33573803a03803403203026aae7540044dd50009aba1500233500c75c6ae84d5d1280111931900b19ab9c017016014135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80b11191999aab9f0022500823350073355009300635573aa004600a6aae794008c010d5d100180989aba10011122002122122330010040031122123300100300212232323333573466e1d4005200023212230020033005357426aae79400c8cccd5cd19b8750024800884880048c98c8040cd5ce00880800700689aab9d500113754002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c8040cd5ce00880800700680600589aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8030cd5ce00680600509aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401466ae7002c0280204dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402666ae7005004c04404003c03803403002c4d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200c33573801a01801401226aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900499ab9c00a009007006135573aa00226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263200a33573801601401000e00c26aae7540044dd50009191999ab9a3370ea0029001100491999ab9a3370ea0049000100491931900319ab9c007006004003135573a6ea800526120014901035054310022333573466e1c00800401000c48800848800488cdc0001000889191800800911980198010010009"

-------------------------------- Off-Chain -----------------------------------


legacyCurrencySymbol :: LegacyOneShotCurrencyParams -> CurrencySymbol
legacyCurrencySymbol = scriptCurrencySymbol . legacyOneShotCurrencyPolicy

legacyCurrencyValue :: LegacyOneShotCurrencyParams -> P.Value
legacyCurrencyValue cur = (`legacyOneShotCurrencyValue` cur) $ legacyCurrencySymbol cur

-- Constraints that the OneShotCurrency is minted in the transaction
legacyOneShotCurrencyMintTx :: LegacyOneShotCurrencyParams -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
legacyOneShotCurrencyMintTx par@(LegacyOneShotCurrencyParams ref _ _) = do
    tokensMintedTx (Versioned (legacyOneShotCurrencyPolicy par) PlutusV2) () (legacyCurrencyValue par)
    utxoSpentPublicKeyTx (\r _ -> r == ref)
