{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Cardano.Ledger.Alonzo      (Alonzo)
import           Cardano.Ledger.Alonzo.Core (eraProtVerLow)
import           Cardano.Ledger.Babbage     (Babbage)
import           Cardano.Ledger.Conway      (Conway)
import           Cardano.Ledger.Plutus      (Language (..), Plutus (..), PlutusBinary (..), PlutusLanguage (..), PlutusRunnable (..),
                                             getCostModelEvaluationContext)
import           Cardano.Ledger.Shelley     (Shelley)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Short      as SBS
import           Data.Coerce                (coerce)
import           Data.Maybe                 (fromJust)
import           Ledger                     (Script (..), Validator (..), ValidatorHash (..), Versioned (..), validatorHash)
import           PlutusCore.Builtin.Debug   (DefaultFun, DefaultUni, plcVersion100)
import           PlutusLedgerApi.Common     (Data (..), ScriptNamedDeBruijn (..), SerialisedScript (..), VerboseMode (Verbose),
                                             deserialisedScript, serialiseUPLC, serialisedScript)
import           PlutusLedgerApi.V1         (toBuiltin)
import qualified PlutusLedgerApi.V2         as PV2
import           PlutusPrelude              (over)
import           Prelude
import           Test.Cardano.Ledger.Plutus
import           Tests.Scripts
import           Text.Hex                   (decodeHex, encodeHex)
import           Unsafe.Coerce              (unsafeCoerce)
import qualified UntypedPlutusCore          as UPLC

main :: IO ()
main = do
    let ppFile = "plutus-apps-extra-test/config/protocol-params.json"
    uplc <- SBS.toShort <$> BS.readFile "plutus-apps-extra-test/z.flat"
    let s = either (error. show) id $ decodePlutusRunnable @PlutusV2 (eraProtVerLow @Babbage) $ Plutus $ PlutusBinary uplc
    print s
    -- print $ decodePlutusRunnable @PlutusV1 (eraProtVerLow @Alonzo) $ Plutus $ PlutusBinary uplc

    -- print $ evaluatePlutusRunnableBudget @PlutusV1 (eraProtVerLow @Alonzo) Verbose (getCostModelEvaluationContext $ testingCostModel PlutusV2) run [I 42, I 2, I 42, I 4]
    -- print $ evaluatePlutusRunnableBudget @PlutusV1 (eraProtVerLow @Alonzo) Verbose (getCostModelEvaluationContext $ testingCostModel PlutusV2) run [I 42, I 2, I 3, I 4]



    runScriptTest ppFile

run :: PlutusRunnable 'PlutusV1
run = either (error. show) id $ decodePlutusRunnable @PlutusV1 (eraProtVerLow @Alonzo) $ Plutus $ PlutusBinary
    sbs
  where
     Just sbs =  SBS.toShort <$> decodeHex "581d010000322322223253330073370e00200a2930b1bad002375a002ae681"


toNameless ::
      UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun () ->
      UPLC.Program UPLC.DeBruijn DefaultUni DefaultFun ()
toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn

te = let
    decoded = decodePlutusRunnable @PlutusV2 (eraProtVerLow @Babbage) $ Plutus $ PlutusBinary $ SBS.toShort (fromJust (decodeHex "590b6d010000323233223232323232323232323232323232332232323232323232323232323232323232332232323222323253353232350012233016330134910243300032323335530121200135019501723500122333553015120013501c501a2350012233350012330364800000488cc0dc0080048cc0d800520000013300b002001350032222222222220083332233550020012001301012001335018223355301112001235001223355025002335530141200123500122335502800233350012330394800000488cc0e80080048cc0e400520000013300b002001501f3355301412001235001223233502233550240013350223355024003002502350233500622333350012326320313357389201024c680003120012326320313357389201024c68000312326320313357389201024c680003133501e335502048810048009407ccc04d2401024331003500222222222222233355301c120013501a501d25335333573466e3d2214066323536353530303764383961313435396663306232393333653839383838613739353564633835363065303434306362333731613563306134356435393637003500122350022200203c03b1333573466e1d20043500122350022200103c03b103b00c33573892012353637269707420636f6e74657874206465636f646564207375636365737366756c6c79003333573466e1cd55cea801a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4094098d5d0a80619a8128131aba1500b33502502735742a014666aa052eb940a0d5d0a804999aa814bae502835742a01066a04a0626ae85401cccd540a40c9d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40f1d69aba15002303d357426ae8940088c98c80fccd5ce02001f81e89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a81e3ad35742a004607a6ae84d5d1280111931901f99ab9c04003f03d135573ca00226ea8004d5d09aba2500223263203b33573807807607226aae7940044dd50009aba1500533502575c6ae854010ccd540a40b88004d5d0a801999aa814bae200135742a00460606ae84d5d1280111931901b99ab9c038037035135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00660406ae84d5d1280191931901499ab9c02a02902733573892011d52656465656d6572206465636f646564207375636365737366756c6c79003333573466e1cd55ce9baa0044800080a08c98c80a0cd5ce014814013081389931901399ab9c49010350543500027135573ca00226ea800488cd54c020480048d400488cd54070008ccd40048cd54c030480048d400488cd54080008d5403400400488ccd5540200400080048cd54c030480048d400488cd54080008d54030004004ccd55400c02c008004444888ccd54c01048005405ccd54c020480048d400488cd54070008d54024004ccd54c0104800488d4008894cd4ccd54c03448004d402d40388d400488cc028008014018400c4cd406c01000d4060004cd54c020480048d400488c8cd5407400cc004014c8004d540b4894cd40044d5402800c884d4008894cd4cc03000802044888cc0080280104c01800c008c8004d5409888448894cd40044008884cc014008ccd54c01c480040140100044484888c00c0104484888c00401048cd402c88ccd400c88008008004d400488004c8004d540888844894cd400454050884cd4054c010008cd54c01848004010004c8004d5408488448894cd40044d400c88004884ccd401488008c010008ccd54c01c48004014010004448cc004894cd40084084400407888ccd5cd19b8f00200101f01e225335001101e133573800403a640026aa03a442244a66a0022a01e44266a020600800466aa600c240020080022246600200403644a66a004200220342466a00444666a006440040040026a0024400224424660020060042464460046eb0004c8004d5406488cccd55cf80092805119a80498021aba100230033574400402a464646666ae68cdc39aab9d5002480008cc8848cc00400c008c034d5d0a80118029aba135744a004464c6402a66ae7005805404c4d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a0080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c058d5d0a80119a80800a9aba135744a004464c6403466ae7006c0680604d55cf280089baa00135742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263201c33573803a03803403203026aae7540044dd50009aba1500233500c75c6ae84d5d1280111931900b19ab9c017016014135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80b11191999aab9f0022500823350073355009300635573aa004600a6aae794008c010d5d100180989aba10011122002122122330010040031122123300100300212232323333573466e1d4005200023212230020033005357426aae79400c8cccd5cd19b8750024800884880048c98c8040cd5ce00880800700689aab9d500113754002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c8040cd5ce00880800700680600589aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8030cd5ce00680600509aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401466ae7002c0280204dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402666ae7005004c04404003c03803403002c4d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200c33573801a01801401226aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900499ab9c00a009007006135573aa00226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263200a33573801601401000e00c26aae7540044dd50009191999ab9a3370ea0029001100491999ab9a3370ea0049000100491931900319ab9c007006004003135573a6ea800526120014901035054310022333573466e1c00800401000c48800848800488cdc0001000889191800800911980198010010009"))
    in case decoded of
        Left err -> print err
        Right (PlutusRunnable sce) -> do
            let ScriptNamedDeBruijn p = deserialisedScript $ sce
                np = toNameless p
            print $ validatorHash  $ (`Versioned` PlutusV2) $ coerce $ serialiseUPLC $ np


-- >>> te
-- 384ec5f5b05cd1d3454f066ff4d84777eb01581856bb77362924741d



-- {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Apply () (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (Force () (Apply () (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 8})) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 1})) (Var () (DeBruijn {dbnIndex = 1}))) (Var () (DeBruijn {dbnIndex = 3})))) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (Force () (Apply () (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 17})) (Var () (DeBruijn {dbnIndex = 1}))) (Delay () (Constant () (Some (ValueOf bool False))))) (Delay () (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (Force () (Apply () (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 17})) (Apply () (Apply () (Builtin () EqualsData) (Var () (DeBruijn {dbnIndex = 6}))) (Var () (DeBruijn {dbnIndex = 1})))) (Delay () (Constant () (Some (ValueOf bool True))))) (Delay () (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 5})) (Var () (DeBruijn {dbnIndex = 5}))) (Var () (DeBruijn {dbnIndex = 2}))))))) (Apply () (Var () (DeBruijn {dbnIndex = 17})) (Var () (DeBruijn {dbnIndex = 2}))))) (Apply () (Var () (DeBruijn {dbnIndex = 21})) (Var () (DeBruijn {dbnIndex = 2}))))) (Apply () (Var () (DeBruijn {dbnIndex = 19})) (Var () (DeBruijn {dbnIndex = 1}))))))))))) (Apply () (Apply () (Builtin () ConstrData) (Constant () (Some (ValueOf integer 0)))) (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 15})) (Apply () (Builtin () BData) (Var () (DeBruijn {dbnIndex = 3})))) (Constant () (Some (ValueOf list (data) []))))))) (Apply () (Builtin () UnMapData) (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 7})) (Apply () (Var () (DeBruijn {dbnIndex = 9})) (Var () (DeBruijn {dbnIndex = 1})))) (Constant () (Some (ValueOf integer 9))))))) (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 6})) (Apply () (Var () (DeBruijn {dbnIndex = 8})) (Var () (DeBruijn {dbnIndex = 2})))) (Constant () (Some (ValueOf integer 0)))))) (Apply () (Builtin () UnBData) (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 5})) (Apply () (Var () (DeBruijn {dbnIndex = 7})) (Var () (DeBruijn {dbnIndex = 4})))) (Constant () (Some (ValueOf integer 0))))))) (Delay () (Constant () (Some (ValueOf unit ()))))) (Delay () (Error ()))))))))) (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 1})) (Var () (DeBruijn {dbnIndex = 1}))) (Constant () (Some (ValueOf integer 0)))))) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (Apply () (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 6})) (Apply () (Apply () (Builtin () EqualsInteger) (Var () (DeBruijn {dbnIndex = 1}))) (Var () (DeBruijn {dbnIndex = 3})))) (Var () (DeBruijn {dbnIndex = 11}))) (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (Apply () (Apply () (Apply () (Var () (DeBruijn {dbnIndex = 5})) (Var () (DeBruijn {dbnIndex = 5}))) (Apply () (Apply () (Builtin () AddInteger) (Var () (DeBruijn {dbnIndex = 4}))) (Constant () (Some (ValueOf integer 1))))) (Apply () (Var () (DeBruijn {dbnIndex = 13})) (Var () (DeBruijn {dbnIndex = 1})))) (Var () (DeBruijn {dbnIndex = 2}))))) (Var () (DeBruijn {dbnIndex = 2}))))))))) (LamAbs () (DeBruijn {dbnIndex = 0}) (Apply () (Var () (DeBruijn {dbnIndex = 4})) (Apply () (Builtin () UnConstrData) (Var () (DeBruijn {dbnIndex = 1}))))))) (Force () (Builtin () IfThenElse)))) (Force () (Force () (Builtin () FstPair))))) (Force () (Force () (Builtin () SndPair))))) (Force () (Force () (Builtin () ChooseList))))) (Force () (Builtin () MkCons)))) (Force () (Builtin () HeadList)))) (Force () (Builtin () TailList))) (Constant () (Some (ValueOf data (Constr 121 [B "\146.\251\221\RS\222\210$\236\f=\178cD\243\166\SYN\181\129\SYN\216\137\182\133\236\249yd"]))))}














-- >>> evaluatePlutusRunnableBudget @PlutusV1 (eraProtVerLow @Alonzo) Verbose (getCostModelEvaluationContext $ testingCostModel PlutusV2) run [I 42, I 2, I 42, I 4]
-- ([],Right (ExBudget {exBudgetCPU = ExCPU 1273303, exBudgetMemory = ExMemory 4066}))


-- >>> evaluatePlutusRunnableBudget @PlutusV1 (eraProtVerLow @Alonzo) Verbose (getCostModelEvaluationContext $ testingCostModel PlutusV2) run [I 42, I 2, I 3, I 4]
-- ([],Left (CekError An error has occurred:  User error:
-- The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.))
