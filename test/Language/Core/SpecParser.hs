module Language.Core.SpecParser where

import qualified Data.ByteString as B
import Language.Core.Lexer
import Language.Core.Parser
import Language.Core.Syntax
import Test.Tasty.Hspec hiding (Failure, Success)

spec_parser :: Spec
spec_parser = do
  describe "lexes and parse" $ do
    it "parses lvl1_rdgS" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}"
        , "lvl1_rdgS :: GHC.Prim.Addr#"
        , "[GblId, Caf=NoCafRefs, Unf=OtherCon []]"
        , "lvl1_rdgS = \"error\"#"
        ]
      shouldBe simpl $ Right $ NonRec
        (Token "lvl1_rdgS")
        ( Func
          (TyConApp (QToken "GHC.Prim" "Addr#") [])
          [("IdType", "GlobalId"), ("Caf", "NoCafRefs"), ("Unf", "OtherCon []")]
          (Lit (MachStr "error"))
        )

    it "parses lvl2_rdgS" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}"
        , "lvl2_rdgT :: [Char]"
        , "[GblId]"
        , "lvl2_rdgT = GHC.CString.unpackCString# lvl1_rdgS"
        ]
      shouldBe simpl $ Right $ NonRec
        (Token "lvl2_rdgT")
        ( Func
          (TyConApp (Token "List") [TyConApp (Token "Char") []])
          [("IdType", "GlobalId")]
          ( App (Var (QToken "GHC.CString" "unpackCString#"))
                (Var (Token "lvl1_rdgS"))
          )
        )

    it "parses lvl2_rdgS" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 4, types: 0, coercions: 0, joins: 0/0}"
        , "lvl13_rdh4 :: GHC.Stack.Types.CallStack"
        , "[GblId, Str=m2, Unf=OtherCon []]"
        , "lvl13_rdh4"
        , "  = GHC.Stack.Types.PushCallStack"
        , "     lvl2_rdgT lvl12_rdh3 GHC.Stack.Types.EmptyCallStack"
        ]
      shouldBe simpl $ Right $ NonRec
        (Token "lvl13_rdh4")
        ( Func
          (TyConApp (QToken "GHC.Stack.Types" "CallStack") [])
          [("IdType", "GlobalId"), ("Str", "m2"), ("Unf", "OtherCon []")]
          ( App
            ( App
              ( App (Var (QToken "GHC.Stack.Types" "PushCallStack"))
                    (Var (Token "lvl2_rdgT"))
              )
              (Var (Token "lvl12_rdh3"))
            )
            (Var (QToken "GHC.Stack.Types" "EmptyCallStack"))
          )
        )

    it "parses lvl10_rdh1" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}"
        , "lvl10_rdh1 :: Int"
        , "[GblId, Caf=NoCafRefs, Str=m, Unf=OtherCon []]"
        , "lvl10_rdh1 = GHC.Types.I# 16#"
        ]
      shouldBe simpl $ Right $ NonRec
        (Token "lvl10_rdh1")
        ( Func
          (TyConApp (Token "Int") [])
          [ ("IdType", "GlobalId")
          , ("Caf"   , "NoCafRefs")
          , ("Str"   , "m")
          , ("Unf"   , "OtherCon []")
          ]
          (App (Var (QToken "GHC.Types" "I#")) (Lit (LitNumber 16)))
        )

