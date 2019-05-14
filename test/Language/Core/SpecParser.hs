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
      shouldBe simpl $ NonRec
        (Token "lvl1_rdgS")
        (Func (TyConApp (QToken "GHC.Prim" "Addr#") []) "NoCafRefs" (Lit ()))
    it "parses lvl2_rdgS" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}"
        , "lvl2_rdgT :: [Char]"
        , "[GblId]"
        , "lvl2_rdgT = GHC.CString.unpackCString# lvl1_rdgS"
        ]
      shouldBe simpl $ NonRec
        (Token "lvl2_rdgT")
        ( Func
          (TyConApp (Token "List") [TyConApp (Token "Char") []])
          ""
          ( App (Var (QToken "GHC.CString" "unpackCString#"))
                (Var (Token "lvl1_rdgS"))
          )
        )
