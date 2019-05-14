module Language.Core.SpecParser where

import qualified Data.ByteString as B
import Language.Core.Lexer
import Language.Core.Parser
import Language.Core.Syntax
import Test.Tasty.Hspec hiding (Failure, Success)

spec_parser :: Spec
spec_parser = do
  describe "lexes and parse" $ do
    it "parses a function declaration" $ do
      simpl <- parseByteString $ B.concat
        [ "-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}"
        , "lvl1_rdgS :: GHC.Prim.Addr#"
        , "[GblId, Caf=NoCafRefs, Unf=OtherCon []]"
        , "lvl1_rdgS = \"error\"#"
        ]
      simpl
        `shouldBe` NonRec "lvl1_rdgS" (Func (TyVarTy "lvl1_rdgS") "" (Lit ()))
