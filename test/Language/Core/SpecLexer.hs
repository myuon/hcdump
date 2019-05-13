module Language.Core.SpecLexer where

import Language.Core.Lexer
import Test.Tasty.Hspec hiding (Failure, Success)

spec_lexer :: Spec
spec_lexer = do
  describe "alexScanTokens" $ do
    it "lexes a line comment"
      $          alexScanTokens "-- foobar"
      `shouldBe` [TokenLineComment "-- foobar"]
    it "lexes a primitive type"
      $          alexScanTokens "Addr#"
      `shouldBe` [TokenCon "Addr#"]
    it "lexes a type declaration"
      $          alexScanTokens "lvl2_rdgT :: [Char]"
      `shouldBe` [ TokenVar "lvl2_rdgT"
                 , TokenDColon
                 , TokenLBracket
                 , TokenCon "Char"
                 , TokenRBracket
                 ]
