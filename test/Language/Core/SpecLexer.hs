module Language.Core.SpecLexer where

import Language.Core.Lexer
import Test.Tasty.Hspec hiding (Failure, Success)

spec_lexer :: Spec
spec_lexer = do
  describe "alexScanTokens" $ do
    it "lexes a line comment" $ do
      alexScanTokens "-- foobar" `shouldBe` [TokenLineComment "-- foobar"]
