module Language.Core.SpecLexer where

import Language.Core.Lexer
import Test.Tasty.Hspec hiding (Failure, Success)

spec_lexer :: Spec
spec_lexer = do
  describe "alexScanTokens" $ do
    it "lexes a line comment"
      $          alexScanTokens "-- foobar"
      `shouldBe` [TokenLineComment "-- foobar"]

    it "lexes a qualified name"
      $          alexScanTokens "GHC.CString.unpackCString#"
      `shouldBe` [TokenVar "GHC.CString.unpackCString#"]

    it "lexes a type declaration"
      $          alexScanTokens "lvl2_rdgT :: [Char]"
      `shouldBe` [ TokenVar "lvl2_rdgT"
                 , TokenDColon
                 , TokenLBracket
                 , TokenCon "Char"
                 , TokenRBracket
                 ]

    it "lexes an integer"
      $          alexScanTokens "20100"
      `shouldBe` [TokenInteger "20100"]

    it "lexes a double"
      $          alexScanTokens "-8.20430"
      `shouldBe` [TokenDouble "-8.20430"]

    it "lexes a hashed type"
      $          alexScanTokens "Addr#"
      `shouldBe` [TokenCon "Addr#"]

    it "lexes a hashed number"
      $          alexScanTokens "19#"
      `shouldBe` [TokenInteger "19#"]

    it "lexes a string"
      $          alexScanTokens "\"foo\\nbar\""
      `shouldBe` [TokenStrLit "\"foo\\nbar\""]

    it "lexes a char" $ alexScanTokens "\'A\'" `shouldBe` [TokenCharLit "\'A\'"]

    it "lexes a special character containing symbol"
      $          alexScanTokens "n#_a8qS"
      `shouldBe` [TokenVar "n#_a8qS"]

--    it "lexes an operator" $ alexScanTokens "(<$>)" `shouldBe` [TokenVar "<$>"]
