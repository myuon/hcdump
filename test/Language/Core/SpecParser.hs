module Language.Core.SpecParser where

import qualified Data.ByteString as B
import Language.Core.Lexer
import Language.Core.Parser
import Language.Core.Syntax
import Test.Tasty.Hspec hiding (Failure, Success)

spec_parser :: Spec
spec_parser = do
  describe "lexes and parse" $ do
    it "parses arrow and unboxed_tuples" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}"
        , "lvl1_rdgS :: x -> GHC.Prim.State# GHC.Prim.RealWorld -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #)"
        , "[GblId, Caf=NoCafRefs, Unf=OtherCon []]"
        , "lvl1_rdgS = 0"
        ]
      shouldBe simpl $ Right
        ( NonRec
          (Token "lvl1_rdgS")
          ( Func
            ( TyConApp
              (Token "(->)")
              [ TyVarTy (Token "x")
              , TyConApp
                (Token "(->)")
                [ AppTy (TyConApp (QToken "GHC.Prim" "State#") [])
                        (TyConApp (QToken "GHC.Prim" "RealWorld") [])
                , TyConApp
                  (Token "(# .. #)")
                  [ AppTy (TyConApp (QToken "GHC.Prim" "State#") [])
                          (TyConApp (QToken "GHC.Prim" "RealWorld") [])
                  , TyConApp (Token "()") []
                  ]
                ]
              ]
            )
            ( IdInfo
              { getIdInfo = [ ("IdType", "GlobalId")
                            , ("Caf"   , "NoCafRefs")
                            , ("Unf"   , "OtherCon []")
                            ]
              }
            )
            (Lit (LitNumber 0 False))
          )
        )

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
          ( IdInfo
            [ ("IdType", "GlobalId")
            , ("Caf"   , "NoCafRefs")
            , ("Unf"   , "OtherCon []")
            ]
          )
          (Lit (MachStr "error" True))
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
          (TyConApp (Token "[]") [TyConApp (Token "Char") []])
          (IdInfo [("IdType", "GlobalId")])
          ( App (Var (QToken "GHC.CString" "unpackCString#"))
                (Var (Token "lvl1_rdgS"))
          )
        )

    it "parses lvl13_rdh4" $ do
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
          ( IdInfo
            [("IdType", "GlobalId"), ("Str", "m2"), ("Unf", "OtherCon []")]
          )
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
          ( IdInfo
            [ ("IdType", "GlobalId")
            , ("Caf"   , "NoCafRefs")
            , ("Str"   , "m")
            , ("Unf"   , "OtherCon []")
            ]
          )
          (App (Var (QToken "GHC.Types" "I#")) (Lit (LitNumber 16 True)))
        )

    it "parses lvl33_rhdp" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 7, types: 2, coercions: 0, joins: 0/0}"
        , "lvl33_rdhp :: Data.Vector.Fusion.Util.Id Int"
        , "[GblId, Str=x]"
        , "lvl33_rdhp"
        , "  = Data.Vector.Internal.Check.$werror"
        , "      @ (Data.Vector.Fusion.Util.Id Int)"
        , "      (GHC.CString.unpackCString# lvl31_rdhn)"
        , "      291#"
        , "      (GHC.CString.unpackCString# lvl32_rdho)"
        , "      Data.Vector.Fusion.Stream.Monadic.emptyStream"
        ]
      shouldBe simpl $ Right
        ( NonRec
          (Token "lvl33_rdhp")
          ( Func
            ( AppTy (TyConApp (QToken "Data.Vector.Fusion.Util" "Id") [])
                    (TyConApp (Token "Int") [])
            )
            (IdInfo {getIdInfo = [("IdType", "GlobalId"), ("Str", "x")]})
            ( App
              ( App
                ( App
                  ( App
                    ( App
                      (Var (QToken "Data.Vector.Internal.Check" "$werror"))
                      ( Type
                        ( AppTy
                          (TyConApp (QToken "Data.Vector.Fusion.Util" "Id") [])
                          (TyConApp (Token "Int") [])
                        )
                      )
                    )
                    ( App (Var (QToken "GHC.CString" "unpackCString#"))
                          (Var (Token "lvl31_rdhn"))
                    )
                  )
                  (Lit (LitNumber 291 True))
                )
                ( App (Var (QToken "GHC.CString" "unpackCString#"))
                      (Var (Token "lvl32_rdho"))
                )
              )
              (Var (QToken "Data.Vector.Fusion.Stream.Monadic" "emptyStream"))
            )
          )
        )

    it "parses :main" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}"
        , ":Main.main :: IO ()"
        , "[GblId,"
        , " Arity=1,"
        , " Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,"
        , "         WorkFree=True, Expandable=True,"
        , "         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)"
        , "         Tmpl= Main.main2"
        , "               `cast` (Sym (GHC.Types.N:IO[0] <()>_R)"
        , "                       :: (GHC.Prim.State# GHC.Prim.RealWorld"
        , "                           -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #))"
        , "                          ~R# IO ())}]"
        , ":Main.main"
        , "  = Main.main2"
        , "    `cast` (Sym (GHC.Types.N:IO[0] <()>_R)"
        , "            :: (GHC.Prim.State# GHC.Prim.RealWorld"
        , "                -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #))"
        , "               ~R# IO ())"
        ]
      shouldBe simpl $ Right
        ( NonRec
          (QToken "Main" ":main")
          ( Func
            (AppTy (TyConApp (Token "IO") []) (TyConApp (Token "()") []))
            ( IdInfo
              { getIdInfo = [ ("IdType", "GlobalId")
                            , ("Arity" , "1")
                            , ("Unf"   , "CoreUnfolding {...}")
                            ]
              }
            )
            ( Cast
              (Var (QToken "Main" "main2"))
              ( SymCo
                ( NthCo
                  Nominal
                  0
                  ( TyConAppCo
                    Nominal
                    (Token "IO")
                    [Refl Representational (TyConApp (Token "()") [])]
                  )
                )
              )
              ( TyConApp
                (Token "~R#")
                [ TyConApp
                  (Token "(->)")
                  [ AppTy (TyConApp (QToken "GHC.Prim" "State#") [])
                          (TyConApp (QToken "GHC.Prim" "RealWorld") [])
                  , TyConApp
                    (Token "(# .. #)")
                    [ AppTy (TyConApp (QToken "GHC.Prim" "State#") [])
                            (TyConApp (QToken "GHC.Prim" "RealWorld") [])
                    , TyConApp (Token "()") []
                    ]
                  ]
                , AppTy (TyConApp (Token "IO") []) (TyConApp (Token "()") [])
                ]
              )
            )
          )
        )

    it "parses lvl28_rdhk" $ do
      simpl <- parseByteString $ B.intercalate
        "\n"
        [ "-- RHS size: {terms: 10, types: 3, coercions: 0, joins: 0/0}"
        , "lvl28_rdhk :: GHC.Prim.Int# -> GHC.Prim.Int# -> Double"
        , "[GblId, Arity=2, Str=<B,U><B,U>x, Unf=OtherCon []]"
        , "lvl28_rdhk"
        , "  = \\ (x_a8pv :: GHC.Prim.Int#) (n#_a8qS :: GHC.Prim.Int#) ->"
        , "      Data.Vector.Internal.Check.$wcheckError"
        , "        @ Double"
        , "        lvl27_rdhj"
        , "        245#"
        , "        Data.Vector.Internal.Check.Bounds"
        , "        lvl26_rdhh"
        , "        (Data.Vector.Internal.Check.checkIndex_msg# x_a8pv n#_a8qS)"
        ]
      shouldBe simpl $ Right
        ( NonRec
          (Token "lvl28_rdhk")
          ( Func
            ( TyConApp
              (Token "(->)")
              [ TyConApp (QToken "GHC.Prim" "Int#") []
              , TyConApp
                (Token "(->)")
                [ TyConApp (QToken "GHC.Prim" "Int#") []
                , TyConApp (Token "Double")           []
                ]
              ]
            )
            ( IdInfo
              { getIdInfo = [ ("IdType", "GlobalId")
                            , ("Arity" , "2")
                            , ("Str"   , "<B,U><B,U>x")
                            , ("Unf"   , "OtherCon []")
                            ]
              }
            )
            ( Lam
              [ (Token "x_a8pv" , TyConApp (QToken "GHC.Prim" "Int#") [])
              , (Token "n#_a8qS", TyConApp (QToken "GHC.Prim" "Int#") [])
              ]
              ( App
                ( App
                  ( App
                    ( App
                      ( App
                        ( App
                          ( Var
                            (QToken "Data.Vector.Internal.Check" "$wcheckError")
                          )
                          (Type (TyConApp (Token "Double") []))
                        )
                        (Var (Token "lvl27_rdhj"))
                      )
                      (Lit (LitNumber 245 True))
                    )
                    (Var (QToken "Data.Vector.Internal.Check" "Bounds"))
                  )
                  (Var (Token "lvl26_rdhh"))
                )
                ( App
                  ( App
                    ( App
                      ( Var
                        (QToken "Data.Vector.Internal.Check" "checkIndex_msg#")
                      )
                      (Var (Token "x_a8pv"))
                    )
                    (Var (Token "n#"))
                  )
                  (Var (Token "_a8qS"))
                )
              )
            )
          )
        )