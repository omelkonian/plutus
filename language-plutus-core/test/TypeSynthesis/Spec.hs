{-# LANGUAGE OverloadedStrings #-}

module TypeSynthesis.Spec
    ( test_typecheck
    ) where

import           Language.PlutusCore
import           Language.PlutusCore.Pretty
import           Language.PlutusCore.StdLib.Everything

import           Control.Monad.Except
import           Test.Tasty
import           Test.Tasty.HUnit

kindcheckQuoted
    :: (MonadError (Error ()) m, MonadQuote m)
    => Quote (Type TyName ()) -> m (Type TyName ())
kindcheckQuoted getType = do
    ty <- liftQuote getType
    _ <- annotateType ty >>= kindCheck (TypeCheckCfg 1000 True)
    return ty

typecheckQuoted
    :: (MonadError (Error ()) m, MonadQuote m)
    => Quote (Term TyName Name ()) -> m (Term TyName Name ())
typecheckQuoted getTerm = do
    term <- liftQuote getTerm
    _ <- annotateTerm term >>= typecheckTerm (TypeCheckCfg 1000 True)
    return term

-- | Assert a 'Type' is well-kinded.
assertWellKinded :: HasCallStack => Quote (Type TyName ()) -> Assertion
assertWellKinded getTy = case runExcept . runQuoteT $ kindcheckQuoted getTy of
    Left  err -> assertFailure $ "Kind error: " ++ prettyPlcCondensedErrorClassicString err
    Right _   -> return ()

-- | Assert a 'Term' is well-typed.
assertWellTyped :: HasCallStack => Quote (Term TyName Name ()) -> Assertion
assertWellTyped getTerm = case runExcept . runQuoteT $ typecheckQuoted getTerm of
    Left  err -> assertFailure $ "Type error: " ++ prettyPlcCondensedErrorClassicString err
    Right _   -> return ()

-- | Assert a term is ill-typed.
assertIllTyped :: HasCallStack => Quote (Term TyName Name ()) -> Assertion
assertIllTyped getTerm = case runExcept . runQuoteT $ typecheckQuoted getTerm of
    Right term -> assertFailure $ "Well-typed: " ++ prettyPlcCondensedErrorClassicString term
    Left  _    -> return ()

test_typecheckStdLib :: TestTree
test_typecheckStdLib =
    foldStdLib
        testGroup
        (\name -> testCase name . assertWellKinded)
        (\name -> testCase name . assertWellTyped)

-- | Self-application. An example of ill-typed term.
--
-- > /\ (A :: *) -> \(x : A) -> x x
getBuiltinSelfApply :: Quote (Term TyName Name ())
getBuiltinSelfApply = do
    a <- freshTyName () "a"
    x <- freshName () "x"
    return
        . TyAbs () a (Type ())
        . LamAbs () x (TyVar () a)
        . Apply () (Var () x)
        $ Var () x

test_typecheckIllTyped :: TestTree
test_typecheckIllTyped =
    testCase "ill-typed" $
        foldMap assertIllTyped
            [ getBuiltinSelfApply
            ]

test_typecheck :: TestTree
test_typecheck =
    testGroup "typecheck"
        [ test_typecheckStdLib
        , test_typecheckIllTyped
        ]