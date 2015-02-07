{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE TupleSections #-}

module Swearjure.Eval where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Generics.Fixplate (Mu(..))
import qualified Data.Map as M
import           Swearjure.AST
import           Swearjure.Errors


initEnv :: Env
initEnv = Toplevel M.empty
--initEnv = M.fromList $ map (,"clojure.core")
--          ["/", "+", "*", "+", "->", "->>", "<", ">", "<=", ">=", "=", "=="]
--          ++ [(">>", "swearjure.core"), ("<<", "swearjure.core")]

-- map from ns to ns
type EvalState = ReaderT Env (StateT Int (Except SwjError))

-- to begin with.
specials :: M.Map String a
specials = M.fromList
           [ ("fn*", undefined)
           , ("quote", undefined)
           , (".", undefined)
           , ("var", undefined)
           , ("&", undefined)
           ]

_quote :: Expr
_quote = Fix $ ESym Nothing "quote"

_unquote :: Expr
_unquote = Fix $ ESym (Just "clojure.core") "unquote"

_unquoteSplicing :: Expr
_unquoteSplicing = Fix $ ESym (Just "clojure.core") "unquote-splicing"

_seq :: Expr
_seq = Fix $ ESym (Just "clojure.core") "seq"

_concat :: Expr
_concat = Fix $ ESym (Just "clojure.core") "concat"

_apply :: Expr
_apply = Fix $ ESym (Just "clojure.core") "apply"

_list :: Expr
_list = Fix $ ESym (Just "clojure.core") "list"

_vector :: Expr
_vector = Fix $ ESym (Just "clojure.core") "vector"

_hashset :: Expr
_hashset = Fix $ ESym (Just "clojure.core") "hash-set"

_hashmap :: Expr
_hashmap = Fix $ ESym (Just "clojure.core") "hash-map"
