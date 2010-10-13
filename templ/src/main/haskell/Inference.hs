module Inference where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Expression
import InferenceState

type Env = Map.Map String Type

principal :: Exp -> Either Problem Type
principal e = runISM monad
    where
        nowhere = ((0, 0), (0, 0))
        monad = do 
            let env = Map.empty
            t <- infer nowhere env e
            tv <- newTypeVar
            unify nowhere t (TFunction tv TAtom)
            apply t


infer :: Segment -> Env -> Exp -> ISM Type
infer s env (EVar i) = do 
  case Map.lookup i env of
    Just t -> return t
    Nothing -> problem s ("Undefined variable '" ++ i ++ "'")       
infer s env (EText _) = return TAtom
infer s env (EConcat e1 e2) = do
  t1 <- infer s env e1
  unify s t1 TAtom
  t2 <- infer s env e2
  unify s t2 TAtom
  return TAtom
infer s env (EChoice e1 e2) = do
  t1 <- infer s env e1
  t2 <- infer s env e2
  unify s t1 t2
  return t1
infer s env (ELoop i e1 e2) = do
  tv <- newTypeVar
  t1 <- infer s env e1
  unify s t1 (TList tv)
  let env' = Map.insert i tv env
  infer s env' e2
infer s env (EField e i) = do
  rv <- newRowVar
  tv <- newTypeVar
  t <- infer s env e
  unify s t (TRecord (RField i tv rv))
  return tv
infer s env (EFunction i e) = do
  tv <- newTypeVar
  t <- infer s (Map.insert i tv env) e
  return (TFunction tv t)
infer s env (EAt s' e) = infer s' env e


unify :: Segment -> Type -> Type -> ISM ()
unify s t1 t2 = do
  t1' <- apply t1
  t2' <- apply t2
  unifyType t1' t2'
  where
    unifyType (TVar i) t = bind s i t
    unifyType t (TVar i) = bind s i t
    unifyType (TAtom) (TAtom) = return ()
    unifyType (TList t) (TList t') = unifyType t t'
    unifyType (TFunction t1 t2) (TFunction t1' t2') = do
      unifyType t1 t1'
      -- Applies the substitution to t2 and t2'
      unify s t2 t2'
    unifyType (TRecord r1) (TRecord r2) = unifyRow r1 r2
    unifyType t t' = problem s ("Cannot unify " ++ pretty t ++ " and " ++ pretty t')
    unifyRow (RVar i) r = bindRow s i r
    unifyRow r (RVar i) = bindRow s i r
    unifyRow f@(RField _ _ _) f'@(RField _ _ _) = do
      let (m, rv) = fromRow f
      let (m', rv') = fromRow f'
      let l = Map.elems $ Map.intersectionWith (,) m m'
      mapM_ (uncurry $ unify s) l
      let mDiffM' = Map.difference m m'
      let m'DiffM = Map.difference m' m
      rv'' <- newRowVar
      unifyRow rv (toRow m'DiffM rv'')
      -- Applies the substitution to rv' and rv''
      unify s (TRecord rv') (TRecord (toRow mDiffM' rv''))
    fromRow (RVar i) = (Map.empty, RVar i)
    fromRow (RField i t r) = 
      let (m, rv) = fromRow r
      in (Map.insert i t m, rv)
    toRow m r = Map.foldWithKey RField r m

