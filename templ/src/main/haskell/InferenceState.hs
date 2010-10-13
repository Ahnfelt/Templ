module InferenceState (ISM, newTypeVar, newRowVar, apply, applyRow, 
                       problem, bind, bindRow, Problem, runISM) where
    
import Control.Monad.State
import Control.Monad.Error
import Expression
import qualified Data.Map as Map
import qualified Data.Set as Set


type TypeVarSubst = Map.Map String Type
type TypeVarCounter = Int
type RowVarSubst = Map.Map String Row
type RowVarCounter = Int

type ISM a = ErrorT Problem (State Sack) a

data Sack = Sack (TypeVarSubst, TypeVarCounter, RowVarSubst, RowVarCounter)


data Problem = Problem (Segment, String)

initSack = Sack (Map.empty, 0, Map.empty, 0)

runISM :: ISM a -> Either Problem a
runISM m = fst $ runState (runErrorT m) initSack

instance Error Problem where
    strMsg t = Problem (((-1, -1), (-1, -1)), t)

instance Show Problem where
    show (Problem (((l, c), (l', c')), t)) = 
        "Error at line " ++ show l ++ ", column " ++ show c ++ 
        (if l == l' 
            then if c == c' then "" else " to " ++ show c' 
            else " to line " ++ show l' ++ ", column " ++ show c') ++
        ":\n" ++ t

problem :: Segment -> String -> ISM a
problem s t = do
    throwError (Problem (s, t))


newTypeVar :: ISM Type
newTypeVar = do
  Sack (tvs, tvc, rvs, rvc) <- get
  put $ Sack (tvs, tvc+1, rvs, rvc)
  return $ TVar ("TV" ++ show tvc)

getTypeVarSubst :: ISM TypeVarSubst
getTypeVarSubst = do 
  Sack (tvs, _, _, _) <- get
  return tvs


getRowVarSubst :: ISM RowVarSubst
getRowVarSubst = do 
  Sack (_, _, rvs, _) <- get
  return rvs

setTypeVarSubst :: TypeVarSubst -> ISM ()
setTypeVarSubst tvs = do 
  Sack (_, tvc, rvs, rvc) <- get
  put $ Sack (tvs, tvc, rvs, rvc)

setRowVarSubst :: RowVarSubst -> ISM ()
setRowVarSubst rvs = do 
  Sack (tvs, tvc, _, rvc) <- get
  put $ Sack (tvs, tvc, rvs, rvc)


newRowVar :: ISM Row
newRowVar = do
  Sack (tvs, tvc, rvs, rvc) <- get
  put $ Sack (tvs, tvc, rvs, rvc+1)
  return $ RVar ("RV" ++ show rvc)



apply :: Type -> ISM Type
apply (TAtom) = return TAtom
apply (TList t) = do 
  t' <- apply t
  return (TList t')
apply (TVar i) = do
  s <- getTypeVarSubst
  case Map.lookup i s of
    Just t -> apply t
    Nothing -> return (TVar i)
apply (TRecord r) = do
  r' <- applyRow r
  return $ TRecord r'
apply (TFunction t1 t2) = do
  t1' <- apply t1
  t2' <- apply t2
  return $ TFunction t1' t2'
-- More cases

applyRow :: Row -> ISM Row
applyRow (RField i t r) = do
  t' <- apply t
  r' <- applyRow r
  return $ RField i t' r'
applyRow (RVar i) = do
  s <- getRowVarSubst
  case Map.lookup i s of
    Just r -> applyRow r
    Nothing -> return $ RVar i
  

bind :: Segment -> String -> Type -> ISM ()
bind s i (TVar i') | i == i' = return ()
bind s i t = do
  if i `Set.member` (freeTypeVars t)
   then problem s ("Occur check failed: '" ++ i ++ "' occurs in " ++ show t)
   else do
     s <- getTypeVarSubst
     setTypeVarSubst (Map.insert i t s)

bindRow :: Segment -> String -> Row -> ISM ()
bindRow s i (RVar i') | i == i' = return ()
bindRow s i r = do 
  if i `Set.member` (freeRowVars r)
   then problem s ("Occur check failed: '" ++ i ++ "' occurs in " ++ show r)
   else do
     s <- getRowVarSubst
     setRowVarSubst (Map.insert i r s)


freeTypeVars :: Type -> Set.Set String
freeTypeVars (TAtom) = Set.empty
freeTypeVars (TVar i) = Set.singleton i
freeTypeVars (TList t) = freeTypeVars t
freeTypeVars (TRecord (RField i t r)) = Set.union (freeTypeVars t) (freeTypeVars $ TRecord r)
freeTypeVars (TRecord _) = Set.empty
freeTypeVars (TFunction t t') = Set.union (freeTypeVars t) (freeTypeVars t')

freeRowVars :: Row -> Set.Set String
freeRowVars (RVar i) = Set.singleton i
freeRowVars (RField i t r) = Set.union (freeTypeRowVars t) (freeRowVars r)
    where
      freeTypeRowVars (TAtom) = Set.empty
      freeTypeRowVars (TVar i) = Set.empty
      freeTypeRowVars (TList t) = freeTypeRowVars t
      freeTypeRowVars (TRecord r) = freeRowVars r
      freeTypeRowVars (TFunction t t') = Set.union (freeTypeRowVars t) (freeTypeRowVars t')

