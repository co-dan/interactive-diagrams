{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}

-- | Source modifications
module Diagrams.Interactive.Eval.SourceMod where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Monoid
import DynFlags
import GHC
import GhcMonad
import HsExpr
import OccName
import RdrName
import SrcLoc

type Decls = ([LImportDecl RdrName], [LHsDecl RdrName])
newtype SourceMod a = SourceMod (State Decls a)
                    deriving (Monad)
-- data SourceMod a = SourceMod
--     { getImportsMod :: Endo [LImportDecl RdrName]
--     , getDeclsMod   :: Endo [LHsDecl RdrName]
--     , getValue      :: a
--     }

instance Monoid (SourceMod ()) where
    mempty = fromEndo mempty mempty
    mappend (SourceMod s1) (SourceMod s2) =
        SourceMod $ s1 >> s2

-- instance Monad SourceMod where
--     return a = SourceMod mempty mempty a
--     (SourceMod{..}) >>= f = f getValue

modifyModule :: SourceMod a -> HsModule RdrName -> HsModule RdrName
modifyModule (SourceMod s) m@HsModule{..} =
    m { hsmodImports = newImports
      , hsmodDecls   = newDecls
      }
  where
    (newImports, newDecls) = execState s (hsmodImports, hsmodDecls)

ignoreValue :: SourceMod a -> SourceMod ()
ignoreValue = (=<<) (const (return ()))

fromEndo :: Endo [LImportDecl RdrName] -> Endo [LHsDecl RdrName] -> SourceMod ()
fromEndo e1 e2 = SourceMod $ state $ \(imp,decls) ->
    ((), (appEndo e1 imp, appEndo e2 decls))

removeSig :: OccName -> SourceMod (Maybe (HsType RdrName))
removeSig target = SourceMod $ state $ \(inc,decls) ->
    let (ans, decls') = go decls
    in (ans, (inc, decls'))
  where    
    go ((L l1 (SigD (TypeSig names (L l2 hst)))):xs)
          | Just (name, rest) <- containsOcc target names
          = (Just hst, ((L l1 (SigD (TypeSig rest  (L l2 hst)))):xs))
    go (x:xs) = let (ans, lst) = go xs in (ans, x:lst)
    go []     = (Nothing, [])

addImportSimple :: String -> SourceMod ()
addImportSimple mname = fromEndo (Endo (imp:)) mempty
  where
    imp = noLoc $ simpleImportDecl $ mkModuleName mname

addImportSimpleQual :: String -> SourceMod ()
addImportSimpleQual mname = fromEndo (Endo (imp:)) mempty
  where
    imp = noLoc $ (simpleImportDecl (mkModuleName mname))
                { ideclQualified = True }

removeImport :: ModuleName -> SourceMod (Maybe (ImportDecl RdrName))
removeImport target = SourceMod $ state $ \(imps, decls) ->
    let (ans, imps') = go imps
    in (ans, (imps', decls))
  where
    go ((L l idecl):xs)
        | unLoc (ideclName idecl) == target
        = (Just idecl, xs)
    go (x:xs) = let (ans, lst) = go xs in (ans, x:lst)
    go []     = (Nothing, [])


replaceDefinition :: OccName
                  -> (HsBind RdrName -> HsBind RdrName) 
                  -> SourceMod ()
replaceDefinition target modify = fromEndo mempty (Endo (map go))
  where
    go (L l (ValD d)) =
        let relevant = case d of
                FunBind{..} -> rdrNameOcc (unLoc fun_id) == target
                VarBind{..} -> rdrNameOcc var_id == target
                _           -> False
        in
         if relevant
         then L l (ValD (modify d))
         else L l (ValD d)
    go x              = x
    
modifyDefinition :: OccName -> (LHsExpr RdrName -> LHsExpr RdrName) -> SourceMod ()
modifyDefinition target modify = fromEndo mempty (Endo (map go)) 
  where
    go (L l (ValD f@(FunBind{..})))
      | target == rdrNameOcc (unLoc fun_id)
      = let alts = mg_alts fun_matches
            alts' = map modifyMatch alts
        in L l (ValD (f
                         { fun_matches = fun_matches
                           { mg_alts = alts' }}))
    go x = x

    modifyMatch :: LMatch RdrName (LHsExpr RdrName)
                -> LMatch RdrName (LHsExpr RdrName)
    modifyMatch (L l1 (Match lpat lty rs@(GRHSs{..}))) =
        let f (L l1 (GRHS guards body)) =
               L l1 (GRHS guards (modify body))
        in L l1 (Match lpat lty (rs
                                  { grhssGRHSs = map f grhssGRHSs }))                

containsOcc :: OccName
            -> [Located RdrName]
            -> Maybe (Located RdrName, [Located RdrName])
containsOcc name = go [] 
  where is (L _ x) = rdrNameOcc x == name
        go acc []     = Nothing
        go acc (x:xs)
          | is x 
          = Just (x, (reverse acc)++xs)
  
          | otherwise
          = go (x:acc) xs
                 
