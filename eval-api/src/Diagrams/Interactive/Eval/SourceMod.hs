{-# LANGUAGE RecordWildCards #-}

-- | Source modifications
module Diagrams.Interactive.Eval.SourceMod where

import Control.Applicative
import Data.Monoid
import DynFlags
import GHC
import GhcMonad
import HsExpr
import OccName
import RdrName
import SrcLoc

data SourceMod = SourceMod
    { getImportsMod :: Endo [LImportDecl RdrName]
    , getDeclsMod   :: Endo [LHsDecl RdrName]
    }

instance Monoid SourceMod where
    mempty = SourceMod mempty mempty
    mappend (SourceMod a b) (SourceMod c d) = SourceMod (a <> c) (b <> d)


modifyModule :: SourceMod -> HsModule RdrName -> HsModule RdrName
modifyModule SourceMod{..} m@HsModule{..} =
    m { hsmodImports = newImports
      , hsmodDecls   = newDecls
      }
  where
    newImports   = appEndo getImportsMod hsmodImports
    newDecls     = appEndo getDeclsMod   hsmodDecls


removeSig :: OccName -> SourceMod
removeSig target = SourceMod mempty (Endo (map modify))
  where
    modify (L l1 (SigD (TypeSig names (L l2 hst))))
          | Just (name, rest) <- containsOcc target names
          = L l1 (SigD (TypeSig rest  (L l2 hst)))
    modify x = x

addImportSimple :: String -> SourceMod
addImportSimple mname = SourceMod (Endo (imp:)) mempty
  where
    imp = noLoc $ simpleImportDecl $ mkModuleName mname

replaceDefinition :: OccName
                  -> (HsBind RdrName -> HsBind RdrName) 
                  -> SourceMod
replaceDefinition target modify = SourceMod mempty (Endo (map go))
  where
    go (L l (ValD d)) = L l (ValD (modify d))
    go x              = x
    
modifyDefinition :: OccName -> (LHsExpr RdrName -> LHsExpr RdrName) -> SourceMod
modifyDefinition target modify = SourceMod mempty (Endo (map go))
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
                 
