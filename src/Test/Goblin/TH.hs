{-# LANGUAGE TemplateHaskell       #-}
module Test.Goblin.TH where

import           Control.Monad (foldM, forM, unless)
import           Data.Typeable (Typeable)
import           Language.Haskell.TH
import           TH.ReifySimple

import Test.Goblin.Core


--------------------------------------------------------------------------------
-- Goblin instance derivation
--------------------------------------------------------------------------------

deriveGoblin :: Name -> Q [Dec]
deriveGoblin name = do
  (DataType _dName dTyVars _dCtx dCons) <- reifyDataType name
  genomeVar <- VarT <$> newName "genome"
  classParamNames <- forM dTyVars (const (newName "arg"))
  let con = ensureSingleton dCons
  ctx <- wrapWithGoblinConstraints genomeVar classParamNames
  decTy <- makeInstanceType genomeVar classParamNames
  tink <- makeTinker con
  conj <- makeConjure con
  pure [InstanceD Nothing ctx decTy [tink, conj]]
 where
  ensureSingleton dCons =
    case dCons of
      []  -> error "deriveGoblin: cannot derive Goblin for a void type"
      [x] -> x
      _   -> error "deriveGoblin: cannot derive Goblin for a sum type"

  -- Create constraints `(Goblin g a, AddShrinks a ...) => ...` for the instance
  wrapWithGoblinConstraints genomeVar = sequence .
    concatMap (\cpn -> [ [t| Goblin $(pure genomeVar) $(pure (VarT cpn)) |]
                       , [t| AddShrinks $(pure (VarT cpn)) |]
                       ])

  -- Make instance type `... => Goblin g (Foo a b ...)`
  makeInstanceType genomeVar classParamNames =
    [t| Goblin $(pure genomeVar)
               ( $(pure (foldl wrapTyVars (ConT name) classParamNames)) ) |]
  wrapTyVars acc cpn = AppT acc (VarT cpn)

  makeTinker con = do
    argName <- newName "arg"
    let pat = VarP argName
    body <- mkBody argName
    pure (FunD (mkName "tinker") [Clause [pat] (NormalB body) []])
   where
    mkBody argName = do
      fieldNames <- forM (dcFields con) (const (newName "field"))
      let accessors = makeAccessors (dcName con) fieldNames
      start <- [| $(pure (makeConPacker (dcName con) fieldNames))
                    <$$> (tinker ($(pure (head accessors))
                            <$> $(pure (VarE argName)))) |]
      let tinkerBody =
            foldM (\acc getter -> [| $(pure acc) <**> (tinker ($(pure getter)
                                                   <$> $(pure (VarE argName)))) |])
                  start
                  (tail accessors)
      [| tinkerRummagedOrConjureOrSave $(tinkerBody) |]

  makeConjure con = do
    unless (length (dcFields con) > 0) $
      error "not enough fields"
    start <- [| $(pure (ConE (dcName con))) <$> conjure |]
    body <- foldM (\acc _ -> [| $(pure acc) <*> conjure |])
                  start
                  (tail (dcFields con))
    cb <- [| saveInBagOfTricks =<< $(pure body) |]
    pure (FunD (mkName "conjure") [Clause [] (NormalB cb) []])


--------------------------------------------------------------------------------
-- AddShrinks instance derivation
--------------------------------------------------------------------------------

deriveAddShrinks :: Name -> Q [Dec]
deriveAddShrinks name = do
  (DataType _dName dTyVars _dCtx dCons) <- reifyDataType name
  classParamNames <- forM dTyVars (const (newName "arg"))
  let con = ensureSingleton dCons
  ctx <- wrapWithConstraints classParamNames
  decTy <- makeInstanceType classParamNames
  addShr <- makeAddShrinks con
  pure [InstanceD Nothing ctx decTy [addShr]]
 where
  ensureSingleton dCons =
    case dCons of
      []  -> error "deriveAddShrinks: cannot derive AddShrinks for a void type"
      [x] -> x
      _   -> error "deriveAddShrinks: cannot derive AddShrinks for a sum type"

  -- Create constraints `(AddShrinks a ...) => ...` for the instance
  wrapWithConstraints = sequence .
    map (\cpn -> [t| AddShrinks $(pure (VarT cpn)) |])

  -- Make instance type `... => AddShrinks (Foo a b ...)`
  makeInstanceType classParamNames =
    [t| AddShrinks
          ( $(pure (foldl wrapTyVars (ConT name) classParamNames)) ) |]
  wrapTyVars acc cpn = AppT acc (VarT cpn)

  makeAddShrinks con = do
    fieldNames <- forM (dcFields con) (const (newName "field"))
    let pat = ConP (dcName con) (map VarP fieldNames)

    start <- [| $(pure (makeConPacker (dcName con) fieldNames))
                  <$> addShrinks $(pure (VarE (head fieldNames))) |]
    body <- foldM (\acc v -> [| $(pure acc) <*> addShrinks $(pure (VarE v)) |])
                  start
                  (tail (fieldNames))
    pure (FunD (mkName "addShrinks") [Clause [pat] (NormalB body) []])


--------------------------------------------------------------------------------
-- SeedGoblin instance derivation
--------------------------------------------------------------------------------

deriveSeedGoblin :: Name -> Q [Dec]
deriveSeedGoblin name = do
  (DataType _dName dTyVars _dCtx dCons) <- reifyDataType name
  classParamNames <- forM dTyVars (const (newName "arg"))
  let con = ensureSingleton dCons
  ctx <- wrapWithConstraints classParamNames
  decTy <- makeInstanceType classParamNames
  sdr <- makeSeeder con
  pure [InstanceD Nothing ctx decTy [sdr]]
 where
  ensureSingleton dCons =
    case dCons of
      []  -> error "deriveSeedGoblin: cannot derive SeedGoblin for a void type"
      [x] -> x
      _   -> error "deriveSeedGoblin: cannot derive SeedGoblin for a sum type"

  -- Create constraints `(SeedGoblin a ...) => ...` for the instance
  wrapWithConstraints = sequence .
    concatMap (\cpn -> [ [t| SeedGoblin $(pure (VarT cpn)) |]
                       , [t| Typeable $(pure (VarT cpn)) |]
                       ])


  -- Make instance type `... => SeedGoblin (Foo a b ...)`
  makeInstanceType classParamNames =
    [t| SeedGoblin
          ( $(pure (foldl wrapTyVars (ConT name) classParamNames)) ) |]
  wrapTyVars acc cpn = AppT acc (VarT cpn)

  makeSeeder con = do
    asName <- newName "argAs"
    fieldNames <- forM (dcFields con) (const (newName "field"))
    let pat = AsP asName (ConP (dcName con) (map VarP fieldNames))

    seedAs <- [| () <$ saveInBagOfTricks $(pure (VarE asName)) |]
    seedRest <- forM fieldNames $ \vName -> do
                  [| seeder $(pure (VarE vName)) |]
    let stmts = map NoBindS (seedAs:seedRest)
    pure (FunD (mkName "seeder") [Clause [pat] (NormalB (DoE stmts)) []])


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Take a constructor name and a list of arguments, and return a lambda
-- which receives the arguments and packs them into the constructor.
makeConPacker :: Name -> [Name] -> Exp
makeConPacker conName argNames =
  let body = foldl (\acc v -> AppE acc (VarE v))
                   (ConE conName)
                   argNames
   in LamE (map VarP argNames) body

-- Take a constructor name and a list of arguments, and return a list
-- of lambdas which access each respective constructor field, in order.
makeAccessors :: Name -> [Name] -> [Exp]
makeAccessors conName argNames =
  [ let front = replicate i WildP
        back  = replicate (length argNames - i - 1) WildP
        arg   = argNames !! i
        pat = ConP conName (front ++ [VarP arg] ++ back)
     in LamE [pat] (VarE arg)
  | i <- [0 .. length argNames - 1]
  ]
