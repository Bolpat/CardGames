{-# LANGUAGE TemplateHaskell #-}

module Utility.Templates where

import Language.Haskell.TH
import Control.Monad
import Control.Applicative

-- zips n lists
zipN :: Int -> ExpQ
zipN n = do
    -- n listen nehmen
    -- ZipList auf jede Liste
    -- (, , ... , ) auf alle Listen mit <*> verknüpfen
    lsN <- replicateM n $ newName "l"
    let lsP = varP <$> lsN
        lsE = varE <$> lsN
        zlsE = appE [| ZipList |] <$> lsE
        appZlsE = appE [| (<**>) |] <$> zlsE
    lamE lsP $ appE [| getZipList |] $
        foldl (flip appE) ( appE [| pure |] (tupleN n) ) appZlsE

tupleN :: Int -> ExpQ
tupleN n = do
    xsN <- replicateM n $ newName "x"
    lamE (varP <$> xsN) $ tupE (varE <$> xsN)

v :: Int -> ExpQ
v n = do
    fs <- replicateM n $ newName "f"
    xN <- newName "x"
    lamE [tupP (varP <$> fs), varP xN] $ tupE $ appE <$> (varE <$> fs) <*> pure (varE xN)

curryN :: Int -> ExpQ
curryN n = do
    xNs <- replicateM n $ newName "x"
    let fN = mkName "f"
        fP = varP fN
        fE = varE fN
        xPs = varP <$> xNs
        xEs = varE <$> xNs
    lamE (fP:xPs) $ appE fE $ tupE xEs
    
uncurryN :: Int -> ExpQ
uncurryN n = do
    xNs <- replicateM n $ newName "x"
    let fN = mkName "f"
        fP = varP fN
        fE = varE fN
        xPs = varP <$> xNs
        xEs = varE <$> xNs
    lamE [fP, tupP xPs] $ foldl appE fE xEs

tIns :: Int -> Int -> Int -> ExpQ
tIns i m n = do
    xNs <- replicateM m $ newName "x"
    yNs <- replicateM n $ newName "y"
    let yEs = varE <$> yNs
    lamE ((tupP.(varP <$>)) <$> [xNs, yNs]) $ tupE (take i yEs ++ (varE <$> xNs) ++ drop i yEs)

tConc :: [Int] -> ExpQ
tConc l = do
    xNs <- forM l $ flip replicateM $ newName "x"
    let xPs = (varP <$>) <$> xNs
        xEs = varE <$> concat xNs
    lamE (tupP <$> xPs) $ tupE xEs

tFlatten :: [Int] -> ExpQ
tFlatten l = [| $(uncurryN (length l)) $(tConc l) |]
