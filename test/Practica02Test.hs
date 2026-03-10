{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Test.Hspec
import Practica02
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig specs


specs :: Spec
specs = do

    let formTt =  Syss (Not (Or (Var "p") (Var "q"))) (And (Not (Var "p")) (Not (Var "q")))
    let formCr = Syss (Or (Var "p") (Var "q")) (And (Not (Var "p")) (Not (Var "q")))
    let formCg = Impl (And (And (Impl (Var "p") (Not (Var "q"))) (Impl (Var "s") (Var "q"))) (Var "s")) (Var "p")
    
    describe "Tests variables" $ do
        it "Fórmula Tt" $ do
            variables formTt `shouldMatchList` ["p", "q"]
        
        it "Fórumla Cr" $ do
            variables formCr `shouldMatchList` ["p", "q"]

        it "Fórmula Cg" $ do
            variables formCg `shouldMatchList` ["p", "q", "s"]
    
    describe "Tests interpretacion" $ do
        it "Fórmula Tt" $ do
            interpretacion formTt ["p"] `shouldBe` True
        
        it "Fórumla Cr" $ do
            interpretacion formCr ["q"] `shouldBe` False

        it "Fórmula Cg-True" $ do
            interpretacion formCg ["p", "q", "s"] `shouldBe` True

        it "Fórmula Cg-False" $ do
            interpretacion formCg ["q", "s"] `shouldBe` False
        
    describe "Tests estadosPosibles" $ do
        it "Fórmula Tt" $ do
            estadosPosibles formTt `shouldMatchList` [["p","q"],["p"],["q"],[]]
        
        it "Fórmula Cr" $ do
            estadosPosibles formCr `shouldMatchList` [["p","q"],["p"],["q"],[]]

        it "Fórmula Cg" $ do
            estadosPosibles formCg `shouldMatchList` [["q","s","p"],["q","s"],["q","p"],["q"],["s","p"],["s"],["p"],[]]
        
    describe "Tests modelos" $ do
        it "Fórmula Tt" $ do
            modelos formTt `shouldMatchList` [["p","q"],["p"],["q"],[]]
        
        it "Fórmula Cr" $ do
            modelos formCr `shouldMatchList` []

        it "Fórmula Cg" $ do
            modelos formCg `shouldMatchList` [["q","s","p"],["q","p"],["q"],["s","p"],["s"],["p"],[]]
    
    describe "Tests sonEquivalentes" $ do
        it "Fórmulas Equivalentes" $ do
            sonEquivalentes (Or (Var "q") (Not (Var "p"))) (Impl (Var "p") (Var "q")) `shouldBe` True 
        
        it "Fórmulas No Equivalentes" $ do
            sonEquivalentes (And (Var "q") (Not (Var "p"))) (Impl (Var "p") (Var "q")) `shouldBe` False 

    describe "Tests tautologia" $ do
        it "Fórmula Tt" $ do
            tautologia formTt `shouldBe` True
        
        it "Fórmula Cr" $ do
            tautologia formCr `shouldBe` False

        it "Fórmula Cg" $ do
            tautologia formCg `shouldBe` False
    
    describe "Tests contradiccion" $ do
        it "Fórmula Tt" $ do
            contradiccion formTt `shouldBe` False
        
        it "Fórumla Cr" $ do
            contradiccion formCr `shouldBe` True

        it "Fórmula Cg" $ do
            contradiccion formCg `shouldBe` False
    
    describe "Tests consecuenciaLogica" $ do
        it "Argumento válido" $ do
            consecuenciaLogica [Impl p q, Impl q r, p] r `shouldBe` True
        
        it "Argumento válido 2" $ do
            consecuenciaLogica [Impl p q, Impl r s, Or p q] (Or q s) `shouldBe` True

        it "Argumento inválido" $ do
            consecuenciaLogica [Impl p q, Impl r s, Or q s] (Or p r) `shouldBe` False

        it "Gamma insatisfacible" $ do
            consecuenciaLogica [And p q, Not p, Not q, Or r s] (Impl p q) `shouldBe` True