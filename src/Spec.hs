module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 6" $ do

describe "Parte 1" $ do
    it "El precioFinal de un cuartoDeLibra es 54" $ do
      precioFinal cuartoDeLibra `shouldBe` 20 + 2 + 20 + 10 + 2
    it "Agrandar una hamburguesa con carne le agrega otra carne" $ do
      agrandar cuartoDeLibra `shouldBe` Hamburguesa 20 [Pan, Carne, Cheddar, Pan, Carne]
    it "Agrandar una hamburguesa con pollo le agrega otro pollo" $ do
      let hamb = Hamburguesa 25 [Pollo, Pan]
      agrandar hamb `shouldBe` Hamburguesa 25 [Pollo, Pan, Pollo]
    it "agregarIngrediente añade el ingrediente al final de la lista de ingredientes de la hamburguesa" $ do
      agregarIngrediente Panceta cuartoDeLibra `shouldBe` Hamburguesa 20 [Pan, Carne, Cheddar, Pan, Panceta]
    it "Un 50% de descuento a una hamburguesa reduce su precio base a la mitad" $ do
      descuento 50 (Hamburguesa 100 []) `shouldBe` Hamburguesa 50 []
  
describe "Parte 2" $ do

    it "El precioFinal de una pdepBurger es 110" $ do
      precioFinal pdepBurger `shouldBe` 110

    it "El precioFinal de una dobleCuarto es 84" $ do
      precioFinal pdepBurger `shouldBe` 84

    it "El precioFinal de una bigPdep es 89" $ do
      precioFinal pdepBurger `shouldBe` 89

    it "delDia agrega papas y aplica 30% de descuento al precio base" $ do
      delDia dobleCuarto `shouldBe` Hamburguesa 88 [Pan, Carne, Cheddar, Pan, Carne, Cheddar, Papas]

    
