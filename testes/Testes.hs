module Main where

import Trabalho_1

import Data.List (sort)
import Test.Hspec


caso11 = maior4 0 0 0 0 == 0
caso12 = maior4 0 1 0 0 == 1
caso13 = maior4 3 3 3 2 == 3
caso14 = maior4 3 2 1 4 == 4

testeExercicio1 = and [caso11, caso12, caso13, caso14]

caso21 = converterNotaParaMencao 10 == "SS"
caso22 = converterNotaParaMencao 9.5 == "SS"
caso23 = converterNotaParaMencao 8.9 == "MS"
caso24 = converterNotaParaMencao 7 == "MS"
caso25 = converterNotaParaMencao 5 == "MM"
caso26 = converterNotaParaMencao 3.9 == "MI"
caso27 = converterNotaParaMencao 2.5 == "II"
caso28 = converterNotaParaMencao 0.1 == "II"
caso29 = converterNotaParaMencao 0 == "SR"

testeExercicio2 = and [caso21, caso22, caso23, caso24, caso25, caso26, caso27, caso28, caso29]

caso31 = isDecrescente []
caso32 = isDecrescente [2]
caso33 = isDecrescente [3,2,1]
caso34 = not (isDecrescente [3,2,1,4])
caso35 = not (isDecrescente [3,2,1,1])
caso36 = not (isDecrescente [1,1,1,1])
caso37 = isDecrescente [35,7,4,0,-3]
testeExercicio3 = and [caso31,caso32,caso33,caso34,caso35,caso36,caso37]

caso41 = sort (histograma []) == sort []
caso42 = sort (histograma ["Batata"]) == [("Batata", 1)]
caso43 = sort (histograma ["Tomate", "Pepino", "Cenoura", "Tomate"]) == sort [("Tomate", 2),("Pepino", 1),("Cenoura", 1)]
caso44 = sort (histograma ["Mamão", "Mamão", "Mamão", "Mamão"]) == sort [("Mamão", 4)]
caso45 = sort (histograma ["Chocolate", "Bolo", "Chocolate", "Bolo", "Bolo", "Chocolate"]) == sort [("Chocolate", 3), ("Bolo", 3)]
testeExercicio4 = and [caso41, caso42, caso43, caso44, caso45]

caso51 = null $ myZipWith (+) [1,2] []
caso52 = null $ myZipWith (+) [] [1,2]
caso53 = myZipWith (+) [1,2] [1,2] == [2,4]
caso54 = myZipWith (+) [1,2,3] [1,2] == [2,4]
caso55 = myZipWith (+) [1,2] [1,2,3] == [2,4]
caso56 = myZipWith (+) [7,5,3,1] [1,3,5,7] == [8,8,8,8]
testeExercicio5 = and [caso51, caso52, caso53, caso54, caso55, caso56]

caso61 = null $ aprovadosOrdemDeMedia []
caso62 = aprovadosOrdemDeMedia [("Alberto",6,8)] == [("Alberto", (6+8)/2.0)]
caso63 = null (aprovadosOrdemDeMedia [("Alberto",0,9.9)])
caso64 = aprovadosOrdemDeMedia [("Alberto",5,5)] == [("Alberto", (5+5)/2.0)]
caso65 = aprovadosOrdemDeMedia [("Alberto",0,10)] == [("Alberto", (0+10)/2.0)]

caso66 = aprovadosOrdemDeMedia [("Primeiro",9,10),("Segundo",8,9),("Penultimo",6,7),("Ultimo",5,5),("Reprovado",4,5)]
        == [("Ultimo",(5+5)/2),("Penultimo",(6+7)/2),("Segundo",(8+9)/2),("Primeiro",(9+10)/2)]

testeExercicio6 = and [caso61, caso62, caso63, caso64, caso65, caso66]


caso71a = null (somaMatricial [] [])
caso72a = somaMatricial [[1,2,3]] [[3,2,1]] == [[4,4,4]]
caso73a = somaMatricial [[1,2,3],[4,5,6]] [[-1,-2,-3],[-4,-5,-6]] == [[0,0,0],[0,0,0]]
caso74a = somaMatricial [[3]] [[4]] == [[7]]
caso75a = somaMatricial [[1],[2],[3]] [[4],[5],[6]] == [[5],[7],[9]]

caso71b = null (matrizTransposta [])
caso72b = matrizTransposta [[1,2,3]] == [[1],[2],[3]]
caso73b = matrizTransposta [[1],[2],[3]] == [[1,2,3]]
caso74b = matrizTransposta [[1,2,3],[4,5,6],[7,8,9]] == [[1,4,7],[2,5,8],[3,6,9]] 

caso71c = null (multiplicacaoMatricial [] [])
caso72c = multiplicacaoMatricial [[1,2],[3,4]] [[5,6],[7,8]] == [[19,22],[43,50]]
caso73c = multiplicacaoMatricial [[1]] [[2]] == [[2]]
caso74c = multiplicacaoMatricial [[1,2,3]] [[4],[5],[6]] == [[32]]
caso75c = multiplicacaoMatricial [[1],[2],[3]] [[4,5,6]] == [[4,5,6],[8,10,12],[12,15,18]]

testeExercicio7a = and [caso71a, caso72a, caso73a, caso74a, caso75a]
testeExercicio7b = and [caso71b, caso72b, caso73b, caso74b]
testeExercicio7c = and [caso71c, caso72c, caso73c, caso74c, caso75c]


main = hspec $ do

  describe "Suite de Testes do Trabalho 01" $ do

    it "O resultado global dos testes do exercício 1 deve 'True'" $ do
      testeExercicio1 `shouldBe` True

    it "O resultado global dos testes do exercício 2 deve 'True'" $ do
      testeExercicio2 `shouldBe` True

    it "O resultado global dos testes do exercício 3 deve 'True'" $ do
      testeExercicio3 `shouldBe` True

    it "O resultado global dos testes do exercício 4 deve 'True'" $ do
      testeExercicio4 `shouldBe` True

    it "O resultado global dos testes do exercício 5 deve 'True'" $ do
      testeExercicio5 `shouldBe` True

    it "O resultado global dos testes do exercício 6 deve 'True'" $ do
      testeExercicio6 `shouldBe` True

    it "O resultado global dos testes do exercício 7a deve 'True'" $ do
      testeExercicio7a `shouldBe` True
    it "O resultado global dos testes do exercício 7b deve 'True'" $ do
      testeExercicio7b `shouldBe` True
    it "O resultado global dos testes do exercício 7c deve 'True'" $ do
      testeExercicio7c `shouldBe` True
