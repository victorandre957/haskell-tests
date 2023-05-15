module Trabalho_1 where
import Data.List (sortBy)

maior4 :: Int -> Int -> Int -> Int -> Int
maior4 n1 n2 n3 n4 = max (max n1 n2) (max n3 n4)

converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota
  | nota >= 9.0 = "SS"
  | nota >= 7.0 = "MS"
  | nota >= 5.0 = "MM"
  | nota >= 3.0 = "MI"
  | nota >= 0.1 = "II"
  | otherwise   = "SR"

isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente lista = foldl (\acumulador (x,y) -> acumulador && x > y) True (zip lista (tail lista))

histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma (x:xs) =
  let count = 1 + length (filter (==x) xs)
  in (x, count) : histograma (filter (/=x) xs)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = foldr (\(x, y) acumulador -> f x y : acumulador) [] (zip xs ys)

aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia xs = sortBy (
  \(_, media1) (_, media2) -> compare media1 media2)
  [
    (nome, (nota1 + nota2) / 2)
    | (nome, nota1, nota2)
    <- xs, (nota1 + nota2) / 2 >= 5
  ]

somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial = zipWith (zipWith (+))

matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta matriz
    | null matriz = []
    | otherwise = foldr (zipWith (:)) (repeat []) matriz


multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial matrizA matrizB =
  [
    [sum $ zipWith (*) ar bc | bc <- matrizTransposta matrizB]
    | ar <- matrizA
  ]