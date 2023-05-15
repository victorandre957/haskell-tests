module Main where

import Trabalho_1

main :: IO ()
main = do
    putStrLn "## TRABALHO 01 -- Linguagens de Programacao 2023.1_T1 UnB ##"

    -- 1
    putStrLn ("O maior numero entre '1 3 4 2' eh: "
        ++ show (maior4 1 3 4 2))

    -- 2
    putStrLn ("Uma nota 8.7 deve corresponder a uma mencao: "
        ++ show (converterNotaParaMencao 8.7))

    -- 3
    putStrLn ("A lista [4,3,2,1] eh decrescente?: "
        ++ show (isDecrescente [4,3,2,1]))

    -- 4
    putStrLn ("O histograma da lista de strings [\"Batata\", \"Tomate\", \"Batata\", \"Cebola\", \"Pepino\"] eh: "
        ++ show (histograma ["Batata", "Tomate", "Batata", "Cebola", "Pepino"]))

    -- 5
    putStrLn ("O resultado do myZipWith com a soma (+) entre [1,2,3] e [1,1,1,1]: eh:"
        ++ show (myZipWith (+) [1,2,3] [1,1,1]))

    -- 6
    putStrLn ("Para a entrada '[(\"Fulano\",8,6),(\"Ciclano\",4,5),(\"Beltrano\",5,6)]', a lista dos aprovados com as medias em ordem crescente eh: "
        ++ show (aprovadosOrdemDeMedia [("Fulano",8,6),("Ciclano",4,5),("Beltrano",5,6)]))

    -- 7a
    putStrLn ("A soma das matrizes [[1,2],[3,4]] e [[1,1],[2,2]] eh: "
        ++ show (somaMatricial [[1,2],[3,4]] [[1,1],[2,2]]))
    -- 7b
    putStrLn ("A transposta da matriz [[1,2,3],[4,5,6]] eh: "
        ++ show (matrizTransposta [[1,2,3],[4,5,6]]))
    putStrLn ("A multiplicacao entre as matrizes [[1,2],[3,4]] e [[4,3],[2,1]] eh: "
        ++ show (multiplicacaoMatricial [[1,2],[3,4]] [[4,3],[2,1]]))

