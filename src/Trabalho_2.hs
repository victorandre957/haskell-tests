{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Trabalho_2 where

import Dados

{-
 *** Aluno: Victor André de Moraes
 *** Matricula: 211026664

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

  QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamento quantidade [] = [(medicamento, quantidade)]
comprarMedicamento medicamento quantidade ((med, q) : resto)
  | medicamento == med = (med, q + quantidade) : resto
  | medicamento < med = (medicamento, quantidade) : (med, q) : resto
  | otherwise = (med, q) : comprarMedicamento medicamento quantidade resto

{-
  QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento medicamento estoque = go estoque []
  where
    go [] _ = Nothing
    go ((med, quant) : resto) acc
      | med == medicamento && quant > 0 = Just (reverse acc ++ ((med, quant - 1) : resto))
      | otherwise = case go resto ((med, quant) : acc) of
          Just novoEstoque -> Just (reverse acc ++ ((med, quant) : novoEstoque))
          Nothing -> Nothing

{-
  QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento medicamento estoque = consultarMedicamento' medicamento estoque 0
  where
    consultarMedicamento' _ [] acc = acc
    consultarMedicamento' medicamento ((med, quant) : resto) acc
      | med == medicamento = quant + acc
      | otherwise = consultarMedicamento' medicamento resto acc

{-
  QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos receituario = ordenarEstoqueMedicamentos estoque
  where
    estoque = contarMedicamentos receituario

    contarMedicamento :: (Medicamento, [Horario]) -> EstoqueMedicamentos -> EstoqueMedicamentos
    contarMedicamento (medicamento, horarios) acc =
      let quantidade = contarHorarios horarios
      in if quantidade > 0 then (medicamento, quantidade) : acc else acc

    contarHorarios :: [Horario] -> Int
    contarHorarios = foldr (\_ acc -> acc + 1) 0

    ordenarEstoqueMedicamentos :: EstoqueMedicamentos -> EstoqueMedicamentos
    ordenarEstoqueMedicamentos = ordenarPorNome

    ordenarPorNome :: EstoqueMedicamentos -> EstoqueMedicamentos
    ordenarPorNome = sortBy (\(m1, _) (m2, _) -> compare m1 m2)

    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x : xs) = sortBy cmp lesser ++ [x] ++ sortBy cmp greater
      where
        lesser = filter (\y -> cmp y x == LT) xs
        greater = filter (\y -> cmp y x /= LT) xs
    contarMedicamentos = foldr contarMedicamento []

{-
  QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
para cada medicamento, seus horários também estão ordenados e são distintos.

Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

-}

sortList :: Ord a => [a] -> [a]
sortList [] = []
sortList (x : xs) = sortList smaller ++ [x] ++ sortList larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [a | a <- xs, a >= x]

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)

receituarioValido :: Receituario -> Bool
receituarioValido receituario =
  medicamentosDiferentes receituario
    && medicamentosOrdenados receituario
    && all distinctHorariosPrescricao receituario
    && all orderedHorariosPrescricao receituario
  where
    medicamentosDiferentes :: Receituario -> Bool
    medicamentosDiferentes [] = True
    medicamentosDiferentes ((medicamento, _) : resto) =
      medicamento `notElem` map fst resto && medicamentosDiferentes resto

    medicamentosOrdenados :: Receituario -> Bool
    medicamentosOrdenados [] = True
    medicamentosOrdenados [_] = True
    medicamentosOrdenados ((medicamento1, _) : (medicamento2, _) : resto) =
      medicamento1 <= medicamento2 && medicamentosOrdenados ((medicamento2, []) : resto)

    distinctHorariosPrescricao :: Prescricao -> Bool
    distinctHorariosPrescricao (_, horarios) = length horarios == length (nub horarios)

    orderedHorariosPrescricao :: Prescricao -> Bool
    orderedHorariosPrescricao (_, horarios) = horarios == sortList horarios

planoValido :: PlanoMedicamento -> Bool
planoValido plano =
  horariosDiferentes plano
    && horariosOrdenados plano
    && all medicamentosEmHorariosDiferentes plano
    && all ordenarMedicamentosPorHorario plano
  where
    horariosDiferentes :: PlanoMedicamento -> Bool
    horariosDiferentes [] = True
    horariosDiferentes ((horario, _) : resto) =
      horario `notElem` map fst resto && horariosDiferentes resto

    horariosOrdenados :: PlanoMedicamento -> Bool
    horariosOrdenados [] = True
    horariosOrdenados [_] = True
    horariosOrdenados ((horario1, _) : (horario2, _) : resto) =
      horario1 <= horario2 && horariosOrdenados ((horario2, []) : resto)

    medicamentosEmHorariosDiferentes :: (Horario, [Medicamento]) -> Bool
    medicamentosEmHorariosDiferentes (_, medicamentos) = length medicamentos == length (nub medicamentos)

    ordenarMedicamentosPorHorario :: (Horario, [Medicamento]) -> Bool
    ordenarMedicamentosPorHorario (_, medicamentos) = medicamentos == sortList medicamentos

{-

  QUESTÃO 6  VALOR: 1,0 ponto,

Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

1. Os horários da lista são distintos e estão em ordem crescente;
2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

-}

plantaoValido :: Plantao -> Bool
plantaoValido plantao =
  horariosDiferentes plantao
    && not (ocorrenciasRepetidas plantao)
    && medicamentosOrdenados plantao
  where
    horariosDiferentes :: Plantao -> Bool
    horariosDiferentes [] = True
    horariosDiferentes [_] = True
    horariosDiferentes ((h1, _) : (h2, _) : rest)
      | h1 < h2 = horariosDiferentes ((h2, []) : rest)
      | otherwise = False

    ocorrenciasRepetidas :: Plantao -> Bool
    ocorrenciasRepetidas [] = False
    ocorrenciasRepetidas ((_, cuidados) : rest) =
      any duplicados cuidados || ocorrenciasRepetidas rest
      where
        duplicados :: Cuidado -> Bool
        duplicados c = case c of
          Medicar medicamento -> any (\x -> case x of Comprar m _ -> m == medicamento; _ -> False) cuidados
          _ -> False

    medicamentosOrdenados :: Plantao -> Bool
    medicamentosOrdenados [] = True
    medicamentosOrdenados ((_, cuidados) : rest) =
      isSorted cuidados && medicamentosOrdenados rest
      where
        isSorted :: [Cuidado] -> Bool
        isSorted [] = True
        isSorted [_] = True
        isSorted (c1 : c2 : cs) =
          case (c1, c2) of
            (Medicar m1, Medicar m2) -> m1 <= m2 && isSorted (c2 : cs)
            _ -> isSorted (c2 : cs)

{-
  QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receituario = ordenaPlano $ agrupaMedicamentos $ foldl expandeMedicamento [] receituario
  where
    ordenaPlano :: PlanoMedicamento -> PlanoMedicamento
    ordenaPlano = iserirEmOrdem

    iserirEmOrdem :: PlanoMedicamento -> PlanoMedicamento
    iserirEmOrdem [] = []
    iserirEmOrdem (x : xs) = inserirNoHorario x (iserirEmOrdem xs)

    inserirNoHorario :: (Horario, [Medicamento]) -> PlanoMedicamento -> PlanoMedicamento
    inserirNoHorario x [] = [x]
    inserirNoHorario x@(h, _) ys@(y@(h', _) : ys')
      | h <= h' = x : ys
      | otherwise = y : inserirNoHorario x ys'

    expandeMedicamento :: PlanoMedicamento -> Prescricao -> PlanoMedicamento
    expandeMedicamento plano (medicamento, horarios) = plano ++ [(horario, [medicamento]) | horario <- horarios]

    agrupaMedicamentos :: PlanoMedicamento -> PlanoMedicamento
    agrupaMedicamentos = agrupaMedicamentos' . iserirEmOrdem

    agrupaMedicamentos' :: PlanoMedicamento -> PlanoMedicamento
    agrupaMedicamentos' [] = []
    agrupaMedicamentos' [x] = [x]
    agrupaMedicamentos' ((h1, m1) : (h2, m2) : xs)
      | h1 == h2 = agrupaMedicamentos' ((h1, m1 ++ m2) : xs)
      | otherwise = (h1, m1) : agrupaMedicamentos' ((h2, m2) : xs)

{- QUESTÃO 8  VALOR: 1,0 ponto

  Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
  plano de medicamentos válido.
  Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
  compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
  geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano plano = concatMap agrupaPrescricao (groupBy (\(m1, _) (m2, _) -> m1 == m2) (sort (trocaTuplas plano)))
  where
    trocaTuplas :: PlanoMedicamento -> Receituario
    trocaTuplas [] = []
    trocaTuplas ((horario, medicamentos) : xs) = trocaTupla medicamentos horario ++ trocaTuplas xs

    trocaTupla :: [Medicamento] -> Horario -> Receituario
    trocaTupla [] _ = []
    trocaTupla (m : ms) horario = (m, [horario]) : trocaTupla ms horario

    agrupaPrescricao :: Receituario -> Receituario
    agrupaPrescricao [] = error "Prescrição vazia"
    agrupaPrescricao prescricoes@((m, _) : _) = [(m, concatMap snd prescricoes)]

    sort :: Ord a => [a] -> [a]
    sort [] = []
    sort (x : xs) = sort menores ++ [x] ++ sort maiores
      where
        menores = filter (<= x) xs
        maiores = filter (> x) xs

    groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy _ [] = []
    groupBy eq (x : xs) = (x : group) : groupBy eq rest
      where
        (group, rest) = span (eq x) xs

{-  QUESTÃO 9 VALOR: 1,0 ponto

  Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
  resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
  Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
  deve ser Just v, onde v é o valor final do estoque de medicamentos.

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = realizarCuidados
  where
    realizarCuidados :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
    realizarCuidados [] estoque = Just estoque
    realizarCuidados ((horario, cuidados) : restantePlantao) estoque = do
      novoEstoque <- executarCuidados cuidados estoque
      realizarCuidados restantePlantao novoEstoque

    executarCuidados :: [Cuidado] -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
    executarCuidados [] estoque = Just estoque
    executarCuidados (cuidado : restanteCuidados) estoque = do
      novoEstoque <- executarCuidado cuidado estoque
      executarCuidados restanteCuidados novoEstoque

    executarCuidado :: Cuidado -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
    executarCuidado (Comprar medicamento quantidade) estoque = adicionarMedicamento medicamento quantidade estoque
    executarCuidado (Medicar medicamento) estoque = removerMedicamento medicamento estoque

    adicionarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
    adicionarMedicamento medicamento quantidade estoque = do
      let medicamentoExistente = lookup medicamento estoque
      case medicamentoExistente of
        Just q -> Just $ atualizarQuantidade medicamento (q + quantidade) estoque
        Nothing -> Just $ (medicamento, quantidade) : estoque

    removerMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
    removerMedicamento medicamento estoque = do
      let medicamentoExistente = lookup medicamento estoque
      case medicamentoExistente of
        Just q ->
          if q > 0
            then Just $ atualizarQuantidade medicamento (q - 1) estoque
            else Nothing
        Nothing -> Nothing

    atualizarQuantidade :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
    atualizarQuantidade medicamento quantidade [] = [(medicamento, quantidade)]
    atualizarQuantidade medicamento quantidade ((m, q) : restanteEstoque) =
      if m == medicamento
        then (m, quantidade) : restanteEstoque
        else (m, q) : atualizarQuantidade medicamento quantidade restanteEstoque

{-
QUESTÃO 10 VALOR: 1,0 ponto

  Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
  de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
  implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
  Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
  Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
  juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque =
  case executaPlantao plantao estoque of
    Nothing -> False
    Just estoqueFinal -> verificaPlano plano estoqueFinal
  where
    verificaPlano :: PlanoMedicamento -> EstoqueMedicamentos -> Bool
    verificaPlano plano estoque =
      all (verificaPrescricao estoque) plano

    verificaPrescricao :: EstoqueMedicamentos -> (Horario, [Medicamento]) -> Bool
    verificaPrescricao estoque (_, medicamentos) =
      all (verificaMedicamento estoque . Medicar) medicamentos

    verificaMedicamento :: EstoqueMedicamentos -> Cuidado -> Bool
    verificaMedicamento estoque (Medicar medicamento) =
      case lookup medicamento estoque of
        Just quantidade -> quantidade > 0
        Nothing -> False
    verificaMedicamento _ _ = False

{-

QUESTÃO 11 VALOR: 1,0 ponto

  Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
  medicamentos válido e um estoque de medicamentos.
  Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto plano estoque = go plano estoque []
  where
    go :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao -> Plantao
    go [] _ plantaoAtual = plantaoAtual
    go ((horario, medicamentos) : restante) estoqueAtual plantaoAtual =
      let (cuidados, estoqueAtualizado) = cuida medicamentos estoqueAtual []
      in go restante estoqueAtualizado ((horario, cuidados) : plantaoAtual)

    cuida :: [Medicamento] -> EstoqueMedicamentos -> [Cuidado] -> ([Cuidado], EstoqueMedicamentos)
    cuida [] estoqueAtual cuidadosAtual = (reverse cuidadosAtual, estoqueAtual)
    cuida (medicamento : restante) estoqueAtual cuidadosAtual =
      case lookup medicamento estoqueAtual of
        Just quantidade ->
          if quantidade > 0
            then cuida restante (atualizaEstoque medicamento estoqueAtual) (Medicar medicamento : cuidadosAtual)
            else cuida restante estoqueAtual (Comprar medicamento 1 : cuidadosAtual)
        Nothing -> cuida restante estoqueAtual (Comprar medicamento 1 : cuidadosAtual)

    atualizaEstoque :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
    atualizaEstoque medicamento = map (atualiza medicamento)
      where
        atualiza m (m', q)
          | m == m' = (m', q - 1)
          | otherwise = (m', q)