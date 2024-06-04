-- Thiago Prado Dalla Dea  NUSP: 12691710
-- André Dalla Dea Trombini  NUSP: 14558082


import Data.List (intercalate)

-- Função para a pontuação total e os frames
bolichePontuacao :: [Int] -> ([String], Int)
bolichePontuacao jogadas = (formataSaida frames, pontuacaoTotal frames)
  where
    frames = obterFrames jogadas --requisição dos frames a partir de jogadas

-- Função para a lista de frames a partir de jogadas
obterFrames :: [Int] -> [(Int, Int)]
obterFrames [] = []
obterFrames (10:xs) = (10, 0) : obterFrames xs --strike
obterFrames (x:y:xs) = (x, y) : obterFrames xs --jogada dois lançamentos
obterFrames (x:xs) = (x, 0) : obterFrames xs --completa com 0

-- Função para a pontuação total e os frames
pontuacaoTotal :: [(Int, Int)] -> Int
pontuacaoTotal frames = sum (zipWith framePontuacao [0..9] frames)
  where
    framePontuacao idx (10, _) = 10 + strikeBonus idx
    framePontuacao idx (x, y)
      | x + y == 10 = 10 + spareBonus idx
      | otherwise = x + y
    strikeBonus idx     --calcula bonus strike
      | idx + 1 < length frames = let (a, b) = frames !! (idx + 1)
                                   in a + if a == 10 && idx + 2 < length frames then fst (frames !! (idx + 2)) else b
      | otherwise = 0
    spareBonus idx      --calcula bonus spare
      | idx + 1 < length frames = fst (frames !! (idx + 1))
      | otherwise = 0

-- Função para formatar a resposta
formataSaida :: [(Int, Int)] -> [String]
formataSaida frames = map formataFrame (take 9 frames) ++ [formataUltimoFrame (drop 9 frames)]
  where
    formataFrame (10, _) = "X _"             --strike
    formataFrame (x, y)
      | x + y == 10 = show x ++ " /"         --spare
      | otherwise = showFrame x ++ " " ++ showFrame y  --normal
      
    -- Caso tenha 3 lançamentos no ultimo lançamento
    formataUltimoFrame [(10, _), (10, _), (a, _)]
      | a == 10 = "X X X"
      | otherwise = "X X " ++ showFrame a
    formataUltimoFrame [(10, _), (a, b)]
      | a == 10 = "X X " ++ showFrame b
      | a + b == 10 = "X " ++ showFrame a ++ " /"
      | otherwise = "X " ++ showFrame a ++ " " ++ showFrame b
    formataUltimoFrame [(x, y), (a, _)]
      | x + y == 10 = showFrame x ++ " / " ++ showFrame a
      | otherwise = showFrame x ++ " " ++ showFrame y
    -- Caso dois lançamentos no ultimo lançamento
    formataUltimoFrame [f] = formataFrame f
    
    showFrame 10 = "X"
    showFrame n = show n

-- Função main para ler a jogada e imprimir a resposta
main :: IO ()
main = do
    entrada <- getLine
    let jogada = map read (words entrada) :: [Int] -- leitura
    let (saida, pontuacao) = bolichePontuacao jogada
    putStrLn $ intercalate " | " saida ++ " | " ++ show pontuacao