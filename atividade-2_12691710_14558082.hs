-- Thiago Prado Dalla Dea  NUSP: 12691710
-- André Dalla Dea Trombini  NUSP: 14558082

-- Verifica se o número é primo
ehPrimo :: Int -> Int -> Bool
ehPrimo num div
    | num <= 1 = False
    | num == 2 = True
    | num `mod` div == 0 = False
    | div * div > num = True
    -- Recursão
    | otherwise = ehPrimo num (div + 1)

-- Números primos no intervalo dado de x e y
numerosPrimos :: Int -> Int -> [Int] -> [Int]
numerosPrimos x y listaPrimos
    | x > y = listaPrimos
    | ehPrimo x 2 = numerosPrimos (x + 1) y (listaPrimos ++ [x])
    -- Recursão
    | otherwise = numerosPrimos (x + 1) y listaPrimos

maiorDiferenca :: [Int] -> Int -> Int
maiorDiferenca listaPrimos posicao
    -- Final da lista
    | posicao == length listaPrimos - 1 = 0
    -- Diferença entre o atual e a calda da lista
    | otherwise = max diferencaCabeca diferencaCalda
    where
        diferencaCabeca = listaPrimos !! (posicao + 1) - listaPrimos !! posicao
        diferencaCalda = maiorDiferenca listaPrimos (posicao + 1)

main :: IO ()
main = do
    -- Requisição de dados
    x <- readLn
    y <- readLn
    let listaPrimos = numerosPrimos x y []
    let resultado = maiorDiferenca listaPrimos 0
    print resultado