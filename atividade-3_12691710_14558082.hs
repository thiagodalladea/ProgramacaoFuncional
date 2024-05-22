-- Thiago Prado Dalla Dea - 12691710
-- André Dalla Dea Trombini - 14558082


comprimentoMaior :: [Int] -> Int
comprimentoMaior [] = 0
comprimentoMaior lista = maxComprimetoSegmento 1 lista
    where
        maxComprimetoSegmento maxCont [x] = maxCont
        maxComprimetoSegmento maxCont (x:y:xs)
            | x < y = maxComprimetoSegmento (maxCont + 1) (y:xs)
            | otherwise = max maxCont (maxComprimetoSegmento 1 (y:xs))

main :: IO ()
main = do
    entrada <- getLine
    let numeros = map read (words entrada) :: [Int]
    let resultado = comprimentoMaior numeros
    putStrLn $ show resultado
