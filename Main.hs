import System.IO
import System.Random (randomRIO)

data TypeTabuleiro = ConstructorTabuleiro [[(Int, Int)]] deriving (Show, Eq)

{-  (x,y)

x = 0/X (casa nao revelada)
x = 1/0 (casa revelada vazia)
x = 2/ (casa com bomba nao revelada)
x = 3/ (casa com bomba revelada (game over))
y = numero de bombas nos 8 adjacentes

  -}


criarTabuleiro :: Int -> Int -> IO TypeTabuleiro
criarTabuleiro linhas colunas = do
    let tabuleiro = replicate linhas (replicate colunas (0, 0))
    return (ConstructorTabuleiro tabuleiro)

gerarCoordenadaRandom :: TypeTabuleiro -> Int -> Int -> IO (Int, Int)
gerarCoordenadaRandom (ConstructorTabuleiro t) linhas colunas = do
    linha <- randomRIO (0, linhas - 1)
    coluna <- randomRIO (0, colunas - 1)
    if fst ((t !! linha) !! coluna) == 2
    then gerarCoordenadaRandom (ConstructorTabuleiro t) linhas colunas
    else return (linha, coluna)

gerarMinas :: TypeTabuleiro -> Int -> Int -> Int -> IO TypeTabuleiro
gerarMinas (ConstructorTabuleiro t) linhas colunas quantidadeMinas = do
    coordenadasMinas <- sequence (replicate quantidadeMinas (gerarCoordenadaRandom (ConstructorTabuleiro t) linhas colunas))
    let tabuleiroComMinas = foldl (\tab (linha, coluna) -> inserirMina linha coluna tab) t coordenadasMinas
    return (ConstructorTabuleiro tabuleiroComMinas)

inserirMina :: Int -> Int -> [[(Int, Int)]] -> [[(Int, Int)]]
inserirMina linha coluna tabuleiro =
    take linha tabuleiro ++
    [take coluna (tabuleiro !! linha) ++ [(2, 0)] ++ drop (coluna + 1) (tabuleiro !! linha)] ++
    drop (linha + 1) tabuleiro

contarMinasAdjacentes :: TypeTabuleiro -> Int -> Int -> Int
contarMinasAdjacentes (ConstructorTabuleiro t) linha coluna =
  let direcoes = [(-1, -1), (-1, 0), (-1, 1), 
                  (0, -1),         (0, 1), 
                  (1, -1), (1, 0), (1, 1)]
      checarMinas (dL, dC) =
        let novaLinha = linha + dL
            novaColuna = coluna + dC
        in if novaLinha >= 0 && novaColuna >= 0 && 
            novaLinha < length t && 
            novaColuna < length (t !! novaLinha) && 
            fst ((t !! novaLinha) !! novaColuna) == 2 
            then 1
            else 0
  in sum (map checarMinas direcoes)

atualizarTabuleiro :: TypeTabuleiro -> IO TypeTabuleiro
atualizarTabuleiro (ConstructorTabuleiro t) =
  let tabuleiroChecado = [[if fst (t !! linha !! coluna) == 2
                          then (2, 0)
                          else (fst (t !! linha !! coluna), contarMinasAdjacentes (ConstructorTabuleiro t) linha coluna)
                          | coluna <- [0..length (t !! linha) - 1]]
                          | linha <- [0..length t - 1]]
  in return (ConstructorTabuleiro tabuleiroChecado)

selecionaCasa :: TypeTabuleiro -> (Int, Int) -> IO TypeTabuleiro
selecionaCasa (ConstructorTabuleiro t) (x, y) =
  let valorAtual = fst (t !! x !! y)
      novoValor = if valorAtual == 2
                then 3  -- Escolheu casa com bomba
                else 1
      linhaAtualizada = take y (t !! x) ++ [(novoValor, snd (t !! x !! y))] ++ drop (y + 1) (t !! x)
      tabuleiroAtualizado = take x t ++ [linhaAtualizada] ++ drop (x + 1) t
  in return (ConstructorTabuleiro tabuleiroAtualizado)

checaTabuleiroGameOver :: TypeTabuleiro -> Int
checaTabuleiroGameOver (ConstructorTabuleiro t) =
  if any (\linha -> any (\coluna -> fst (t !! linha !! coluna) == 3) [0 .. length (t !! linha) - 1]) [0 .. length t - 1]
  then 0
  else 1 

imprimirTabuleiro :: TypeTabuleiro -> IO ()
imprimirTabuleiro (ConstructorTabuleiro t) = mapM_ putStrLn (map formatarLinha t)
  where
    formatarLinha linha = unwords (map formatarTupla linha)
    formatarTupla (x, y) = if x == 1 then ("[" ++ show y ++ "]") else "[X]"

jogar :: TypeTabuleiro -> IO ()
jogar tabuleiro = do
  putStrLn "Tabuleiro atual:"
  imprimirTabuleiro tabuleiro
  putStrLn "Insira as coordenadas que deseja selecionar: (x, y)"
  (linha, coluna) <- readLn :: IO (Int, Int)
  newTabuleiro <- selecionaCasa tabuleiro (linha, coluna)

  estadoDeJogo <- return (checaTabuleiroGameOver newTabuleiro)

  if estadoDeJogo == 0 
  then putStrLn "A casa escolhida continha uma bomba! Game over."
  else jogar newTabuleiro

main :: IO ()
main = do
  let estadoDeJogo = 1   -- jogo ta rolando
  tabuleiro <- criarTabuleiro 4 4     -- número de linhas e colunas do tabuleiro
  tabuleiro <- gerarMinas tabuleiro 4 4 5 -- número de linhas e colunas do tabuleiro e numero de minas a serem inseridas no tabuleiro
  tabuleiro <- atualizarTabuleiro tabuleiro
  jogar tabuleiro