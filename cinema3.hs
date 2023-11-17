import System.IO
import System.Process

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- Cancela a bufferização
  menuOpcoes

  return ()

data Filme = Filme {codigo :: Int, nome :: String, horarios :: [String], preco :: Float, classificacao :: String}
    deriving (Eq, Show)

-- Definição da lista de filmes disponíveis
filmesDisponiveis :: [Filme]
filmesDisponiveis =
  [ Filme {codigo = 1, nome = "Capitão América", horarios = ["14:00", "18:20"], preco = 10.0, classificacao = "12 anos"},
    Filme {codigo = 2, nome = "Homem de Ferro", horarios = ["15:40", "20:00"], preco = 12.5, classificacao = "12 anos"},
    Filme {codigo = 3, nome = "Thor", horarios = ["13:30", "17:45"], preco = 9.99, classificacao = "10 anos"}
  ]

-- Menu inicial
menuInicio :: IO Int
menuInicio = do
  limparTela
  putStrLn "\n    --| Cinema Haskell |--"
  putStrLn "MENU:"
  putStrLn "    1. Listar de Filmes"
  putStrLn "    2. Comprar Ingressos"
  putStrLn "    3. Cupom Fiscal"
  putStrLn "    4. Encerrar programa"
  putStr "Escolha o nº da opção: "
  hFlush stdout
  readLn

-- Opções do menu
menuOpcoes :: IO ()
menuOpcoes = do
  opcao <- menuInicio
  case opcao of
    1 -> do
         listarFilmes filmesDisponiveis
         voltarMenuInicial

    2 -> do
         comprarIngressos filmesDisponiveis
         voltarMenuInicial

    3 -> cupomFiscal >> voltarMenuInicial
    4 -> putStrLn "Obrigado por escolher nossos serviços! <3"
    _ -> menuOpcoes

-- Ações para cada opção do menu
listarFilmes :: [Filme] -> IO ()
listarFilmes filmes = do
    limparTela
    putStrLn "=================================================================="
    putStrLn "                 Cinema Haskell - Lista de Filmes                 "
    putStrLn "==================================================================\n"
    mapM_ (\f -> putStrLn $ "ID: " ++ show (codigo f) ++ " - " ++ nome f ++ " - Horários: " ++ show (horarios f) ++ " - Classificação: " ++ show (classificacao f)++ " - Preço: " ++ show (preco f)) filmes

comprarIngressos :: [Filme] -> IO ()
comprarIngressos filmes = do
  limparTela
  putStrLn "Lista de Filmes:"
  listarFilmes filmes
  putStrLn "\nPara comprar, insira o código do filme e o horário da sessão."
  putStrLn "Digite '0' para finalizar a seleção."
  comprarLoop filmes []

comprarLoop :: [Filme] -> [Filme] -> IO ()
comprarLoop filmes filmesSelecionados = do
  putStr "Código do filme: "
  codigoFilmeStr <- getLine
  let codigoFilme = read codigoFilmeStr :: Int
  if codigoFilme == 0
    then do
      putStrLn "Filmes adicionados com sucesso!"
      menuOpcoes
    else do
      putStr "Horário da sessão: "
      horario <- getLine
      case selecionarFilme codigoFilme horario filmes of
        Nothing -> do
          putStrLn "Filme ou horário inválido."
          comprarLoop filmes filmesSelecionados
        Just filmeSelecionado -> do
          let filmesSelecionadosAtualizados = filmeSelecionado : filmesSelecionados
          comprarLoop filmes filmesSelecionadosAtualizados

selecionarFilme :: Int -> String -> [Filme] -> Maybe Filme
selecionarFilme codigo horario filmes =
    case filter (\f -> codigo == Main.codigo f && horario `elem` horarios f) filmes of
        [filme] -> Just filme
        _ -> Nothing

cupomFiscal :: IO ()
cupomFiscal = do
  limparTela
  if null filmesDisponiveis
    then putStrLn "Você não escolheu os filmes. Vá ao menu Comprar ingresso."
    else do
      putStrLn "Cupom Fiscal:"
      mapM_ (\f -> putStrLn $ nome f ++ " - Preço: " ++ show (preco f)) filmesDisponiveis
      let total = sum $ map preco filmesDisponiveis
      putStrLn $ "Total a pagar: " ++ show total

voltarMenuInicial :: IO ()
voltarMenuInicial = do
  putStrLn "\nPressione qualquer tecla para voltar ao menu inicial."
  _ <- getLine -- Aguarda a entrada do usuário (qualquer tecla)
  menuOpcoes -- Volta pro menu

-- Função para limpar a tela
limparTela :: IO ()
limparTela = do
  _ <- system "cls"
  return ()
