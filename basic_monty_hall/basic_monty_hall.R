library(DiagrammeR)
library(bnlearn)

# Gera os dados de escolhas
simular_monty_hall <- function(z = 0) {
  v <- 1:3                                  # opcoes
  escolha_inicial <- sample(v, 1)           # escolha inicial aleatoria
  premio <- sample(v, 1)                   # premio aleatoria
  
  # Qual porta retirar?
  if (escolha_inicial == premio) {
    porta_retirada <- sample(setdiff(v, premio), 1)
  } else {
    porta_retirada <- setdiff(v, c(escolha_inicial, premio))
  }
  
  # Trocar porta?
  trocar <- sample(c("s", "n"), 1)
  
  # calculando resultado
  if (trocar == "s") {
    escolha_final <- setdiff(v, c(escolha_inicial, porta_retirada))
  } else {
    escolha_final <- escolha_inicial
  }
  result <- ifelse(escolha_final == premio, "ganhei", "perdi")
  
  # guardando no BD
  data.frame(escolha_inicial, premio, porta_retirada, trocar, result)
}

dados <- purrr::map_dfr(seq_len(1e4), simular_monty_hall) %>% 
  dplyr::mutate_all(as.factor)

# Cria nodes do grafo
nodes <- c("escolha_inicial", "premio", "porta_retirada", "trocar", "result")

# Matriz de adjacência
edges <- matrix(
  c("escolha_inicial", "porta_retirada",
    "premio", "porta_retirada",
    "porta_retirada", "trocar",
    "trocar", "result",
    "premio", "result",
    "escolha_inicial", "result"),
  ncol = 2, 
  byrow = TRUE)

# Cria grafo vazio
g <- bnlearn::empty.graph(nodes)
bnlearn::arcs(g) <- edges

# Ajuste da rede com os dados gerados aleatóriamente
fit <- bnlearn::bn.fit(g, dados)
bnlearn::cpquery(
  fitted = fit,
  event = (result == 'ganhei'), # o que queremos saber?
  evidence = (trocar == 's'), # informação adicional
  n = 5e6) # n grande para aumentar precisão


# Gerando diagrama do caso
diagrama <- "
graph LR;
A{D1Escolha inicial}-->B(X2Porta retirada);
C(X1Ferrari)-->B;
B-->D{D2Trocar porta};
D-->E[U1Ganhar]
C-->E
A-->E
"
# tweak para centralizar e grifar as variáveis
diagrama <- stringr::str_replace_all(
  diagrama, 
  pattern = "([XDU][0-9])", 
  replacement = "<center><b>\\1</b></center><br/>")

#DiagrammeR::DiagrammeR(diagrama)
