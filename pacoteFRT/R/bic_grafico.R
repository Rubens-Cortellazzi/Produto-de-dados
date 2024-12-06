#' @export
#' @title BIC grafico
#' @import leaps
#' @param data base de dados que deseja fazer a regressão linear
#' @param resposta variável resposta do banco de dados "data".
#' @author Felipe Marques, Pedro Galera, Rubens Roncato, Tiago Monteiro e Vitor Rizzo.
#' @returns Retorna o gráfico de BIC com os melhores 5 modelos.
#' @description Irá pegar o banco de dados e a variável resposta que o usuário passar e irá retornar o gráfico BIC com os melhores 5 modelos.
bic_grafico <- function(dados, resposta){
  # Crie um novo quadro de dados que inclui a variável de resposta e todas as variáveis preditoras
  df <- cbind(dados[, resposta, drop = FALSE], dados[, !colnames(dados) %in% resposta])
  leaps <- regsubsets(formula(paste(resposta, "~ .")), data = df, nbest = 5)
  b <- plot(leaps, scale = "bic")
  return(b)
}

# Exemplo de uso
#bic_grafico(mtcars, "mpg")
