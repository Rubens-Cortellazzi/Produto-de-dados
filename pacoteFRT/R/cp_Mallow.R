#' @export
#' @title C_p de Mallow
#' @import leaps
#' @param data base de dados que deseja fazer a regressão linear.
#' @param resposta variável resposta do banco de dados "data".
#' @author Felipe Marques, Pedro Galera, Rubens Roncato, Tiago Monteiro e Vitor Rizzo.
#' @returns Retorna o gráfico de C_p de Mallow com os melhores 5 modelos.
#' @description Irá pegar o banco de dados e a variável resposta que o usuário passar e irá retornar o gráfico de C_p de Mallow com os melhores 5 modelos.
Cp_Mallow <- function(dados, resposta){
  # Crie um novo quadro de dados que inclui a variável de resposta e todas as variáveis preditoras
  df <- cbind(dados[, resposta, drop = FALSE], dados[, !colnames(dados) %in% resposta])
  leaps <- regsubsets(formula(paste(resposta, "~ .")), data = df, nbest = 5)
  a <- plot(leaps, scale = "Cp")
  return(a)
}

# Exemplo de uso
# library(leaps)
# Cp_Mallow(mtcars, "mpg")
