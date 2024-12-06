#' @export
#' @title Backward Regression
#' @param data base de dados que deseja fazer a regressão linear.
#' @param response variável resposta do banco de dados "data".
#' @author Felipe Marques, Pedro Galera, Rubens Roncato, Tiago Monteiro e Vitor Rizzo.
#' @returns Retorna o modelo de seleção Backward Regression, em que retorna todos os passos para a seleção do melhor modelo.
#' @description Irá pegar o banco de dados e a variável resposta que o usuário passar e irá aplicar o método Backward Regression e retornará o melhor modelo segundo esse critério de seleção.
backward_regression <- function(dados, resposta) {
  # Crie um quadro de dados apenas com as variáveis preditoras e a variável de resposta
  df <- dados[, c(resposta, setdiff(names(dados), resposta))]

  completo <- lm(reformulate(setdiff(names(dados), resposta), response = resposta), data = df)  # Modelo inicial com todas as variáveis
  melhores <- step(completo, direction = "backward", trace = 1)
  return(melhores)
}
# data("mtcars")
# a = backward_regression(mtcars, "mpg")
# r2 = summary(a)$r.squared
# r2adj = summary(fit)$adj.r.squared
#
# plot(a, which = 1, las = 1, pch = 19, col = "blue")
# plot(a, which = 2, las = 1, pch = 19, col = "blue")
# plot(a, which =3, las = 1, pch = 19, col = "blue")
#Criação de um novo quadro de dados df: O erro ocorreu porque a função step exige
#que todas as variáveis preditoras e a variável de resposta estejam no mesmo quadro
#de dados. Portanto, na função, criamos um novo quadro de dados chamado df que inclui
#apenas as variáveis preditoras e a variável de resposta necessárias para a regressão


#Comparação com o verdadeiro
# completo = lm(mpg ~ . ,data = mtcars) #Regressão com todas as variáveis
# sozinho = lm(mpg ~ 1 ,data = mtcars)
# step(completo, scope=list(upper = completo, lower = sozinho), direction='backward', trace=TRUE)

