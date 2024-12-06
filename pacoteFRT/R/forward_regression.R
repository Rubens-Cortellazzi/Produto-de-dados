#' @export
#' @title Forward Regression
#' @param data base de dados que deseja fazer a regressão linear.
#' @param response variável resposta do banco de dados "data".
#' @author Felipe Marques, Pedro Galera, Rubens Roncato, Tiago Monteiro e Vitor Rizzo.
#' @returns Retorna o modelo de seleção Forward Regression, em que retorna todos os passos para a seleção do melhor modelo.
#' @description Irá pegar o banco de dados e a variável resposta que o usuário passar e irá aplicar o método Forward Regression e retornará o melhor modelo segundo esse critério de seleção.
forward_regression <- function(data, response) {
  all_vars <- setdiff(names(data), response)  # Todas as variáveis preditoras
  selected_vars <- character(0)  # Inicialmente, nenhuma variável é selecionada
  best_model <- NULL  # Melhor modelo inicialmente nulo
  best_aic <- Inf  # Melhor critério AIC inicialmente infinito

  while (length(all_vars) > 0) {
    aic_values <- numeric(length(all_vars))
    for (i in seq_along(all_vars)) {
      formula <- as.formula(paste(response, "~", paste(c(selected_vars, all_vars[i]), collapse = " + ")))
      model <- lm(formula, data = data)
      aic_values[i] <- AIC(model)
    }

    index_to_add <- which.min(aic_values)
    if (aic_values[index_to_add] < best_aic) {
      best_aic <- aic_values[index_to_add]
      selected_vars <- c(selected_vars, all_vars[index_to_add])
      best_model <- lm(paste(response, "~", paste(selected_vars, collapse = " + ")), data = data)
    } else {
      break
    }

    all_vars <- all_vars[-index_to_add]
  }

  return(best_model)
}

# plot(a, which = 1, las = 1, pch = 19, col = "blue")
# plot(a, which = 2, las = 1, pch = 19, col = "blue")
# plot(a, which =3, las = 1, pch = 19, col = "blue")
# # Exemplo de uso
# # data("mtcars")
# a <- forward_regression(mtcars, "mpg")
# a

#Comparação de resultados do verdadeiro
# completo = lm(mpg ~ . ,data = mtcars) #Regressão com todas as variáveis
# sozinho = lm(mpg ~ 1 ,data = mtcars)
# step(sozinho, scope=list(upper=completo, lower=sozinho), direction='forward', trace=TRUE)

#Criação de Variáveis Iniciais: A função forward_regression recebe dois argumentos: data, que é o conjunto de dados contendo todas as variáveis preditoras e a variável de resposta, e response, que é o nome da variável de resposta. Inicialmente, criamos algumas variáveis para acompanhar o progresso:
#
# all_vars: Uma lista de todas as variáveis preditoras no conjunto de dados.
# selected_vars: Uma lista vazia para acompanhar as variáveis selecionadas durante a seleção forward.
# best_model: Inicialmente definido como NULL, ele vai manter o melhor modelo encontrado até agora.
# best_aic: Inicialmente definido como infinito, para acompanhar o melhor valor de critério AIC.
# Loop While: Começamos um loop while que será executado enquanto houver variáveis preditoras não selecionadas (all_vars não estiver vazia).
#
# Cálculo do AIC para Modelos com Variáveis Adicionais: No interior do loop, calculamos o AIC para modelos que incluem cada variável preditora que ainda não foi selecionada. Para cada variável não selecionada, construímos um modelo linear (usando a função lm) que inclui essa variável juntamente com as variáveis já selecionadas (selected_vars). Calculamos o AIC para cada um desses modelos.
#
# Escolha da Melhor Variável: Encontramos a variável que resulta no menor valor de AIC, indicando que essa variável melhora o ajuste do modelo. A variável correspondente é armazenada em index_to_add.
#
# Verificação e Atualização do Modelo: Comparamos o valor de AIC encontrado com o melhor AIC encontrado até agora (best_aic). Se o novo valor de AIC for menor, isso indica que a variável atualmente considerada é uma adição valiosa ao modelo. Portanto, adicionamos essa variável à lista de selected_vars e atualizamos o best_model com o novo modelo que inclui a variável.
#
# Remoção da Variável Selecionada da Lista de Variáveis Não Selecionadas: A variável selecionada é removida da lista de all_vars para que não seja considerada novamente nas iterações subsequentes.
#
# Término do Loop: O loop continua até que não seja mais possível melhorar o modelo (ou seja, quando nenhuma variável adicionada melhora o AIC).
#
# Retorno do Melhor Modelo Encontrado: Finalmente, a função retorna o modelo linear final que inclui as variáveis selecionadas durante o processo de seleção forward.
#
# Essa função permite a seleção de variáveis forward com base no critério AIC, que é um indicador de qualidade do ajuste do modelo. Ela adiciona variáveis uma por uma, começando com um modelo que contém apenas o intercepto, e seleciona aquelas que melhoram o ajuste do modelo.
