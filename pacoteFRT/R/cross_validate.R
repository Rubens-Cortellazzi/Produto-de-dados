#' @export
#' @title Faz validação cruzada de modelos de regressão
#' @param modelos Lista de modelos lineares
#' @param dados `Data.Frame` que está sendo usado na modelagem
#' @param y Nome da variável resposta
#' @param prop_train Proporção das observações de `dados` que será alocado para o grupo de treino, default é `0.7`
#' @author Felipe Marques, Pedro Galera, Rubens Roncato, Tiago Monteiro e Vitor Rizzo.
#' @returns Matriz com AIC, R^2, R^ Ajustado, MSE e MAPE de cada modelo
cross_validate <- function(modelos, dados, y, prop_train = 0.7) {

  if (prop_train <= 0 | prop_train >= 1) {
    stop("Proporção de dados de treino deve estar entre 0 e 1")
  }

  # Crie uma matriz para armazenar os resultados
  results <- matrix(NA, nrow = length(modelos), ncol = 5)

  folds <- sample(c("treino", "teste"), size = nrow(dados), replace = TRUE,
                  prob = c(prop_train, 1 - prop_train))

  df_cv <- cbind(dados, folds)

  for (i in seq_along(modelos)) {
    model <- modelos[[i]]

    # Selecione os dados de treino e teste para a dobra atual
    train_data <- df_cv[folds == "treino", ]
    test_data <- df_cv[folds == "teste", ]

    # Ajuste o modelo aos dados de treino
    fit <- lm(model, data = train_data)

    # Faça previsões nos dados de teste
    predictions <- predict(fit, newdata = test_data)

    # Calcule o erro para a dobra atual
    se <- (test_data[y] - predictions)^2
    results[i, 4] <- mean(se[[y]])

    ape <- abs((test_data[y] - predictions) / test_data[y]) * 100
    results[i, 5] <- mean(ape[[y]])

    # métricas

    results[i, 1] <- AIC(model)
    results[i, 2] <- summary(model)[["r.squared"]]
    results[i, 3] <- summary(model)[["adj.r.squared"]]

  }

  colnames(results) <- c("AIC", "R^2", "R^2 Ajustado", "MSE", "MAPE")

  return(results)
}
