#' @export
#' @title Cria combinações de modelos lineares
#' @param data `data.frame` com variável resposta e covariáveis
#' @param y Nome da variável resposta do tipo `factor` ou `numeric`
#' @param variaveis Nomes das variáveis que podem entrar no modelo do tipo `factor` ou `numeric`
#' @author Felipe Marques, Pedro Galera, Rubens Roncato, Tiago Monteiro e Vitor Rizzo.
#' @returns Retorna uma lista de modelos lineares
modelos <- function(data, y, variaveis){

  if(!(y %in% names(data))){
    stop('"y" não encontrado em "data".')
  }

  if(!(class(data[[y]])) %in% c("factor", "numeric")){
    stop('"y" não é `factor` ou `numeric`.')
  }

  for (i in variaveis){

    if(!(i %in% names(data))){
      stop(paste0('Variável "', i,'"  não encontrada em "data".'))
    }

    if(!(class(data[[i]])) %in% c("factor", "numeric")){
      stop(paste0(i, 'não é `factor` ou `numeric`.'))
    }
  }

  combinacoes <- lapply(1:length(variaveis),
                        function(x) combn(variaveis, x, simplify = FALSE))

  combinacoes <- do.call(c, combinacoes)

  regressao <- function(combs){

    df <- data |> dplyr::select(y, combs)
    lm(as.formula(paste0(y, " ~ .")), df)

  }

  lista_modelos <- purrr::map(combinacoes, function(x) regressao(x))

  return(lista_modelos)

}

# lista <- modelos(iris, y = "Species", variaveis = c("Sepal.Length", "Sepal.Width",
#                                            "Petal.Length", "Petal.Width"))
