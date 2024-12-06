#' @export
#' @importFrom tibble rownames_to_column
#' @title Tabela Análise Descritiva
#' @param data base de dados
#' @param variavel variável de interesse
#' @author Felipe Marques, Pedro Galera, Rubens Roncato, Tiago Monteiro e Vitor Rizzo.
#' @returns Retorna um data.frame formatado com métricas descritivas da variável de interesse.
#' @description Gera um data.frame com métricas descritivas para tabela.
tabela_descritiva <- function(dados, variavel) {

  dados_subset <- dados[, variavel, drop = FALSE]


  classes <- lapply(dados_subset, class)


  var_numeric <- classes %in% "numeric"

  if (!all(var_numeric)) {

    return()

  } else {

    dados_numeric <- dados_subset[, var_numeric, drop = FALSE]

    tabelas <- apply(dados_numeric, 2, summary)
    tabela_formatada <- data.frame(t(as.matrix(tabelas)))

    tabela_formatada <- tibble::rownames_to_column(tabela_formatada)

    colnames(tabela_formatada) <- c("Variável", "Mínimo", "1st Quantil", "Mediana", "Média", "3st Quantil", "Máximo")


    return(tabela_formatada)

  }

}
