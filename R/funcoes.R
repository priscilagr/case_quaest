#' @title Tabela de contingência
#' 
#' @description Função para criar tabelas de contingência.
#' 
#' @param dados base de dados em formato data.frame.
#' @param var_linha nome da variável que aparecerá nas linhas da tabela de contigência.
#' @param var_coluna nome da variável que aparecerá nas colunas da tabela de contingência 
#' (percentual é calculado por coluna).
#'
#' @return Retorna um data.frame com a tabela de contingência. Colunas que iniciam com 
#' perc_ apresenta o resultado em percentual de acordo com a coluna. Colunas que iniciam 
#' com freq_ apresentam a frequência de acordo com a coluna.
#' 
tabela_contig <- function(dados, var_linha, var_coluna) {
  
  tabela_longa <- dados %>%
    count({{ var_linha }}, {{ var_coluna }}, name = "freq") %>%
    group_by({{ var_linha }}) %>%
    mutate(perc = 100 * freq/sum(freq))
  
  tabela_larga <- 
    tabela_longa %>%
    pivot_wider(id_cols = {{ var_coluna }}, names_from = {{ var_linha }}, values_from = c(perc, freq))
  
  return(tabela_larga)  
  
}

#' @title Tabela de contingência
#' 
#' @description Função para criar tabelas de contingência. Aceita como argumento do tipo character. Por examplo, "renda" ao invés de renda.
#' 
#' @param dados base de dados em formato data.frame.
#' @param var_linha nome da variável que aparecerá nas linhas da tabela de contigência.
#' @param var_coluna nome da variável que aparecerá nas colunas da tabela de contingência 
#' (percentual é calculado por coluna).
#'
#' @return Retorna um data.frame com a tabela de contingência. Colunas que iniciam com 
#' perc_ apresenta o resultado em percentual de acordo com a coluna. Colunas que iniciam 
#' com freq_ apresentam a frequência de acordo com a coluna.
#' 
tabela_contig_var <- function(dados, var_linha, var_coluna) {
  
  tabela_longa <- dados %>%
    count(.data[[var_linha]], .data[[var_coluna]], name = "freq") %>%
    group_by(.data[[var_linha]]) %>%
    mutate(perc = 100 * freq/sum(freq))
  
  tabela_larga <- 
    tabela_longa %>%
    pivot_wider(id_cols = {{ var_coluna }}, names_from = {{ var_linha }}, values_from = c(perc, freq))
  
  return(tabela_larga)  
  
}







