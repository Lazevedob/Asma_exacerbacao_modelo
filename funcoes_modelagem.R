# funcoes_modelagem.R

# Funções utilitárias para modelagem de exacerbação em asma
# Requer: tidyverse, broom, readxl, pROC

library(tidyverse)
library(broom)
library(readxl)
library(pROC)

#----------------------------------------------------------
# Função para leitura padronizada da base analítica
#----------------------------------------------------------

#' Lê a planilha de dados analíticos
#' @param path Caminho para "dados_analitico_FINAL.xlsx"
#' @param sheet Nome da aba "Sheet1"
#' @return tibble com os dados

ler_dados_analiticos <- function(path = "dados_analitico_FINAL.xlsx",
                                 sheet = 1) {
  readxl::read_excel(path, sheet = sheet) |>
    janitor::clean_names()
}

#----------------------------------------------------------
# Função genérica de regressão logística simples
#----------------------------------------------------------

#' Ajusta regressão logística simples e calcula OR, IC95%, p e AUC
#'
#' @param data tibble com os dados
#' @param outcome nome (string) da variável desfecho binária (0/1)
#' @param predictor nome (string) do preditor
#' @param excluir_niveis vetor de níveis a excluir (tratados como missing)
#' @param ref nível de referência (string) para fatores; se NULL, usa o primeiro nível
#'
#' @return tibble com OR, IC95%, p, AUC e n para o preditor

ajustar_logistico_simples <- function(data,
                                      outcome,
                                      predictor,
                                      excluir_niveis = character(),
                                      ref = NULL) {
  # Seleciona apenas desfecho e preditor
  df <- data |>
    dplyr::select(dplyr::all_of(c(outcome, predictor))) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, predictor)))
  
  # Exclui níveis considerados "missing de fato"
  if (length(excluir_niveis) > 0) {
    df <- df |>
      dplyr::filter(!.data[[predictor]] %in% excluir_niveis)
  }
  
  # Garante que desfecho é 0/1 numérico
  df[[outcome]] <- as.numeric(df[[outcome]])
  
  # Ajusta tipo do preditor
  if (is.character(df[[predictor]])) {
    df[[predictor]] <- factor(df[[predictor]])
  }
  
  # Define referência, se aplicável
  if (is.factor(df[[predictor]]) && !is.null(ref)) {
    if (ref %in% levels(df[[predictor]])) {
      df[[predictor]] <- stats::relevel(df[[predictor]], ref = ref)
    } else {
      warning(glue::glue("Nível de referência '{ref}' não encontrado em {predictor}."))
    }
  }
  
  # Ajuste do modelo
  formula_str <- paste(outcome, "~", predictor)
  fit <- stats::glm(stats::as.formula(formula_str),
                    data = df,
                    family = stats::binomial(link = "logit"))
  
  # Tidy com OR e IC95%
  est <- broom::tidy(fit,
                     conf.int = TRUE,
                     conf.level = 0.95,
                     exponentiate = TRUE)
  
  # Remove intercepto
  est <- est |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(
      predictor = predictor,
      n_total = nrow(df),
      n_event = sum(df[[outcome]] == 1, na.rm = TRUE)
    ) |>
    dplyr::rename(
      or = estimate,
      ic95_inf = conf.low,
      ic95_sup = conf.high,
      p_valor = p.value
    )
  
  # AUC univariada com IC95%
  # Usa as probabilidades preditas do modelo
  prob <- stats::fitted(fit)
  roc_obj <- pROC::roc(response = df[[outcome]],
                       predictor = prob,
                       quiet = TRUE)
  auc_val <- as.numeric(roc_obj$auc)
  ci_auc <- pROC::ci.auc(roc_obj)
  
  est |>
    dplyr::mutate(
      auc = auc_val,
      auc_ic95_inf = as.numeric(ci_auc[1]),
      auc_ic95_sup = as.numeric(ci_auc[3])
    ) |>
    dplyr::select(
      predictor,
      term,
      n_total,
      n_event,
      or,
      ic95_inf,
      ic95_sup,
      p_valor,
      auc,
      auc_ic95_inf,
      auc_ic95_sup
    )
}

#----------------------------------------------------------
# Função auxiliar para rodar lista de variáveis categóricas
#----------------------------------------------------------

#' Ajusta modelos logísticos simples para lista de variáveis categóricas
#'
#' @param data tibble com dados
#' @param outcome nome da variável desfecho
#' @param vars_list lista de listas com elementos:
#'        - var: nome da variável
#'        - label: rótulo para tabela
#'        - excluir_niveis: vetor de níveis a excluir (opcional)
#'        - ref: nível de referência (opcional)
#'
#' @return tibble empilhando resultados dos modelos

ajustar_bloco_categoricas <- function(data,
                                      outcome,
                                      vars_list) {
  purrr::map_dfr(
    vars_list,
    function(x) {
      var_nome <- x$var
      var_label <- x$label
      excluir <- x$excluir_niveis %||% character()
      ref <- x$ref %||% NULL
      
      res <- ajustar_logistico_simples(
        data = data,
        outcome = outcome,
        predictor = var_nome,
        excluir_niveis = excluir,
        ref = ref
      )
      
      res |>
        dplyr::mutate(
          variavel = var_label,
          .before = predictor
        )
    }
  )
}

#----------------------------------------------------------
# Função auxiliar para variáveis numéricas (contínuas)
#----------------------------------------------------------

#' Ajusta modelos logísticos simples para variáveis contínuas
#'
#' @param data tibble com dados
#' @param outcome nome da variável desfecho
#' @param vars_num tibble ou lista com:
#'        - var: nome da variável
#'        - label: rótulo
#'
#' @return tibble com resultados

ajustar_bloco_numericas <- function(data,
                                    outcome,
                                    vars_num) {
  purrr::map_dfr(
    vars_num,
    function(x) {
      var_nome <- x$var
      var_label <- x$label
      
      res <- ajustar_logistico_simples(
        data = data,
        outcome = outcome,
        predictor = var_nome
      )
      
      res |>
        dplyr::mutate(
          variavel = var_label,
          .before = predictor
        )
    }
  )
}