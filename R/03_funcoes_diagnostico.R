# =========================================================
# 03_funcoes_diagnostico.R
# Diagnóstico de pressupostos da ANOVA
#
# ✔️ Usa o MESMO modelo da ANOVA
# ✔️ Compatível com DIC e DBC
# ✔️ Compatível com 1 ou n fatores
# =========================================================

#' Diagnostica pressupostos da ANOVA fatorial
#'
#' Ajusta o mesmo modelo usado na ANOVA fatorial e executa testes
#' de normalidade (Shapiro-Wilk) e homogeneidade (Levene) para uma
#' ou mais variaveis resposta.
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param variaveis Vetor de nomes das variaveis resposta.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor de nomes dos fatores.
#' @param alpha Nivel de significancia para destacas violacoes.
#' @param mostrar_graficos Se `TRUE`, plota os graficos diagnosticos do modelo.
#' @param caption Legenda da tabela formatada.
#'
#' @return Lista com `tabela` (kable formatada) e `dados_brutos` (data.frame).
#' @export
anova_diagnostico <- function(
    dados,
    variaveis,
    bloco = NULL,
    fatores,
    alpha = 0.05,
    mostrar_graficos = TRUE,
    caption = "Testes de normalidade (Shapiro-Wilk) e homogeneidade de variâncias (Levene)"
) {
  
  stopifnot(
    is.character(variaveis),
    is.character(fatores)
  )
  
  resumo <- data.frame(
    Variavel = character(),
    p_normalidade = numeric(),
    p_homogeneidade = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (v in variaveis) {
    
    # -------------------------------------------------
    # Ajusta o MESMO modelo usado na ANOVA
    # -------------------------------------------------
    modelo <- ajusta_modelo_fatorial(
      dados    = dados,
      resposta = v,
      bloco    = bloco,
      fatores  = fatores
    )
    
    # -------------------------------------------------
    # Resíduos do modelo
    # -------------------------------------------------
    res <- residuals(modelo)
    
    # Shapiro-Wilk (normalidade dos resíduos)
    testeN <- if (length(unique(res)) > 1) {
      shapiro.test(res)
    } else {
      list(p.value = NA)
    }
    
    
    # -------------------------------------------------
    # Levene (homogeneidade de variâncias)
    # Agrupa por todas as combinações fatoriais
    # -------------------------------------------------
    dados_modelo <- modelo$model
    
    grupo <- interaction(dados_modelo[fatores], drop = TRUE)
    
    testeH <- car::leveneTest(
      res ~ grupo,
      center = median
    )
    
    resumo <- rbind(
      resumo,
      data.frame(
        Variavel = v,
        p_normalidade = testeN$p.value,
        p_homogeneidade = testeH$`Pr(>F)`[1]
      )
    )
    
    # -------------------------------------------------
    # Gráficos diagnósticos padrão da ANOVA
    # -------------------------------------------------
    if (mostrar_graficos) {
      par(mfrow = c(2, 2))
      plot(modelo, main = v)
    }
  }
  
  if (mostrar_graficos) par(mfrow = c(1, 1))
  
  # -------------------------------------------------
  # Tabela final formatada
  # -------------------------------------------------
  tabela <- resumo |>
    dplyr::mutate(
      Normalidade   = ifelse(p_normalidade < alpha, "Não", "Sim"),
      Homogeneidade = ifelse(p_homogeneidade < alpha, "Não", "Sim")
    ) |>
    dplyr::select(
      Variavel,
      p_normalidade, Normalidade,
      p_homogeneidade, Homogeneidade
    )
  
  
  linhas <- which(
    tabela$p_normalidade < alpha |
      tabela$p_homogeneidade < alpha
  )
  
  list(
    tabela =
      tabela |>
      knitr::kable(
        caption = caption,
        digits = 4
      ) |>
      kableExtra::kable_classic(html_font = "Arial") |>
      kableExtra::add_header_above(
        c(
          " " = 1,
          "Normalidade (Shapiro-Wilk)" = 2,
          "Homogeneidade (Levene)" = 2
        )
      ) |>
      kableExtra::row_spec(
        linhas,
        background = "#fdecea",
        bold = TRUE
      ),
    dados_brutos = resumo
  )
}
