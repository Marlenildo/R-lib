# =========================================================
# 02_funcoes_anova.R
# Funções para ANOVA e testes de médias
# =========================================================

# ---------------------------------------------------------
# Símbolos de significância
# ---------------------------------------------------------
sig_star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else ""
}

# ---------------------------------------------------------
# Pintar células com significância (kable)
# ---------------------------------------------------------
pinta_se_signif <- function(x) {
  ifelse(
    grepl("\\*", x),
    kableExtra::cell_spec(
      x,
      background = "#FFF3CD",
      format = "html"
    ),
    x
  )
}

# ---------------------------------------------------------
# ANOVA (Quadrado Médio) – DBC Fatorial
# ---------------------------------------------------------
anova_qm_tabela <- function(
    dados,
    variaveis,
    bloco = "bloco",
    fatores = c("dose", "hid"),
    caption = "Análise de Variância."
) {
  
  termo_fatores <- paste(fatores, collapse = " * ")
  formula_base  <- paste(bloco, "+", termo_fatores)
  
  modelo_ref <- aov(
    as.formula(paste(variaveis[1], "~", formula_base)),
    data = dados
  )
  
  anova_ref <- summary(modelo_ref)[[1]]
  
  tabela <- data.frame(
    FV = rownames(anova_ref),
    GL = anova_ref$Df,
    stringsAsFactors = FALSE
  )
  
  for (var in variaveis) {
    
    modelo <- aov(
      as.formula(paste(var, "~", formula_base)),
      data = dados
    )
    
    anova_tab <- summary(modelo)[[1]]
    
    qm <- anova_tab$`Mean Sq`
    p  <- anova_tab$`Pr(>F)`
    
    tabela[[var]] <- mapply(
      function(qm_i, p_i) {
        
        if (is.na(qm_i)) return("")
        
        texto <- paste0(
          sprintf("%.4f", qm_i),
          ifelse(sig_star(p_i) == "", "", paste0(" ", sig_star(p_i)))
        )
        
        pinta_se_signif(texto)
        
      },
      qm,
      p
    )
  }
  
  # CV (%)
  cv_valores <- sapply(variaveis, function(var) {
    
    modelo <- aov(
      as.formula(paste(var, "~", formula_base)),
      data = dados
    )
    
    anova_tab <- summary(modelo)[[1]]
    
    qm_erro <- tail(anova_tab$`Mean Sq`, 1)
    media   <- mean(dados[[var]], na.rm = TRUE)
    
    (sqrt(qm_erro) / media) * 100
  })
  
  tabela <- rbind(
    tabela,
    c("CV (%)", NA, sprintf("%.2f", cv_valores))
  )
  
  colnames(tabela) <- c("FV", "GL", variaveis)
  
  tabela |>
    knitr::kable(
      caption = caption,
      align   = "l",
      escape  = FALSE,
      format  = "html"
    ) |>
    kableExtra::kable_classic(
      bootstrap_options = "striped",
      full_width = FALSE
    ) |>
    footnote(
      "QM = quadrado médio; * p < 0,05; ** p < 0,01; *** p < 0,001",
      general_title = ""
    )
}

# ---------------------------------------------------------
# Tabela automática de médias – easyanova
# ---------------------------------------------------------
# Função tabela_easyanova
# 
# A função tabela_easyanova automatiza a análise de variância e a apresentação de resultados de experimentos conduzidos em delineamento em blocos, utilizando o pacote easyanova no R. A partir de um banco de dados, do fator de interesse, das variáveis resposta e do método de comparação de médias, a função gera uma tabela contendo as médias acompanhadas do erro-padrão da média e das letras indicativas dos grupos de comparação.
# 
# Internamente, a função identifica automaticamente a posição do fator de interesse no conjunto de dados, determinando se este corresponde ao factor 1 ou factor 2 no objeto retornado pelo ea2(), independentemente do nome da variável. Em seguida, executa a análise de variância, extrai as médias ajustadas e seleciona dinamicamente o teste de comparação de médias indicado pelo usuário (LSD/t, Tukey, SNK, Duncan ou Scott-Knott).
# 
# As médias e erros-padrão são calculados diretamente a partir dos dados originais e combinados com os grupos de comparação em uma única tabela, organizada no formato largo. A saída é retornada já formatada para relatório, com legenda personalizável, garantindo padronização, reprodutibilidade e redução de código manual em análises estatísticas.
tabela_easyanova <- function(
    dados,
    fator,
    variaveis,
    teste = "t",
    caption = "Média ± erro-padrão (teste de médias)."
) {
  
  fator_nome <- rlang::as_name(rlang::ensym(fator))
  
  dados_anova <-
    dados |>
    dplyr::select(bloco, all_of(fator_nome), all_of(variaveis))
  
  fatores <- setdiff(names(dados_anova), c("bloco", variaveis))
  posicao_fator <- which(fatores == fator_nome)
  
  fator_easy <- paste0("Adjusted means (factor ", posicao_fator, ")")
  
  ea_res <- suppressWarnings(
    suppressMessages(
      {
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off(), add = TRUE)
        ea2(dados_anova, design = 2, list = TRUE)
      }
    )
  )
  
  tabela_grupo <-
    purrr::map_dfr(
      variaveis,
      \(v) {
        tab <- ea_res[[v]][[fator_easy]]
        col_fator <- names(tab)[1]
        
        tab |>
          transmute(
            nivel    = .data[[col_fator]],
            variavel = v,
            grupo    = .data[[teste]]
          )
      }
    )
  
  tabela_media <-
    dados |>
    dplyr::select(all_of(fator_nome), bloco, all_of(variaveis)) |>
    pivot_longer(cols = all_of(variaveis)) |>
    group_by(across(all_of(fator_nome)), name) |>
    get_summary_stats(value, type = "mean_se") |>
    mutate(media = paste0(round(mean, 2), " ± ", round(se, 2))) |>
    ungroup() |>
    dplyr::select(
      nivel = all_of(fator_nome),
      variavel = name,
      media
    )
  
  tabela_media |>
    left_join(tabela_grupo, by = c("nivel", "variavel")) |>
    mutate(media_grupo = paste(media, grupo)) |>
    dplyr::select(nivel, variavel, media_grupo) |>
    pivot_wider(
      names_from  = variavel,
      values_from = media_grupo
    ) |>
    knitr::kable(caption = caption) |>
    kableExtra::kable_classic(
      html_font = "Arial",
      bootstrap_options = "striped",
      full_width = FALSE
    )
}

