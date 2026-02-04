# =========================================================
# 06_funcoes_modelo_fatorial_medias.R
#
# Conjunto de funções genéricas para:
# - Ajuste de modelos fatoriais (DIC ou DBC)
# - Análise de variância (ANOVA)
# - Testes de comparação de médias
#
# ✔️ Aceita 1 ou mais fatores
# ✔️ Aceita DIC (sem bloco) ou DBC (com bloco)
# ✔️ Funciona para qualquer banco de dados
#
# Autor: Marlenildo Ferreira Melo
# =========================================================


# ---------------------------------------------------------
# Função auxiliar: símbolos de significância estatística
#
# Retorna:
#   ""    → p ≥ 0,05
#   "*"   → p < 0,05
#   "**"  → p < 0,01
#   "***" → p < 0,001
#
# Usada em tabelas de ANOVA para marcar efeitos significativos
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
# 1️⃣ Ajuste genérico do modelo fatorial
#
# Esta função constrói e ajusta automaticamente um modelo
# de ANOVA para:
# - DIC (quando bloco = NULL)
# - DBC (quando bloco é informado)
#
# A fórmula é criada dinamicamente:
# - DIC: resposta ~ fator1 * fator2 * ...
# - DBC: resposta ~ bloco + fator1 * fator2 * ...
#
# Parâmetros:
# - dados: data.frame com o experimento
# - resposta: nome da variável resposta (string)
# - bloco: nome do bloco (string) ou NULL
# - fatores: vetor com nomes dos fatores experimentais
# ---------------------------------------------------------
ajusta_modelo_fatorial <- function(
    dados,
    resposta,
    bloco = NULL,
    fatores
) {
  
  stopifnot(
    is.character(resposta),
    is.character(fatores)
  )
  
  # Termo fatorial completo (inclui todas as interações)
  termo_fatores <- paste(fatores, collapse = " * ")
  
  # Define a fórmula conforme o delineamento
  formula_txt <- if (!is.null(bloco)) {
    paste(resposta, "~", bloco, "+", termo_fatores)
  } else {
    paste(resposta, "~", termo_fatores)
  }
  
  # Ajusta o modelo de ANOVA
  aov(as.formula(formula_txt), data = dados)
}


# ---------------------------------------------------------
# 2️⃣ Tabela de ANOVA fatorial (Quadrados Médios)
#
# Gera uma tabela única de ANOVA contendo:
# - Fonte de variação (FV)
# - Graus de liberdade (GL)
# - Quadrado médio (QM) para cada variável resposta
# - Asteriscos de significância
# - Coeficiente de variação (CV%)
#
# Compatível com:
# ✔️ DIC ou DBC
# ✔️ 1 ou múltiplos fatores
#
# Parâmetros:
# - variaveis: vetor de variáveis resposta
# - bloco: nome do bloco ou NULL
# - fatores: vetor de fatores experimentais
# ---------------------------------------------------------
anova_fatorial_qm_tabela <- function(
    dados,
    variaveis,
    bloco = NULL,
    fatores,
    caption = "Resumo da análise de variância (Quadrados Médios).",
    digitos = 4
) {
  
  stopifnot(
    is.character(variaveis),
    is.character(fatores)
  )
  
  # -------------------------------------------------
  # Seleciona apenas colunas necessárias
  # (evita erros e melhora desempenho)
  # -------------------------------------------------
  colunas_usadas <- c(bloco, fatores, variaveis)
  colunas_usadas <- colunas_usadas[!is.null(colunas_usadas)]
  
  dados_anova <- dados |>
    dplyr::select(all_of(colunas_usadas))
  
  # Termo fatorial completo
  termo_fatores <- paste(fatores, collapse = " * ")
  
  # Define a parte direita da fórmula
  formula_base <- if (!is.null(bloco)) {
    paste(bloco, "+", termo_fatores)
  } else {
    termo_fatores
  }
  
  # -------------------------------------------------
  # Modelo de referência (define FV e GL)
  # -------------------------------------------------
  modelo_ref <- aov(
    as.formula(paste(variaveis[1], "~", formula_base)),
    data = dados_anova
  )
  
  anova_ref <- summary(modelo_ref)[[1]]
  
  tabela <- data.frame(
    FV = rownames(anova_ref),
    GL = anova_ref$Df,
    stringsAsFactors = FALSE
  )
  
  # -------------------------------------------------
  # Loop pelas variáveis resposta
  # -------------------------------------------------
  for (v in variaveis) {
    
    modelo <- aov(
      as.formula(paste(v, "~", formula_base)),
      data = dados_anova
    )
    
    anova_tab <- summary(modelo)[[1]]
    
    qm <- anova_tab$`Mean Sq`
    p  <- anova_tab$`Pr(>F)`
    
    tabela[[v]] <- mapply(
      function(qm_i, p_i) {
        
        if (is.na(qm_i)) return("")
        
        texto <- paste0(
          formatC(qm_i, digits = digitos, format = "f"),
          ifelse(sig_star(p_i) == "", "", paste0(" ", sig_star(p_i)))
        )
        
        pinta_se_signif(texto)
        
      },
      qm,
      p
    )
  }
  
  # -------------------------------------------------
  # Coeficiente de variação (CV%)
  # -------------------------------------------------
  cv_valores <- sapply(variaveis, function(v) {
    
    modelo <- aov(
      as.formula(paste(v, "~", formula_base)),
      data = dados_anova
    )
    
    anova_tab <- summary(modelo)[[1]]
    qm_erro <- tail(anova_tab$`Mean Sq`, 1)
    media   <- mean(dados_anova[[v]], na.rm = TRUE)
    
    (sqrt(qm_erro) / media) * 100
  })
  
  tabela <- rbind(
    tabela,
    c("CV (%)", NA, sprintf("%.2f", cv_valores))
  )
  
  colnames(tabela) <- c("FV", "GL", variaveis)
  
  # -------------------------------------------------
  # Saída formatada para relatório
  # -------------------------------------------------
  tabela |>
    knitr::kable(
      caption = caption,
      escape  = FALSE,
      align   = "l",
      format  = "html"
    ) |>
    kableExtra::add_header_above(
      c(
        " " = 2,
        "QM" = length(variaveis)
      ),
      align = "l"
    ) |> 
    kableExtra::kable_classic(
      bootstrap_options = "striped",
      full_width = FALSE
    ) |>
    kableExtra::footnote(
      "QM = quadrado médio; * p < 0,05; ** p < 0,01; *** p < 0,001",
      general_title = ""
    )
}

# ---------------------------------------------------------
# 3️⃣ Escolha automática do teste de médias
#
# - 2 níveis  → teste t (sem ajuste)
# - ≥3 níveis → Tukey
# ---------------------------------------------------------
escolhe_teste_medias <- function(dados, fator) {
  
  stopifnot(is.character(fator))
  
  n_niveis <- length(unique(dados[[fator]]))
  
  if (n_niveis == 2) {
    "t"
  } else {
    "tukey"
  }
}


# ---------------------------------------------------------
# Função auxiliar: erro-padrão descritivo
#
# Calcula média e erro-padrão a partir dos dados brutos,
# por nível de fator, usando estatística descritiva.
#
# ⚠️ NÃO usa o modelo de ANOVA
# ---------------------------------------------------------
se_descritivo <- function(dados, resposta, fator) {
  
  dados |>
    dplyr::group_by(.data[[fator]]) |>
    rstatix::get_summary_stats(
      !!rlang::sym(resposta),
      type = "mean_se"
    ) |>
    dplyr::select(
      nivel = .data[[fator]],
      se_desc = se
    )
}

# ---------------------------------------------------------
# Função auxiliar: erro-padrão descritivo para interação
#
# Calcula média e erro-padrão por combinação
# fator_linha × fator_coluna
# ---------------------------------------------------------
se_descritivo_interacao <- function(
    dados,
    resposta,
    fator_linha,
    fator_coluna
) {
  
  dados |>
    dplyr::group_by(
      .data[[fator_linha]],
      .data[[fator_coluna]]
    ) |>
    rstatix::get_summary_stats(
      !!rlang::sym(resposta),
      type = "mean_se"
    ) |>
    dplyr::select(
      nivel_linha  = .data[[fator_linha]],
      nivel_coluna = .data[[fator_coluna]],
      se_desc = se
    )
}


# ---------------------------------------------------------
# 4️⃣ Médias ajustadas + erro-padrão + letras (CLD)
#
# Usa EXATAMENTE o mesmo modelo da ANOVA fatorial
# ---------------------------------------------------------
medias_fatorial_cld <- function(
    dados,
    resposta,
    fator_interesse,
    bloco = NULL,
    fatores,
    alpha = 0.05,
    tipo_se = c("modelo", "descritivo")
) {
  tipo_se <- match.arg(tipo_se)
  
  # Ajusta o modelo fatorial completo
  modelo <- ajusta_modelo_fatorial(
    dados    = dados,
    resposta = resposta,
    bloco    = bloco,
    fatores  = fatores
  )
  
  # Escolha automática do teste
  teste <- escolhe_teste_medias(dados, fator_interesse)
  
  # Médias ajustadas
  em <- emmeans::emmeans(
    modelo,
    as.formula(paste("~", fator_interesse))
  )
  
  # Letras de comparação
  letras <- multcomp::cld(
    em,
    alpha    = alpha,
    adjust   = ifelse(teste == "t", "none", "tukey"),
    Letters  = letters,
    reversed = TRUE
  )
  
  resultado <- tibble::tibble(
    nivel = letras[[fator_interesse]],
    media = letras$emmean,
    se    = letras$SE,
    grupo = letras$.group
  )
  
  # -------------------------------------------------
  # Substitui SE se for descritivo
  # -------------------------------------------------
  if (tipo_se == "descritivo") {
    
    se_desc <- se_descritivo(
      dados    = dados,
      resposta = resposta,
      fator    = fator_interesse
    )
    
    resultado <- resultado |>
      dplyr::left_join(se_desc, by = "nivel") |>
      dplyr::mutate(se = se_desc) |>
      dplyr::select(-se_desc)
  }
  
  resultado
  
}


# ---------------------------------------------------------
# 5️⃣ Tabela final de médias fatoriais
#
# Gera:
# - média ± erro-padrão
# - letras de comparação
# - tabela formatada (kable)
# ---------------------------------------------------------
tabela_medias_fatorial <- function(
    dados,
    variaveis,
    fator_interesse,
    bloco = NULL,
    fatores,
    nomes_variaveis = NULL,
    caption = "Médias ajustadas ± erro-padrão (teste de comparação de médias).",
    digitos = 2,
    tipo_se = c("modelo", "descritivo")
) {
  
  stopifnot(
    is.character(variaveis),
    is.character(fator_interesse),
    is.character(fatores)
  )
  tipo_se <- match.arg(tipo_se)
  
  resultado <- purrr::map_dfr(
    variaveis,
    function(v) {
      
      res <- medias_fatorial_cld(
        dados           = dados,
        resposta        = v,
        fator_interesse = fator_interesse,
        bloco           = bloco,
        fatores         = fatores,
        tipo_se         = tipo_se
      )
      
      nome_var <- if (!is.null(nomes_variaveis) && v %in% names(nomes_variaveis)) {
        nomes_variaveis[[v]]
      } else {
        v
      }
      
      res |>
        dplyr::mutate(
          variavel = nome_var,
          media_grupo = paste0(
            round(media, digitos),
            " ± ",
            round(se, digitos),
            " ",
            grupo
          )
        ) |>
        dplyr::select(nivel, variavel, media_grupo)
    }
  )
  
  resultado |>
    tidyr::pivot_wider(
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



# ---------------------------------------------------------
# 6️⃣ Tabela de desdobramento da interação fatorial
#
# Esta função é utilizada quando a interação entre dois
# fatores é significativa na ANOVA fatorial.
#
# São realizados DOIS desdobramentos estatisticamente
# independentes, utilizando o MESMO modelo ajustado:
#
# 1) fator_coluna dentro de cada nível de fator_linha
#    → comparação nas COLUNAS
#    → letras MINÚSCULAS
#
# 2) fator_linha dentro de cada nível de fator_coluna
#    → comparação nas LINHAS
#    → letras MAIÚSCULAS
#
# O teste de médias é escolhido automaticamente:
# - 2 níveis  → teste t (sem ajuste)
# - ≥3 níveis → Tukey
#
# Letras minúsculas comparam médias na coluna
# Letras maiúsculas comparam médias na linha
# ---------------------------------------------------------
tabela_interacao_fatorial <- function(
    dados,
    resposta,
    fator_linha,
    fator_coluna,
    bloco = NULL,
    fatores,
    digitos = 2,
    alpha = 0.05,
    caption = "Desdobramento da interação fatorial."
) {
  
  stopifnot(
    is.character(resposta),
    is.character(fator_linha),
    is.character(fator_coluna),
    is.character(fatores)
  )
  
  # -------------------------------------------------
  # Ajusta o MESMO modelo fatorial da ANOVA
  # -------------------------------------------------
  modelo <- ajusta_modelo_fatorial(
    dados    = dados,
    resposta = resposta,
    bloco    = bloco,
    fatores  = fatores
  )
  
  # =================================================
  # 1️⃣ Desdobramento: fator_coluna | fator_linha
  # (comparações nas COLUNAS → letras minúsculas)
  # =================================================
  
  teste_coluna <- escolhe_teste_medias(dados, fator_coluna)
  
  em_col <- emmeans::emmeans(
    modelo,
    as.formula(paste("~", fator_coluna, "|", fator_linha))
  )
  
  letras_col <- multcomp::cld(
    em_col,
    alpha    = alpha,
    adjust   = ifelse(teste_coluna == "t", "none", "tukey"),
    Letters  = LETTERS,
    reversed = TRUE
  )
  
  
  letras_col <- letras_col |>
    dplyr::mutate(
      nivel_linha  = .data[[fator_linha]],
      nivel_coluna = .data[[fator_coluna]],
      letra_col    = .group
    )
  
  # =================================================
  # 2️⃣ Desdobramento: fator_linha | fator_coluna
  # (comparações nas LINHAS → letras MAIÚSCULAS)
  # =================================================
  
  teste_linha <- escolhe_teste_medias(dados, fator_linha)
  
  em_lin <- emmeans::emmeans(
    modelo,
    as.formula(paste("~", fator_linha, "|", fator_coluna))
  )
  
  letras_lin <- multcomp::cld(
    em_lin,
    alpha    = alpha,
    adjust   = ifelse(teste_linha == "t", "none", "tukey"),
    Letters  = letters,
    reversed = TRUE
  )
  
  
  letras_lin <- letras_lin |>
    dplyr::mutate(
      nivel_linha  = .data[[fator_linha]],
      nivel_coluna = .data[[fator_coluna]],
      letra_lin    = .group
    )
  
  # =================================================
  # 3️⃣ Combinação das médias e das letras
  # =================================================
  
  tabela_final <- letras_col |>
    dplyr::left_join(
      letras_lin |>
        dplyr::select(nivel_linha, nivel_coluna, letra_lin),
      by = c("nivel_linha", "nivel_coluna")
    ) |>
    dplyr::mutate(
      media_fmt = paste0(
        round(emmean, digitos), " ",
        letra_lin, letra_col 
      )
    ) |>
    dplyr::select(
      !!fator_linha  := nivel_linha,
      !!fator_coluna := nivel_coluna,
      media_fmt
    ) |>
    tidyr::pivot_wider(
      names_from  = !!rlang::sym(fator_coluna),
      values_from = media_fmt
    )
  
  # =================================================
  # 4️⃣ Saída formatada para relatório
  # =================================================
  tabela_final |>
    knitr::kable(caption = caption) |>
    kableExtra::kable_classic(
      html_font  = "Arial",
      full_width = FALSE
    ) |>
    kableExtra::footnote(
      general_title = "",
      "Letras maiúsculas comparam médias na coluna e letras minúsculas comparam médias na linha, segundo teste t (2 níveis) ou Tukey (≥ 3 níveis), a 5% de probabilidade."
    )
}


# ---------------------------------------------------------
# 7️⃣ Tabela de desdobramento da interação fatorial
#    para MÚLTIPLAS variáveis resposta
#
# Versão SEGURA para uso em R_lib (via source()).
#
# - NÃO depende de kable interno
# - Retorna data.frame → depois formata
# - Mantém a MESMA lógica estatística da função unitária
# ---------------------------------------------------------
tabela_interacao_fatorial_multivariaveis <- function(
    dados,
    variaveis,
    fator_linha,
    fator_coluna,
    bloco = NULL,
    fatores,
    nomes_variaveis = NULL,
    digitos = 2,
    alpha = 0.05,
    tipo_se = c("modelo", "descritivo"),
    caption = "Desdobramento da interação fatorial."
) {
  
  stopifnot(
    is.character(variaveis),
    is.character(fator_linha),
    is.character(fator_coluna),
    is.character(fatores)
  )
  tipo_se <- match.arg(tipo_se)
  
  resultado <- purrr::map_dfr(
    variaveis,
    function(v) {
      
      # Nome amigável
      nome_var <- if (!is.null(nomes_variaveis) && v %in% names(nomes_variaveis)) {
        nomes_variaveis[[v]]
      } else {
        v
      }
      
      # Ajusta modelo
      modelo <- ajusta_modelo_fatorial(
        dados    = dados,
        resposta = v,
        bloco    = bloco,
        fatores  = fatores
      )
      
      # -------------------------------
      # COLUNAS → letras MAIÚSCULAS
      # -------------------------------
      teste_col <- escolhe_teste_medias(dados, fator_coluna)
      
      em_col <- emmeans::emmeans(
        modelo,
        as.formula(paste("~", fator_coluna, "|", fator_linha))
      )
      
      col_cld <- multcomp::cld(
        em_col,
        alpha    = alpha,
        adjust   = ifelse(teste_col == "t", "none", "tukey"),
        Letters  = LETTERS,
        reversed = TRUE
      ) |>
        dplyr::mutate(
          nivel_linha  = .data[[fator_linha]],
          nivel_coluna = .data[[fator_coluna]],
          letra_col    = trimws(.group)
        )
      
      # -------------------------------
      # LINHAS → letras minúsculas
      # -------------------------------
      teste_lin <- escolhe_teste_medias(dados, fator_linha)
      
      em_lin <- emmeans::emmeans(
        modelo,
        as.formula(paste("~", fator_linha, "|", fator_coluna))
      )
      
      lin_cld <- multcomp::cld(
        em_lin,
        alpha    = alpha,
        adjust   = ifelse(teste_lin == "t", "none", "tukey"),
        Letters  = letters,
        reversed = TRUE
      ) |>
        dplyr::mutate(
          nivel_linha  = .data[[fator_linha]],
          nivel_coluna = .data[[fator_coluna]],
          letra_lin    = trimws(.group)
        )
      
      # -------------------------------
      # Combinação final
      # -------------------------------
      tab_inter <- col_cld |>
        dplyr::left_join(
          lin_cld |>
            dplyr::select(nivel_linha, nivel_coluna, letra_lin),
          by = c("nivel_linha", "nivel_coluna")
        )
      
      # -------------------------------------------------
      # Substitui SE se for descritivo
      # -------------------------------------------------
      if (tipo_se == "descritivo") {
        
        se_desc <- se_descritivo_interacao(
          dados        = dados,
          resposta     = v,
          fator_linha  = fator_linha,
          fator_coluna = fator_coluna
        )
        
        tab_inter <- tab_inter |>
          dplyr::left_join(
            se_desc,
            by = c("nivel_linha", "nivel_coluna")
          ) |>
          dplyr::mutate(SE = se_desc) |>
          dplyr::select(-se_desc)
      } else {
        tab_inter <- tab_inter |>
          dplyr::mutate(SE = SE)
      }
      
      tab_inter |>
        dplyr::mutate(
          Variavel = nome_var,
          media_fmt = paste0(
            round(emmean, digitos), " ± ",
            round(SE, digitos), " ",
            letra_lin, letra_col
          )
        )|>
        dplyr::select(
          Variavel,
          !!fator_linha  := nivel_linha,
          !!fator_coluna := nivel_coluna,
          media_fmt
        )
    }
  )
  
  tabela_final <- resultado |>
    tidyr::pivot_wider(
      names_from  = !!rlang::sym(fator_coluna),
      values_from = media_fmt
    )
  tabela_final <- tabela_final |>
    dplyr::mutate(
      quebra = Variavel != dplyr::lag(Variavel, default = first(Variavel))
    )
  
  # número de níveis do fator da coluna
  n_colunas <- length(unique(dados[[fator_coluna]]))
  
  # cria o header dinamicamente
  header <- c(" " = 2)
  header[fator_coluna] <- n_colunas
  
  kb <- tabela_final |>
    dplyr::select(-quebra) |>
    knitr::kable(caption = caption, escape = FALSE) |>
    kableExtra::kable_classic(
      html_font  = "Arial",
      full_width = FALSE
    ) |>
    kableExtra::add_header_above(
      header,
      align = "l"
    )
  
  
  linhas_separadoras <- which(tabela_final$quebra)
  
  for (l in linhas_separadoras) {
    kb <- kb |>
      kableExtra::row_spec(
        l - 1,
        extra_css = "border-bottom: 2px solid black;"
      )
  }
  
  
  kb |>
    kableExtra::footnote(
      general_title = "",
      "Letras maiúsculas comparam médias na coluna e letras minúsculas comparam médias na linha.
     Teste t (2 níveis) ou Tukey (≥ 3 níveis), p < 0,05."
    )
  
}
