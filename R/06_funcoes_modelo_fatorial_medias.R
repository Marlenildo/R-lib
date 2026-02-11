# =========================================================
# 06_funcoes_modelo_fatorial_medias.R
#
# Conjunto de funções genéricas para:
# - Ajuste de modelos fatoriais (DIC ou DBC)
# - Análise de variância (ANOVA)
# - Testes de comparação de médias
#
# PRINCÍPIO ARQUITETURAL:
# - Este arquivo NÃO conhece variáveis experimentais
# - Nomes, siglas e unidades são definidos no R_local
# - A biblioteca apenas CONSOME um dicionário opcional
#
# ✔️ Aceita 1 ou mais fatores
# ✔️ Aceita DIC (sem bloco) ou DBC (com bloco)
# ✔️ Funciona para qualquer banco de dados
#
# Autor: Marlenildo Ferreira Melo
# =========================================================


# =========================================================
# FUNÇÕES AUXILIARES GERAIS
# =========================================================

# ---------------------------------------------------------
# Símbolos de significância estatística
# ---------------------------------------------------------
#' @keywords internal
#' @noRd
sig_star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else ""
}

# ---------------------------------------------------------
# Formatação de p-valor
# ---------------------------------------------------------
#' @keywords internal
#' @noRd
formata_p_valor <- function(p, digitos = 4) {
  if (is.na(p)) return("")
  if (p < 10^(-digitos)) {
    paste0("< ", formatC(10^(-digitos), digits = digitos, format = "f"))
  } else {
    formatC(p, digits = digitos, format = "f")
  }
}

# ---------------------------------------------------------
# Pintar células com significância (kable)
# ---------------------------------------------------------
#' @keywords internal
#' @noRd
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
# Resolver rótulo de variável via dicionário (GENÉRICO)
#
# - dic_vars é DEFINIDO NO R_local
# - Estrutura mínima esperada:
#   tibble(var, label, sigla)
# ---------------------------------------------------------
#' Resolve rotulo de variavel por dicionario
#'
#' @param var Nome da variavel.
#' @param dic_vars Dicionario opcional com colunas `var`, `label`, `sigla`.
#' @param type Tipo de rotulo retornado.
#'
#' @return Vetor de caracteres com o rotulo resolvido.
#' @export
resolve_var_label <- function(
    var,
    dic_vars = NULL,
    type = c("label", "sigla", "var")
) {
  type <- match.arg(type)
  
  # Sem dicionário → fallback total
  if (is.null(dic_vars)) return(var)
  
  # Verificação mínima de contrato
  stopifnot(all(c("var", "label", "sigla") %in% names(dic_vars)))
  
  # Variável fora do dicionário → fallback
  if (!var %in% dic_vars$var) return(var)
  
  linha <- dic_vars[dic_vars$var == var, , drop = FALSE]
  
  if (type == "label") return(linha$label)
  if (type == "sigla") return(linha$sigla)
  
  var
}


# =========================================================
# 1️⃣ Ajuste genérico do modelo fatorial
# =========================================================
#' Ajusta modelo fatorial (DIC ou DBC)
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#'
#' @return Objeto `aov`.
#' @export
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
  
  termo_fatores <- paste(fatores, collapse = " * ")
  
  formula_txt <- if (!is.null(bloco)) {
    paste(resposta, "~", bloco, "+", termo_fatores)
  } else {
    paste(resposta, "~", termo_fatores)
  }
  
  aov(as.formula(formula_txt), data = dados)
}


# =========================================================
# 2️⃣ Tabela de ANOVA fatorial (Quadrados Médios)
# =========================================================
#' Gera tabela de ANOVA fatorial
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param variaveis Vetor de nomes das variaveis resposta.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param dic_vars Dicionario opcional com colunas `var`, `label`, `sigla`.
#' @param label_type Tipo de rotulo para variaveis resposta.
#' @param formato Formato de exibicao da tabela (`qm_star`, `f_p_colunas`, `f_p_inline`).
#' @param caption Legenda da tabela.
#' @param digitos Numero de casas decimais.
#'
#' @return Objeto `knitr_kable`.
#' @export
anova_fatorial_qm_tabela <- function(
    dados,
    variaveis,
    bloco = NULL,
    fatores,
    dic_vars = NULL,
    label_type = c("label", "sigla", "var"),
    formato = c("qm_star", "f_p_colunas", "f_p_inline"),
    caption = "Resumo da análise de variância.",
    digitos = 4
) {
  stopifnot(
    is.character(variaveis),
    is.character(fatores)
  )
  label_type <- match.arg(label_type)
  formato <- match.arg(formato)
  
  colunas_usadas <- c(bloco, fatores, variaveis)
  colunas_usadas <- colunas_usadas[!is.null(colunas_usadas)]
  
  dados_anova <- dados |>
    dplyr::select(dplyr::all_of(colunas_usadas))
  
  termo_fatores <- paste(fatores, collapse = " * ")
  
  formula_base <- if (!is.null(bloco)) {
    paste(bloco, "+", termo_fatores)
  } else {
    termo_fatores
  }
  
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
  
  for (v in variaveis) {
    
    modelo <- aov(
      as.formula(paste(v, "~", formula_base)),
      data = dados_anova
    )
    
    anova_tab <- summary(modelo)[[1]]
    
    qm <- anova_tab$`Mean Sq`
    f  <- anova_tab$`F value`
    p  <- anova_tab$`Pr(>F)`
    
    if (formato == "qm_star") {
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
    
    if (formato == "f_p_colunas") {
      tabela[[paste0(v, "_F")]] <- ifelse(
        is.na(f), "",
        formatC(f, digits = digitos, format = "f")
      )
      tabela[[paste0(v, "_p")]] <- sapply(p, formata_p_valor, digitos = digitos)
    }
    
    if (formato == "f_p_inline") {
      tabela[[v]] <- mapply(
        function(f_i, p_i) {
          if (is.na(f_i)) return("")
          paste0(
            formatC(f_i, digits = digitos, format = "f"),
            " (",
            formata_p_valor(p_i, digitos = digitos),
            ")"
          )
        },
        f,
        p
      )
    }
  }
  
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
  
  linha_cv <- c("CV (%)", NA)
  
  if (formato == "f_p_colunas") {
    cv_expandido <- as.vector(rbind(sprintf("%.2f", cv_valores), rep("", length(cv_valores))))
    linha_cv <- c(linha_cv, cv_expandido)
  } else {
    linha_cv <- c(linha_cv, sprintf("%.2f", cv_valores))
  }
  
  tabela <- rbind(tabela, linha_cv)
  
  nomes_cols <- sapply(
    variaveis,
    resolve_var_label,
    dic_vars = dic_vars,
    type = label_type
  )
  
  header_top <- NULL
  nota_rodape <- NULL
  
  if (formato == "f_p_colunas") {
    colnames(tabela) <- c("FV", "GL", rep(c("F", "p"), length(variaveis)))
    header_top <- c(" " = 2, setNames(rep(2, length(variaveis)), nomes_cols))
    nota_rodape <- "F = valor do teste F; p = valor-p."
  } else if (formato == "f_p_inline") {
    colnames(tabela) <- c("FV", "GL", nomes_cols)
    header_top <- c(" " = 2, "F (p)" = length(variaveis))
    nota_rodape <- "F (p) = valor do teste F com valor-p entre parênteses."
  } else {
    colnames(tabela) <- c("FV", "GL", nomes_cols)
    header_top <- c(" " = 2, "QM" = length(variaveis))
    nota_rodape <- "QM = quadrado médio; * p < 0,05; ** p < 0,01; *** p < 0,001"
  }
  
  tab_html <- tabela |>
    knitr::kable(
      caption = caption,
      escape  = FALSE,
      align   = "l",
      format  = "html"
    )
  
  tab_html <- tab_html |>
    kableExtra::add_header_above(header_top, align = "l") |>
    kableExtra::kable_classic(
      bootstrap_options = "striped",
      full_width = FALSE
    ) |>
    kableExtra::footnote(
      nota_rodape,
      general_title = ""
    )
  
  tab_html
}


# =========================================================
# 3️⃣ Escolha automática do teste de médias
# =========================================================
#' @keywords internal
#' @noRd
escolhe_teste_medias <- function(dados, fator) {
  stopifnot(is.character(fator))
  if (length(unique(dados[[fator]])) == 2) "t" else "tukey"
}


# =========================================================
# 4️⃣ Erros-padrão descritivos (simples e interação)
# =========================================================
#' @keywords internal
#' @noRd
se_descritivo <- function(dados, resposta, fator) {
  dados |>
    dplyr::group_by(.data[[fator]]) |>
    rstatix::get_summary_stats(
      !!rlang::sym(resposta),
      type = "mean_se"
    ) |>
    dplyr::select(nivel = .data[[fator]], se_desc = se)
}

#' @keywords internal
#' @noRd
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


# =========================================================
# 5️⃣ Médias ajustadas + CLD
# =========================================================
#' Calcula medias ajustadas e grupos de comparacao
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param fator_interesse Nome do fator para comparacao.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param alpha Nivel de significancia.
#' @param tipo_se Tipo de erro-padrao (`modelo` ou `descritivo`).
#'
#' @return `tibble` com niveis, medias, erro-padrao e grupos.
#' @export
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
  
  modelo <- ajusta_modelo_fatorial(
    dados, resposta, bloco, fatores
  )
  
  teste <- escolhe_teste_medias(dados, fator_interesse)
  
  em <- emmeans::emmeans(
    modelo,
    as.formula(paste("~", fator_interesse))
  )
  
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
  
  if (tipo_se == "descritivo") {
    se_desc <- se_descritivo(dados, resposta, fator_interesse)
    resultado <- resultado |>
      dplyr::left_join(se_desc, by = "nivel") |>
      dplyr::mutate(se = se_desc) |>
      dplyr::select(-se_desc)
  }
  
  resultado
}


# =========================================================
# 6️⃣ Tabela final de médias fatoriais
# =========================================================
#' Monta tabela de medias fatoriais
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param variaveis Vetor de nomes das variaveis resposta.
#' @param fator_interesse Nome do fator para comparacao.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param dic_vars Dicionario opcional com colunas `var`, `label`, `sigla`.
#' @param label_type Tipo de rotulo para variaveis resposta.
#' @param caption Legenda da tabela.
#' @param digitos Numero de casas decimais.
#' @param tipo_se Tipo de erro-padrao (`modelo` ou `descritivo`).
#'
#' @return Objeto `knitr_kable`.
#' @export
tabela_medias_fatorial <- function(
    dados,
    variaveis,
    fator_interesse,
    bloco = NULL,
    fatores,
    dic_vars = NULL,
    label_type = c("label", "sigla", "var"),
    caption = "Médias ajustadas ± erro-padrão.",
    digitos = 2,
    tipo_se = c("modelo", "descritivo")
) {
  label_type <- match.arg(label_type)
  tipo_se    <- match.arg(tipo_se)
  
  resultado <- purrr::map_dfr(
    variaveis,
    function(v) {
      res <- medias_fatorial_cld(
        dados, v, fator_interesse,
        bloco, fatores, tipo_se = tipo_se
      )
      
      nome_var <- resolve_var_label(
        v, dic_vars, label_type
      )
      
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
# 7️⃣ Tabela de desdobramento da interação fatorial
#
# Suporta dicionário externo para rotulagem da variável
# resposta, definido no R_local.
# ---------------------------------------------------------
#' Tabela de desdobramento da interacao fatorial
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param fator_linha Fator para linhas.
#' @param fator_coluna Fator para colunas.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param dic_vars Dicionario opcional com colunas `var`, `label`, `sigla`.
#' @param label_type Tipo de rotulo para variavel resposta.
#' @param digitos Numero de casas decimais.
#' @param alpha Nivel de significancia.
#' @param caption Legenda da tabela.
#'
#' @return Objeto `knitr_kable`.
#' @export
tabela_interacao_fatorial <- function(
    dados,
    resposta,
    fator_linha,
    fator_coluna,
    bloco = NULL,
    fatores,
    dic_vars = NULL,
    label_type = c("label", "sigla", "var"),
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
  
  label_type <- match.arg(label_type)
  
  # Nome da variável resposta (semântico)
  nome_var <- resolve_var_label(
    resposta,
    dic_vars = dic_vars,
    type     = label_type
  )
  
  # Ajuste do modelo fatorial completo
  modelo <- ajusta_modelo_fatorial(
    dados    = dados,
    resposta = resposta,
    bloco    = bloco,
    fatores  = fatores
  )
  
  # =================================================
  # 1️⃣ COLUNAS → letras MAIÚSCULAS
  # =================================================
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
  
  # =================================================
  # 2️⃣ LINHAS → letras minúsculas
  # =================================================
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
  
  # =================================================
  # 3️⃣ Combinação final
  # =================================================
  tabela_final <- col_cld |>
    dplyr::left_join(
      lin_cld |>
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
  
  knitr::kable(
    tabela_final,
    caption = paste(nome_var, "—", caption),
    escape  = FALSE
  ) |>
    kableExtra::kable_classic(
      html_font  = "Arial",
      full_width = FALSE
    ) |>
    kableExtra::footnote(
      general_title = "",
      "Letras maiúsculas comparam médias na coluna e letras minúsculas comparam médias na linha.
       Teste t (2 níveis) ou Tukey (≥ 3 níveis), p < 0,05."
    )
}


# ---------------------------------------------------------
# 8️⃣ Desdobramento da interação fatorial
#    para múltiplas variáveis resposta
#
# Totalmente compatível com dicionário externo
# ---------------------------------------------------------
#' Desdobra interacao fatorial para varias respostas
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param variaveis Vetor de nomes das variaveis resposta.
#' @param fator_linha Fator para linhas.
#' @param fator_coluna Fator para colunas.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param dic_vars Dicionario opcional com colunas `var`, `label`, `sigla`.
#' @param label_type Tipo de rotulo para variavel resposta.
#' @param digitos Numero de casas decimais.
#' @param alpha Nivel de significancia.
#' @param tipo_se Tipo de erro-padrao (`modelo` ou `descritivo`).
#' @param caption Legenda da tabela.
#'
#' @return Objeto `knitr_kable`.
#' @export
tabela_interacao_fatorial_multivariaveis <- function(
    dados,
    variaveis,
    fator_linha,
    fator_coluna,
    bloco = NULL,
    fatores,
    dic_vars = NULL,
    label_type = c("label", "sigla", "var"),
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
  
  label_type <- match.arg(label_type)
  tipo_se    <- match.arg(tipo_se)
  
  resultado <- purrr::map_dfr(
    variaveis,
    function(v) {
      
      nome_var <- resolve_var_label(
        v,
        dic_vars = dic_vars,
        type     = label_type
      )
      
      modelo <- ajusta_modelo_fatorial(
        dados    = dados,
        resposta = v,
        bloco    = bloco,
        fatores  = fatores
      )
      
      # COLUNAS
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
      
      # LINHAS
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
      
      tab_inter <- col_cld |>
        dplyr::left_join(
          lin_cld |>
            dplyr::select(nivel_linha, nivel_coluna, letra_lin),
          by = c("nivel_linha", "nivel_coluna")
        )
      
      if (tipo_se == "descritivo") {
        se_desc <- se_descritivo_interacao(
          dados, v, fator_linha, fator_coluna
        )
        
        tab_inter <- tab_inter |>
          dplyr::left_join(
            se_desc,
            by = c("nivel_linha", "nivel_coluna")
          ) |>
          dplyr::mutate(SE = se_desc) |>
          dplyr::select(-se_desc)
      }
      
      tab_inter |>
        dplyr::mutate(
          Variavel = nome_var,
          media_fmt = paste0(
            round(emmean, digitos), " ± ",
            round(SE, digitos), " ",
            letra_lin, letra_col
          )
        ) |>
        dplyr::select(
          Variavel,
          !!fator_linha  := nivel_linha,
          !!fator_coluna := nivel_coluna,
          media_fmt
        )
    }
  )
  
  resultado |>
    tidyr::pivot_wider(
      names_from  = !!rlang::sym(fator_coluna),
      values_from = media_fmt
    ) |>
    knitr::kable(
      caption = caption,
      escape  = FALSE
    ) |>
    kableExtra::kable_classic(
      html_font  = "Arial",
      full_width = FALSE
    )
}
