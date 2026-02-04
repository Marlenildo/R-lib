# =========================================================
# 05_funcoes_graficos.R
#
# Funções gráficas reutilizáveis para experimentos fatoriais
#
# PRINCÍPIO ARQUITETURAL:
# - Este arquivo NÃO conhece variáveis experimentais
# - Rótulos, siglas e unidades vêm do R_local (dic_vars)
# - Todas as funções usam resolve_var_label()
#
# Compatível com:
# ✔️ DIC e DBC
# ✔️ 1, 2 ou n fatores
# ✔️ Relatórios HTML, PDF, Shiny
#
# Depende de:
# - ggplot2
# - dplyr
# - ggpubr
# - emmeans
#
# Autor: Marlenildo Ferreira Melo
# =========================================================


# =========================================================
# 1️⃣ Gráfico simples: média ± erro-padrão
#
# Usado quando os dados já estão resumidos (mean, se),
# normalmente após summarise().
# =========================================================
grafico_media_ep <- function(
    dados,
    variavel,
    x_var,
    x_label,
    dic_vars   = NULL,
    label_type = c("label", "sigla", "var")
) {
  
  label_type <- match.arg(label_type)
  
  # Resolve rótulo da variável resposta
  y_label <- resolve_var_label(
    variavel,
    dic_vars = dic_vars,
    type     = label_type
  )
  
  ggplot(
    dados |> dplyr::filter(name == variavel),
    ggplot2::aes(x = {{ x_var }}, y = mean)
  ) +
    ggplot2::geom_col(fill = "grey80", color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = mean - se,
        ymax = mean + se
      ),
      width = 0.2
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid = element_blank())
}


# =========================================================
# 2️⃣ Painel com múltiplas variáveis
#
# Combina vários gráficos de média ± EP em um layout único
# =========================================================
grafico_multiplas_variaveis <- function(
    dados,
    variaveis,
    x_var,
    x_label,
    dic_vars   = NULL,
    label_type = c("label", "sigla", "var"),
    ncol = NULL,
    nrow = NULL,
    labels = TRUE
) {
  
  label_type <- match.arg(label_type)
  
  graficos <- lapply(
    variaveis,
    function(var) {
      grafico_media_ep(
        dados      = dados,
        variavel   = var,
        x_var      = {{ x_var }},
        x_label    = x_label,
        dic_vars   = dic_vars,
        label_type = label_type
      )
    }
  )
  
  n_graficos <- length(graficos)
  
  # Definição automática do layout
  if (is.null(ncol) & is.null(nrow)) {
    ncol <- 2
    nrow <- ceiling(n_graficos / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(n_graficos / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n_graficos / ncol)
  }
  
  ggpubr::ggarrange(
    plotlist = graficos,
    ncol     = ncol,
    nrow     = nrow,
    labels   = if (labels) LETTERS[seq_along(graficos)] else NULL
  )
}


# =========================================================
# 3️⃣ Gráfico de médias fatoriais com letras (CLD)
#
# Usa o MESMO modelo da ANOVA
# =========================================================
grafico_medias_fatorial <- function(
    dados,
    resposta,
    fator_interesse,
    bloco = NULL,
    fatores,
    dic_vars   = NULL,
    label_type = c("label", "sigla", "var"),
    digitos = 2
) {
  
  label_type <- match.arg(label_type)
  
  # Médias ajustadas + letras
  res <- medias_fatorial_cld(
    dados           = dados,
    resposta        = resposta,
    fator_interesse = fator_interesse,
    bloco           = bloco,
    fatores         = fatores
  )
  
  # Rótulo semântico da variável resposta
  y_label <- resolve_var_label(
    resposta,
    dic_vars = dic_vars,
    type     = label_type
  )
  
  ggplot2::ggplot(
    res,
    ggplot2::aes(x = nivel, y = media)
  ) +
    ggplot2::geom_col(fill = "grey80", color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = media - se,
        ymax = media + se
      ),
      width = 0.2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = grupo),
      vjust = -0.5,
      size = 5
    ) +
    ggplot2::labs(
      x = fator_interesse,
      y = y_label
    ) +
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid = element_blank())
}


# =========================================================
# 4️⃣ Gráfico de interação fatorial
#
# Representa médias ajustadas para dois fatores
# =========================================================
grafico_interacao_fatorial <- function(
    dados,
    resposta,
    fator_x,
    fator_traco,
    bloco = NULL,
    fatores,
    dic_vars   = NULL,
    label_type = c("label", "sigla", "var"),
    digitos = 2
) {
  
  label_type <- match.arg(label_type)
  
  # Ajuste do modelo fatorial completo
  modelo <- ajusta_modelo_fatorial(
    dados    = dados,
    resposta = resposta,
    bloco    = bloco,
    fatores  = fatores
  )
  
  # Médias ajustadas
  em <- emmeans::emmeans(
    modelo,
    as.formula(paste("~", fator_x, "*", fator_traco))
  )
  
  df <- as.data.frame(em)
  
  # Rótulo da variável resposta
  y_label <- resolve_var_label(
    resposta,
    dic_vars = dic_vars,
    type     = label_type
  )
  
  ggplot2::ggplot(
    df,
    ggplot2::aes_string(
      x     = fator_x,
      y     = "emmean",
      group = fator_traco,
      color = fator_traco
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = emmean - SE,
        ymax = emmean + SE
      ),
      width = 0.15
    ) +
    ggplot2::labs(
      x     = fator_x,
      y     = y_label,
      color = fator_traco
    ) +
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid = element_blank())
}
