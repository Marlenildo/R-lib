# =========================================================
# 05_funcoes_graficos.R
# Funções gráficas reutilizáveis
# =========================================================

grafico_media_ep <- function(
    dados,
    variavel,
    y_label,
    x_var,
    x_label
) {
  
  ggplot(
    dados |> filter(name == variavel),
    aes(x = {{ x_var }}, y = mean)
  ) +
    geom_col() +
    geom_errorbar(
      aes(
        ymin = mean - se,
        ymax = mean + se
      ),
      width = 0.2
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    xlab(x_label) +
    ylab(y_label)
}

# -------------------------------------------------------------------------
# Função: grafico_multiplas_variaveis
# -------------------------------------------------------------------------
# Descrição:
# Esta função automatiza a construção de figuras compostas por múltiplos
# gráficos de barras com erro-padrão da média, organizados em um layout
# flexível de linhas e colunas. A função é destinada à visualização de
# variáveis resposta provenientes de experimentos fatoriais, permitindo
# a comparação gráfica entre níveis de um fator (por exemplo, condição
# hídrica ou dose aplicada).
#
# A função utiliza internamente a função grafico_media_ep(), responsável
# pela construção de cada gráfico individual. Os gráficos são então
# combinados em um painel único por meio da função ggarrange() do pacote
# ggpubr.
#
# O layout da figura pode ser controlado explicitamente pelo usuário por
# meio dos argumentos ncol e nrow. Caso apenas um deles seja informado,
# o outro é calculado automaticamente com base no número total de gráficos.
# Se nenhum for especificado, assume-se um layout padrão de duas colunas.
#
# Argumentos:
# - dados: data.frame contendo as estatísticas resumo (média e erro-padrão),
#          no formato longo, com as colunas utilizadas pela função
#          grafico_media_ep().
# - variaveis: vetor de caracteres com os nomes das variáveis a serem
#              representadas graficamente.
# - nomes_y: vetor nomeado contendo os rótulos do eixo y associados a cada
#            variável.
# - x_var: fator a ser utilizado no eixo x (por exemplo, hid ou dose).
# - x_label: rótulo do eixo x.
# - ncol: número de colunas do painel de gráficos (opcional).
# - nrow: número de linhas do painel de gráficos (opcional).
# - labels: lógico indicando se letras identificadoras (A, B, C, ...)
#           devem ser adicionadas aos gráficos.
#
# Valor retornado:
# - Um objeto ggplot/ggarrange contendo o painel de gráficos, pronto para
#   visualização ou exportação em relatórios e artigos científicos.
#
# Observações:
# - A função foi desenvolvida para garantir padronização visual,
#   reprodutibilidade e redução de código manual na geração de figuras
#   multi-painel em análises experimentais.
# - Compatível com figuras destinadas a relatórios técnicos, dissertações,
#   teses e artigos científicos.
# -------------------------------------------------------------------------

grafico_multiplas_variaveis <- function(
    dados,
    variaveis,
    x_var,
    x_label,
    nomes_y = NULL,
    ncol = NULL,
    nrow = NULL,
    labels = TRUE
) {
  
  graficos <- lapply(
    variaveis,
    function(var) {
      
      y_label <- if (!is.null(nomes_y) && var %in% names(nomes_y)) {
        nomes_y[[var]]
      } else {
        var
      }
      
      grafico_media_ep(
        dados    = dados,
        variavel = var,
        y_label  = y_label,
        x_var    = {{ x_var }},
        x_label  = x_label
      )
    }
  )
  
  n_graficos <- length(graficos)
  
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
    ncol = ncol,
    nrow = nrow,
    labels = if (labels) LETTERS[seq_along(graficos)] else NULL
  )
}



grafico_medias_fatorial <- function(
    dados,
    resposta,
    fator_interesse,
    bloco = NULL,
    fatores,
    nomes_variaveis = NULL,
    digitos = 2,
    ylab = NULL
) {
  
  # Médias + letras
  res <- medias_fatorial_cld(
    dados           = dados,
    resposta        = resposta,
    fator_interesse = fator_interesse,
    bloco           = bloco,
    fatores         = fatores
  )
  
  # Nome amigável
  ylab <- if (!is.null(ylab)) {
    ylab
  } else if (!is.null(nomes_variaveis) && resposta %in% names(nomes_variaveis)) {
    nomes_variaveis[[resposta]]
  } else {
    resposta
  }
  
  ggplot2::ggplot(res, ggplot2::aes(x = nivel, y = media)) +
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
      y = ylab
    ) +
    ggplot2::theme_classic()
}


###########
grafico_medias_fatorial <- function(
    dados,
    resposta,
    fator_interesse,
    bloco = NULL,
    fatores,
    nomes_variaveis = NULL,
    digitos = 2,
    ylab = NULL
) {
  
  # Médias + letras
  res <- medias_fatorial_cld(
    dados           = dados,
    resposta        = resposta,
    fator_interesse = fator_interesse,
    bloco           = bloco,
    fatores         = fatores
  )
  
  # Nome amigável
  ylab <- if (!is.null(ylab)) {
    ylab
  } else if (!is.null(nomes_variaveis) && resposta %in% names(nomes_variaveis)) {
    nomes_variaveis[[resposta]]
  } else {
    resposta
  }
  
  ggplot2::ggplot(res, ggplot2::aes(x = nivel, y = media)) +
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
      y = ylab
    ) +
    ggplot2::theme_classic()
}

##
grafico_interacao_fatorial <- function(
    dados,
    resposta,
    fator_x,
    fator_traco,
    bloco = NULL,
    fatores,
    nomes_variaveis = NULL,
    digitos = 2
) {
  
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
  
  ylab <- if (!is.null(nomes_variaveis) && resposta %in% names(nomes_variaveis)) {
    nomes_variaveis[[resposta]]
  } else {
    resposta
  }
  
  ggplot2::ggplot(
    df,
    ggplot2::aes_string(
      x = fator_x,
      y = "emmean",
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
      x = fator_x,
      y = ylab,
      color = fator_traco
    ) +
    ggplot2::theme_classic()
}
