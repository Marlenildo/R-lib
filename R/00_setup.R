#' Configura opcoes globais recomendadas
#'
#' Define opcoes para nao converter strings automaticamente em fatores,
#' evitar notacao cientifica e, opcionalmente, ajustar locale de datas.
#'
#' @param locale_time Locale de tempo para datas. Use `NULL` para nao alterar.
#'
#' @return Vetor nomeado com os valores anteriores de `options()`.
#' @export
configurar_ambiente_rlib <- function(locale_time = "pt_BR.UTF-8") {
  op_antigas <- options(
    stringsAsFactors = FALSE,
    scipen = 999
  )

  if (!is.null(locale_time)) {
    try(Sys.setlocale("LC_TIME", locale_time), silent = TRUE)
  }

  invisible(op_antigas)
}
