# =========================================================
# 04_funcoes_tabelas.R
# Funcoes para tabelas
# =========================================================

#' Cria tabela interativa exportavel via DataTables
#'
#' @param dados `data.frame` a ser exibido.
#' @param titulo Titulo mostrado na tabela.
#'
#' @return Objeto `DT::datatable`.
#' @export
tabela_dt_exportavel <- function(
    dados,
    titulo = "Visualizacao dos dados"
) {

  DT::datatable(
    dados,
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = FALSE,
      buttons = c("copy", "csv", "excel", "pdf"),
      language = list(
        url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json"
      )
    ),
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left; font-weight: bold;",
      titulo
    )
  )
}
