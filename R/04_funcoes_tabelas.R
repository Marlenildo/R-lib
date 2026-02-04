# =========================================================
# 04_funcoes_tabelas.R
# Funções para tabelas
# =========================================================

tabela_dt_exportavel <- function(
    dados,
    titulo = "Visualização dos dados"
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