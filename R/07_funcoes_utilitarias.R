# =========================================================
# 07_funcoes_utilitarias.R
# Funções genéricas de apoio e utilidades
#
# ⚠️ Estas funções NÃO ajustam modelos estatísticos.
# São utilitários para inspeção, organização e preparação
# de dados para tabelas, gráficos e relatórios.
# =========================================================


# ---------------------------------------------------------
# 1️⃣ Remover colunas que contêm QUALQUER valor NA
#
# Mantém apenas colunas completamente observadas (100% sem NA).
#
# Útil para:
# - tabelas descritivas
# - visualização de dados
# - exportação de bancos "limpos"
#
# ⚠️ Não recomendado para uso direto em ANOVA ou modelos,
# pois pode alterar a estrutura experimental.
# ---------------------------------------------------------
remove_colunas_com_na <- function(dados) {
  dados |>
    dplyr::select(where(~ !any(is.na(.))))
}


# ---------------------------------------------------------
# 2️⃣ Selecionar apenas colunas que POSSUEM algum NA
#
# Retorna somente as colunas que apresentam pelo menos
# um valor ausente (NA).
#
# Útil para:
# - diagnóstico de dados faltantes
# - auditoria do banco
# - decidir estratégias de imputação
# ---------------------------------------------------------
seleciona_colunas_com_na <- function(dados) {
  dados |>
    dplyr::select(where(~ any(is.na(.))))
}


# ---------------------------------------------------------
# 3️⃣ Remover apenas colunas TOTALMENTE vazias (todos NA)
#
# Mantém colunas que têm ao menos um valor observado.
#
# Útil quando:
# - algumas variáveis não foram medidas
# - existem colunas "placeholder" no banco
#
# ✔️ Mais seguro que remove_colunas_com_na()
# ---------------------------------------------------------
remove_colunas_todas_na <- function(dados) {
  dados |>
    dplyr::select(where(~ !all(is.na(.))))
}


# ---------------------------------------------------------
# 4️⃣ Listar nomes das colunas que possuem NA
#
# Retorna um vetor de caracteres com os nomes das colunas
# que apresentam pelo menos um valor ausente.
#
# Útil para relatórios automáticos e logs.
# ---------------------------------------------------------
colunas_com_na <- function(dados) {
  names(dados)[colSums(is.na(dados)) > 0]
}


# ---------------------------------------------------------
# 5️⃣ Manter colunas específicas + colunas com algum NA
#
# Mantém:
# - todas as colunas explicitamente informadas pelo usuário
# - TODAS as colunas que possuem ao menos um NA
#
# Útil para:
# - auditoria de dados
# - relatórios de qualidade do banco
# - inspeção de variáveis problemáticas sem perder fatores
#
# Parâmetros:
# - dados: data.frame
# - colunas_fixas: vetor de nomes de colunas que SEMPRE
#   devem ser mantidas (ex.: bloco, fatores, id)
# ---------------------------------------------------------
manter_colunas_fixas_e_com_na <- function(dados, colunas_fixas) {
  
  stopifnot(is.character(colunas_fixas))
  
  # Colunas que possuem algum NA
  colunas_na <- names(dados)[colSums(is.na(dados)) > 0]
  
  # União das colunas
  colunas_manter <- union(colunas_fixas, colunas_na)
  
  dados |>
    dplyr::select(any_of(colunas_manter))
}
