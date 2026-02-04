# =========================================================
# 00_setup.R
# Configurações globais do ambiente R
#
# Este script define opções gerais que afetam:
# - comportamento padrão do R
# - formatação numérica em tabelas e relatórios
# - idioma de datas (quando possível)
#
# Deve ser carregado no início de todo relatório ou projeto
# que utilize o R_lib.
# =========================================================


# ---------------------------------------------------------
# Opções globais do R
# ---------------------------------------------------------

options(
  # Impede a conversão automática de strings em fatores.
  # Evita comportamentos inesperados em manipulação de dados
  # e garante maior controle sobre variáveis categóricas.
  stringsAsFactors = FALSE,
  
  # Evita notação científica em saídas numéricas.
  # Muito importante para relatórios científicos e tabelas
  # (ex.: 0.000123 em vez de 1.23e-04).
  scipen = 999
)


# ---------------------------------------------------------
# Configuração de locale para datas em português
# ---------------------------------------------------------
# Essa configuração afeta:
# - nomes de meses
# - nomes de dias da semana
# - formatação de datas em textos e gráficos
#
# O uso de try(..., silent = TRUE) garante que:
# - o código não quebre em sistemas sem locale pt_BR
# - funcione em Windows, Linux, servidores e CI
#
# Caso o locale não esteja disponível, o R continuará
# usando o locale padrão do sistema.
# ---------------------------------------------------------
try(
  Sys.setlocale("LC_TIME", "pt_BR.UTF-8"),
  silent = TRUE
)
