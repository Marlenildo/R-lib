# =========================================================
# 01_pacotes.R
# Pacotes utilizados nos relatórios estatísticos
#
# Este script centraliza o carregamento de todos os pacotes
# necessários para:
# - manipulação de dados
# - análises estatísticas
# - modelos fatoriais
# - gráficos
# - tabelas e relatórios
#
# Deve ser carregado uma única vez no início do relatório.
# =========================================================


# ---------------------------------------------------------
# Manipulação e organização de dados (core)
# ---------------------------------------------------------
library(tidyverse)   # dplyr, tidyr, ggplot2, tibble, stringr, etc.
library(readxl)      # leitura de arquivos Excel
library(readr)       # leitura de arquivos texto
library(janitor)     # limpeza e padronização de nomes
library(purrr)       # programação funcional (map, map_dfr, etc.)
library(broom)       # organiza saídas de modelos estatísticos


# ---------------------------------------------------------
# Estatística experimental e testes
# ---------------------------------------------------------
library(easyanova)   # ANOVA e testes de médias automatizados
library(ExpDes.pt)   # delineamentos experimentais (português)
library(rstatix)     # estatística descritiva e testes
library(car)         # testes de pressupostos (Levene, etc.)


# ---------------------------------------------------------
# Modelos lineares, médias ajustadas e comparações múltiplas
# ---------------------------------------------------------
library(emmeans)     # médias ajustadas (LS-means)
library(multcomp)    # letras de comparação múltipla (CLD)


# ---------------------------------------------------------
# Análises multivariadas
# ---------------------------------------------------------
library(FactoMineR)  # PCA, CA, MFA, etc.
library(factoextra) # visualização de análises multivariadas
library(missMDA)     # imputação de dados ausentes
library(MVN)         # normalidade multivariada
library(psych)       # estatística descritiva e psicometria


# ---------------------------------------------------------
# Gráficos
# ---------------------------------------------------------
library(ggplot2)     # sistema gráfico (já incluso no tidyverse)
library(ggpubr)      # gráficos prontos para publicação
library(grid)        # sistema gráfico base
library(tiff)        # exportação de imagens TIFF
library(extrafont)   # fontes personalizadas em gráficos


# ---------------------------------------------------------
# Tabelas e relatórios
# ---------------------------------------------------------
library(kableExtra)  # tabelas formatadas para HTML/PDF
library(DT)          # tabelas interativas (DataTables)
