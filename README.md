# rlibfatorial

Pacote R para analise de experimentos fatoriais em DIC e DBC, com suporte a:

- ANOVA fatorial para 1, 2, 3 ou mais fatores
- Diagnostico de normalidade e homogeneidade
- Tabelas de medias ajustadas com letras (CLD)
- Desdobramento de interacao fatorial
- Graficos de medias e interacao
- Utilitarios para preparo e auditoria de dados

## Escopo atual

Versao atual cobre experimentos fatoriais em DIC e DBC.
Parcelas subdivididas podem ser adicionadas no mesmo padrao em uma proxima versao.

## Instalacao local

```r
install.packages("remotes")
remotes::install_local(".", upgrade = "never")
```

Ou, durante desenvolvimento:

```r
devtools::load_all(".")
```

## Exemplo rapido

```r
library(rlibfatorial)

configurar_ambiente_rlib()

anova_fatorial_qm_tabela(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco = "bloco",
  fatores = c("dose", "hid"),
  formato = "qm_star"
)

anova_diagnostico(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco = "bloco",
  fatores = c("dose", "hid")
)

tabela_medias_fatorial(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  fator_interesse = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)
```

## Funcos principais

- `ajusta_modelo_fatorial()`
- `anova_fatorial_qm_tabela()`
- `anova_diagnostico()`
- `medias_fatorial_cld()`
- `tabela_medias_fatorial()`
- `tabela_interacao_fatorial()`
- `tabela_interacao_fatorial_multivariaveis()`
- `grafico_media_ep()`
- `grafico_multiplas_variaveis()`
- `grafico_medias_fatorial()`
- `grafico_interacao_fatorial()`
- `tabela_dt_exportavel()`

## Autor

Marlenildo Ferreira Melo
