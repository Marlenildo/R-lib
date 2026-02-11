# rlibfatorial

Pacote R para analise de experimentos fatoriais em DIC e DBC.

## Visao geral

O `rlibfatorial` organiza um fluxo unico para:

- ajuste de modelo fatorial
- ANOVA para uma ou varias respostas
- diagnostico de pressupostos
- comparacao de medias (t ou Tukey)
- tabelas para relatorio
- graficos de medias e interacao

Escopo atual:

- DIC e DBC
- 1, 2, 3 ou mais fatores
- numero livre de niveis por fator

## Instalacao

### GitHub

```r
install.packages("remotes")
remotes::install_github("Marlenildo/R-lib")
```

### Local (tar.gz)

```r
install.packages("rlibfatorial_0.4.1.tar.gz", repos = NULL, type = "source")
```

### Desenvolvimento

```r
install.packages(c("devtools", "roxygen2"))
devtools::load_all(".")
```

## Dependencias principais

- `dplyr`, `tidyr`, `purrr`, `tibble`
- `ggplot2`, `ggpubr`
- `emmeans`, `multcomp`, `car`, `rstatix`
- `knitr`, `kableExtra`, `DT`, `htmltools`

## Inicio rapido

```r
library(rlibfatorial)

# Opcional: ajustes globais de ambiente
configurar_ambiente_rlib()
```

## Estrutura minima de dados

Seu `data.frame` precisa ter:

- uma coluna para cada fator
- opcionalmente uma coluna de bloco (para DBC)
- uma ou mais variaveis resposta numericas

Exemplo de nomes:

- `bloco`
- `dose`, `hid`, `cultivar`
- `ci`, `gs`, `mvr`

## Exemplo de uso (DBC)

```r
# ANOVA para varias respostas
anova_fatorial_qm_tabela(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco = "bloco",
  fatores = c("dose", "hid"),
  formato = "qm_star" # qm_star, f_p_colunas, f_p_inline
)

# Diagnostico de pressupostos
anova_diagnostico(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco = "bloco",
  fatores = c("dose", "hid"),
  mostrar_graficos = TRUE
)

# Tabela de medias com letras
Tabela <- tabela_medias_fatorial(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  fator_interesse = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid"),
  tipo_se = "modelo" # modelo ou descritivo
)
```

## Exemplo de uso (DIC)

```r
anova_fatorial_qm_tabela(
  dados = dados,
  variaveis = c("ci", "gs"),
  bloco = NULL,
  fatores = c("dose", "hid")
)
```

## Interacao fatorial

```r
# Uma variavel resposta
tabela_interacao_fatorial(
  dados = dados,
  resposta = "ci",
  fator_linha = "dose",
  fator_coluna = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)

# Varias variaveis resposta
tabela_interacao_fatorial_multivariaveis(
  dados = dados,
  variaveis = c("ci", "gs"),
  fator_linha = "dose",
  fator_coluna = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)
```

## Graficos

```r
grafico_medias_fatorial(
  dados = dados,
  resposta = "ci",
  fator_interesse = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)

grafico_interacao_fatorial(
  dados = dados,
  resposta = "ci",
  fator_x = "dose",
  fator_traco = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)
```

## Dicionario de variaveis (opcional)

Para usar rotulos amigaveis nas tabelas/graficos:

```r
dic_vars <- tibble::tribble(
  ~var, ~label, ~sigla,
  "ci", "Conteudo de clorofila", "CI",
  "gs", "Condutancia estomatica", "gs",
  "mvr", "Massa verde da raiz", "MVR"
)
```

Passe `dic_vars` e `label_type` nas funcoes que suportam isso.

## API principal

### Modelagem e inferencia

- `ajusta_modelo_fatorial()`
- `anova_fatorial_qm_tabela()`
- `medias_fatorial_cld()`

### Diagnostico

- `anova_diagnostico()`

### Tabelas

- `tabela_medias_fatorial()`
- `tabela_interacao_fatorial()`
- `tabela_interacao_fatorial_multivariaveis()`
- `tabela_dt_exportavel()`

### Graficos

- `grafico_media_ep()`
- `grafico_multiplas_variaveis()`
- `grafico_medias_fatorial()`
- `grafico_interacao_fatorial()`

### Utilitarios

- `remove_colunas_com_na()`
- `seleciona_colunas_com_na()`
- `remove_colunas_todas_na()`
- `colunas_com_na()`
- `manter_colunas_fixas_e_com_na()`

## Fluxo recomendado no projeto

1. Organizar dados e fatores no script local.
2. Ajustar/avaliar ANOVA com `anova_fatorial_qm_tabela()`.
3. Verificar pressupostos com `anova_diagnostico()`.
4. Gerar tabelas de medias e interacao.
5. Gerar graficos finais para relatorio.

## Qualidade

Validacao local executada:

- `R CMD build`
- `R CMD check --no-manual`

Status mais recente: `OK`.

## Roadmap

Proxima frente prevista:

- parcelas subdivididas em DIC e DBC, mantendo o mesmo padrao de API.

## Autor

Marlenildo Ferreira Melo

## Licenca

MIT (`LICENSE`).
