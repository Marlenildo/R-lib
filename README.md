# R_lib — Biblioteca Estatística Reutilizável em R

## Visão geral

O **R_lib** é uma biblioteca modular de funções em R desenvolvida para padronizar, automatizar e garantir **consistência estatística** na análise de experimentos, especialmente em **ciências agrárias**.

A biblioteca foi projetada para ser:

- ✔️ **Genérica** (independente de cliente ou experimento)
- ✔️ **Reutilizável** (mesmas funções para vários relatórios)
- ✔️ **Reprodutível** (um único modelo → uma única verdade)
- ✔️ **Compatível com DIC e DBC**
- ✔️ **Compatível com 1, 2 ou n fatores**
- ✔️ **Orientada a relatórios científicos (HTML, PDF)**

O R_lib funciona em conjunto com scripts locais (`R_local`) específicos de cada projeto, separando claramente **infraestrutura estatística** de **conteúdo do experimento**.

---

## Estrutura da biblioteca

R_lib/
├── 00_setup.R
├── 01_pacotes.R
├── 02_funcoes_anova.R
├── 03_funcoes_diagnostico.R
├── 04_funcoes_tabelas.R
├── 05_funcoes_graficos.R
├── 06_funcoes_modelo_fatorial_medias.R
└── README.md

---

## Filosofia do projeto

### 🔑 Princípios fundamentais

1. **Separação de responsabilidades**
   - `R_lib` → funções genéricas
   - `R_local` → dados, nomes de variáveis, decisões do experimento

2. **Modelo único**
   - O mesmo modelo estatístico é usado para:
     - ANOVA
     - diagnóstico de pressupostos
     - médias ajustadas
     - gráficos

3. **Flexibilidade**
   - Aceita qualquer nome de:
     - bloco (`bloco`, `rep`, `trat`, etc.)
     - fatores (`dose`, `hid`, `cultivar`, etc.)
     - variáveis resposta

4. **Padronização**
   - Tabelas prontas para publicação
   - Gráficos consistentes
   - Redução de código repetido nos relatórios

---

## Descrição dos arquivos

### `00_setup.R`
Configurações globais do ambiente:
- opções do R
- locale para datas
- comportamento numérico

---

### `01_pacotes.R`
Carregamento centralizado de todos os pacotes utilizados:
- manipulação de dados
- estatística experimental
- modelos fatoriais
- gráficos
- tabelas e relatórios

Evita carregamentos repetidos e erros silenciosos.

---

### `02_funcoes_anova.R`
Funções para análise de variância:
- símbolos de significância (*, **, ***)
- tabelas de ANOVA com:
  - FV
  - GL
  - Quadrados Médios
  - destaque automático de efeitos significativos
  - coeficiente de variação (CV%)

Compatível com:
- DIC
- DBC
- fatorial simples ou múltiplo

---

### `03_funcoes_diagnostico.R`
Diagnóstico dos pressupostos da ANOVA:

- Normalidade dos resíduos (Shapiro-Wilk)
- Homogeneidade de variâncias (Levene)
- Gráficos diagnósticos do modelo

🔒 **Importante**:  
Utiliza exatamente o mesmo modelo ajustado na ANOVA, garantindo consistência estatística.

---

### `04_funcoes_tabelas.R`
Funções auxiliares para:
- tabelas de dados
- tabelas resumidas
- exportação para HTML
- padronização visual

---

### `05_funcoes_graficos.R`
Funções para geração de gráficos:
- médias ± erro-padrão
- múltiplas variáveis em painéis
- compatíveis com resultados de ANOVA e médias ajustadas
- prontas para publicação

---

### `06_funcoes_modelo_fatorial_medias.R`

# Funções para ANOVA Fatorial em R

Conjunto de funções genéricas para análise de experimentos fatoriais,
suportando delineamentos inteiramente casualizados (DIC) e blocos casualizados (DBC).

## Funcionalidades

- Ajuste automático de modelos fatoriais
- ANOVA com quadrados médios e significância
- Testes de comparação de médias (t ou Tukey)
- Médias ajustadas com erro-padrão e letras (CLD)
- Desdobramento estatístico de interações fatoriais
- Suporte a múltiplas variáveis resposta
- Integração com relatórios HTML (kableExtra)

## Uso básico

```r
source("R/06_funcoes_modelo_fatorial_medias.R")
```
As funções foram desenvolvidas para uso em relatórios técnicos,
artigos científicos e aplicações Shiny.


Inclui funções para:
- ajuste genérico de modelos fatoriais
- ANOVA fatorial (DIC ou DBC)
- escolha automática do teste de médias:
  - teste t (2 níveis)
  - Tukey (≥ 3 níveis)
- médias ajustadas (emmeans)
- letras de comparação (CLD)
- tabelas finais formatadas (média ± EP + letras)

Compatível com:
- 1, 2 ou n fatores
- fatorial simples, duplo ou múltiplo
- qualquer banco de dados

#### Erro-padrão das médias

As tabelas de médias permitem escolher o tipo de erro-padrão a ser apresentado por meio do argumento `tipo_se`:

- `"modelo"` (padrão):  
  Erro-padrão das médias ajustadas, estimado a partir do modelo de ANOVA (emmeans).  
  ✔️ Estatisticamente consistente com os testes t e Tukey.

- `"descritivo"`:  
  Erro-padrão calculado diretamente a partir dos dados brutos (sd/√n), por nível de fator.  
  ⚠️ Recomendado apenas para fins descritivos ou exploratórios.

> Por padrão, a biblioteca utiliza o erro-padrão do modelo, garantindo coerência entre a ANOVA, os testes de médias e as letras de comparação.


#### Interação entre fatores

Nas tabelas de desdobramento da interação fatorial, as letras de comparação de médias
são sempre obtidas a partir do modelo de ANOVA (emmeans + teste t ou Tukey).

O erro-padrão apresentado pode ser escolhido por meio do argumento `tipo_se`:

- `"modelo"` (padrão): erro-padrão marginal estimado pelo modelo.
- `"descritivo"`: erro-padrão calculado diretamente a partir dos dados observados,
  por combinação dos níveis dos fatores.

Essa separação garante coerência estatística entre inferência e descrição.


---

## Fluxo de uso recomendado

### 1️⃣ No relatório (R Markdown)

```r
source("../../R_lib/00_setup.R")
source("../../R_lib/01_pacotes.R")
source("../../R_lib/02_funcoes_anova.R")
source("../../R_lib/03_funcoes_diagnostico.R")
source("../../R_lib/04_funcoes_tabelas.R")
source("../../R_lib/05_funcoes_graficos.R")
source("../../R_lib/06_funcoes_modelo_fatorial_medias.R")
```

---

### 2️⃣ No script local (R_local)

- Importação de dados
- Tratamento específico
- Definição de:
  - variáveis resposta
  - fatores
  - nomes amigáveis para tabelas e gráficos

---

### 3️⃣ No relatório

- ANOVA
- Diagnóstico
- Testes de médias
- Gráficos
- Tabelas finais

---

Exemplo de uso

```r
anova_fatorial_qm_tabela(
  dados     = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco     = "bloco",
  fatores   = c("dose", "hid")
)
```

```r
anova_diagnostico(
  dados     = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco     = "bloco",
  fatores   = c("dose", "hid")
)
```

```r 
tabela_medias_fatorial(
  dados           = dados,
  variaveis       = c("ci", "gs", "mvr"),
  fator_interesse = "hid",
  bloco           = "bloco",
  fatores         = c("dose", "hid")
)
```

Autor

Marlenildo Ferreira Melo
Engenheiro Agrônomo — Doutor em Fitotecnia

Biblioteca desenvolvida para apoiar análises estatísticas reprodutíveis e relatórios científicos padronizados em R.


---
