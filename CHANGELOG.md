# Changelog

## [0.4.0] - 2026-02-11

### Changed
- Migracao do projeto de scripts para pacote R valido (`rlibfatorial`).
- Escopo inicial focado em analise fatorial (DIC e DBC).
- Remocao dos scripts `R/01_pacotes.R` e `R/02_funcoes_anova.R` do fluxo principal.

### Added
- Estrutura formal de pacote: `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `.Rbuildignore`.
- Funcao `configurar_ambiente_rlib()` para setup opcional do ambiente.
- Documentacao roxygen para API publica.

### Notes
- API central mantida em modelo fatorial, diagnostico, tabelas, graficos e utilitarios.

## [0.3.0] - 2026-02-10

### Added
- Novo argumento `formato` em `anova_fatorial_qm_tabela()` com tres opcoes de saida.
- `qm_star`: quadrado medio com asteriscos de significancia (padrao).
- `f_p_colunas`: duas colunas por variavel resposta (`F` e `p`).
- `f_p_inline`: uma coluna por variavel no formato `F (p)`.
- Cabecalhos e nota de rodape dinamicos conforme o formato selecionado.
- Formatacao consistente de p-valor, incluindo limiar minimo (`< 0.0001`).

## [0.1.0] - 2026-02-04

### Added
- Funcoes genericas para ANOVA fatorial (DIC e DBC)
- Ajuste automatico de modelos com 1 ou mais fatores
- Tabela de ANOVA com quadrados medios e significancia
- Calculo automatico do CV (%)
- Testes de comparacao de medias (t ou Tukey)
- Medias ajustadas com erro-padrao e letras (CLD)
- Desdobramento completo de interacao fatorial
- Suporte a multiplas variaveis resposta
- Opcao de erro-padrao do modelo ou descritivo

### Notes
- Codigo preparado para uso via `source()`
- Pensado para integracao com R Markdown, Quarto e Shiny
