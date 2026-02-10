# Changelog

## [0.3.0] - 2026-02-10

### Added
- Novo argumento `formato` em `anova_fatorial_qm_tabela()` com três opções de saída.
- `qm_star`: quadrado médio com asteriscos de significância (padrão).
- `f_p_colunas`: duas colunas por variável resposta (`F` e `p`).
- `f_p_inline`: uma coluna por variável no formato `F (p)`.
- Cabeçalhos e nota de rodapé dinâmicos conforme o formato selecionado.
- Formatação consistente de p-valor, incluindo limiar mínimo (`< 0.0001`).

## [0.1.0] - 2026-02-04

### Added
- Funções genéricas para ANOVA fatorial (DIC e DBC)
- Ajuste automático de modelos com 1 ou mais fatores
- Tabela de ANOVA com quadrados médios e significância
- Cálculo automático do CV (%)
- Testes de comparação de médias (t ou Tukey)
- Médias ajustadas com erro-padrão e letras (CLD)
- Desdobramento completo de interação fatorial
- Suporte a múltiplas variáveis resposta
- Opção de erro-padrão do modelo ou descritivo

### Notes
- Código preparado para uso via `source()`
- Pensado para integração com R Markdown, Quarto e Shiny
