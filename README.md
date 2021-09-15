---
output:
  pdf_document: default
  html_document: default
---
## Reproducibility Archive

This reproducibility archive allows to reproduce all results & figures of the paper Ryan, Bringmann & Schuurman (pre-print) "The Challenge of Generating Causal Hypotheses using Network Models." [[PsyArXiv]](https://psyarxiv.com/ryg69/)

1. `figure_generation_main_text.R` reproduces all figures and results. The adjacency matrices for larger figures (Figures 2, 4,5 an 8) are sourced from `create_graph_lists.R`
2. `create_graph_lists.R` defines the adjacency matrices for graphs in figures 2, 4, 5 and 8.

Note that the code supplied here is dependent on the installation of the [`SEset`](https://github.com/ryanoisin/SEset) package, which can be downloaded seperately from its own [github repository](https://github.com/ryanoisin/SEset)

