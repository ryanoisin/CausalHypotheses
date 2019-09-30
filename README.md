## Reproducibility Archive

This reproducibility archive allows to reproduce all results & figures of the paper Ryan, Bringmann & Schuurman (2019) "The Challenge of Generating Causal Hypotheses using Network Models." [[PsyArXiv]](https://psyarxiv.com)

1. `figure_generation_main_text.R` reproduces all figures and results in Sections 1, 2,3 and Appendix A. The adjacency matrices for Figures 2, 3, 4(a), 4(b) are sourced from `create_graph_lists.R`
2. `create_graph_lists.R` defines the adjacency matrices for Figures 2, 3, 4(a), 4(b)
3. `figure_generation_empirical_illustration.R` reproduces all figures in results in the Empirical Illustration, Section 4. Specifically, Figures 5, 6, 7 and 8

Note that the code supplied here is dependent on the installation of the [`SEset`](https://github.com/ryanoisin/SEset) package, which can be downloaded seperately from its own [github repository](https://github.com/ryanoisin/SEset)

