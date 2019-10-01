# ryanoisin@gmail.com / o.ryan@uu.nl ; September 2019
# Ryan, Bringmann, Schuurman (2019)

# The code in this file reproduces all figures and results in Section 4

# ----------------------------------------------------------------------
# ------------ Load Libraries and Source -------------------------------
# ----------------------------------------------------------------------

# Install and load the SEset package
if(isFALSE(require(SEset))){
  library(devtools)
  install_github("ryanoisin/SEset")}
library(SEset)

# Load other required packages for figures and additional analyses
require(qgraph)
require(relaimpo)
require(tidyr)
require(ggridges)
require(ggplot2)

# Load the sample correlation matrix reported by Hoorelebeke et al 2016
# This is included in the SEset package
data(riskcor)

# ----------------------------------------------------------------------
# ------------------------ Figure 5 ------------------------------------
# ----------------------------------------------------------------------

# ----------------------- Estimate the GGM  ---------------------------- 

# Estimate the precision matrix using qgraph
estimate <- qgraph::EBICglasso(riskcor, n = 69, returnAllResults = TRUE)
# qgraph method estimates a non-symmetric omega matrix, but uses forceSymmetric to create
# a symmetric matrix (see qgraph:::EBICglassoCore line 65)
omega <- as.matrix(forceSymmetric(estimate$optwi)) # returns the precision matrix

# Name the variables
dimnames(omega) <- dimnames(riskcor)
varnames <- rownames(omega)

# Calculate partial correlations from omega
parcormat <- wi2net(omega)

# -------------- Calculate Relative Importance  ------------------------ 

# exclude PASAT_ACC from analysis
riskcor.sub <- riskcor[c(1, 3, 4, 5, 6), c(1, 3, 4, 5, 6)]
names.sub <- rownames(riskcor.sub)

# relmat will contain the weights matrix of the relative importance network
relmat <- matrix(0, 5, 5)
# r2vec will contain the predictablity scores
r2vec <- numeric(5)

# loop through variables, calculating relative importance and predictability
for(i in 1:5) {
  # reorder variables so each in turn is first
  temp <- reorder(riskcor.sub, c(names.sub[i], names.sub[seq(1:5)[-i]]))
  # Then estimate relative importance for each variable
  relmat[seq(1:5)[-i], i] <- calc.relimp(temp)$lmg
  # and the predictability of each variable
  r2vec[i] <- calc.relimp(temp)$R2
}

# ----------------------- Plot Figure 5  ---------------------------- 

# Set up qgraph node layout
layout_5a <- matrix(c(  2,  0,
                    -2, -1,
                    -2,  1,
                     0,  2,
                   0.5,  0,
                     0,  -2),6,2,byrow = TRUE)
# Plot Figure 5a

pdf("figures/Figure5a_Hoorelbeke_estimate.pdf", height = 2, width = 2)
  parcornet <- qgraph(parcormat,labels = varnames,layout = layout_5a,
                    repulsion = .8,vsize = c(10,15), theme = "colorblind", fade = F)
dev.off()

# Set up qgraph node layout
layout_5b <- matrix(c( 2,   0,
                       0, 2.5,
                      -2,   0,
                      .5,   0,
                       0,  -2), 5, 2, byrow = TRUE)

# Plot Figure 5b

pdf("figures/Figure5b_Hoorelbeke_relimp.pdf", height = 2, width = 2)
  relimpnet <- qgraph(relmat, labels = names.sub, layout = layout_5b,
                      repulsion = .8, vsize = c(10, 15), asize = 5, 
                      pie = r2vec)
dev.off()

# Calculate centrality scores
relimp_centrality <- centrality(relimpnet)

# ----------------------------------------------------------------------
# ------------------------ Figure 6 ------------------------------------
# ----------------------------------------------------------------------

# --------------- Statistical Equivalence-Set --------------------------

# Estimate SE-set with rounding
SE.h <- precision_to_SEset(omega, orderings = NULL, digits = 2)

# Remove duplicates
SE.h <- unique(SE.h, MARGIN = 1) 

# Total number of unique DAGs in SE-set
ndag <- nrow(SE.h)

# --------------- Plot all SE-set DAGs (Figure 6) ----------------------

# To plot weights matrices in SE.h as weighted DAGs, first write the row
# corresponding to the DAG of interest to a matrix, e.g. for row number 1
# DAG1 <- matrix(SE.h[1,],6,6)
# to plot this in qgraph, it must be transposed t(DAG1)

# Create Figure 6 - DAG numbers 12, 52, 14 and 20 are shown in text #
pdf("figures/Figure6_Hoorelbeke_allDAGS.pdf", height = 2, width = 2)
for (DAG in 1:ndag) {
  qgraph(t(matrix(SE.h[DAG,],6,6)),
       labels = varnames,
       layout = layout_5a,
       repulsion = .8, 
       vsize = c(10,15), 
       fade = F,
       maximum = max(abs(SE.h)),
       asize = 5, 
       theme = "colorblind")
}
dev.off()

# ----------------------------------------------------------------------
# ------------------------ Figure 7 ------------------------------------
# ----------------------------------------------------------------------

# ---------------  Compute edge frequencies in the SE-set --------------

# Undirected edge frequency 
propu <- propcal(SE.h, names = varnames, rm_duplicate = F, directed = F)

# Directed edge frequency
propd <- propcal(SE.h,names = varnames,rm_duplicate = F,directed = T)

# ----------------------- Plot Figure 7 -------------------------------- 

# Create Figure 7a #
pdf("figures/Figure7a_Hoorelbeke_propu.pdf", height = 2, width = 2)
  qgraph(propu, 
       labels = varnames, 
       layout = layout_5a, 
       repulsion = .8,
       vsize = c(7,14), 
       maximum = 1, 
       edge.color = "darkgreen",
       fade = T)
dev.off()

# Create Figure 7b #
pdf("figures/Figure7b_Hoorelbeke_propd.pdf", height = 2, width = 2)
  qgraph(t(propd), 
       labels = varnames, 
       layout = layout_5a, 
       repulsion = .8,
       vsize = c(7,14), 
       maximum = 1, 
       edge.color = "darkgreen",
       fade = T)
dev.off()

# ----------------------------------------------------------------------
# ------------------------ Figure 8 ------------------------------------
# ----------------------------------------------------------------------

# -------  Compute Predictability/Controllability Distribution ---------

r2set <- r2_distribution(SEmatrix = SE.h, cormat = riskcor, names = NULL, indices = c(1,3,4,5,6))

df <- as.data.frame(r2set, col.names = names.sub)
r2rows <- gather(df)

# ----------------------- Plot Figure 8 -------------------------------- 

# Create ggplot object
 p <- ggplot(r2rows, aes(y = key, x = value)) + 
   geom_density_ridges(fill = "light seagreen") + 
    labs(y = "Variable", x = expression(paste("Controllability value ", R^2)))
 
# Create Figure 8 #
pdf("figures/Figure8_Hoorelbeke_r2_dist.pdf", width = 14, height = 7)
p + theme(text = element_text(size = 20), 
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20), 
          panel.background = element_blank())
dev.off()
