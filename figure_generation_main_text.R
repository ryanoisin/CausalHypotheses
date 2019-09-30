# ryanoisin@gmail.com / o.ryan@uu.nl ; September 2019
# Ryan, Bringmann, Schuurman (2019)

# The code in this file reproduces all figures and results in Sections 1,
# 2,3 and Appendix A. The adjacency matrices for Figures 2, 3, 4(a), 4(b),
# 4(c) and A1 are created in the seperate file "graph_lists.R" which is 
# sourced below

# ----------------------------------------------------------------------
# ------------ Load Libraries and Source -------------------------------
# ----------------------------------------------------------------------

if(isFALSE(require(SEset))){
library(devtools)
install_github("ryanoisin/SEset")
}
library(SEset)
library(qgraph)
source("create_graph_lists.R")

# ----------------------------------------------------------------------
# ------------------------ Figure 1 ------------------------------------
# ----------------------------------------------------------------------

# set variable names
varnames <- c("Support", "Pressure", "Stress", "Worry")

# Make a weights matrix of a weighted DAG
adj <- matrix(c(0, 0, 0, 0, 
                0, 0, 0, 0, 
                -.3, .5, 0, 0, 
                0, 0, .2, 0), 4, 4, byrow = T)
dimnames(adj) <- list(varnames,varnames)

# Get the precision matrix if variables are standard normal
omega <- path_to_precision(adj)
omega <- zapsmall(omega, digits = 16) # set very small parameters to zero

# Get unweighted moral graph by setting non-zero weights to 1
moral <- apply(omega,c(1,2),function(el){ifelse(el!=0,1,0)})

# Get DAG adjacency matrix
dag_adj <- apply(adj,c(1,2),function(el){ifelse(el!=0,1,0)})

# ---------- Set plotting parameters ---------------- 

# Plot layout
layout <- matrix(c( -1, .9, 
                   -.5, -1, 
                     0,  0, 
                     1,  0), 4, 2, byrow = T)
vsize = 20 # node size
esize = 15 # edge size
asize = 15 # arrowhead size
mar = rep(4,4) # margins around plot

# ---------- Plot Figure 1 --------------------------

# Panel (a)
pdf("figures/example_b_q_ggm.PDF",width=8,height=6.8)
qgraph(wi2net(omega),diag=F,labels=varnames,maximum=.6,fade=F,layout=layout,
       vsize = vsize,
       esize = esize,
       theme="colorblind",
       mar = mar)
dev.off()

# Panel (b)
pdf("figures/example_b_q_ldag.PDF",width=8,height=6.8)
qgraph(t(adj),diag=F,labels=varnames,maximum=.6,fade=F,layout=layout,
       vsize = vsize,
       esize = esize,
       asize = asize,
       theme="colorblind",
       mar = mar)
dev.off()

# Panel (c)
pdf("figures/example_b_q_moral.PDF",width=8,height=6.8)
qgraph(moral,diag=F,labels=varnames,maximum=10,fade=F,layout=layout,
       vsize = vsize,
       esize = esize,
       edge.color="black",
       mar = mar)
dev.off()

# Panel (d)
pdf("figures/example_b_q_dag.PDF",width=8,height=6.8)
qgraph(t(dag_adj)*.6,diag=F,labels=varnames,maximum=.6,fade=F,layout=layout,
       vsize = vsize,
       esize = esize,
       asize = asize,
       edge.color="black",
       mar = mar)
dev.off()


# ----------------------------------------------------------------------
# ------------------------ Figure 2 ------------------------------------
# ----------------------------------------------------------------------

# ---------- Set plotting parameters ---------------- 

# Layout of qgraph object
layout_f2 <- matrix(c(-.75, 1, 
                       .75, 1, 
                         0,  .2, 
                         0,  -1), 4, 2, byrow = T)
# Variable names
varnames_f2 <- c("A","B","C","D")

# Labels for each column of Figure 2
col.labels <- c("DAG", "Skeleton","Moral Graph \n (PMRF)")

# Node / edge / arrowhead sizes
vsize = 25
esize = 15
asize = 40

# Scaling parameter to tune pdf output
sc <- 3

# Set up layout of R plot environment
lmat <- rbind(c(1,2,3), 
              c(4,5,6),
              c(7,8,9),
              c(10,11,12))


# ---------- Plot Figure 2 --------------------------

pdf("figures/Figure2_graphtable.pdf", height = sc*3.5, width = sc*3)
  # Set layout
  lo <- layout(lmat, heights = c(.5,1,1,1))

  # Create Titles
  for(i in 1:3){
    par(mar=c(.1,.1,.1,.1))
    plot.new()
    text(0.5,0.5,col.labels[i], cex = 3, font = 2)
    }

  # Place each graph
  for(i in 1:9){
    par(mar=c(.2,.2,.2,.2))
    qgraph(
      t(graph_list_f2[[i]]),
      diag = F,
      labels = varnames_f2,
      maximum = 1,
      fade = F,
      layout = layout_f2,
      vsize = vsize,
      esize = esize,
      asize = asize,
      edge.color = "black",
      label.cex = 2.5,
      mar = c(5,5,5,5)
      )
    }
dev.off() # end of plot

# ----------------------------------------------------------------------
# ------------------------ Figure 3 ------------------------------------
# ----------------------------------------------------------------------

# The moral equivalent set is stored in graph_list_f3

# ---------- Plot Figure 3 Left-Column (PMRF) --------------------------
sc <- 3
pdf("figures/Fig3_moral_table_pmrf.pdf", height = sc*1, width = sc*1)

qgraph(moral,
  diag = F,
  labels = varnames_f2,
  maximum = 1,
  fade = F,
  layout = layout_f2,
  vsize = 15,
  esize = 15,
  asize = asize,
  edge.color = "black",
  label.cex = 2.5,
  mar = c(5,5,5,5))
dev.off()


# ---------- Plot Figure 3 Right-Column (moral-equivalent set) ---------
for(i in 1:13){
  sc <- 1.8
  pdf(paste0("figures/Fig3_moral_table_",i,".pdf"), height = sc*1, width = sc*1)
    layout_f3 <- matrix(c(-.5, 1, 
                         .5, 1, 
                         0,  .2, 
                         0,  -1), 4, 2, byrow = T)
qgraph(t(graph_list_f3[[i]]),
       diag = F,
       labels = varnames_f2,
       maximum = 1,
       fade = F,
       layout = layout_f3,
       vsize = vsize*.66,
       esize = esize*.5,
       asize = asize*.5,
       edge.color = "black",
       label.cex = 2.75,
       mar = c(6,6,6,6))
dev.off()
}

# ----------------------------------------------------------------------
# ------------------------ Figure 4 ------------------------------------
# ----------------------------------------------------------------------

# ------------------ Find unfaithful weighted DAG ----------------------

# Get SEset of omega
ex_equiv <- precision_to_SEset(omega)

# Save variable orderings fo reference
orders <- order_gen(omega)

# Remove small parameters
ex_equiv_r <- zapsmall(ex_equiv, digits = 10)

# Remove duplicates
ex_equiv_r <- unique(ex_equiv_r, MARGIN = 1)

ex_ar <- array(NA,c(4,4,16))
for (i in 1:16){
  ex_ar[,,i] <- matrix(ex_equiv_r[i,],4,4)
}

# ------------------ Appendix A, equation 5 ----------------------------
weights_unfaith <- round(ex_ar[,,13],3)


# --------------------- Plot Figure 4 ----------------------------------

sc <- 2
pdf("figures/Figure4_suff_faith.pdf", height = sc*1, width = sc*4)
  # Set up layout of graph
  par(mfrow = c(1,4))

  # Loop through sufficiency-violating DAGs (Panels (a), (b) and (c))
  for(i in 1:3){
    qgraph(t(graph_list_f4[[i]][[1]]),
       diag = F,
       labels = graph_list_f4[[i]][[3]],
       maximum = 1,
       fade = F,
       layout = graph_list_f4[[i]][[2]],
       vsize = vsize,
       esize = esize,
       asize = asize*2,
       edge.color = "black",
       label.cex = 2.75,
       mar = c(6,6,6,6))
    }
  
  # Set up layout of qgraph object for panel(d)
  
  layout_uf <- matrix(c(-.75,   1, 
                         .75,   1, 
                           0,  .2, 
                           0,  -1), 4, 2, byrow = T)
  
  # Plot panel (d) - unfaithful DAG
  qgraph(t(weights_unfaith),
       diag = F,
       labels = varnames_f2,
       maximum = max(weights_unfaith),
       fade = F,
       layout = layout_uf,
       vsize = vsize,
       esize = esize,
       asize = asize*2,
       theme="colorblind",
       label.cex = 2.75,
       mar = c(6,6,6,6))
dev.off()


# ----------------------------------------------------------------------
# ------------------------ Figure 9 ------------------------------------
# ----------------------------------------------------------------------

# Set up layout of qgraph object
layout_f9 <- matrix(c(-.75,   1, 
                       .75,   1, 
                         0,  .2, 
                         0,  -1,
                         0, 1.5), 5, 2, byrow = T)
varnames_f9 <- c("A","B","C","D","E")
sc <- 2

# --------------------- Plot Figure 9 ----------------------------------
pdf("figures/Figure9_suff_app.pdf", height = sc*5, width = sc*8)
  # Set up layout of graph
  par(mfrow = c(2,4))

  # Loop through the list of adjacency matrices
  for(i in 1:8){
     qgraph(t(graph_list_f9[[i]]),
         diag = F,
         directed = TRUE,
         labels = varnames_f9,
         maximum = 1,
         fade = F,
         layout = layout_f9,
         vsize = 25,
         esize = 15,
         asize = 40*.75,
         edge.color = "black",
         label.cex = 2.75,
         mar = c(6,6,6,6))
    }
dev.off()
