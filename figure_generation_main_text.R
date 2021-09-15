# o.ryan@uu.nl ; September 2021

# The code in this file reproduces all figures in the main text Sections 1,
# The adjacency matrices for Figures 2, 4, 5 and 8
# are created in the seperate file "create_graph_lists.R" which is 
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

# create regression weights matrix of the linear SEM
adj <- matrix(c(0, 0, 0, 0,
                0, 0, 0, 0,
                -.5, .5, 0, 0,
                0, 0, .2, 0), 4, 4, byrow = T)

dimnames(adj) <- list(varnames,varnames)

# Get the precision matrix if variables are standard normal
omega <- path_to_network(adj)
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

# Layout of qgraph object
layout_f2 <- matrix(c(-.75, 1, 
                      .75, 1, 
                      0,  .2, 
                      0,  -1), 4, 2, byrow = T)
# Variable names
varnames_f2 <- c("A","B","C","D")

# The moral equivalent set is stored in graph_list_f3

# ---------- Plot Figure 2 Left-Column (PMRF) --------------------------
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


# ---------- Plot Figure 2 Right-Column (moral-equivalent set) ---------
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
# ------------------------ Figure 3 ------------------------------------
# ----------------------------------------------------------------------

# ------------------ Find unfaithful weighted DAG ----------------------

# Get SEset of omega using the SEset package
ex_equiv <- network_to_SEset(omega)

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

# choose an unfaithful SEM model
beta <- ex_ar[,,13]

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
  qgraph(t(beta),
       diag = F,
       labels = varnames_f2,
       maximum = max(beta),
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
# ------------------------ Figure 4 ------------------------------------
# ----------------------------------------------------------------------

layout_f3 <- matrix(c(-.5, 1, 
                      .5, 1, 
                      0,  .2, 
                      0,  -1), 4, 2, byrow = T)

pdf("fignew/FigNew_SE_example_.pdf", height = sc*.5, width = sc*1)
layout(matrix(c(1,1,2,3,4,5,
                1,1,6,7,8,9,
                1,1,10,11,12,13,
                1,1,14,15,16,17),4,6,byrow = T))

qgraph(wi2net(omega),
       diag = F,
       labels = varnames_f2,
       maximum = .6,
       fade = F,
       layout = layout_f2,
       vsize = 15,
       esize = 15,
       asize = asize,
       theme = "colorblind",
       label.cex = 2.5,
       mar = c(5,5,5,5))


sc <- 1.8

dagorder <- c(1,8,7,11,9,10,12,5,6,13,14,2,3,4,15,16)
for (DAG in dagorder) {
  qgraph(t(matrix(ex_ar[,,DAG],4,4)),
         labels = varnames_f2,
         layout = layout_f2,
         maximum = .7,
         fade = F,
         layout = layout_f3,
         vsize = vsize*.66,
         esize = esize*.5,
         asize = asize*.5,
         theme = "colorblind",
         label.cex = 2.75,
         mar = c(6,6,6,6))
  
}
dev.off()

# strongest edges analysis
sapply(dagorder, function(i) which(abs(ex_ar[,,i])==max(abs(ex_ar[,,i])), arr.ind= TRUE))



# ----------------------------------------------------------------------
# ------------------------ Figure 5 ------------------------------------
# ----------------------------------------------------------------------


# ---------- Set plotting parameters ---------------- 

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


# ---------- Plot Figure 5 --------------------------

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
# ------------------------ Figure 6 ------------------------------------
# ----------------------------------------------------------------------

# -------------  First-Row Figures  ------------------------------------

# Plotting  parameters
edge.label.cex <- 2
vsize = c(15)
laymat <- matrix(c(-1 ,  -1,
                   -1 ,   1,
                   1 ,   0), 3,2, byrow = T)

# Specify path model
B <- matrix(c(0,0,0,
              0,0,0,
              .25,.25,0), 3, 3, byrow =T,
            dimnames = list(c("B","A","C"), c("B", "A","C")))
# specify residual covariances (such that B represents standardized coefficients)
psi <- diag(c(1,1,1-(2*(.25)^2)))

# obtain model-implied covariance matrix
sigma <- path_to_network(B, psi = psi, output = "covariance")
sigma

# solve for partial correlations (GGM)
parcor <- -cov2cor(solve(sigma))
diag(parcor) <- 0

# Create right and left panels of first row
pdf("fignew/Figure_NegEdges_1.pdf", height = 4, width = 4)
qgraph::qgraph(t(B), edge.labels = T, layout = laymat, repulsion = .8,vsize = vsize, theme = "colorblind", fade = F,
               asize = 8, maximum = .4, edge.label.cex = edge.label.cex)
dev.off()

pdf("fignew/Figure_NegEdges_2.pdf", height = 4, width = 4)
qgraph::qgraph(parcor, edge.labels = T, layout = laymat, repulsion = .8,vsize = vsize, theme = "colorblind", fade = F,
               asize = 8, maximum = .4,  edge.label.cex = edge.label.cex)
dev.off()


# ---------------------- Second-Row Figures -----------------------------

# set plotting parameters
laymat <- matrix(c(-1 ,   0,
                   0 ,  -1,
                   0 ,   1,
                   1 ,   0), 4,2, byrow = T)

# Specify path model in form of a beta matrix
B <- matrix(
  c(0,0,0,0,
    .25,0,0,0,
    .25,.25,0,0,
    .25,.25,.25,0
  ),4,4,byrow =T, dimnames = list(c("A","B","C","D"), c("A", "B","C","D"))
)


# specify error covariance matrix
psi <- diag(c(1,1-(.25)^2,1-(2.5*(.25)^2),1-(4.75*(.25)^2)))

# solve for the model-implied variance-covariance matrix
sigma <- path_to_network(B, psi = psi, output = "covariance")
sigma

# convert to partial correlations
parcor <- -cov2cor(solve(sigma))
diag(parcor) <- 0

# create left and right panel figures
pdf("fignew/Figure_NegEdges_3.pdf", height = 4, width = 4)
qgraph::qgraph(t(B), edge.labels = T, layout = laymat, repulsion = .8,vsize = vsize, theme = "colorblind", fade = F,
               asize = 8, maximum = .4,  edge.label.cex = edge.label.cex)
dev.off()

pdf("fignew/Figure_NegEdges_4.pdf", height = 4, width = 4)
qgraph::qgraph(parcor, edge.labels = T, layout = laymat, repulsion = .8,vsize = vsize, theme = "colorblind", fade = T,
               asize = 8, maximum = .3,  edge.label.cex = edge.label.cex)

dev.off()


# ----------------------------------------------------------------------
# ------------------------ Figure 7 ------------------------------------
# ----------------------------------------------------------------------

edge.label.cex <- 2
vsize = c(15)
asize = 15
esize = 10

# -------------------  Top-Left Panel  -------------------------

# plotting parameters

laymat <- matrix(c(-1,0,
                   0,1,
                   1,0,
                   0,-1), 4,2, byrow = T)

# DAG adjacency matrix
B <- matrix(c(0,0,0,1,
              1,0,0,0,
              0,1,0,0,
              0,0,1,0), 4, 4, byrow =T,
            dimnames = list(c("A","B","C","D"), c("A", "B","C", "D")))

# create figure
pdf("fignew/Figure_CyclesA1.pdf", height = 4, width = 4)

qgraph::qgraph(t(B), layout = laymat, repulsion = .8,vsize = vsize, theme = "colorblind", fade = F,
               asize = asize, esize = esize)
dev.off()

# -------------------  Top-Right Panel  -------------------------
B <- matrix(c(0,1,0,1,
              1,0,1,0,
              0,1,0,1,
              1,0,1,0), 4, 4, byrow =T,
            dimnames = list(c("A","B","C","D"), c("A", "B","C", "D")))


pdf("fignew/Figure_CyclesA2.pdf", height = 4, width = 4)
qgraph::qgraph(t(B), layout = laymat, repulsion = .8,vsize = vsize, theme = "colorblind", fade = F,
               asize = asize, esize = esize)
dev.off()

# -------------------  Bottom-Left Panel  -------------------------

laymat <- matrix(c(-1,1,
                   1,1,
                   1,-1,
                   -1,-1), 4,2, byrow = T)

B <- matrix(c(0,0,0,0,
              1,0,1,0,
              0,1,0,1,
              0,0,0,0), 4, 4, byrow =T,
            dimnames = list(c("A","B","C","D"), c("A", "B","C", "D")))


pdf("fignew/Figure_CyclesB1.pdf", height = 4, width = 4)
qgraph::qgraph(t(B), layout = laymat, repulsion = .8,vsize = vsize, theme = "colorblind", fade = F,
               asize = asize, esize = esize)
dev.off()


# -------------------  Bottom-right Panel  -------------------------

B <- matrix(c(0,1,1,0,
              1,0,1,1,
              1,1,0,1,
              0,1,1,0), 4, 4, byrow =T,
            dimnames = list(c("A","B","C","D"), c("A", "B","C", "D")))


pdf("fignew/Figure_CyclesB2.pdf", height = 4, width = 4)
qgraph::qgraph(t(B), layout = laymat, repulsion = .8,vsize = vsize, theme = "colorblind", fade = F,
               asize = asize, esize = esize)
dev.off()


# ----------------------------------------------------------------------
# ------------------------ Figure 8 ------------------------------------
# ----------------------------------------------------------------------


# Set up layout of qgraph object
layout_f9 <- matrix(c(-.75,   1, 
                       .75,   1, 
                         0,  .2, 
                         0,  -1,
                         0, 1.5), 5, 2, byrow = T)
varnames_f9 <- c("A","B","C","D","E")
sc <- 2

# --------------------- Plot Figure 8 ----------------------------------
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

