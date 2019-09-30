# ryanoisin@gmail.com / o.ryan@uu.nl ; September 2019
# Ryan, Bringmann, Schuurman (2019)

# The code in this file creats lists of adjacency and weights matrices
# used in figure generation in the file "figure_generation_part1.R"

# ----------------------------------------------------------------------
# ------------------------ Figure 2 ------------------------------------
# ----------------------------------------------------------------------

# Create adjacency matrices for the DAGs
D1 <- matrix(c(0,0,0,0,
               0,0,0,0,
               1,1,0,1,
               0,0,0,0),4,4, byrow = TRUE)
D2 <- matrix(c(0,0,1,0,
               0,0,1,0,
               0,0,0,0,
               0,0,1,0),4,4, byrow = TRUE)
D3 <- matrix(c(0,0,0,0,
               0,0,1,0,
               1,0,0,1,
               0,0,0,0),4,4, byrow = TRUE)

# Skeleton adjacency matrix
Skel <- matrix(c(0,0,1,0,
                 0,0,1,0,
                 1,1,0,1,
                 0,0,1,0),4,4, byrow = TRUE)

# Moral graph adjacency matrix
M1 <- matrix(1,4,4, byrow = TRUE)
M2 <- Skel
M3 <-  matrix(c(0,0,1,1,
                0,0,1,0,
                1,1,0,1,
                1,0,1,0),4,4, byrow = TRUE)

# Create list of graph objects
graph_list_f2 <- list(D1,Skel,M1,
                   D2,Skel,M2,
                   D3,Skel,M3)

# remove unneccessary matrices from memory
rm(D1, D2, D3, Skel, M1, M2, M3)

# ----------------------------------------------------------------------
# ------------------------ Figure 3 ------------------------------------
# ----------------------------------------------------------------------

graph_list_f3 <- list()
graph_list_f3[[1]] <- matrix(c(0,0,0,0,
                            0,0,0,0,
                            1,1,0,0,
                            0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[2]] <- matrix(c(0,0,0,0,
                            1,0,1,0,
                            0,0,0,1,
                            0,0,0,0),4,4, byrow = TRUE)
graph_list_f3[[3]] <- matrix(c(0,0,0,0,
                            1,0,1,0,
                            0,0,0,0,
                            0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[4]] <- matrix(c(0,1,1,0,
                            0,0,0,0,
                            0,0,0,1,
                            0,0,0,0),4,4, byrow = TRUE)
graph_list_f3[[5]] <- matrix(c(0,1,1,0,
                            0,0,0,0,
                            0,0,0,0,
                            0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[6]] <- matrix(c(0,0,0,0,
                            1,0,0,0,
                            1,1,0,0,
                            0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[7]] <- matrix(c(0,1,0,0,
                            0,0,0,0,
                            1,1,0,0,
                            0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[8]] <- matrix(c(0,0,1,0,
                            1,0,1,0,
                            0,0,0,0,
                            0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[9]] <- matrix(c(0,1,1,0,
                            0,0,1,0,
                            0,0,0,0,
                            0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[10]] <- matrix(c(0,0,0,0,
                             1,0,1,0,
                             1,0,0,0,
                             0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[11]] <- matrix(c(0,1,1,0,
                             0,0,0,0,
                             0,1,0,0,
                             0,0,1,0),4,4, byrow = TRUE)
graph_list_f3[[12]] <- matrix(c(0,0,1,0,
                             1,0,1,0,
                             0,0,0,1,
                             0,0,0,0),4,4, byrow = TRUE)
graph_list_f3[[13]] <- matrix(c(0,1,1,0,
                             0,0,1,0,
                             0,0,0,1,
                             0,0,0,0),4,4, byrow = TRUE)

# ----------------------------------------------------------------------
# ------------------------ Figure 4 ------------------------------------
# ----------------------------------------------------------------------

# Create sufficiency-violating DAGs

graph_list_f4 <- list()

# a) Example: One unobserved common cause of A-B
adj_tmp <- matrix(c(0,0,1,0,1,
                    0,0,0,0,1,
                    0,0,0,1,0,
                    0,0,0,0,0,
                    0,0,0,0,0),5,5, byrow = TRUE)
layout_tmp <- matrix(c(-.75,1, 
                       .75,1, 
                       0,  .2, 
                       0,  -1,
                       0, 1.5), 5, 2, byrow = T)
varnames_tmp <- c("A","B","C","D","E")
graph_list_f4[[1]] <- list(adj_tmp,layout_tmp,varnames_tmp)

# b) Example: Unobserved common cause of A-B-C
adj_tmp  <- matrix(c(0,0,0,0,1,
                     0,0,0,0,1,
                     0,0,0,0,1,
                     0,0,1,0,0,
                     0,0,0,0,0),5,5, byrow = TRUE)
layout_tmp <-  graph_list_f4[[1]][[2]]  ; varnames_tmp <- graph_list_f4[[1]][[3]]
graph_list_f4[[2]] <- list(adj_tmp,layout_tmp,varnames_tmp)

# c) Example: Two unobserved common causes
adj_tmp <- matrix(c(0,0,0,0,1,1,
                    0,0,0,0,1,0,
                    0,0,0,0,0,1,
                    0,0,1,0,0,0,
                    0,0,0,0,0,0,
                    0,0,0,0,0,0),6,6, byrow = TRUE)
layout_tmp  <- matrix(c(-.75,1, 
                        .75,1, 
                        0,  .2, 
                        0,  -1,
                        0, 1.5,
                        -.6,-.5), 6, 2, byrow = T)
varnames_tmp  <- c("A","B","C","D","E","F")
graph_list_f4[[3]] <- list(adj_tmp,layout_tmp,varnames_tmp)

# remove temporary objects
rm(adj_tmp, layout_tmp, varnames_tmp)


# ----------------------------------------------------------------------
# ------------------------ Figure 9 ------------------------------------
# ----------------------------------------------------------------------

graph_list_f9 <- list()

graph_list_f9[[1]] <- matrix(c(0, 0, 1, 0, 1,
                               0, 0, 0, 0, 1,
                               0, 0, 0, 0, 0,
                               0, 0, 1, 0, 0,
                               0, 0, 0, 0, 0), 5, 5, byrow = TRUE)

graph_list_f9[[2]] <- matrix(c(0, 0, 0, 0, 1,
                               0, 0, 1, 0, 1,
                               0, 0, 0, 1, 0,
                               0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0), 5, 5, byrow = TRUE)

graph_list_f9[[3]] <- matrix(c(0, 0, 0, 0, 1,
                               0, 0, 1, 0, 1,
                               0, 0, 0, 0, 0,
                               0, 0, 1, 0, 0,
                               0, 0, 0, 0, 0), 5, 5, byrow = TRUE)

graph_list_f9[[4]] <- matrix(c(0, 0, 0, 0, 1,
                               0, 0, 0, 0, 1,
                               1, 1, 0, 0, 0,
                               0, 0, 1, 0, 0,
                               0, 0, 0, 0, 0), 5, 5, byrow = TRUE)

graph_list_f9[[5]] <- matrix(c(0, 0, 1, 0, 1,
                               0, 0, 1, 0, 1,
                               0, 0, 0, 0, 0,
                               0, 0, 1, 0, 0,
                               0, 0, 0, 0, 0), 5, 5, byrow = TRUE)

graph_list_f9[[6]] <- matrix(c(0, 0, 0, 0, 1,
                               0, 0, 1, 0, 1,
                               1, 0, 0, 0, 0,
                               0, 0, 1, 0, 0,
                               0, 0, 0, 0, 0), 5, 5, byrow = TRUE)

graph_list_f9[[7]] <- matrix(c(0, 0, 1, 0, 1,
                               0, 0, 0, 0, 1,
                               0, 1, 0, 0, 0,
                               0, 0, 1, 0, 0,
                               0, 0, 0, 0, 0), 5, 5, byrow = TRUE)

graph_list_f9[[8]] <- matrix(c(0, 0, 1, 0, 1,
                               0, 0, 1, 0, 1,
                               0, 0, 0, 1, 0,
                               0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0), 5, 5, byrow = TRUE)