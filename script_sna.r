rm(list=ls())
##### Load Packages ####
library(sna) 
library(Matrix)   # for build a block diagonal matrix
library(reldist)  # for gini coefficient (also: https://stat.ethz.ch/pipermail/r-help/2007-June/133875.html)
library(plyr)     # for everithing? :P
library(rjson)    # for exporting data

##### Functions ####
degree_sna <- function(net, norm = TRUE, ...){
  degree(net, ...)/2/(if(norm) ncol(net)-1 else 1)
}

betweenness_sna <- function(net, norm = FALSE, ...){
  n <- ncol(net)
  betweenness(net, ...)/2/(if(norm) (n-1)*(n-2)/2 else 1)
}



##### Networks ####
net.butterfly <- matrix(c(0,1,1,0,0,0,0,
                          1,0,1,0,0,0,0,
                          1,1,0,1,0,0,0,
                          0,0,1,0,1,0,0,
                          0,0,0,1,0,1,1,
                          0,0,0,0,1,0,1,
                          0,0,0,0,1,1,0),
                        byrow = TRUE, nrow = 7)

net.star <- matrix(c(0,1,1,1,1,1,
                     1,0,0,0,0,0,
                     1,0,0,0,0,0,
                     1,0,0,0,0,0,
                     1,0,0,0,0,0,
                     1,0,0,0,0,0),
                        byrow = TRUE, nrow = 6)

net.line <- matrix(c(0,1,0,0,0,
                     1,0,1,0,0,
                     0,1,0,1,0,
                     0,0,1,0,1,
                     0,0,0,1,0),
                   byrow = TRUE, nrow = 5)

net.circular <- matrix(c(0,1,0,0,1,
                         1,0,1,0,0,
                         0,1,0,1,0,
                         0,0,1,0,1,
                         1,0,0,1,0),
                       byrow = TRUE, nrow = 5)

nets <- list(net.butterfly, net.star, net.line, net.circular)
net.all <- as.matrix(bdiag(net.butterfly, net.star, net.line, net.circular))


##### Plots ####
gplot(net.butterfly, displaylabels=TRUE, usearrows=FALSE)
gplot(net.star, displaylabels=TRUE, usearrows=FALSE)
gplot(net.line, displaylabels=TRUE, usearrows=FALSE)
gplot(net.circular, displaylabels=TRUE, usearrows=FALSE)
gplot(net.all,
      usearrows=FALSE,
      label = unlist(llply(nets, degree_sna, norm = FALSE)))



#### Indicators ####
# Degrees for each node of each network
llply(nets, degree_sna)
llply(nets, degree_sna, norm = FALSE)

# Differences beetween degree for nodes in each network
laply(nets, function(net){ gini(degree_sna(net)) })
laply(nets, function(net){   sd(degree_sna(net)) })

# Centralization coefficient $C_D$
laply(nets, centralization, degree)

# Betweenness
llply(nets, betweenness_sna)
llply(nets, betweenness_sna, norm = TRUE)


# Closeness
llply(nets, closeness)

# Eigenvector Centrality
llply(nets, evcent)


#### Consolidate Data ####
net.all

names <- paste(rep(1:length(nets), laply(nets, ncol)),
               unlist(llply(nets, function(x) 1:ncol(x))), sep = "_")

colnames(net.all) <- names
rownames(net.all) <- names

links <- ldply(names, function(name){
  # name <- sample(names, size = 1)
  # name <- names[1]
  data.frame(source = which(names==name)-1, 
             target = which(net.all[name,] == 1)-1)
})

nodes <- data.frame(name = names)
nodes$degree_norm <- unlist(llply(nets, degree_sna))
nodes$degree <- unlist(llply(nets, degree_sna, norm = FALSE))
nodes$betweenness <- unlist(llply(nets, betweenness_sna))
nodes$betweenness_norm <- unlist(llply(nets, betweenness_sna, norm = TRUE))
nodes$closeness <- unlist(llply(nets, closeness))
nodes$eigen_vector_centrality <- unlist(llply(nets, evcent))



#### Exporting Data ####
nodes_json <- adply(nodes, 1, toJSON )$V1
nodes_json <- paste(" \"nodes\" : [", paste("\n", nodes_json, collapse=", "), "\n]")

links_json <- adply(links, 1, toJSON)$V1
links_json <- paste(" \"links\" : [", paste("\n", links_json, collapse=", "), "\n]")

data_json <- paste("{\n", nodes_json, "\n,\n", links_json, "}")
write(data_json, "data.json")





