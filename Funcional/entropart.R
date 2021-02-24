library("entropart")
library("ggplot2")
library("dplyr")

#dados####
getwd()
data <- read.csv("tudojunto.csv", h=T, ";")
names(data)

N18 <- data$n_ind
Abd18 <- as.AbdVector(N18)
autoplot(Abd18, Distribution="lnorm")

P18 <- as.ProbaVector(N18)

rc <- rCommunity(1, size=10000, Distribution = "lseries", alpha = 30)
autoplot(rc, Distribution="lseries")

Richness(P18)
Shannon(P18)
Simpson(P18)

Richness(Abd18)
Shannon(Abd18)
Simpson(Abd18)

Tsallis(Abd18, q=1)

Diversity(Abd18, q=1)
(d2 <- Diversity(Abd18,q=2))
lnq(d2, q=2)
(e2 <-Tsallis(Abd18,q=2))
expq(e2, q=2)

DP <- CommunityProfile(Diversity, Abd18)
autoplot(DP)

library(ape)
library(phytools)
library(phangorn)

tree <- read.tree("arvore_pronta.new")

sp<-tree$tip.label

write.csv(sp,"especies_arvore_search_try.csv")



is.ultrametric(tree)

force.ultrametric(tree, method=c("nnls","extend"))

plot(tree)



force.ultrametric<-function(tree,method=c("nnls","extend")){
  method<-method[1]
  if(method=="nnls") tree<-nnls.tree(cophenetic(tree),tree,
                                     rooted=TRUE,trace=0)
  else if(method=="extend"){
    h<-diag(vcv(tree))
    d<-max(h)-h
    ii<-sapply(1:Ntip(tree),function(x,y) which(y==x),
               y=tree$edge[,2])
    tree$edge.length[ii]<-tree$edge.length[ii]+d
  } else 
    cat("method not recognized: returning input tree\n\n")
  tree
}

tree_orthofinder_ultra = force.ultrametric(tree)
write.tree(tree_orthofinder_ultra, file = "tree_ultrametric.txt", append = FALSE, digits = 10, tree.names = FALSE)
is.ultrametric(tree_orthofinder_ultra)

plot(tree_orthofinder_ultra)

#binária
bifurcatr <- function(phy,runs=1) {
  trees <- vector("list",length=runs)
  for(i in 1:runs) {
    tree <- phy
    resolves <- Ntip(tree)-Nnode(tree)-1
    for(j in 1:resolves) {
      descendent_counts <- rle(sort(tree$edge[,1]))
      polytomies <- descendent_counts$values[which(descendent_counts$lengths>2)] # these are parent nodes with more than 2 child nodes
      if(length(polytomies)>1) target_polytomy <- sample(polytomies,size=1) else target_polytomy <- polytomies
      polytomy_edges <- which(tree$edge[,1]==target_polytomy)
      target_edges <- sample(polytomy_edges,size=2)
      new_node <- max(tree$edge)+1
      tree$edge[target_edges,1] <- new_node
      new_edge <- c(target_polytomy,new_node)
      tree$edge <- rbind(tree$edge,new_edge)
      new_length <- runif(n=1,min=0,max= min(tree$edge.length[target_edges]))
      tree$edge.length <- c(tree$edge.length,new_length)
      tree$edge.length[target_edges] <- tree$edge.length[target_edges] - new_length
      tree$Nnode <- tree$Nnode+1
    }
    trees[[i]] <- tree
  }
  if(runs==1) {
    trees <- trees[[1]]
    class(trees) <- "phylo"
  }
  else {
    class(trees) <- "multiPhylo"
  }	
  return(trees)
}

arvore<-bifurcatr(tree_orthofinder_ultra, runs = 1)


summary(PhyloDiversity(Abd18,q=1,Tree=arvore))



