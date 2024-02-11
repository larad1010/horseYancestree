library(ape)
library(phangorn)

data<- read.table("/Users/radovicl/Desktop/horseYancestree/matrix_tree.csv", header = T, sep = ";")
data2 <- data[,-1]
rownames(data2) <- data[,1]

x<- as.matrix(data2) # each row is individual
phy_data<- as.phyDat(x,type="USER", levels = c(0, 1))
class(phy_data) 
phy_data

#create a distance matrix
dist_mat <- dist.gene(x,method = "pairwise") 

#upgma tree
tree_upgma <- upgma(dist_mat)
tree_upgma<-root(tree_upgma,outgroup = "Outgroup")
class(tree_upgma)
plot(tree_upgma,cex=0.6)

#nj tree
tree_nj <- nj(dist_mat)
class(tree_nj)
plot(tree_nj,cex=0.6)

#Parsimony
# parsimony scores of trees are given as comments
parsimony(tree_nj, phy_data) #512
parsimony(tree_upgma, phy_data) #553


treeRatchet  <- pratchet(phy_data, trace = 0, minit=100) #ratchet 100iterations, k=10
parsimony(treeRatchet, phy_data)
treeRatchet  <- acctran(treeRatchet, phy_data)
plot(treeRatchet, cex = 0.6)
is.rooted(treeRatchet)

treeRatchet  <- acctran(treeRatchet, phy_data)
treeRatchet2  <- di2multi(treeRatchet)
treeRatchet3 <- root(treeRatchet2, outgroup = "Outgroup",resolve.root = TRUE )
plot(treeRatchet3, cex = 0.6,node.pos = F )

if(inherits(treeRatchet3, "multiPhylo")){
  treeRatchet3 <- unique(treeRatchet3)
}

is.rooted(treeRatchet3)

plot(treeRatchet3, cex = 0.6,node.pos = F )
add.scale.bar(x =20, y=0,lcol = "black", cex=0.6)

#following manual available at https://cran.r-project.org/web/packages/phangorn/vignettes/Trees.html

write.tree(treeRatchet3)
#manually changed order on branches, inputed data into FigTree and adjusted final figure with Canva Pro (https://www.canva.com)