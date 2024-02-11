library(ggplot2)
library(tidyverse)

setwd("/Users/radovicl/Desktop/horseYancestree/")

#data input and prep
eigenval <- read.table("data_daC_pca.eigenval",sep=" ",header=F)
pca <- read.table("data_daC_pca.eigenvec",sep=" ",header=F)
pca <- pca[,-2]
names(pca)[1] <- "ind"
names(pca)[2:ncol(pca)] <- paste0("PC", 1:(ncol(pca)-1))
write.table(pca,"daCpca.txt",row.names =F,quote=F) 

#manually added Breed group names for coloring (name)
name<- read.table(file="Breed_group_list.txt", header = T, sep = "\t")
head(name)

pca1 <- merge(name, pca, by = "ind", all = TRUE)
pca1$Breed_group<-as.factor(pca1$Breed_group)
head(pca1)

#percentages of variation for PCs
pve <- data.frame(PC = 1:1365, pve = eigenval/sum(eigenval)*100)
cumsum(pve$V1)
pve$V1[1]
pve$V1[2]

p<-ggplot(pca1,aes(x=PC1,y=PC2,color=Breed_group))+labs(y= "PC2 (6.63%)", x = "PC1 (9.78%)")+
  geom_point(position=position_jitter(h=.003, w=.003),shape=2,size=2) + #avoid overplotting data points
  theme(legend.position = c(0.85, 0.2)) +
  theme(legend.title = "Breed group")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black")) +
  scale_color_manual(breaks = c("AR","TB","CB","SP","Local","Riding" ),
                     labels = c("Arabian and Arabian lines" ,"Thoroughbred and Thoroughbred lines","Coldbloods","Spanish influenced breeds", "Local riding and lightdraft horses","Riding Europe and USA horses"),
                     values=c("AR"="blue","TB"="#ff0000","CB"="#448c0f","SP"="orange","Local"="#ccc083","Riding"="#ed9887"))+
  theme_classic() +
  theme(legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="black"))+ 
  guides(color = guide_legend(override.aes = list(size=5))) 
p


ggsave("PCA.pdf", plot = p,dpi = 600, width =35,height = 20, units = "cm")

# Revised and merged Figure with Canva Pro (https://www.canva.com)


