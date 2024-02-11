setwd("/Users/radovicl/Desktop/horseYancestree")
getwd()

library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(reshape2)

data<- read.csv(file="Dataset1.csv", header = T, sep = ";")

#subset Crown samples
Crown <- data %>% filter(Allocation=="Crown")
head(Crown)

#extract Arabians and their HTs, make count table
AR <- Crown %>% filter(Breed.group.main=="Arabian")
PopAR = AR[["Haplotype"]]
popAcounts <- table(PopAR)

#extract Thoroughbreds and their HTs, make count table
TB<- Crown %>% filter(Breed.group.main=="Thoroughbred")
PopTB = TB[["Haplotype"]]
popBcounts <- table(PopTB)

#extract Spanish influenced breeds and their HTs, make count table
SP <- Crown %>% filter(Breed.group.main=='Spanish influenced')
PopSP = SP[["Haplotype"]]
popDcounts <- table(PopSP)

#extract Coldbloods and their HTs, make count table
CB <- Crown %>% filter(Breed.group.main=="Coldblood")
PopCB = CB[["Haplotype"]]
popCcounts <- table(PopCB)

#extract Asian and their HTs, make count table
AS<- Crown %>% filter(Breed.group.detailed=="Local Asia")
PopAS = AS[["Haplotype"]]
popEcounts <- table(PopAS)

##all data
all<- Crown 
Popall = all[["Haplotype"]]
pop_all_counts <- table(Popall)

#Calculate Frequencies and make dataframe
popA<-as.data.frame(popAcounts/69) #Arabian
colnames(popA)[1]  <- "Haplotypes"
popB<-as.data.frame(popBcounts/93) #Thoroughbred
colnames(popB)[1]  <- "Haplotypes"
popC<-as.data.frame(popCcounts/195) #Coldblood
colnames(popC)[1]  <- "Haplotypes"
popD<-as.data.frame(popDcounts/345) #Spanish influenced
colnames(popD)[1]  <- "Haplotypes"
popE<-as.data.frame(popEcounts/194) #Local Asia
colnames(popE)[1]  <- "Haplotypes"
popALL<-as.data.frame(pop_all_counts/1365) #All
colnames(popALL)[1]  <- "Haplotypes"

#merge data
combined_data <- full_join(popA, popB, by = "Haplotypes") %>%
  full_join(., popC, by = "Haplotypes") %>%
  full_join(., popD, by = "Haplotypes") %>%
  full_join(., popE, by = "Haplotypes") %>%
  full_join(., popALL, by = "Haplotypes") %>%
  mutate(across(contains("Freq"), ~replace(.x, is.na(.x), 0))) #replace missing values with 0

# Rename the frequency columns with the population names
head(combined_data)
colnames(combined_data) <- c("Haplotype", "Arabians", "Thoroughbreds", "Coldbloods", "Spanish influenced", "Local-Asia", "Total daC")
unique<- unique(all$Haplotype)

#matching the order of haplotypes
order <- c("daC*",
           # "daC_O",
           "daC_A*",
           # "daC_Ak",
           "daC_Ad*",
           # "daC_Ad-m",
           "daC_Ad-s",
           "daC_Ad-b*",
           "daC_Ad-bA",
           "daC_Ad-bF",
           "daC_Ad-bM",
           "daC_Ad-bN",
           "daC_Ad-bP",
           "daC_Ad-bS",
           "daC_Ad-h*",
           "daC_Ad-hA*",
           "daC_Ad-hA1*",
           "daC_Ad-hA1a",
           # "daC_Ad-hA2",
           "daC_Ad-hB",
           "daC_Ad-hC",
           "daC_Ad-hR",
           "daC_Am*",
           "daC_Am-a*",
           "daC_Am-aA",
           "daC_Am-aB",
           "daC_Am-s*",
           "daC_Am-sM",
           "daC_Am-sP",
           "daC_Ao*",
           "daC_Ao-n*",
           "daC_Ao-nM*",
           "daC_Ao-nM1*",
           "daC_Ao-nM1a*",
           "daC_Ao-nM1a1",
           "daC_Ao-nM1a2",
           "daC_Ao-nM1b",
           "daC_Ao-nM2",
           "daC_Ao-nN",
           "daC_Ao-b*",
           "daC_Ao-bB",
           "daC_Ao-bC",
           "daC_Ao-a*",
           "daC_Ao-aA*",
           "daC_Ao-aA1a",
           "daC_Ao-aA1b",
           "daC_Ao-aA2", 
           "daC_Ao-aA3",
           # "daC_Ao-a1*",
           "daC_Ao-a1D*",
           "daC_Ao-a1D1",
           "daC_Ao-a1D2",
           "daC_Ao-a1H",
           "daC_Ao-a1P",
           "daC_Ao-aM",
           "daC_Tb1-a*",
           "daC_Tb1-aK",
           "daC_Tb1-aB",
           "daC_Tb-d*",
           "daC_Tb-dW*",
           "daC_Tb-dW4",
           "daC_Tb-dW3",
           "daC_Tb-dW2",
           "daC_Tb-dW1",
           "daC_Tb-dM",
           "daC_Tb1*",
           "daC_Tb-o*",
           "daC_Tb-oL",
           "daC_Tb-oB*",
           "daC_Tb-oB5",
           # "daC_Tb-oB4",
           "daC_Tb-oB3*",
           "daC_Tb-oB3b",
           "daC_Tb-oB3a",
           "daC_Tb-oB2",
           "daC_Tb-oB1",
           "daC_Tb-oH",
           "daC_Tb-k",
           "daC_Tb*",
           "daC_T3u*",
           "daC_T3u-f",
           # "daC_T3u-c",
           # "daC_T3i",
           "daC_T3a",
           "daC_T3*",
           "daC_T2r",
           "daC_T2k",
           "daC_T2*",
           "daC_T1m",
           "daC_T1c",
           "daC_T1*",
           "daC_Hs*",
           "daC_Hs-b",
           "daC_Hs-aS",
           "daC_Hs-aO",
           # "daC_Hc",
           "daC_S*",
           "daC_Se",
           "daC_Sc")

input_list <- c( "daC_Tb-dW1","daC_Tb-dW4","daC_Tb-oB3b","daC_Ao-aA1a","daC*","daC_Ao-a1D2","daC_Ao-nM1a2",
                 "daC_Hs-b","daC_Hs-aS","daC_Tb-oB*","daC_Tb-dM","daC_Ad-hC","daC_Ao-aA1b","daC_Tb-dW*",
                 "daC_Tb*","daC_Tb-d*","daC_Ao*","daC_Tb-oB1","daC_Ad-bS","daC_Tb-oL","daC_Tb-dW3", 
                 "daC_Tb-dW2","daC_T3u-f","daC_T2k","daC_Ad-bN","daC_Ao-a1D1","daC_Tb-oB3a","daC_Ad-bA", 
                 "daC_T3a","daC_Ao-a1D*","daC_Tb-oH","daC_Tb1-aB","daC_T1*","daC_Tb-o*","daC_Am-sM",  
                 "dAC_Ao-nM2", "daC_Ad-b*","daC_Ad-bM","daC_Ad-hA1*","daC_Ad-hA1a","daC_T3u*","daC_S*",      
                 "daC_Ad-h*","daC_Tb1-aK","daC_Tb1-a*","daC_Ao-aA3","daC_Ao-aA*","daC_T2r","daC_Tb-oB3*",
                 "daC_Ao-nM1a1","daC_Ao-aA2","daC_Ad-hB","daC_Ao-aM","daC_Ad-hA*","daC_Am-s*","daC_Ad-bP",  
                 "daC_Tb-k","daC_Ao-bC","daC_Am-sP","daC_Sc","daC_Ao-b*","daC_Am*","daC_T2*",  
                 "daC_T1m","daC_Ad-bF","daC_T1c","daC_Tb1*","daC_Se","daC_Am-aA","daC_Ao-nM2",  
                 "daC_Am-aB","daC_Ao-nM1b","daC_T3*","daC_A*","daC_Ao-bB","daC_Ao-a*","daC_Ao-n*",   
                 "daC_Ad*","daC_Ad-s","daC_Ao-a1H","daC_Ao-nM*","daC_Tb-oB5","daC_Ao-nN","daC_Am-a*", 
                 "daC_Ao-a1P","daC_Hs*","daC_Tb-oB2","daC_Ad-hR","daC_Hs-aO","daC_Ao-nM1*","daC_Ao-nM1a*"
)

custom_ordering <- match(input_list, order)

# Sort the input_list using the custom ordering
sorted_list <- input_list[order(custom_ordering)]
print(sorted_list) # Print the sorted list

#adjust data for plotting
data1<-melt(combined_data, id.vars="Haplotype")

colnames(data1)[1]  <- "HT"
colnames(data1)[2]  <- "Signature"
colnames(data1)[3]  <- "Frequencies"

data1$HT <- factor(data1$HT,levels=rev(sorted_list))

gg_heat<-ggplot(data1, aes(x = Signature, y = HT, fill = Frequencies)) +
  geom_tile(color = "white", size = 0.1)+
  scale_fill_distiller(palette = "Reds", direction = 1) +
  coord_equal() +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(hjust = 0, size = 7)) +
  scale_y_discrete(limits = rev(order)) +
  theme(plot.title.position = "plot", plot.title = element_text(face = "bold")) +
  geom_text(aes(label = sprintf("%.4f", Frequencies)), size = 0.5, vjust = 0.5, hjust = 0.5) 

gg_heat

# Save the ggplot 
ggsave("heatmap.pdf", plot = gg_heat, bg = "transparent", path="/Users/radovicl/Desktop/coronatYon_v3/Analysis_vol3/", width = 6, height = 8, dpi = 600)  
write.table(combined_data,file = "sampleset_heatmap_info", quote = FALSE, row.names=F)

#refined with Canva Pro (https://www.canva.com)
