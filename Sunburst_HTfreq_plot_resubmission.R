### Haplotype frequencies 
#Data and plots for Figure 2, Figure 4b, and Supporting Figure S6
setwd("/Users/radovicl/Desktop/CoronatYon/PNAS_resubmission_27052024/coronatYon_resubmission_analysis/Users/radovicl/Desktop/CoronatYon/PNAS_resubmission_27052024/coronatYon_resubmission_analysis/CoronatYon_Supplement_resubmission")
getwd()

library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)

data<- read.csv(file="Dataset1_resubmission.csv", header = T, sep = ";")

#subset Crown samples
Crown <- data %>% filter(Allocation=="Crown")
head(Crown)

df_fitting1<- Crown %>% 
  group_by(Breed.group.detailed) %>% 
  count(Haplotype)


#### Make order of haplotypes to fit sunburst backbone 
vec <- c("daC*",
         "daC_O",
         "daC_A*",
         "daC_Ak",
         "daC_Ad*",
         "daC_Ad-m",
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
         "daC_Ad-hA2",
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
         "daC_Ao-a1*",
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
         "daC_Tb-oB4",
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
         "daC_T3u-c",
         "daC_T3i",
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
         "daC_Hc",
         "daC_S*",
         "daC_Se",
         "daC_Sc")

order<-as.data.frame(vec)

##################
### Figure 3 ###
##################

##### Thoroughbred ######
TB <- df_fitting1 %>% filter(Breed.group.detailed=="Thoroughbred")
TB_lines <- df_fitting1 %>% filter(Breed.group.detailed=="Thoroughbred lines")

#make the order in dataframe according to vector order and plot the data
TB1 <- TB[match(vec, TB$Haplotype), ] #fill the HTs in vector based on data
TB1["n"][is.na(TB1["n"])] <- 0 #put 0 in remaining HTs (NA)
TB1["Breed.group.detailed"][is.na(TB1["Breed.group.detailed"])] <- "Thoroughbred" #fill in with Breed group name where NA
TB1$Haplotype <- order$vec #Order HTs the same as in sunburst order (in case if not)
sum(TB1$n)
TB1 <- rowid_to_column(TB1, "id")

TBl <- TB_lines[match(vec, TB_lines$Haplotype), ]
TBl["n"][is.na(TBl["n"])] <- 0
TBl["Breed.group.detailed"][is.na(TBl["Breed.group.detailed"])] <- "Thoroughbred lines"
TBl$Haplotype <- order$vec
sum(TBl$n)

TBl <- rowid_to_column(TBl, "id")
data_TB <- rbind(TB1,TBl)

#######
empty_bar=2 #add empty space in the beginning of plot to have space for root  daC branch

# Get the name and the y position of each label
label_data= data_TB %>% group_by(id, Breed.group.detailed) %>% summarize(tot=sum(n))
number_of_bar=nrow(label_data)
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines and plotting
base_data=data_TB %>% 
  group_by(Breed.group.detailed) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

#plot Thoroughbred HT spectra
PTB<- ggplot(data_TB) + 
  geom_bar(aes(x=as.factor(id), y=n, fill=reorder(Breed.group.detailed, -n)), stat="identity", alpha=1)+ 
  # Add a value=100/75/50/25 lines. 
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "black", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  # Add the stacked bar
  scale_fill_manual(breaks = c("Thoroughbred", "Thoroughbred lines"),
                    labels = c("Thoroughbred", "Thoroughbred lines"),                    
                    values = c("Thoroughbred"="#F80D06","Thoroughbred lines"="#F97C78"))+
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data_TB$id),3), y = c(0,25, 50), label = c( "","25", "50") , color="black", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-150,max(50, na.rm=F)) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar() 
PTB

ggsave(plot =PTB, file = "sunburst_TB_23_vol3.png", path="//Users/radovicl/Desktop/horseYancestree",
       bg = "transparent",
       width = 30, height = 30, units = "cm", dpi = 600)

##### Arabian ######
AR<- df_fitting1 %>% filter(Breed.group.detailed=="Arabian")
AR_lines<- df_fitting1 %>% filter(Breed.group.detailed=="Arabian lines")

#make the order in dataframe according to vector order and plot the data
AR <- AR[match(vec,AR$Haplotype), ]
AR["n"][is.na(AR["n"])] <- 0
AR["Breed.group.detailed"][is.na(AR["Breed.group.detailed"])] <- "Arabian"
AR$Haplotype <- order$vec
sum(AR$n)

AR<- rowid_to_column(AR, "id")

#make the order in dataframe according to vector order and plot the data
AR_l <- AR_lines[match(vec,AR_lines$Haplotype), ]
AR_l["n"][is.na(AR_l["n"])] <- 0
AR_l["Breed.group.detailed"][is.na(AR_l["Breed.group.detailed"])] <- "Arabian lines"
AR_l$Haplotype <- order$vec
sum(AR_l$n)
AR_l<- rowid_to_column(AR_l, "id")

AR<- rbind(AR,AR_l)
sum(AR$n)

dataAR <- as.data.frame(AR)
#########
# Get the name and the y position of each label
label_data= dataAR %>% group_by(id, Haplotype) %>% summarize(tot=sum(n))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=dataAR %>% 
  group_by(Haplotype) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

####
PAR<- ggplot(dataAR) + 
  geom_bar(aes(x=as.factor(id), y=n, fill=reorder(Breed.group.detailed, -n)), stat="identity", alpha=1)+ #alpha is rendering color brightness
  # Add a val=100/75/50/25 lines. 
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "black", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  # Add the stacked bar
  scale_fill_manual(labels = c("Arabian","Arabian lines"),                    
                    values = c("Arabian"="#3335FF","Arabian lines"="#9596EE"))+
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(dataAR$id),3), y = c(0,25,50), label = c("","25", "50") , color="black", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-150,max(50, na.rm=F)) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar() 
PAR

ggsave(plot =PAR , file = "sunburst_AR_23_vol3.png", path="/Users/radovicl/Desktop/horseYancestree",
       bg = "transparent",width = 30, height = 30, units = "cm", dpi = 600)

##### Coldblood ########
CB_Asia<- df_fitting1 %>% filter(Breed.group.detailed=="Coldblood Asia")
CBB<- df_fitting1 %>% filter(Breed.group.detailed=="Coldblood British")
CB_EuAm<- df_fitting1 %>% filter(Breed.group.detailed=="Coldblood Europe and USA")

#make the order in dataframe according to vector order and plot the data
CBA <- CB_Asia[match(vec,CB_Asia$Haplotype), ]
CBA["n"][is.na(CBA["n"])] <- 0
CBA["Breed.group.detailed"][is.na(CBA["Breed.group.detailed"])] <- "Coldblood Asia"
CBA$Haplotype <- order$vec
sum(CBA$n)

CBA<- rowid_to_column(CBA, "id")
##
CBB <- CBB[match(vec,CBB$Haplotype), ]
CBB["n"][is.na(CBB["n"])] <- 0
CBB["Breed.group.detailed"][is.na(CBB["Breed.group.detailed"])] <- "Coldblood British"
CBB$Haplotype <- order$vec
sum(CBB$n)

CBB<- rowid_to_column(CBB, "id")

##
CB_EuAm <- CB_EuAm[match(vec,CB_EuAm$Haplotype), ]
CB_EuAm ["n"][is.na(CB_EuAm ["n"])] <- 0
CB_EuAm ["Breed.group.detailed"][is.na(CB_EuAm ["Breed.group.detailed"])] <- "Coldblood Europe and USA"
CB_EuAm $Haplotype <- order$vec
sum(CB_EuAm $n)

CB_EuAm<- rowid_to_column(CB_EuAm, "id")

CB<- rbind(CBA,CBB,CB_EuAm)
sum(CB$n)
dataCB <- as.data.frame(CB)

#########
# Get the name and the y position of each label
label_data= dataCB %>% group_by(id, Haplotype) %>% summarize(tot=sum(n))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=dataCB %>% 
  group_by(Haplotype) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

####
PCB<- ggplot(dataCB) + 
  geom_bar(aes(x=as.factor(id), y=n, fill=reorder(Breed.group.detailed, -n)), stat="identity", alpha=1)+ #alpha is rendering color brightness
  # Add a val=100/75/50/25 lines. 
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "black", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  # Add the stacked bar
  scale_fill_manual(labels = c("Coldblood Asia","Coldblood British","Coldblood Europe and USA"),                    
                    values = c("Coldblood Asia"="#91ad78","Coldblood British"="#598037","Coldblood Europe and USA"="#c8dbb8"))+
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(dataCB$id),3), y = c(0,25,50), label = c("","25", "50") , color="black", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-150,max(50, na.rm=F)) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar() 
PCB

ggsave(plot =PCB , file = "sunburst_CB_23_vol3.png", path="/Users/radovicl/Desktop/horseYancestree",
       bg = "transparent",
       width = 30, height = 30, units = "cm", dpi = 600)

####Spanish influenced####
Spanish <- df_fitting1 %>% filter(Breed.group.detailed=='Spanish Colonial')
Iberian <- df_fitting1 %>% filter(Breed.group.detailed=='Spanish Iberian')
NAf <- df_fitting1 %>% filter(Breed.group.detailed=='Spanish North Africa')

#make the order in dataframe according to vector order and plot the data
# Colonial Spanish
Spanish <- Spanish[match(vec, Spanish$Haplotype), ]
Spanish["n"][is.na(Spanish["n"])] <- 0
Spanish["Breed.group.detailed"][is.na(Spanish["Breed.group.detailed"])] <- "Spanish Colonial"
Spanish$Haplotype <- order$vec
sum(Spanish$n)

Spanish<- rowid_to_column(Spanish, "id")
#######
#North African
NAf <- NAf[match(vec, NAf$Haplotype), ]
NAf["n"][is.na(NAf["n"])] <- 0
NAf["Breed.group.detailed"][is.na(NAf["Breed.group.detailed"])] <- "Spanish North Africa"
NAf$Haplotype <- order$vec
sum(NAf$n)

NAf<- rowid_to_column(NAf, "id")

#Iberian
#make the order in dataframe according to vector order and plot the data
Iberian <- Iberian[match(vec, Iberian$Haplotype), ]
Iberian["n"][is.na(Iberian["n"])] <- 0
Iberian["Breed.group.detailed"][is.na(Iberian["Breed.group.detailed"])] <- "Spanish Iberian"
Iberian$Haplotype <- order$vec
sum(Iberian$n)

Iberian<- rowid_to_column(Iberian, "id")

Spanishh<- rbind(Iberian,NAf,Spanish)
sum(Spanishh$n)
dataSP <- as.data.frame(Spanishh)

#####
# Get the name and the y position of each label 
label_data= dataSP %>% group_by(id, Breed.group.detailed) %>% summarize(tot=sum(n))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines and plotting
base_data=dataSP %>% 
  group_by(Breed.group.detailed) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

#######
PSP<- ggplot(dataSP) + 
  geom_bar(aes(x=as.factor(id), y=n, fill=reorder(Breed.group.detailed, -n)), stat="identity", alpha=1)+ #alpha is rendering color brightness
  # Add a val=100/75/50/25 lines. 
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "black", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  # Add the stacked bar
  scale_fill_manual(breaks = c("Spanish Colonial","Spanish Iberian", "Spanish North Africa"),
                    labels = c("Spanish Colonial","Spanish Iberian", "Spanish North Africa"),                    
                    values = c("Spanish Colonial"="#eba975","Spanish Iberian"="#f76d02", "Spanish North Africa"="#bf5a0a"))+
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(dataSP$id),3), y = c(0,25, 50), label = c( "","25", "50") , color="black", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-150,max(50, na.rm=F)) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar() 
PSP

ggsave(plot =PSP , file = "sunburst_Spanish.png",path="/Users/radovicl/Desktop/horseYancestree/", 
       bg = "transparent",
       width = 30, height = 30, units = "cm", dpi = 600)

##### Local-Asian ####
As<- Crown %>% filter(Breed.group.main=="Local riding and lightdraft-Asia")%>% 
  count(Haplotype)
####
NAs<- As %>% filter(Geographic.region=="North Asia")%>% 
  count(Haplotype)
SAs<- As %>% filter(Geographic.region=="South Asia")%>% 
  count(Haplotype)
WAs<- As %>% filter(Geographic.region=="West Asia")%>% 
  count(Haplotype)
CentralAs<- As %>% filter(Geographic.region=="Central Asia")%>% 
  count(Haplotype)
####

#######
Local_NAs<- NAs[match(vec,NAs$Haplotype), ]
Local_NAs["n"][is.na(Local_NAs["n"])] <- 0
Local_NAs["Geographic.region"] <- "North Asia"
Local_NAs$Haplotype <- order$vec
sum(Local_NAs$n)
Local_NAs<- rowid_to_column(Local_NAs, "id")

Local_SAs<- SAs[match(vec,SAs$Haplotype), ]
Local_SAs["n"][is.na(Local_SAs["n"])] <- 0
Local_SAs["Geographic.region"]  <- "South Asia"
Local_SAs$Haplotype <- order$vec
sum(Local_SAs$n)
Local_SAs<- rowid_to_column(Local_SAs, "id")

Local_WAs<- WAs[match(vec,WAs$Haplotype), ]
Local_WAs["n"][is.na(Local_WAs["n"])] <- 0
Local_WAs["Geographic.region"]<- "West Asia"
Local_WAs$Haplotype <- order$vec
sum(Local_WAs$n)
Local_WAs<- rowid_to_column(Local_WAs, "id")

Local_CAs<- CentralAs[match(vec,CentralAs$Haplotype), ]
Local_CAs["n"][is.na(Local_CAs["n"])] <- 0
Local_CAs["Geographic.region"] <- "Central Asia"
Local_CAs$Haplotype <- order$vec
sum(Local_CAs$n)
Local_CAs<- rowid_to_column(Local_CAs, "id")

dataAS<- rbind(Local_WAs,Local_SAs,Local_NAs,Local_CAs)
dataAS <- as.data.frame(dataAS)


# Get the name and the y position of each label
label_data= dataAS %>% group_by(id, Haplotype) %>% summarize(tot=sum(n))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=dataAS %>% 
  group_by(Haplotype) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

PAs<- ggplot(dataAS) + 
  geom_bar(aes(x=as.factor(id), y=n, fill=reorder(Geographic.region, -n)), stat="identity", alpha=1)+ #alpha is rendering color brightness
  # Add a val=100/75/50/25 lines. 
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "black", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  # Add the stacked bar
  scale_fill_manual(labels = c("North Asia","South Asia","West Asia","Central Asia"),                    
                    values = c("North Asia"="#58083C","South Asia"="#d7ccdb","West Asia"="#9A7EA6","Central Asia"="#C4889F"))+
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(dataAS$id),3), y = c(0,25,50), label = c("","25", "50") , color="black", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-150,max(50, na.rm=F)) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar() 
PAs

ggsave(plot =PAs , file = "sunburst_Local-Asia.png", 
       bg = "transparent",path="/Users/radovicl/Desktop/horseYancestree/",
       width = 30, height = 30, units = "cm", dpi = 600)

#Merged Figures and revised with Canva Pro (https://www.canva.com)

##################
### Figure 5a ###
##################
head(dataAR)
head(dataAS)

# Reorder the columns
dataAS<- dataAS %>%
  select(id, Geographic.region, Haplotype, n) %>%
  rename(Breed.group.detailed = Geographic.region)

# View the rearranged dataframe
print(dataAS)
All<- rbind(dataAR,data_TB,dataCB,dataSP,dataAS)

# Get the name and the y position of each label
label_data= All %>% group_by(id, Haplotype) %>% summarize(tot=sum(n))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=All %>% 
  group_by(Haplotype) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

PAll<- ggplot(All) + 
  geom_bar(aes(x=as.factor(id), y=n, fill=reorder(Breed.group.detailed, -n)), stat="identity", alpha=1)+ #alpha is rendering color brightness
  # Add a val=100/75/50/25 lines. 
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "black", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  # Add the stacked bar
  scale_fill_manual(labels = c("Arabian","Thoroughbred","Coldblood","Spanish influenced","Local Asia"),                    
                    values = c("Arabian"="#808080","Thoroughbred"="#808080","Coldblood"="#808080","Spanish influenced"="#808080","Local Asia"="#808080"))+
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(All$id),5), y = c(0,25,50,75,100), label = c("","25", "50","75","100") , color="black", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-150,max(100, na.rm=F)) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar() 
PAll

ggsave(plot =PAll , file = "sunburst_all_resubmission_2024.png", 
       bg = "transparent",path="/Users/radovicl/Desktop/CoronatYon/coronatYon_resubmission/CoronatYon_Supplement_resubmission",
       width = 30, height = 30, units = "cm", dpi = 600)

#Merged Figures and revised with Canva Pro (https://www.canva.com)

##################
### Figure S6 ###
##################
####
Riding_EuAm<- df_fitting1 %>% filter(Breed.group.detailed=="Riding Europe and USA")
R_EuAm <- Riding_EuAm[match(vec,Riding_EuAm$Haplotype), ]
R_EuAm["n"][is.na(R_EuAm["n"])] <- 0
R_EuAm["Breed.group.detailed"][is.na(R_EuAm["Breed.group.detailed"])] <- "Riding Europe and USA"
R_EuAm$Haplotype <- order$vec
sum(R_EuAm$n)

Riding<- rowid_to_column(R_EuAm, "id")
sum(Riding$n)
dataRiding <- as.data.frame(Riding)

######
# Get the name and the y position of each label
label_data= dataRiding  %>% group_by(id, Haplotype) %>% summarize(tot=sum(n))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=dataRiding  %>% 
  group_by(Haplotype) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

####
PRIDING<- ggplot(dataRiding ) + 
  geom_bar(aes(x=as.factor(id), y=n, fill=reorder(Breed.group.detailed, -n)), stat="identity", alpha=1)+ #alpha is rendering color brightness
  # Add a val=100/75/50/25 lines. 
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "black", alpha=0.5, size=0.1 , inherit.aes = FALSE ) +
  # Add the stacked bar
  scale_fill_manual(labels = c("Riding Europe and USA"),                    
                    values = c("Riding Europe and USA"="#6e0700"))+
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(dataRiding$id),3), y = c(0,25,50), label = c("","25", "50") , color="black", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-150,max(50, na.rm=F)) +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar() 
PRIDING

ggsave(plot =PRIDING , file = "sunburst_Riding_res.png", path="/Users/radovicl/Desktop/CoronatYon/coronatYon_resubmission/CoronatYon_Supplement_resubmission",
       bg = "transparent",
       width = 30, height = 30, units = "cm", dpi = 600)

#Revised Figure with Canva Pro (https://www.canva.com)
#More details available at www.data-to-viz.com (https://github.com/holtzy/data_to_viz, Copyright (c) 2017 Holtz Yan)
