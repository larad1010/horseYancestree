setwd("/Users/radovicl/Desktop/horseYancestree")
getwd()

library(dplyr)
library(ggplot2)
library(reshape2)
library(scatterpie)

df<- read.csv(file="Dataset1.csv", header = T, sep = ";") %>% 
  select(c("Sample_ID", "Breed","Allocation","Country.region.of.breed","Geographic.region"))

regions<- df %>% 
  group_by(Geographic.region)%>% 
  count(Allocation) 

regions<- group_by(regions, Geographic.region) %>% mutate(all_n = sum(n))

#rounded percentages
regions<- group_by(regions, Geographic.region) %>% mutate(percent = n/sum(n))
regions$percent<-round(regions$percent, digits = 2)

#from long format to wide for plotting
data_wide_regions <- dcast(regions, Geographic.region+ all_n  ~ Allocation, value.var="percent")
data_wide_regions <- data_wide_regions %>% replace(is.na(.), 0) #replace all NAs with 0

# Add geographic info columns to dataframe
lat<-c(0.396089, #Africa
       42.972929, #Central Asia
       50.317782, #Central West Europe
       39.402695, #Iberian Peninsula
       -9.578508, #LatinMid America
       26.155962, #North Africa
       49.411353, #North America
       69.240786, #North Europe
       62.124709, #North Asia
       -22.390173,#other colonial territories
       27.722954, #South Asia
       40.212887, #South East Europe
       33.907381) #West Asia

long <- c(23.451480,  #Africa
          67.221012,  #Central Asia
          4.994455,   #Central West Europe
          -3.989025,  #Iberian Peninsula
          -59.887458, #LatinMid America
          9.546135,   #North Africa
          -109.457769, #North America
          24.857734,#North Europe
          75.306949,#North Asia
          134.526600, #other colonial territories
          74.409415, #South Asia
          18.335200, #South East Europe
          46.478826) #West Asia

data_wide_regions$long <- long
data_wide_regions$lat <- lat

## plot

##not showing Antartica
worldmap <- map_data('world')
worldmap <- worldmap[worldmap$region != "Antarctica",]


ggplot(worldmap) + 
  geom_map(data = worldmap, map = worldmap, 
           aes(x=long, y=lat, map_id=region), col = "gray50", fill = "gray50") +
  geom_scatterpie(aes(x=long, y=lat, group=Geographic.region, r=all_n/20), #scale sizes
                  data=data_wide_regions, cols= c("Crown", "Non Crown"), color=NA, alpha=.8) +
  coord_fixed() + 
  geom_scatterpie_legend(data_wide_regions$all_n/20, x=-180, y=-50, n=10, labeller=function(all_n) all_n*20)+
  scale_fill_manual(name = "Allocation",
                    breaks = c("Crown","Non Crown"),
                    labels = c("Crown (n=1,365)","Non Crown (n=152)"),
                    values = c("Crown"="#eba18d","Non Crown"="#A9D0CE" )) +
  theme(plot.title = element_text(size =12,hjust = 0.5 ))+
  theme_minimal()+
  theme(legend.position="right",legend.box = "vertical",legend.spacing.x = unit(0.25, 'cm')) +
  theme(legend.title = element_text(colour="black", size=8)) + 
  theme(legend.text = element_text(colour="black", size=5))+
  theme(legend.background = element_rect(fill = "white", color = "black"))+
  theme(legend.position = c(0.85, 0.1))+
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=9)) + 
  theme(legend.margin=margin(c(5,5,5,5))) 

# Fi


