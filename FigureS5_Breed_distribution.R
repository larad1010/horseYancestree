library(dplyr)
library(ggplot2)

setwd("/Users/radovicl/Desktop/horseYancestree/")

#FigureS5-Breed distribution across daC/NdaC
#Input data with arbitrary sorted breeds (sort2), corresponding to their geographc regions (sort) of origin, listed in Dataset1
#colored according to major HG clustering

data<- read.csv("Dataset1_edited.csv", header = T, sep = ";")

#prep the data
x<-data %>% group_by(Breed,) %>% mutate(n = n())
y <- x[,-1:-2]
y<-unique(y)

yy<-y %>%
  mutate(n = if_else(Allocation == "Crown",-n,n))

####

ggplot() +
  geom_bar(data = yy, aes(y = reorder(Breed,desc(sort2)), x = n, fill = clustering), stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = c("#CE795F", "#DE9071", "#EBB08F", "#F2C9AA", "#F8E1C7",
                               "#064E4D", "#126B6A", "#1E8986", "#2A9A9F", "#38ABB6", "#4BB6C4", "#63C0CF", "#7ACBD9", "#9BDCEB"), 
                    breaks = c("daC*", "daC_T", "daC_H", "daC_A", "daC_S",
                               "db", "da1_N", "da1_R", "da1_S", "da1_U", "da1_Z", "da3_I", "da3_Y", "da4_E"),
                    labels = c("daC*", "daC_T", "daC_H", "daC_A", "daC_S", 
                               "db", "da1_N", "da1_R", "da1_S", "da1_U", "da1_Z", "da3_I", "da3_Y", "da4_E")) +
  labs(y = "Horse breeds",
       x = "Number of individuals",
       fill = "Haplogroup") +
  theme_bw() +
  scale_x_continuous(labels = function(x) abs(x), name = "Number of individuals") # make x labels positive

############
width <- 12  # Width of Figure 
height <- 18  # Height of Figure

ggsave("plot_breeds.png", plot = last_plot(), width = width, height = height, dpi = 600)
