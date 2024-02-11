setwd("/Users/radovicl/Desktop/horseYancestree/")

library(data.table)
library(dplyr)

#inputed file with ISO codes (Country/region_breed_ISO) for all represented countries/regions, listed in Dataset1 (Geo_region)
data<- fread(file="ISO3_geo_regions.txt", header = T, sep = "\t")
data %>% count(Geo_region) 

#select data for plotting
ww <- data %>% 
  select(c("Country/region_breed_ISO", "Geo_region")) 
ddf<- unique(ww) #unique countries

world_sf <- ne_countries(returnclass = "sf")

# Merge the two dataframes
world_df <- merge(world_sf, ddf, by.x = "iso_a3", by.y = "Country/region_breed_ISO", all.x = T)
legend_title <- "Geographic regions"

# Plot
plot<- ggplot() +
  geom_sf(data = world_df, aes(fill = Geo_region2)) +
  coord_sf(crs= "+proj=robin") +
  theme_minimal_grid() +
  scale_fill_manual(legend_title, breaks = c("North America","Latin America","North Europe","Central and West Europe", "South and East Europe", "Iberian Peninsula", "North Africa", "Africa", "Central Asia","West Asia","North Asia", "South Asia", "Other colonial territories"),
                    labels = c("North America, n=258","Latin America, n=145","North Europe, n=323","Central and West Europe, n=280", "South and East Europe, n=102", "Iberian Peninsula, n=45", "North Africa, n=33", "Africa, n=36", "Central Asia, n=87","West Asia, n=80","North Asia, n=94", "South Asia, n=25", "Other colonial territories, n=9"),
                    values= c("North America"="#dda0de","Latin America"="#5e4fa2","North Europe"="#5883fb","Central and West Europe"="#66c2a5", "South and East Europe"="#f5e398", "Iberian Peninsula"="#fdae61", "North Africa"="#f46d43", "Africa"="#b87e6c", "Central Asia"="#994C00", 
                              "West Asia"="#9e0142","North Asia"="#d53e4f","South Asia"="#FF9999", "Other colonial territories"= "#e4ddc0")) +
  theme(legend.position="left",legend.box = "vertical",legend.spacing.x = unit(0.25, 'cm')) +
  theme(legend.position = c(0.02, 0.50))+
  theme(legend.title = element_text(colour="black", size=9)) + 
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.margin=margin(c(5,5,5,5)))+ 
  theme(legend.background = element_rect(fill = "white", color = "black"))+
  theme(plot.title = element_text(size =12,hjust = 0.5 ))
plot

ggsave(plot =plot , file = "Geographical distribution.pdf", 
       bg = "white",device = "pdf",
       width = 1250, height = 850,units = "px", dpi = 800)


