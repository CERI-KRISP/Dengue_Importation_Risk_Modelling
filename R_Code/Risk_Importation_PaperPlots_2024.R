### Paper :  Dengue Introduction Suitability in Africa estimated using Risk Flow Modelling
## Rscript - for Figures 



##############################################################################################################################
#####################################################. FIGURE 1  ###########################################################
##############################################################################################################################

## Map with Transmission Potential * Population density index as background 
# Population density raster 
pop2 <- raster("Data/population density/SEDAC_POP_2000-01-01_gs_3600x1800.tiff")
pop2 <- reclassify(pop2, cbind(255, NA)) # 255 was water 
plot(pop2,col=rev(paletteer_c("grDevices::YlOrRd", 30)))

## Transmission Potential 
merged_raster <- raster("Final/Paper_Reproducibility/merged_raster_India_Brazil.tif")
TP_Africa_plot <- raster("Final/Paper_Reproducibility/Africa_Transmission_Pot_African_Continent.tif")
TP_raster_COI<- raster("Final/Paper_Reproducibility/All_TP_rasters_14_and_Africa.tif")

## Tp multiplied by Population Density
raster1_reproj <- projectRaster(pop2, TP_raster_COI)

result <- overlay(raster1_reproj, TP_raster_COI, fun=function(x,y) x * y)



## You want the shapefile aswell to have a nice outline 
spdf_world <- ne_countries(scale = "medium", returnclass = "sf") 
## Removing antartica
spdf_world <- spdf_world %>% 
  filter(admin != "Antarctica")

spdf_africa <- spdf_world %>% 
  filter(admin %in% c("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
                      "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros",
                      "Democratic Republic of the Congo","Republic of Congo","Republic of the Congo","Ivory Coast","Cote D'Ivoire",
                      "Djibouti","Egypt","Equatorial Guinea","Eritrea","Swaziland","Ethiopia",
                      "Gabon","Gambia","Ghana","Guinea","Guinea Bissau","Kenya","Lesotho","Liberia",
                      "Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco",
                      "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe",
                      "Senegal","Seychelles","Sierra Leone","Somalia","South Africa","South Sudan",
                      "Sudan","United Republic of Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe"))

spdf_africa <- st_make_valid(spdf_africa)


spdf_africa_subregion <-  spdf_africa %>%
  group_by(subregion) %>% 
  summarise(geometry = st_union(geometry), 
            admin = admin) %>%
  st_as_sf() %>% 
  unique()

valid <- sf::st_make_valid(spdf_africa)
centroids <- st_centroid(valid) ## to get mid points of polygons 
centroids$Lat <- st_coordinates(centroids)[, 2]
centroids$Long <- st_coordinates(centroids)[, 1]

### Assigning 

### 

TP.reprojected <- projectRaster(TP_Africa_plot, pop2)
TP.Popden <- TP.reprojected * pop2
Africa_crop <- crop(TP.Popden, extent(TP_Africa_plot))
pop3 <- crop(pop2,extent(TP_Africa_plot))


custom_palette <- rev(c("#110F01", "#111111", "#222222", "#333333", "#444444", "#555555", "#666666", 
                     "#777777", "#888888", "#999999", "#aaaaaa", "#bbbbbb", "#cccccc","#F0F0F0"))

custom_palette <- rev(c("#c3271b", "#c53020", "#c83926", "#ca402b", "#cc4730", "#ce4e36", "#d0543b", "#d25b41",
                        "#d46146", "#d6674c", "#d76d51", "#d97357", "#db785c", "#dc7e62", "#de8467", "#df896d", 
                        "#e08f73", "#e19579", "#e29a7f", "#e3a084", "#e4a58a", "#e5ab90", "#e5b096", "#e6b69c",
                        "#e7ccb5", "#e7d1bb", "#e7d7c1", "grey90"))

my_palette <- rev(paletteer_c("grDevices::RdPu", 30))

travel_palette <- c("#34abe7", "#9498ba", "#ef4b39", "#be2323")
library(ggnewscale)
library(ggrepel)

### Assigining specific colour to the shapefile
spdf_world <- spdf_world %>%
  mutate(admin_color = case_when(
    admin %in% c("India", "Brazil", "Malaysia", "Bangladesh", "Peru", "Bolivia",
                 "Sri Lanka", "Cambodia", "Colombia", "Singapore", "Thailand", 
                 "Vietnam", "Nicaragua", "Belize") ~ as.character(admin),
    TRUE ~ "Other"
  ))

### Plotting Transmission Potential for 14 high incidence countries and African continent. 
world_raster_plot <- ggplot() +
  geom_sf(data=spdf_africa,lwd=0.6,fill = "#C0C0C0",show.legend = FALSE)+
  geom_raster(data = as.data.frame(pop3, xy = TRUE), aes(x = x, y = y, fill = SEDAC_POP_2000.01.01_gs_3600x1800)) +
  geom_sf(data=spdf_africa,lwd=0.6,fill = "transparent",show.legend = FALSE)+
  geom_sf(data=spdf_africa_subregion,lwd=0.6,aes(colour = subregion),fill = "transparent",show.legend = FALSE)+
  scale_colour_manual(values= c("Eastern Africa" = "#9dcced", "Middle Africa" ="#a8e6cf","Southern Africa"="#0077b6","Western Africa"="#e498ff","Northern Africa" = "#9667e0"), 
                      name= "Africa: Sub-Region")+
  scale_fill_gradientn(name = "Population Density", colours = my_palette , na.value = "transparent") +
  #geom_text(data = centroids, aes(label = adm0_a3_is, x = st_coordinates(centroids)[, 1], 
   #                                    y = st_coordinates(centroids)[, 2]),
    #             size = 2.2, fontface = "bold", family = "sans")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal",
        legend.position = c(0.80,0.15),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 10),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22))+
  xlab(" ") + ylab(" ") + coord_sf(ylim = c(-65, 45), xlim = c(-100,125)) +
  guides(color = guide_legend(override.aes = list(size = 8)))
world_raster_plot

ggsave(plot = world_raster_plot, "Final/Paper_Reproducibility/Africa_map_B.pdf")


## Spatial plots with Transmission Potential 
risk.imp.data
spatial.plot <- risk.imp.data %>% 
  #filter(Orig.Country %in% c("Thailand")) %>% 
  group_by(Dest.Country,Airport.Dest,Orig.Country) %>%  
  summarise(minRisk = min(normalized_R_import, na.rm=T),
            maxRisk = max(normalized_R_import, na.rm=T),
            sumRisk = sum(normalized_R_import, na.rm= T),
            meanRisk = mean(normalized_R_import, na.rm = T),
            Continent= Continent,
            TotTrav = sum(Reported.Est..Pax),
            Latitude.x=Latitude.x,
            Longitude.x=Longitude.x,
            Latitude.y=Latitude.y,
            Longitude.y=Longitude.y) %>% 
  distinct(Airport.Dest, .keep_all = TRUE)

spatial.plot.2 <- spatial.plot %>% 
  group_by(Dest.Country, Airport.Dest) %>% 
  summarise(sumRisk= sum(sumRisk),
            Latitude.y= Latitude.y, 
            Longitude.y = Longitude.y) %>% 
  distinct()

spatial.plot.3 <- spatial.plot %>% 
  group_by(Dest.Country,Orig.Country) %>% 
  summarise(sumRisk = sum(sumRisk))

spatial.plot.3$Dest.Country[spatial.plot.3$Dest.Country == "Congo"] <- "Republic of Congo"
spatial.plot.3$Dest.Country[spatial.plot.3$Dest.Country == "Tanzania"] <- "United Republic of Tanzania"


spatial.plot.4 <- spdf_africa %>% 
  left_join(spatial.plot.3, by= c("admin" = "Dest.Country"))


valid.4 <- sf::st_make_valid(spatial.plot.4)
centroids.4 <- st_centroid(valid.4) ## to get mid points of polygons 
spatial.plot.4$Lat <- st_coordinates(centroids.4)[, 2]
spatial.plot.4$Long <- st_coordinates(centroids.4)[, 1]
spatial.plot.4 <- st_drop_geometry(spatial.plot.4)

coloursSouthAmerica<-c("#9a031e","red2","hotpink3","#FA661C","#f8961e","#f9c74f")

coloursSouthAmerica<-c("red3","#F94144","#FA661C","#f8961e","#EF9669","#f9c74f")
coloursAsia<-c('#656d4a',"#90BE6D","#43aa8b","#245953","#577590","#91C8E4","#277da1","#1D24CA")


map1 <- ggplot() +
  geom_sf(data=spdf_world,lwd=0.6,aes(fill = admin_color,colour=admin_color),show.legend = FALSE)+
  scale_fill_manual(values= c("India"= "#656d4a","Malaysia"= "#90BE6D","Sri Lanka" = "#43aa8b","Singapore" = "#245953","Thailand" = "#577590", 
                              "Vietnam" = "#91C8E4","Bangladesh" = "#277da1","Peru"="#9a031e","Bolivia"="red3",
                              "Cambodia"= "#1D24CA", "Colombia" = "hotpink3", "Brazil"="#F3722C", "Nicaragua" = "#F9AC4D", "Belize" = "#f9c74f", "Other"= "#F0F0F0"))+
  scale_colour_manual(values= c("India"= "#656d4a","Malaysia"= "#90BE6D","Sri Lanka" = "#43aa8b","Singapore" = "#245953","Thailand" = "#577590", 
                                "Vietnam" = "#91C8E4","Bangladesh" = "#277da1","Peru"="#9a031e","Bolivia"="red3",
                                "Cambodia"= "#1D24CA", "Colombia" = "hotpink3", "Brazil"="#F3722C", "Nicaragua" = "#F9AC4D", "Belize" = "#f9c74f", "Other"= "#F0F0F0"))+
  new_scale_fill() +
  new_scale_colour() +
  geom_raster(data = as.data.frame(Africa_crop, xy = TRUE), aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(name = "Transmission Potential * \nPopulation Density", colours = custom_palette, na.value = "transparent") +
  geom_sf(data=spdf_africa,lwd=0.25,colour="#2b2d42",fill=NA,show.legend = FALSE)+
  geom_text_repel(data = centroids, aes(label = adm0_a3_is, x = st_coordinates(centroids)[, 1], 
                                  y = st_coordinates(centroids)[, 2]),
            size = 4, fontface = "bold", family = "sans", max.overlaps = 100, min.segment.length = 5, point.padding = 0.1)+
  #geom_text_repel(box.padding = 0.5, max.overlaps = inf) +
  scale_size(range = c(.1, 12), name="Risk of Importation") +
  new_scale_colour() +
  scale_alpha_continuous(guide = "none") +  # Remove alpha legend
  new_scale_colour() +
  new_scale_fill() +
  #geom_sf(data=spdf_africa_subregion,lwd=0.6,fill=NA,colour="#B6A39E",show.legend = FALSE)+ # BOUNDARY FOR THE AFRICAN REGIONS 
  new_scale_colour() +
  geom_point(data = spatial.plot %>% filter(meanRisk > 0.0000001) , aes(x = Longitude.y, y = Latitude.y, size = meanRisk,colour=Orig.Country),shape=21,alpha=0.85,stroke = 2) +
  scale_colour_manual(values=c("India"= "#656d4a","Malaysia"= "#90BE6D","Sri Lanka" = "#43aa8b","Singapore" = "#245953","Thailand" = "#577590", 
                               "Vietnam" = "#91C8E4","Bangladesh" = "#277da1","Peru"="#9a031e","Bolivia"="red3",
                               "Cambodia"= "#1D24CA", "Colombia" = "hotpink3", "Brazil"="#F3722C", "Nicaragua" = "#F9AC4D", "Belize" = "#f9c74f", "Other"= "#F0F0F0"),name= "Origin Country")+ # Define colors
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal",
        legend.position = c(0.80,0.15),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 10),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22))+
  xlab(" ") + ylab(" ") + coord_sf(ylim = c(-65, 45), xlim = c(-100,125)) +
  guides(color = guide_legend(override.aes = list(size = 8)))

map1


#scale_colour_manual(values= c("India"="#b7410e","Brazil"="#0000FF","Malaysia"= "#ed732e","Bangladesh" = "#ECA553","Peru"="#bbdefb","Bolivia"="#8a2be2",
 #                             "Sri Lanka" = "#C6A13B", "Cambodia"= "#000080", "Colombia" = "#98ff98", "Singapore" = "#aaaf32","Thailand" = "#658e0c", 
  #                            "Vietnam" = "#81555a", "Nicaragua" = "#80ced7", "Belize" = "#008000"),name= "Origin Country", guide = "none")


ggsave(map1, filename = "Final/Paper_Reproducibility/World_map_March2024_countrynames.pdf", bg = "transparent")

##############################################################################################################################
#############################################  FIGURE 2   ##############################################################
##############################################################################################################################

###### Barplot showing risk of introduction overlayed on transmission potential 
### We wwant to look at the synchronicity of TP and those risk for all African countries

### Load Risk 
#Risk_import_TP&Pop&TravelFlux  <-> File 
risk.imp.data.all <- fread("Final/Risk_import_TP&Pop&TravelFlux.csv") ## Transmission Potential
risk.imp.data <- risk.imp.data.all[,c(1,4,6,7,8,9,10,11,12,13,16,17,31,33,34,35)]
risk.imp.data$Date <- as.Date(risk.imp.data$Date)

min.max.risk.outside <- risk.imp.data %>% 
  group_by(Dest.Country,Date,Continent) %>%  
  summarise(minRisk = min(normalized_R_import, na.rm=T),
            maxRisk = max(normalized_R_import, na.rm=T),
            sumRisk = sum(normalized_R_import, na.rm= T),
            meanRisk = mean(normalized_R_import, na.rm = T),
            Continent= Continent) %>% 
  distinct()

##Load Transmission Potential
TP.Africa <- fread("Final/Extracted_Africa_TP_withLag_2019.csv") ## Country level
TP.Africa <- TP.Africa[,-c(3,16)]
TP.Africa.long <- pivot_longer(TP.Africa, cols = 3:14, names_to = "Date", values_to = "TP")
TP.Africa.long$Date <- as.Date(TP.Africa.long$Date)

## Joining the TP to the risk 
JJ <- min.max.risk.outside %>% 
  left_join(TP.Africa.long, by = c("Dest.Country" = "COUNTRY", "Date" = "Date")) %>% 
  unique()

JJ$sumRisk_Max <- JJ$sumRisk/max(JJ$sumRisk)
JJ$TP_Max <- JJ$TP/max(JJ$TP, na.rm=T)

JJ_sumcontinent <- JJ %>% 
  group_by(Date, Dest.Country) %>% 
  summarise(megasumrisk_max = sum(sumRisk_Max))
  
  
## Groupings for plots for supplementary

g2 <- c("Tunisia", "Ivory Coast","Ghana","Cameroon","Central African Republic","Equatorial Guinea","South Sudan",
          "Namibia","Zambia","Algeria","Sudan")
g3 <- c("Gabon","Democratic Republic of the Congo","Mauritania","Guinea","Madagascar","Malawi","Zimbabwe","Botswana")
g4 <- c("Togo","Sierra Leone","Burundi","Benin","Burkina Faso","Guinea Bissau","Djibouti","Eritrea","Congo","Liberia")
g5 <- c("Swaziland","Libya","Comoros")
g5.2<- c("Mali","Niger","Somalia","Gambia")
gfree <- c("Sao Tome and Principe", "Lesotho","Chad","Mozambique","Mali", "Niger", "Somalia", "Gambia")
gmain <- c("Angola", "Egypt", "Ethiopia", "Kenya", "Mauritius", "Morocco", "Nigeria", "Rwanda", "Senegal", "South Africa" , "Tanzania", "Uganda")


timelin2 <- JJ %>% 
  #filter(!Dest.Country %in% c("Reunion", "Seychelles") ) %>% 
  #filter(!Dest.Country %in% c( "Mauritius", "Angola","Egypt","Ethiopia", "Kenya",
   #           "Morocco", "Nigeria", "Rwanda", "South Africa", "Uganda","Senegal", "Tanzania", "Cape Verde")) %>% 
  filter(Dest.Country %in% c("Senegal") ) %>% 
  ggplot() +
  geom_col(aes(x=Date, y = TP_Max, fill= "Transmission Potential"),alpha=0.8) +
  scale_fill_manual(values = c("#ccdbfd"), name= " ") +  # Define color and legend title
  geom_line(aes(x=Date, y = sumRisk_Max*10,color = Continent),linewidth=1) +
  geom_line(data=JJ_sumcontinent %>% filter(Dest.Country %in% c("Senegal")),
            aes(x=Date, y = megasumrisk_max*10, colour="Total Risk"),linetype="dashed",linewidth=1) +
  scale_color_manual(values = c("#325070","#ff4a2d","black"), name= "Risk from") + 
  scale_y_continuous(sec.axis = sec_axis(~ ./0.1, name = "Risk of Importation")) +
  facet_wrap(~Dest.Country,scale="free_y",nrow=4)+
  theme_minimal()+
  #theme_ipsum( axis_title_size=20) +
 theme(axis.text.x = element_text(size = 17, angle = 25, hjust = 1,family = "sans"),
        axis.text.y = element_text(size = 17, hjust = 1,family = "sans"),
        axis.title.y = element_text(vjust=0.5),
        axis.title.y.right = element_text(vjust=1),
        axis.title = element_text(size=20,family = "sans"),
        strip.text = element_text(size= 19,family="sans",face = "bold", margin = margin(b = 20)),
        legend.position = "top",
        legend.background = element_rect( colour= "transparent",fill = "transparent"),
        legend.text =element_text(size=21,family = "sans"),
        legend.title = element_text(size=21)) +
  labs(title = " ", x = "Date", y = "Mean Index P")


timelin2

#ggsave(plot=timelin2,"Final/Paper_Reproducibility/barplot_Fig2_main_v2.pdf")
##############################################################################################################################
################################################## FIGURE 3 ##############################################################
##############################################################################################################################

### By Province
spdf_africa_prov <- st_read("shapefile_admine_level1_africa/959cf709-3b35-4278-b297-a7442cdc37d0/afr_g2014_2013_1.shp")
#plot(City)

### Load Risk 
#Risk_import_TP&Pop&TravelFlux
risk.imp.data.all <- fread("Final/Risk_import_TP&Pop&TravelFlux.csv") ## Transmission Potential
risk.imp.data <- risk.imp.data.all[,c(1,4,6,7,8,9,10,11,12,13,16,17,31,33,34,35)]
risk.imp.data$Date <- as.Date(risk.imp.data$Date)

min.max.risk.outside <- risk.imp.data %>% 
  group_by(Dest.Country,Date,Continent,Name_1) %>%  
  summarise(minRisk = min(normalized_R_import, na.rm=T),
            maxRisk = max(normalized_R_import, na.rm=T),
            sumRisk = sum(normalized_R_import, na.rm= T),
            meanRisk = mean(normalized_R_import, na.rm = T),
            Continent= Continent)

##Load Transmission Potential
#TP.Africa <- fread("Final/Extracted_Africa_TP_withLag_2019.csv") ## Country level
TP.Africa <- fread("Final/yearly_highincidence_withlag_AFRICA.csv") ## Admin 1 level
#TP.Africa <- TP.Africa[,-c(3,16)]
TP.Africa.long <- pivot_longer(TP.Africa, cols = 8:19, names_to = "Date", values_to = "TP")
TP.Africa.long$Date <- as.Date(TP.Africa.long$Date)

## Joining the TP to the risk 
JJ <- min.max.risk.outside %>% 
  left_join(TP.Africa.long, by = c("Dest.Country" = "ADM0_NAME", "Date" = "Date","Name_1"="ADM1_NAME")) %>% 
  unique()


#### Attributing those to countries instead of airports 
## JJ has the risk aggegrated by months 
JJ_Asia_annualmeanrisk <- JJ %>% 
  filter(Continent =="Asia") %>% 
  group_by(Continent,Dest.Country) %>% 
  summarise(m.annal.risk = sum(sumRisk),
            ToMatch= ToMatch)

JJ_SouthAmerica_annualmeanrisk <- JJ %>% 
  filter(Continent =="South America") %>% 
  group_by(Continent,Dest.Country) %>% 
  summarise(m.annal.risk = sum(sumRisk),
            ToMatch=ToMatch)

spdf_africa_riskfrAsia <- spdf_africa_prov %>% 
  left_join(JJ_Asia_annualmeanrisk, by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "ToMatch" ))
spdf_africa_riskfrAsia$Continent <- ifelse(is.na(spdf_africa_riskfrAsia$Continent), "Asia", spdf_africa_riskfrAsia$Continent)

spdf_africa_riskfrSouthAmerica <- spdf_africa_prov %>% 
  left_join(JJ_SouthAmerica_annualmeanrisk , by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "ToMatch" ))
spdf_africa_riskfrSouthAmerica$Continent <- ifelse(is.na(spdf_africa_riskfrSouthAmerica$Continent), "South America", spdf_africa_riskfrSouthAmerica$Continent)


combined.jj.spdf_province <- rbind(spdf_africa_riskfrAsia,spdf_africa_riskfrSouthAmerica)


##SCATTERPLOT with pie chart repsenting the proportion of risk from Asia and South America


### Addidng a pie chart to the map 
library(scatterpie)
valid  <- sf::st_make_valid(combined.jj.spdf_province)
center <- st_centroid(valid) ## to get mid points of polygons 
center$Lat <- st_coordinates(center)[, 2]
center$Long <- st_coordinates(center)[, 1]
center <- st_drop_geometry(center)
center$total <- length(na.omit(center$Continent))

# Calculate the proportion of risk from Asia and South America
center<-subset(unique(center),!is.na(m.annal.risk))


center <- center %>% group_by(ADM1_NAME) %>%
  mutate(total.annual.risk=sum(m.annal.risk)) %>%
  ungroup()

center <- center %>% group_by(Continent) %>%
  mutate(proportion.risk=m.annal.risk/total.annual.risk) %>%
  ungroup()

center$prop_asia<-0
center$prop_southamerica<-0

center$prop_asia <- ifelse(center$Continent == "Asia", 
                           center$proportion.risk,
                           0)

center$prop_southamerica <- ifelse(center$Continent == "South America", 
                                   center$proportion.risk,
                                   0)


# Replace NA with 0 in 'prop_asia' and 'prop_south_america'
center$prop_asia[is.na(center$prop_asia)] <- 0
center$prop_south_america[is.na(center$prop_south_america)] <- 0

# Convert 'prop_asia' and 'prop_south_america' to numeric
center$prop_asia <- as.numeric(center$prop_asia)
center$prop_south_america <- as.numeric(center$prop_south_america)

library(ggnewscale)

combined.jj.spdf_province$custom_colour <-  ifelse(is.na(combined.jj.spdf_province$m.annal.risk) | combined.jj.spdf_province$m.annal.risk < 0, "Yes", "NO")


plot1 <- ggplot() + 
  geom_sf(data=unique(combined.jj.spdf_province),aes(fill= custom_colour == "Yes"), colour="#ced4da",lwd=0.2,alpha=0.9) +
  scale_fill_manual(values= c("TRUE"= "transparent", "FALSE" = "#ced4da"),guide = "none") +
  new_scale_fill() +
  geom_sf(data=spdf_africa, fill = "NA",colour= "#ff4a2d", lwd=0.7,alpha=0.7)+
  #geom_text(data = centroids, aes(label = adm0_a3_is, x = st_coordinates(centroids)[, 1], 
  #                                y = st_coordinates(centroids)[, 2]), nudge_y = 0, size = 3.5,color="#2d00f7",fontface= "bold")+
  geom_scatterpie(data=center,aes(x=Long, y=Lat, r=0.55*(4--log10(as.numeric(total.annual.risk)))), cols=c("prop_asia", "prop_southamerica"), 
                  alpha=0.8,color = 'black',position_jitter()) + 
  scale_fill_manual(name="Origin Country",values= c("#325070","#ff4a2d"),
                    labels= c("Asia", "South America"),guide = guide_legend(override.aes = list(size = 5),ncol=3))+
  scale_radius(range = c(1, 10), name="Risk of Importation")+
  # Rename the size legend for geom_point
  theme_minimal()+
  #theme_ipsum(axis_title_size = 15, base_size = 15, subtitle_size = 18)+
  theme(plot.background = element_rect(color='white', fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=24,face = "bold"),
        strip.text = element_text(size = 30, face="bold"),
        legend.box= "vertical",
        legend.position = "bottom",
        legend.text = element_text(size = 23, face="bold"),
        legend.title = element_text(size = 23, face="bold"),
        legend.margin = margin(t = -1, r = 0, b = 0, l = 0),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlab(" ") + ylab(" ") +
  ggtitle("TP")
plot1

ggsave("Final/Paper_Reproducibility/plot3_TP.pdf",plot=plot1) 



#### Zooming on some plots 
centroids2 <- centroids %>% 
  filter(admin %in% c("Morocco", "Tunisia", "Algeria") )

# Zoomed-in plot
plot_zoomed <- ggplot()+ 
  geom_sf(data= unique(combined.jj.spdf_province) %>% filter(ADM0_NAME %in% c("Morocco", "Tunisia", "Algeria")),aes(fill= custom_colour == "Yes"), lwd=0.2) +
  scale_fill_manual(values= c("TRUE"= "transparent", "FALSE" = "grey" ),guide = "none") +
  new_scale_fill() +
  geom_sf(data=spdf_africa %>% filter(admin %in% c("Morocco", "Tunisia", "Algeria")), fill = "NA",colour= "#ff4a2d", lwd=0.7,alpha=0.7)+
  geom_label(data = centroids2, aes(label = adm0_a3_is, x = st_coordinates(centroids2)[, 1], 
                                    y = st_coordinates(centroids2)[, 2]), nudge_y = 2, nudge_x = -0.5,
             size = 10.5,color="#2d00f7",fontface= "bold")+
  geom_scatterpie(data=center %>% filter(ADM0_NAME %in% c("Morocco", "Tunisia", "Algeria")),aes(x=Long, y=Lat, r=0.3*(4--log10(as.numeric(total.annual.risk)))), cols=c("prop_asia", "prop_southamerica"), 
                  alpha=0.8,color = 'black',position_jitter()) + 
  scale_fill_manual(name="Origin Country",values= c("#325070","#ff4a2d"),
                    labels= c("Asia", "South America"),guide = guide_legend(override.aes = list(size = 5),ncol=3))+
  # scale_size(range = c(0.1, 1), name="Risk of Importation")+
  # Rename the size legend for geom_point
  theme_minimal()+
  #theme_ipsum(axis_title_size = 15, base_size = 15, subtitle_size = 18)+
  theme(plot.background = element_rect(color='white', fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=24,face = "bold"),
        strip.text = element_text(size = 30, face="bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlab(" ") + ylab(" ") 


ggsave("Final/Paper_Reproducibility/plot3_TP_zoom3.pdf",plot=plot_zoomed) ## Grouped according to the below
#c("Ghana", "Togo", "Benin", "Nigeria") 
#c("Mozambique", "Tanzania", "Zimbabwe", "Malawi")
#c( "Morocco", "Tunisia", "Algeria")
#c("Egypt", "Sudan", "Ethiopia","Kenya")

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### For TP >0.75 #### For TP > 1 #### #### For TP> 1.25 #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

## Adjust to the threshold you would like

## Now removing those months where TP <1 
JJ_Asia_annualmeanrisk.1.5 <- JJ %>% 
  filter(TP > 1) %>% 
  filter(Continent =="Asia") %>% 
  group_by(Continent,Dest.Country) %>% 
  summarise(m.annal.risk = sum(sumRisk), ToMatch=ToMatch)


JJ_SouthAmerica_annualmeanrisk.1.5 <- JJ %>% 
  filter(TP > 1) %>% 
  filter(Continent =="South America") %>% 
  group_by(Continent,Dest.Country) %>% 
  summarise(m.annal.risk = sum(sumRisk), ToMatch=ToMatch)

# Merge with the shapefile
#Rename countries if needed 

spdf_africa_riskfrAsia2 <- spdf_africa_prov %>% 
  left_join(JJ_Asia_annualmeanrisk.1.5, by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "ToMatch" ))
spdf_africa_riskfrAsia2$Continent <- ifelse(is.na(spdf_africa_riskfrAsia2$Continent), "Asia", spdf_africa_riskfrAsia2$Continent)


spdf_africa_riskfrSouthAmerica2 <- spdf_africa_prov %>% 
  left_join(JJ_SouthAmerica_annualmeanrisk.1.5, by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "ToMatch" ))
spdf_africa_riskfrSouthAmerica2$Continent <- ifelse(is.na(spdf_africa_riskfrSouthAmerica2$Continent), "South America", spdf_africa_riskfrSouthAmerica2$Continent)


combined.jj.spdf2_province <- rbind(spdf_africa_riskfrAsia2,spdf_africa_riskfrSouthAmerica2)
library(scatterpie)
valid  <- sf::st_make_valid(combined.jj.spdf2_province)
center2 <- st_centroid(valid) ## to get mid points of polygons 
center2$Lat <- st_coordinates(center2)[, 2]
center2$Long <- st_coordinates(center2)[, 1]
center2 <- st_drop_geometry(center2)
center2$total <- length(na.omit(center2$Continent))

center2<-subset(unique(center2),!is.na(m.annal.risk))

center2<-center2 %>% group_by(ADM1_NAME) %>%
  mutate(total.annual.risk=sum(m.annal.risk)) %>%
  ungroup()

center2<-center2 %>% group_by(Continent) %>%
  mutate(proportion.risk=m.annal.risk/total.annual.risk) %>%
  ungroup()

center2$prop_asia<-0
center2$prop_southamerica<-0

center2$prop_asia <- ifelse(center2$Continent == "Asia", 
                            center2$proportion.risk,
                            0)

center2$prop_southamerica <- ifelse(center2$Continent == "South America", 
                                    center2$proportion.risk,
                                    0)
combined.jj.spdf2_province$custom_colour <-  ifelse(is.na(combined.jj.spdf2_province$m.annal.risk) | combined.jj.spdf2_province$m.annal.risk < 0, "Yes", "NO")


plot2 <- ggplot() + 
  geom_sf(data=unique(combined.jj.spdf2_province),aes(fill= custom_colour == "Yes"),colour="#ced4da", lwd=0.2) +
  scale_fill_manual(values= c("TRUE"= "transparent", "FALSE" = "#ced4da" ),guide = "none") +
  new_scale_fill() +
  geom_sf(data=spdf_africa, fill = "NA",colour= "#ff4a2d", lwd=0.7,alpha=0.7)+
  geom_scatterpie(data=center2,aes(x=Long, y=Lat, r=0.55*(4--log10(as.numeric(total.annual.risk)))), cols=c("prop_asia", "prop_southamerica"), 
                  alpha=0.8,color = 'black',position_jitter()) + 
  #scale_size(range = c(1, 10), name="Risk of Importation")+
  scale_fill_manual(name="Origin Country",values= c("#325070","#ff4a2d"),
                    labels= c("Asia", "South America"),guide = guide_legend(override.aes = list(size = 5),ncol=3))+
  #geom_text(data = centroids, aes(label = adm0_a3_is, x = st_coordinates(centroids)[, 1], 
  #                                    y = st_coordinates(centroids)[, 2]), nudge_y = 0.1, size = 2.5,color="#2d00f7",fontface = 'bold')+
  #scale_size_area(max_size = 12, name = "Risk of Importation")+
  # Rename the size legend for geom_point
  theme_minimal()+
  #theme_ipsum(axis_title_size = 15, base_size = 15, subtitle_size = 18)+
  theme(plot.background = element_rect(color='white', fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=24,face = "bold"),
        strip.text = element_text(size = 30, face="bold"),
        legend.box= "vertical",
        legend.position = "bottom",
        legend.text = element_text(size = 23, face="bold"),
        legend.title = element_text(size = 23, face="bold"),
        legend.margin = margin(t = -1, r = 0, b = 0, l = 0),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlab(" ") + ylab(" ") +
  ggtitle("TP > 1")

ggsave("TP1plot48.pdf",plot=plot2)


centroids2 <- centroids %>% 
  filter(admin %in%c("Ghana", "Togo", "Benin", "Nigeria"))


# Zoomed-in plot
plot2_zoomed <- ggplot() + 
  geom_sf(data=unique(combined.jj.spdf2_province) %>% filter(ADM0_NAME %in% c("Ghana", "Togo", "Benin", "Nigeria")),aes(fill= custom_colour == "Yes"), lwd=0.2) +
  scale_fill_manual(values= c("TRUE"= "transparent", "FALSE" = "grey" ),guide = "none") +
  new_scale_fill() +
  geom_sf(data=africa %>% filter(admin %in% c("Ghana", "Togo", "Benin", "Nigeria")), fill = "NA",colour= "#ff4a2d", lwd=0.7,alpha=0.7)+
  geom_label(data = centroids2, aes(label = adm0_a3_is, x = st_coordinates(centroids2)[, 1], 
                                    y = st_coordinates(centroids2)[, 2]),nudge_y = 2, nudge_x = -0.5, size = 12.5,color="#2d00f7",fontface = 'bold')+
  geom_scatterpie(data=center2 %>% filter(ADM0_NAME %in% c("Ghana", "Togo", "Benin", "Nigeria")),aes(x=Long, y=Lat, r=0.3*(4--log10(as.numeric(total.annual.risk)))), cols=c("prop_asia", "prop_southamerica"), 
                  alpha=0.8,color = 'black',position_jitter()) + 
  scale_fill_manual(name="Origin Country",values= c("#325070","#ff4a2d"),
                    labels= c("Asia", "South America"),guide = guide_legend(override.aes = list(size = 5),ncol=3))+
  # scale_size(range = c(0.1, 1), name="Risk of Importation")+
  # Rename the size legend for geom_point
  theme_minimal()+
  #theme_ipsum(axis_title_size = 15, base_size = 15, subtitle_size = 18)+
  theme(plot.background = element_rect(color='white', fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=24,face = "bold"),
        strip.text = element_text(size = 30, face="bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlab(" ") + ylab(" ") 

ggsave("plot48_TP1_zoom1.pdf",plot=plot2_zoomed) #c("Ghana", "Togo", "Benin", "Nigeria") #c("Mozambique", "Tanzania", "Zimbabwe", "Malawi") #c( "Morocco", "Tunisia", "Algeria") #c("Egypt", "Sudan", "Ethiopia","Kenya")

##############################################################################################################################
################################################## FIGURE 3 Panel B. #########################################################
##############################################################################################################################
##### Boxplots 
## Adding Regions before

`North Africa` <-  c("Algeria","Egypt","Libya","Mauritania","Morocco","Tunisia","Sahrawi ADR")

`West Africa` <-   c("Benin", "Burkina Faso","Cabo Verde","Ivory Coast","Cote D'Ivoire","Gambia","Ghana","Guinea","Guinea Bissau",
                     "Liberia","Mali","Niger","Nigeria","Senegal","Sierra Leone","Togo")

`Central Africa` <-  c("Burundi","Cameroon","Central African Republic","Chad","Congo","Democratic Republic of the Congo","Republic of Congo",
                       "Equatorial Guinea","Sao Tome and Principe","Gabon")

`East Africa` <- c("Comoros","Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Rwanda","Seychelles","Somalia","South Sudan",
                   "Sudan","United Republic of Tanzania","Tanzania","Uganda")

`Southern Africa` <- c("Angola","Botswana","Eswatini","Lesotho","Malawi","Mauritius","Mozambique","Namibia","South Africa","Zambia","Zimbabwe","Swaziland")


country_region_df <- data.frame(Region = rep(c("North Africa", "West Africa", "Central Africa", "East Africa", "Southern Africa"), 
                                             c(length(`North Africa`), length(`West Africa`), length(`Central Africa`), length(`East Africa`), length(`Southern Africa`))),
                                Country = c(`North Africa`, `West Africa`, `Central Africa`, `East Africa`,`Southern Africa`))

JJ <- JJ %>% 
  left_join(country_region_df, by = c("Dest.Country" = "Country"))


facet_colors <- c("#f79256", "#7dcfb6", "#fbd1a2", "#08a045", "#00b2ca")
library(ggtext)
library(ggplot2)
library(extrafont)
library(scales)
### Bocplot of risk for each destination country
JJ3 <- JJ %>%  
  filter(!Dest.Country %in% c("Reunion", "Seychelles","Cape Verde")) %>% 
  filter( TP >1 )

JJ3$Dest.Country <- with(JJ3, reorder(Dest.Country, -log(sumRisk),median,na.rm=T))

p3 <- JJ3 %>% 
  ggplot()+
  geom_boxplot(aes(x=Dest.Country, y=log(normalized), fill= Region),lwd=0.5) +
  scale_fill_manual(values = c("#cb8587","#8f2b08","#650144","#d6d4ad","#645e33"), name= "Risk to") + 
  scale_y_continuous(labels = c("2.06e-09","3.05e-07","4.5e-05","0.006"), breaks = c(-20,-15,-10,-5)) + # Assign colors based on the continent 
  #scale_y_continuous(labels = c("1.389e-11","2.06e-09","3.05e-07","4.5e-05","0.006","1"), breaks = c(-25,-20,-15,-10,-5,0)) + # Assign colors based on the continent   theme_minimal()+
  theme_minimal()+
  theme(legend.position = "bottom",  # Place legend at the bottom
        legend.box = "horizontal",
        legend.text = element_text(family="serif",size = 20, face="bold"),
        legend.title = element_text(family="serif",size = 20, face="bold"),
        axis.text.x = element_text(size=21,angle = 47, hjust = 1),
        axis.text.y = element_text(size=22),
        axis.title = element_text(size=26),
        plot.title = element_text(family="serif",size=35,face = "bold"),
        plot.subtitle = element_text(size= 22))+
  ylab("Risk of Importation") + xlab("Destination Country") +
  labs(title="Risk of importation 2019 into Africa", subtitle= "Filtered out low risk district (TP < 1)") 
p3

ggsave("Final/Paper_Reproducibility/Barplot_risk_tp_greaterthan1.pdf", units = "in", plot=p3)


##############################################################################################################################
################################################## Plot for Total Risk #####################################################
##############################################################################################################################


## Tiles of risk 
risk_by_country <-  risk.imp.data %>% 
  group_by(Dest.Country,Orig.Country) %>%  
  summarise(minRisk = min(normalized_R_import, na.rm=T),
            maxRisk = max(normalized_R_import, na.rm=T),
            sumRisk = sum(normalized_R_import, na.rm= T),
            meanRisk = mean(normalized_R_import, na.rm = T)) %>% 
  distinct()
risk_by_country <- risk_by_country %>%
  mutate(Risk_Group = cut(sumRisk_Max, breaks = 5))

risk_by_country$sumRisk_Max <- risk_by_country$sumRisk/max(risk_by_country$sumRisk)

risk_by_country %>% 
  ggplot(aes(x=Orig.Country,y=Dest.Country,fill=sumRisk_Max))+
  geom_tile(color= "white", lwd=1.5, linetype=1)+
  scale_fill_gradient(trans = "reverse")+
  coord_fixed()+
  theme_tufte()+
  theme(axis.text.x = element_text(angle=90, vjust=0.6))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 20))


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### Transmission potential in Africa 2019 #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
TP_Africa_plot
plot(TP_Africa_plot)

valid <- sf::st_make_valid(spdf_africa)
centroids <- st_centroid(valid) ## to get mid points of polygons 
centroids$Lat <- st_coordinates(centroids)[, 2]
centroids$Long <- st_coordinates(centroids)[, 1]

centroids2 <- centroids %>% 
  filter(admin %in% c("Gambia","Senegal", "Guinea", "Sierra Leone", "Liberia", "Ghana", 
                                                    "Equatorial Guinea", "Eritrea", "Djibouti", 
                                                    "Mauritius", "Seychelles","Togo", "Benin","Ghana","Tunisia","Lesotho",
                      "Swaziland", "Rwanda","Guinea Bissau","Equatorial Guinea","Congo"))

centroids3 <- centroids %>% 
  filter(!admin %in% c("Gambia","Senegal", "Guinea", "Sierra Leone", "Liberia", "Ghana", 
                        "Equatorial Guinea", "Eritrea", "Djibouti", 
                        "Mauritius", "Seychelles","Togo", "Benin","Ghana","Tunisia","Lesotho",
                       "Swaziland", "Rwanda","Guinea Bissau","Equatorial Guinea"," Congo"))


custom_palette2 <- rev(c("#c3271b", "#c53020", "#c83926", "#ca402b", "#cc4730", "#ce4e36", "#d0543b", "#d25b41",
                        "#d46146", "#d6674c", "#d76d51", "#d97357", "#db785c", "#dc7e62", "#de8467", "#df896d", 
                        "#e08f73", "#e19579", "#e29a7f", "#e3a084", "#e4a58a", "#e5ab90", "#e5b096", "#e6b69c",
                        "#e7ccb5", "#e7d1bb", "#e7d7c1", "grey90"))

Supp2 <- ggplot() +
  geom_raster(data = as.data.frame(TP_Africa_plot, xy = TRUE), aes(x = x, y = y, fill = Africa_Transmission_Pot_African_Continent)) +
  geom_sf(data=spdf_africa,lwd=0.6,fill=NA, colour= "black",show.legend = FALSE)+  theme_minimal()+
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette2, na.value = "transparent") +
  geom_text(data = centroids, aes(label = adm0_a3_is, x = st_coordinates(centroids)[, 1], 
                                        y = st_coordinates(centroids)[, 2]),
                  nudge_y = 0,size = 4, fontface = "bold", family = "sans")+
  #geom_text_repel(data = centroids2, aes(label = adm0_a3_is, x = st_coordinates(centroids2)[, 1], 
   #                                     y = st_coordinates(centroids2)[, 2]),
  #                nudge_y = 0,size = 4, fontface = "bold", family = "sans", box.padding = 1.5, hjust=1)+
  geom_sf(data=spdf_africa_subregion,lwd=1,fill=NA,aes(colour=subregion),show.legend = TRUE)+# BOUNDARY FOR THE AFRICAN REGIONS 
  scale_colour_manual(values= c("Eastern Africa" = "#9dcced", "Middle Africa" ="#a8e6cf","Southern Africa"="#0077b6","Western Africa"="#e498ff","Northern Africa" = "#9667e0"), 
                      name= "Africa: Sub-Region")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "Vertical",
        legend.position = c(0.15,0.30),
        legend.margin = margin(t = 0, r = 10, b = 10, l = 10),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22))+
  xlab(" ") + ylab(" ") 
Supp2

ggsave("Final/Paper_Reproducibility/supp2_TP_Africanregion.pdf", units = "in", plot=Supp2)


################# travel Volume plot or Risk plot #########################################################################
## Grouping into Regions of Origin

`South Asia` <-  c("Bangladesh","India","Sri Lanka")

`South East Asia` <-   c("Cambodia", "Malaysia", "Singapore", "Thailand", "Vietnam")

`South America` <-  c("Belize","Nicaragua","Peru", "Colombia", "Cambodia", "Bolivia", "Brazil") 

origin_region_df <- data.frame(Region = rep(c("South Asia", "South East Asia", "South America"), 
                                            c(length(`South Asia`), length(`South East Asia`), length(`South America`))),
                               Country = c(`South Asia`, `South East Asia`, `South America`))


risk.imp.data.1 <- risk.imp.data.1 %>% 
  left_join(origin_region_df, by = c("Orig.Country" = "Country"), relationship = "many-to-many")


`North Africa` <-  c("Algeria","Egypt","Libya","Mauritania","Morocco","Tunisia")

`West Africa` <-   c("Benin", "Burkina Faso","Cabo Verde","Ivory Coast","Cote D'Ivoire","Gambia","Ghana","Guinea","Guinea Bissau",
                     "Liberia","Mali","Niger","Nigeria","Senegal","Sierra Leone","Togo")

`Central Africa` <-  c("Angola","Cameroon","Central African Republic","Chad","Congo","Democratic Republic of the Congo","Republic of Congo",
                       "Equatorial Guinea","Sao Tome and Principe","Gabon")

`East Africa` <- c("Comoros","Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Rwanda","Seychelles","Somalia","South Sudan",
                   "Sudan","United Republic of Tanzania","Tanzania","Uganda","Malawi")

`Southern Africa` <- c("Botswana","Lesotho","Namibia","South Africa","Swaziland")


country_region_df <- data.frame(Region = rep(c("North Africa", "West Africa", "Central Africa", "East Africa", "Southern Africa"), 
                                             c(length(`North Africa`), length(`West Africa`), length(`Central Africa`), length(`East Africa`), length(`Southern Africa`))),
                                Country = c(`North Africa`, `West Africa`, `Central Africa`, `East Africa`,`Southern Africa`))




risk.imp.data.2 <- risk.imp.data.1 %>% 
  left_join(country_region_df, by = c("Dest.Country" = "Country"), relationship = "many-to-many")

risk.imp.data.2$Region.y <- as.factor(risk.imp.data.2$Region.y)
risk.imp.data.2$category <- reorder(risk.imp.data.2$Dest.Country, risk.imp.data.2$Region.y)

#### Travel plot - Or Risk - Plot showing proportions - Adapt accordingly 
travel.plot <- risk.imp.data.2 %>% 
  #filter(Dest.Country != "South Africa") %>% 
  ggplot(aes(x = Dest.Country,
             y = Mean_Travel_percountry,
             fill = as.factor(Region.x))) +
  geom_col(position = "fill",width=0.75)+
  #geom_bar(stat="identity",position=position_dodge())+
  #coord_flip()+
  #geom_text(aes(label=totRegionsTravel), vjust=1.6, color="white",
  # position = position_dodge(0.9), size=3.5)+
  #scale_fill_manual(values=country_colors)+
  scale_fill_manual(values=c("#e26d5c","#61a5c2","#a4c3b2"))+
 # scale_fill_manual(values=c("#aacc00","#5f0f40","#577590"))+
  #scale_y_continuous(trans = "log")+
  scale_x_discrete(limits= c("Algeria","Egypt","Libya","Morocco","Tunisia","Sudan"," ",
                             "Burkina Faso","Cape Verde","Ivory Coast","Gambia","Ghana","Guinea","Guinea Bissau",
                             "Liberia","Mali","Niger","Nigeria","Senegal","Sierra Leone","Togo", "Benin"," ",
                             "Cameroon","Central African Republic","Chad","Congo","Democratic Republic of the Congo",
                             "Equatorial Guinea","Sao Tome and Principe","Gabon","Angola", " ",
                             "Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Burundi","Rwanda","Seychelles","Somalia","South Sudan",
                             "Tanzania","Uganda","Mauritius","Comoros","Zambia","Zimbabwe", "Mozambique","Malawi"," ",
                             "Botswana","Lesotho","Namibia","South Africa","Swaziland"))+
  labs(title = "Proportion of Travels into Africa", 
       y = "Passenger Volume", x = "Destination", fill = "Originating Country")+
  #theme_ipsum(axis_title_size = 22, subtitle_size = 19,family = "serif")+
  theme_minimal()+
  theme(text = element_text(size = 22),
        plot.background = element_rect(color= "white", fill="white"),
        legend.background = element_rect(color= "white", fill="white"),
        axis.text.y = element_text(angle = 0, hjust = 1),
        axis.text.x = element_text(angle = 50, hjust = 1))  # Adjust the spacing here (increase the value as needed)+


travel.plot
ggsave("Final/Paper_Reproducibility/travel_barplot_supplementary.pdf",plot=travel.plot)

########################################################################################################################

################# travel Volume plot for each month #########################################################################


risk.imp.data.total.travel <- risk.imp.data %>% 
  group_by(Date,Dest.Country) %>% 
  summarise(Tot_travels_month = sum(Total_Travels))



#### Travel plot - Or Risk - Plot showing proportions - Adapt accordingly 
travel.plot <- risk.imp.data.2 %>% 
  #filter(Dest.Country != "South Africa") %>% 
 # filter(Date %in% c("2019-02-01","2019-07-01","2019-09-01","2019-12-01")) %>% 
  ggplot(aes(x = Dest.Country,
             y = Mean_Travel_percountry,
             fill = as.factor(Orig.Country))) +
  geom_col(position = "fill",width=0.75)+
  #geom_bar(stat="identity",position=position_dodge())+
  #coord_flip()+
  #geom_text(aes(label=totRegionsTravel), vjust=1.6, color="white",
  # position = position_dodge(0.9), size=3.5)+
  #scale_fill_manual(values=country_colors)+
  scale_fill_manual(values= c("India"= "#7A5C00","Malaysia"= "#933710","Sri Lanka" = "#8C1A18","Singapore" = "#820D47","Thailand" = "#3E2654", 
                                "Vietnam" = "#2B3A60","Bangladesh" = "#1B5F5A","Peru"="#FF925C","Bolivia"="#FF7A70",
                                "Cambodia"= "#6A7A00", "Colombia" = "#FEAE86", "Brazil"="#F5D000", "Nicaragua" = "#73E2A7", "Belize" = "plum"),name= "Origin Country")+
  #scale_y_continuous(trans = "log")+
  scale_x_discrete(limits= c("Algeria","Egypt","Libya","Morocco","Tunisia","Sudan"," ",
                             "Burkina Faso","Cape Verde","Ivory Coast","Gambia","Ghana","Guinea","Guinea Bissau",
                             "Liberia","Mali","Niger","Nigeria","Senegal","Sierra Leone","Togo", "Benin"," ",
                             "Cameroon","Central African Republic","Chad","Congo","Democratic Republic of the Congo",
                             "Equatorial Guinea","Sao Tome and Principe","Gabon","Angola", " ",
                             "Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Burundi","Rwanda","Seychelles","Somalia","South Sudan",
                             "Tanzania","Uganda","Mauritius","Comoros","Zambia","Zimbabwe", "Mozambique","Malawi"," ",
                             "Botswana","Lesotho","Namibia","South Africa","Swaziland"))+
  labs(title = "Proportion of Travels into Africa", 
       y = "Passenger Volume", x = "Destination", fill = "Originating Country")+
  #theme_ipsum(axis_title_size = 22, subtitle_size = 19,family = "serif")+
  #facet_wrap(~Date,nrow = 4)+
  theme_ipsum()+
  theme(text = element_text(size = 22),
        plot.background = element_rect(color= "white", fill="white"),
        legend.background = element_rect(color= "white", fill="white"),
        axis.text.y = element_text(angle = 0, hjust = 1),
        axis.text.x = element_text(angle = 50, hjust = 1)) + # Adjust the spacing here (increase the value as needed)+
  coord_flip()


travel.plot
#################################################################################################################################################
#################################################################################################################################################
## Risk of EXPORTING DENGUE from India / Bangladesh - Countries hit by Monsoon


Exporting_risk <- risk.imp.data %>% 
  group_by(Orig.Country, Date) %>% 
  summarise(RiskExp = sum(normalized_R_import, na.rm=T))

Exporting_risk$RiskExp_Max <- Exporting_risk$RiskExp/max(Exporting_risk$RiskExp, na.rm = T)

par(mfrow=c(1,3))

India_Export <- ggplot(Exporting_risk %>% filter(Orig.Country=="India"), aes(x=Date, y=RiskExp_Max, colour="#7A5C00")) +
  geom_line(colour="#7A5C00") + 
  geom_point(colour="#7A5C00") +
  annotate("rect",fill="#7E9150",alpha=0.5,xmin=as.Date("2019-06-01"), xmax= as.Date("2019-09-01"),
           ymin=-Inf, ymax=Inf)+
  xlab("") +
  theme_ipsum() +
  #facet_wrap(~Orig.Country)+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11"))) +
  ylim(0,1)+
  labs(title = "Risk from India")+  guides(fill = guide_legend(title = "Monsoon Season"))


India_Export


SriLanka_Export <- ggplot(Exporting_risk %>% filter(Orig.Country=="Sri Lanka"), aes(x=Date, y=RiskExp_Max)) +
  geom_line(colour="#8C1A18") + 
  geom_point(colour="#8C1A18") +
  annotate("rect",fill="#7E9150",alpha=0.5,xmin=as.Date("2019-10-01"), xmax= as.Date("2019-12-01"),
           ymin=-Inf, ymax=Inf)+
  annotate("rect",fill="#7E9150",alpha=0.5,xmin=as.Date("2019-05-01"), xmax= as.Date("2019-08-01"),
           ymin=-Inf, ymax=Inf)+
  xlab("") +
  theme_ipsum() +
  #facet_wrap(~Orig.Country)+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11"))) +
  ylim(0,1)+
  labs(title = "Risk from Sri Lanka", fill= "Monsoon Season")


SriLanka_Export



Bangladesh_Export <- ggplot(Exporting_risk %>% filter(Orig.Country=="Bangladesh"), aes(x=Date, y=RiskExp_Max)) +
  geom_line(colour="#1B5F5A") + 
  geom_point(colour="#1B5F5A") +
  annotate("rect",fill="#7E9150",alpha=0.5,xmin=as.Date("2019-06-01"), xmax= as.Date("2019-10-01"),
           ymin=-Inf, ymax=Inf)+
  xlab("") +
  theme_ipsum() +
  #facet_wrap(~Orig.Country)+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11"))) +
  ylim(0,1)+
  labs(title = "Risk from Bangladesh", fill= "Monsoon Season")

Bangladesh_Export


######### EXPORT ##### RISK ##########
risk_exp <- risk.imp.data %>% 
  group_by(Orig.Country,Date) %>%  
  summarise(minRisk = min(normalized_R_import, na.rm=T),
            maxRisk = max(normalized_R_import, na.rm=T),
            sumRisk = sum(normalized_R_import, na.rm= T),
            meanRisk = mean(normalized_R_import, na.rm = T),
            Continent= Continent) %>% 
  distinct()

##Load Transmission Potential
TP.Orig.Country <- fread("Final/Paper_Reproducibility/TP_OrigCountry.csv") ## Country level
TP.Orig.Country <- TP.Orig.Country[,-c(2:4)]
TP.Orig.Country.long <- pivot_longer(TP.Orig.Country, cols = 2:13, names_to = "Date", values_to = "TP")
TP.Orig.Country.long$Date <- as.Date(TP.Orig.Country.long$Date)


TP.Orig.Country.long <- TP.Orig.Country.long %>% 
  group_by(COUNTRY, Date) %>% 
  summarise(TP= mean(TP, na.rm=T))


## Joining the TP to the risk 
tt <- risk_exp %>% 
  left_join(TP.Orig.Country.long, by = c("Orig.Country" = "COUNTRY", "Date" = "Date")) %>% 
  unique()

tt$sumRisk_Max <- tt$sumRisk/max(tt$sumRisk)
tt$TP_Max <- tt$TP/max(tt$TP, na.rm=T)

Scaling_factor <- diff(range(tt$TP))/ diff(range(tt$sumRisk_Max))

timelin33 <- tt %>% 
  ggplot() +
  geom_col(aes(x=Date, y = TP, fill= "Transmission Potential"),alpha=0.8) +
  scale_fill_manual(values = c("#ccdbfd"), name= " ") +  # Define color and legend title
  geom_line(aes(x=Date, y = sumRisk_Max*Scaling_factor),linewidth=1,colour="#4E0110") +
  scale_y_continuous(sec.axis = sec_axis(~ ./Scaling_factor, name = "Risk of Importation")) +
  facet_wrap(~Orig.Country,scale="free_y",nrow=4)+
  theme_minimal()+
  #theme_ipsum( axis_title_size=20) +
  theme(axis.text.x = element_text(size = 17, angle = 25, hjust = 1,family = "sans"),
        axis.text.y = element_text(size = 17, hjust = 1,family = "sans"),
        axis.title.y = element_text(vjust=0.5),
        axis.title.y.right = element_text(vjust=1),
        axis.title = element_text(size=20,family = "sans"),
        strip.text = element_text(size= 19,family="sans",face = "bold", margin = margin(b = 20)),
        legend.position = "top",
        legend.background = element_rect( colour= "transparent",fill = "transparent"),
        legend.text =element_text(size=21,family = "sans"),
        legend.title = element_text(size=21)) +
  labs(title = " ", x = "Date", y = "Mean Index P")


timelin33
ggsave(plot= timelin33, "Final/Paper_Reproducibility/Risk_Export.pdf")

#################################################################################################
## Risk of Export across Airports in the countries of high incidence
risk_exp2 <- risk.imp.data %>% 
  group_by(State_Orig_airport, Orig.Country) %>%  
  summarise(meanRisk = mean(normalized_R_import, na.rm = T)) %>% 
  distinct()

ToMatch <- unique(risk.imp.data[,c(1,8:10)])


risk_exp2_match <- risk_exp2 %>% 
  left_join(ToMatch, by = c("State_Orig_airport" = "State_Orig_airport")) %>% 
  slice(1)

## World Admin level 1 shapefile
Prov_Shp <- st_read("gadm_410.gdb")

## Filter for countries of Origin 
Prov_shp_14 <- Prov_Shp %>% 
  filter(COUNTRY %in% c("India", "Brazil", "Malaysia", "Bangladesh", "Peru", "Bolivia",
                        "Sri Lanka", "Cambodia", "Colombia", "Singapore", "Thailand", 
                        "Vietnam", "Nicaragua", "Belize"))



map_exp <- ggplot() +
  geom_sf(data=spdf_world,lwd=0.6,fill = "#C0C0C0",show.legend = FALSE)+
  geom_sf(data = Prov_shp_14, lwd=0.65,fill="transparent", show.legend = F) +
  geom_raster(data = as.data.frame(result, xy = TRUE), aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(name = "Transmission Potential", colours = custom_palette, na.value = "transparent") +
  geom_sf(data=spdf_world,lwd=0.7,fill = "NA",show.legend = FALSE)+
  geom_text(data = centroids, aes(label = adm0_a3_is, x = st_coordinates(centroids)[, 1], 
                                  y = st_coordinates(centroids)[, 2]),
            size = 4, fontface = "bold", family = "sans")+
  geom_point(data = risk_exp2_match %>% filter(meanRisk > 0.0000001) , aes(x = Longitude.x, y = Latitude.x, size = meanRisk),shape=21,alpha=0.85,stroke = 2) +
  scale_colour_manual(values=c("India"= "#656d4a","Malaysia"= "#90BE6D","Sri Lanka" = "#43aa8b","Singapore" = "#245953","Thailand" = "#577590", 
                               "Vietnam" = "#91C8E4","Bangladesh" = "#277da1","Peru"="#9a031e","Bolivia"="red3",
                               "Cambodia"= "#1D24CA", "Colombia" = "hotpink3", "Brazil"="#F3722C", "Nicaragua" = "#F9AC4D", "Belize" = "#f9c74f", "Other"= "#F0F0F0"),name= "Origin Country")+ # Define colors
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal",
        legend.position = c(0.80,0.15),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 10),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22))+
  xlab(" ") + ylab(" ") + coord_sf(ylim = c(-65, 45), xlim = c(-100,125)) +
  guides(color = guide_legend(override.aes = list(size = 8)))

map_exp


#### 
spdf_world <- ne_countries(scale = "medium", returnclass = "sf")
TP.reprojected <- projectRaster(TP_Africa_plot, pop2)
TP.Popden <- TP.reprojected * pop2
Africa_crop <- crop(TP.Popden, extent(TP_Africa_plot))
## Removing antartica
need<- spdf_world %>% 
  filter(admin == "Nigeria")

Need2 <- mask(Africa_crop, need)
Need3 <- crop(Need2,need)

ggplot() +
  geom_sf(data=spdf_world %>% filter(admin =="Nigeria"),lwd=0.6,fill= "transparent",show.legend = FALSE)+
  new_scale_fill() +
  new_scale_colour() +
  geom_raster(data = as.data.frame(Need3, xy = TRUE), aes(x = x, y = y, fill = layer),alpha=0.86,show.legend = F) +
  geom_sf(data=spdf_world %>% filter(admin =="Nigeria"),lwd=4,fill= "transparent",show.legend = FALSE)+
  scale_fill_gradientn(name = "Transmission Potential * \nPopulation Density", colours = custom_palette, na.value = "transparent") +
  scale_size(range = c(.1, 50), name="Risk of Importation") +
  new_scale_colour() +
  scale_alpha_continuous(guide = "none") +  # Remove alpha legend
  geom_point(data = spatial.plot %>% filter(meanRisk > 0.0000001) %>% filter(Dest.Country == "Nigeria") , aes(x = Longitude.y, y = Latitude.y, size = meanRisk,colour=Orig.Country),shape=21,alpha=0.88,stroke = 3.5,
             show.legend = F) +
  scale_colour_manual(values=c("India"= "#656d4a","Malaysia"= "#90BE6D","Sri Lanka" = "#43aa8b","Singapore" = "#245953","Thailand" = "#577590", 
                               "Vietnam" = "#91C8E4","Bangladesh" = "#277da1","Peru"="#9a031e","Bolivia"="red3",
                               "Cambodia"= "#1D24CA", "Colombia" = "hotpink3", "Brazil"="#F3722C", "Nicaragua" = "#F9AC4D", "Belize" = "#f9c74f", "Other"= "#F0F0F0"),name= "Origin Country")+ # Define colors
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal",
        legend.position = c(0.80,0.15),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 10),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22))+
  xlab(" ") + ylab(" ") + 
  #coord_sf(ylim = c(-65, 45), xlim = c(-100,125)) +
  guides(color = guide_legend(override.aes = list(size = 8)))


##############################################################################################################################
##################################################### AFRICA ###########################################################
##############################################################################################################################

##############################################################################################################################
#####################################################. FIGURE 5  ###########################################################
##############################################################################################################################

Africa.risk <- fread("Review/Risk_import_within_Africa.csv")

North_Africa <-  c("Algeria","Egypt","Libya","Mauritania","Morocco","Tunisia","Sahrawi ADR","Sudan")

West_Africa <-   c("Benin", "Burkina Faso","Cabo Verde","Ivory Coast","Cote D'Ivoire","Gambia","Ghana","Guinea","Guinea-Bissau",
                   "Liberia","Mali","Niger","Nigeria","Senegal","Sierra Leone","Togo")

Central_Africa <-  c("Angola","Cameroon","Central African Republic","Chad","Democratic Republic of the Congo","Republic of the Congo",
                     "Equatorial Guinea","Sao Tome and Principe","Gabon","Congo")

East_Africa <- c("Comoros","Burundi","Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Malawi","Mozambique","Rwanda","Somalia","South Sudan",
                 "United Republic of Tanzania","Tanzania","Uganda","Zambia","Zimbabwe")

Southern_Africa <- c("Mauritius","Botswana","Eswatini","Lesotho","Namibia","South Africa","Swaziland","Seychelles")

country_region_df <- data.frame(Region = rep(c("Northern Africa", "West Africa", "Central Africa", "East Africa", "Southern Africa"), 
                                             c(length(North_Africa), length(West_Africa), length(Central_Africa), length(East_Africa), length(Southern_Africa))),
                                Country = c(North_Africa, West_Africa, Central_Africa, East_Africa, Southern_Africa))


Africa.risk <- Africa.risk %>% 
  left_join(country_region_df, by = c("Orig.Country" = "Country"))

Africa.risk.mean <- Africa.risk %>% 
  group_by(Dest,Dest.Country, Region, Date) %>% 
  summarise(meanRisk = sum(normalized_R_import, na.rm=T),
            Longitude.y = Longitude.y,
            Latitude.y = Latitude.y) %>% 
  unique()
Africa.risk.mean <-Africa.risk.mean %>% 
  group_by(Dest,Dest.Country, Region) %>% 
  summarise(meanRisk = mean(meanRisk, na.rm=T),
            Longitude.y = Longitude.y,
            Latitude.y = Latitude.y) %>% 
  unique()
  
Africa.risk.mean$Orig.Country <- factor(Africa.risk.mean$Orig.Country, 
                                        levels = c(
                                          # West Africa
                                          "Benin", "Burkina Faso", "Ivory Coast", "Ghana", "Guinea", 
                                          "Mali", "Niger", "Nigeria", "Senegal", "Togo", 
                                          # East Africa
                                          "Comoros", "Ethiopia", "Kenya", "Tanzania", 
                                          # Central Africa
                                          "Chad", "Sao Tome And Principe", 
                                          # Southern Africa
                                          "Angola", "Mauritius", 
                                          # Northern Africa
                                          "Mauritania"
                                        ))


## Plotting by Rergion from which the risk is coming from
pdf("Review/Fig_Africa_review_.pdf", width=13, height=15)
ggplot() +
  geom_raster(data = as.data.frame(Africa_crop, xy = TRUE), aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(name = "Transmission Potential * \nPopulation Density", colours = custom_palette, na.value = "transparent") +
  geom_sf(data=spdf_africa,lwd=0.55,colour="#2b2d42",fill=NA,show.legend = FALSE)+
  geom_sf(data=spdf_africa %>% filter(admin %in%  c("Angola", "Benin", "Burkina Faso", "Chad", "Comoros", "Côte d'Ivoire","Ivory Coast", "Ethiopia", "Ghana", "Guinea", 
                                                    "Kenya", "Mali", "Mauritania", "Mauritius", "Niger", "Nigeria", "Sao Tome And Principe", "Senegal", 
                                                    "Tanzania", "Togo")),lwd=1.2,colour="#2b2d42",fill=NA,show.legend = FALSE)+
  geom_text_repel(data = centroids, aes(label = adm0_iso, x = st_coordinates(centroids)[, 1], 
                                       y = st_coordinates(centroids)[, 2]),
                size = 4.5, fontface = "bold", family = "sans")+
  #geom_text_repel(box.padding = 0.5, max.overlaps = inf) +
  scale_size(range = c(.1, 20), name="Risk of Importation") +
  new_scale_colour() +
  scale_alpha_continuous(guide = "none") +  # Remove alpha legend
  new_scale_colour() +
  #geom_sf(data=spdf_africa_subregion,lwd=0.6,fill=NA,colour="#B6A39E",show.legend = FALSE)+ # BOUNDARY FOR THE AFRICAN REGIONS 
  new_scale_colour() +
  geom_point(data = Africa.risk.mean %>% filter(meanRisk > 0.0000000001) , aes(x = Longitude.y, y = Latitude.y, size = meanRisk,colour=Region),shape=21,alpha=0.95,stroke =2) +
  scale_colour_manual(values = c("West Africa"= "#8ac926","East Africa"="#2a9d8f","Central Africa" = "#ffbe0b","Northern Africa"="#f77f00","Southern Africa"="#904c77"), name = "Risk originating from")+ # Define colors
  theme_minimal()+
  theme(panel.background = element_rect(fill= "#fff", colour= NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal",
        legend.position = c(0.50,0),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 10),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22))+
  xlab(" ") + ylab(" ") + 
  #coord_sf(ylim = c(-65, 45), xlim = c(-100,125)) +
  guides(color = guide_legend(override.aes = list(shape=19,size = 8), nrow = 5))
dev.off()




## Plotting for each country 

map_Africa_risk2 <- ggplot() +
  geom_raster(data = as.data.frame(Africa_crop, xy = TRUE), aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(name = "Transmission Potential * \nPopulation Density", colours = custom_palette, na.value = "transparent") +
  geom_sf(data=spdf_africa,lwd=0.25,colour="#2b2d42",fill=NA,show.legend = FALSE)+
  #geom_text_repel(data = centroids, aes(label = adm0_iso, x = st_coordinates(centroids)[, 1], 
   #                                     y = st_coordinates(centroids)[, 2]),
    #              size = 4, fontface = "bold", family = "sans", max.overlaps = 100, min.segment.length = 5, point.padding = 0.1)+
  #geom_text_repel(box.padding = 0.5, max.overlaps = inf) +
  #scale_size(range = c(.1, 12), name="Risk of Importation") +
  new_scale_colour() +
  scale_alpha_continuous(guide = "none") +  # Remove alpha legend
  new_scale_colour() +
  #geom_sf(data=spdf_africa_subregion,lwd=0.6,fill=NA,colour="#B6A39E",show.legend = FALSE)+ # BOUNDARY FOR THE AFRICAN REGIONS 
  new_scale_colour() +
  geom_point(data = Africa.risk.mean %>% filter(meanRisk > 0.0000001) , aes(x = Longitude.y, y = Latitude.y, size = meanRisk,colour=Orig.Country),shape=21,alpha=0.85,stroke = 1.5) +
  scale_colour_manual(values = c(
    # West Africa (Warm Colors)
    "Benin" = "#8dd3c7", 
    "Burkina Faso" = "#ffffb3", 
    "Ivory Coast" = "#DAA520", 
    "Ghana" = "#bedaba", 
    "Guinea" = "#fb8072", 
    "Mali" = "#80b1d3", 
    "Niger" = "#fdb462", 
    "Nigeria" = "#b3de69", 
    "Senegal" = "#fccde5", 
    "Togo" = "#d9d9d9",
    
    # East Africa (Cold Colors)
    "Comoros" = "#377eb8", 
    "Ethiopia" = "#4daf4a", 
    "Kenya" = "#984ea3", 
    "Tanzania" = "#a65628",
    
    # Central Africa (Mixed Colors)
    "Chad" = "#70e000", 
    "Sao Tome And Principe" = "#0aefff",
    
    # Southern Africa (Neon Colors)
    "Angola" = "#eeef20", 
    "Mauritius" = "#ff206e",
    
    # Northern Africa (Neon Colors)
    "Mauritania" = "#FF4500"
  ), name = "Origin Country")+ # Define colors
  theme_minimal()+
  theme(panel.background = element_rect(fill= "#f8ffe5", colour= NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box= "horizontal",
        legend.position = c(0.50,0.10),
        legend.margin = margin(t = 0, r = 10, b = 0, l = 10),
        legend.title = element_text(size=22),
        legend.text = element_text(size=22))+
  xlab(" ") + ylab(" ") + coord_sf(ylim = c(-65, 45), xlim = c(-100,125)) +
  guides(color = guide_legend(override.aes = list(shape=19,size = 8), nrow = 5))


map_Africa_risk2


##############################################################################################################################
#####################################################. FIGURE 6  ###########################################################
##############################################################################################################################

TP.Africa <- fread("Final/Extracted_Africa_TP_withLag_2019.csv") ## Country level
TP.Africa <- TP.Africa[,-c(3,16)]
TP.Africa.long <- pivot_longer(TP.Africa, cols = 3:14, names_to = "Date", values_to = "TP")
TP.Africa.long$Date <- as.Date(TP.Africa.long$Date)


Africa.risk <- fread("Review/Risk_import_within_Africa.csv")
Africa.risk.synchro <- Africa.risk %>% 
  group_by(Dest.Country, African_Region,Date) %>% 
  summarise(meanRisk = mean(normalized_R_import, na.rm=T)) %>% 
  unique()
Africa.risk.synchro$Date <- as.Date(Africa.risk.synchro$Date)

Africa.risk.synchro.TP <- Africa.risk.synchro %>% 
  left_join(TP.Africa.long, by= c("Dest.Country" = "COUNTRY", "Date" = "Date"))

### Classifying regions
North_Africa <-  c("Algeria","Egypt","Libya","Mauritania","Morocco","Tunisia","Sahrawi ADR")

West_Africa <-   c("Benin", "Burkina Faso","Cabo Verde","Ivory Coast","Cote D'Ivoire","Gambia","Ghana","Guinea","Guinea-Bissau",
              "Liberia","Mali","Niger","Nigeria","Senegal","Sierra Leone","Togo")

Central_Africa <-  c("Burundi","Cameroon","Central African Republic","Chad","Democratic Republic of the Congo","Republic of the Congo",
                "Equatorial Guinea","Sao Tome and Principe","Gabon")

East_Africa <- c("Comoros","Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Mauritius","Rwanda","Seychelles","Somalia","South Sudan",
            "Sudan","United Republic of Tanzania","Tanzania","Uganda")

Southern_Africa <- c("Angola","Botswana","Eswatini","Lesotho","Malawi","Mozambique","Namibia","South Africa","Zambia","Zimbabwe","Swaziland")

country_region_df <- data.frame(Region = rep(c("North Africa", "West Africa", "Central Africa", "East Africa", "Southern Africa"), 
                                             c(length(North_Africa), length(West_Africa), length(Central_Africa), length(East_Africa), length(Southern_Africa))),
                                Country = c(North_Africa, West_Africa, Central_Africa, East_Africa, Southern_Africa))


Africa.risk.synchro.TP <- Africa.risk.synchro.TP %>% 
  left_join(country_region_df, by = c("Dest.Country" = "Country"))



Synchro <- Africa.risk.synchro.TP %>% 
  filter(Region == "Southern Africa") %>% 
  filter(Dest.Country != "Cabo Verde") %>% 
  ggplot() +
  geom_col(aes(x=Date, y = TP, fill= "Transmission Potential"),alpha=0.8) +
  scale_fill_manual(values = c("#ccdbfd"), name= " ") +  # Define color and legend title
  geom_line(aes(x=Date, y = meanRisk*1000,color = African_Region),linewidth=1) +
  #geom_line(data=JJ_sumcontinent %>% filter(Dest.Country %in% c("Senegal")),
   #         aes(x=Date, y = megasumrisk_max*10, colour="Total Risk"),linetype="dashed",linewidth=1) +
  #scale_color_manual(values = c("#325070","#ff4a2d","black"), name= "Risk from") + 
  scale_y_continuous(sec.axis = sec_axis(~ ./1000, name = "Risk of Importation")) +
  facet_wrap(~Dest.Country,scale="free_y")+
  theme_minimal()+
  #theme_ipsum( axis_title_size=20) +
  theme(axis.text.x = element_text(size = 17, angle = 25, hjust = 1,family = "sans"),
        axis.text.y = element_text(size = 17, hjust = 1,family = "sans"),
        axis.title.y = element_text(vjust=0.5),
        axis.title.y.right = element_text(vjust=1),
        axis.title = element_text(size=20,family = "sans"),
        strip.text = element_text(size= 19,family="sans",face = "bold", margin = margin(b = 20)),
        legend.position = "top",
        legend.background = element_rect( colour= "transparent",fill = "transparent"),
        legend.text =element_text(size=21,family = "sans"),
        legend.title = element_text(size=21)) +
  labs(title = " ", x = "Date", y = "Mean Index P")


Synchro


##############################################################################################################################
#####################################################. FIGURE 7  ###########################################################
##############################################################################################################################

## PieChar Scatterplot on Map 

## Transmission Potential
TP.Africa <- fread("Final/Extracted_Africa_TP_withLag_2019.csv") ## Country level
TP.Africa <- TP.Africa[,-c(3,16)]
TP.Africa.long <- pivot_longer(TP.Africa, cols = 3:14, names_to = "Date", values_to = "TP")
TP.Africa.long$Date <- as.Date(TP.Africa.long$Date)

## Risk Within the African Continent
Africa.risk <- fread("Review/Risk_import_within_Africa.csv")
Africa.risk$region_name.y <- gsub("province", "", Africa.risk$region_name.y, ignore.case = TRUE)
Africa.risk$region_name.y <- gsub("district\\s*$", "", Africa.risk$region_name.y, ignore.case = TRUE)
Africa.risk$region_name.y <- gsub("Region\\s*$", "", Africa.risk$region_name.y, ignore.case = TRUE)
Africa.risk$Dest.Country <- trimws(Africa.risk$Dest.Country)
Africa.risk$region_name.y <- trimws(Africa.risk$region_name.y)
Africa.risk <- fread("Review/Risk_import_within_Africa.csv")
Africa.risk$Airport_State.y <- gsub("province", "", Africa.risk$Airport_State.y, ignore.case = TRUE)
Africa.risk$Airport_State.y <- gsub("district\\s*$", "", Africa.risk$Airport_State.y, ignore.case = TRUE)
Africa.risk$Airport_State.y <- gsub("Region\\s*$", "", Africa.risk$Airport_State.y, ignore.case = TRUE)
Africa.risk$Airport_State.y <- gsub("Department\\s*$", "", Africa.risk$Airport_State.y, ignore.case = TRUE)
Africa.risk$Airport_State.y <- gsub("Metropolitan\\s*$", "", Africa.risk$Airport_State.y, ignore.case = TRUE)

Africa.risk$Airport_State.y <- gsub("Governorate\\s*$", "", Africa.risk$Airport_State.y, ignore.case = TRUE)

Africa.risk$Dest.Country <- trimws(Africa.risk$Dest.Country)
Africa.risk$Airport_State.y <- trimws(Africa.risk$Airport_State.y)
Africa.risk$Date <- as.Date(Africa.risk$Date)
  
North_Africa <-  c("Algeria","Egypt","Libya","Mauritania","Morocco","Tunisia","Sahrawi ADR")

West_Africa <-   c("Benin", "Burkina Faso","Cabo Verde","Ivory Coast","Cote D'Ivoire","Gambia","Ghana","Guinea","Guinea-Bissau",
                   "Liberia","Mali","Niger","Nigeria","Senegal","Sierra Leone","Togo")

Central_Africa <-  c("Burundi","Cameroon","Central African Republic","Chad","Democratic Republic of the Congo","Republic of the Congo",
                     "Equatorial Guinea","Sao Tome and Principe","Gabon","Congo")

East_Africa <- c("Comoros","Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Mauritius","Rwanda","Seychelles","Somalia","South Sudan",
                 "Sudan","United Republic of Tanzania","Tanzania","Uganda")

Southern_Africa <- c("Angola","Botswana","Eswatini","Lesotho","Malawi","Mozambique","Namibia","South Africa","Zambia","Zimbabwe","Swaziland")

country_region_df <- data.frame(Region = rep(c("North Africa", "West Africa", "Central Africa", "East Africa", "Southern Africa"), 
                                             c(length(North_Africa), length(West_Africa), length(Central_Africa), length(East_Africa), length(Southern_Africa))),
                                Country = c(North_Africa, West_Africa, Central_Africa, East_Africa, Southern_Africa))


Africa.risk.pie <- Africa.risk %>% 
  left_join(country_region_df, by = c("Orig.Country" = "Country"))


Africa.risk.pie.year <- Africa.risk.pie %>% 
  group_by(Dest.Country, Airport_State.y,Region) %>% 
  summarise(meanRiskYear = mean(normalized_R_import, na.rm=TRUE))

### Shapefile at Admin level 1 (By Province / State / Region) 
spdf_africa_prov <- st_read("shapefile_admine_level1_africa/959cf709-3b35-4278-b297-a7442cdc37d0/afr_g2014_2013_1.shp")
#plot(City)

spdf_africa_prov$ADM0_NAME <- iconv(spdf_africa_prov$ADM0_NAME, from = "LATIN1", to = "UTF-8")
spdf_africa_prov$ADM1_NAME <- iconv(spdf_africa_prov$ADM1_NAME, from = "LATIN1", to = "UTF-8")
spdf_africa_prov$ADM0_NAME <- trimws(spdf_africa_prov$ADM0_NAME)
spdf_africa_prov$ADM1_NAME <- trimws(spdf_africa_prov$ADM1_NAME)

spdf_africa_prov$ADM0_NAME[spdf_africa_prov$ADM0_NAME == "Côte d'Ivoire"] <- "Ivory Coast"
spdf_africa_prov$ADM1_NAME[spdf_africa_prov$ADM1_NAME == "Kigali City/Umujyi wa Kigali"] <- "Kigali"
spdf_africa_prov$ADM1_NAME[spdf_africa_prov$ADM1_NAME == "West/Iburengerazuba"] <- "West"

Africa.risk.pie.year.sf <- spdf_africa_prov %>% 
  left_join(Africa.risk.pie.year, by= c("ADM0_NAME" = "Dest.Country" , "ADM1_NAME"= "Airport_State.y")) 


### Addidng a pie chart to the map 
library(scatterpie)
valid  <- sf::st_make_valid(Africa.risk.pie.year.sf)
center <- st_centroid(valid) ## to get mid points of polygons 
center$Lat <- st_coordinates(center)[, 2]
center$Long <- st_coordinates(center)[, 1]
center <- st_drop_geometry(center)
center$total <- length(na.omit(center$Continent))

# Calculate the proportion of risk from Asia and South America
center<-subset(unique(center),!is.na(meanRiskYear))


center <- center %>% group_by(ADM1_NAME) %>%
  mutate(total.annual.risk=sum(meanRiskYear)) %>%
  ungroup()

center <- center %>% group_by(Region) %>%
  mutate(proportion.risk=meanRiskYear/total.annual.risk) %>%
  ungroup()

center$prop_West<-0
center$prop_East<-0
center$prop_Central<-0
center$prop_Northen<-0
center$prop_Southern<-0


center$prop_West <- ifelse(center$Region == "West Africa", 
                           center$proportion.risk,
                           0)

center$prop_East <- ifelse(center$Region == "East Africa", 
                                   center$proportion.risk,
                                   0)


center$prop_Central <- ifelse(center$Region == "Central Africa", 
                           center$proportion.risk,
                           0)

center$prop_Northen <- ifelse(center$Region == "North Africa", 
                              center$proportion.risk,
                              0)

center$prop_Southern <- ifelse(center$Region == "Southern Africa", 
                              center$proportion.risk,
                              0)


# Replace NA with 0 in 'prop_asia' and 'prop_south_america'
center$prop_West[is.na(center$prop_West)] <- 0
center$prop_East[is.na(center$prop_East)] <- 0
center$prop_Central[is.na(center$prop_Central)] <- 0
center$prop_Northen[is.na(center$prop_Northen)] <- 0
center$prop_Southern[is.na(center$prop_Southern)] <- 0

# Convert 'prop_asia' and 'prop_south_america' to numeric
center$prop_West <- as.numeric(center$prop_West)
center$prop_East <- as.numeric(center$prop_East)
center$prop_Central <- as.numeric(center$prop_Central)
center$prop_Northen <- as.numeric(center$prop_Northen)
center$prop_Southern <- as.numeric(center$prop_Southern)

library(ggnewscale)

Africa.risk.pie.year.sf$custom_colour <-  ifelse(is.na(Africa.risk.pie.year.sf$meanRiskYear) | Africa.risk.pie.year.sf$meanRiskYear < 0, "Yes", "NO")


plot1 <- ggplot() + 
  geom_sf(data=unique(Africa.risk.pie.year.sf),aes(fill= custom_colour == "Yes"), colour="#ced4da",lwd=0.2,alpha=0.9) +
  scale_fill_manual(values= c("TRUE"= "transparent", "FALSE" = "#ced4da"),guide = "none") +
  new_scale_fill() +
  geom_sf(data=spdf_africa, fill = "NA",colour= "#ff4a2d", lwd=0.7,alpha=0.7)+
  #geom_text(data = centroids, aes(label = adm0_a3_is, x = st_coordinates(centroids)[, 1], 
  #                                y = st_coordinates(centroids)[, 2]), nudge_y = 0, size = 3.5,color="#2d00f7",fontface= "bold")+
  geom_scatterpie(data=center,aes(x=Long, y=Lat, r=0.55*(4--log10(as.numeric(meanRiskYear)))), cols=c("prop_West", "prop_East","prop_Central","prop_Northen","prop_Southern"), 
                  alpha=0.8,color = 'black',position_jitter()) + 
  scale_fill_manual(name="Origin Country",values= c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51"),
                    labels= c("West Africa", "East Africa", "Central Africa", "Northern Africa", "Southern Africa"),guide = guide_legend(override.aes = list(size = 5),ncol=3))+
  scale_radius(range = c(1, 12), name="Risk of Importation")+
  # Rename the size legend for geom_point
  theme_minimal()+
  #theme_ipsum(axis_title_size = 15, base_size = 15, subtitle_size = 18)+
  theme(plot.background = element_rect(color='white', fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=24,face = "bold"),
        strip.text = element_text(size = 30, face="bold"),
        legend.box= "vertical",
        legend.position = "bottom",
        legend.text = element_text(size = 23, face="bold"),
        legend.title = element_text(size = 23, face="bold"),
        legend.margin = margin(t = -1, r = 0, b = 0, l = 0),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlab(" ") + ylab(" ") +
  ggtitle("TP")
plot1

##############################################################################################################################
## Combining Africa, Asia and South America Risk 
##############################################################################################################################
combined.jj.spdf_province
##############################################################################################################################
## Transmission Potential
TP.Africa <- fread("Final/Extracted_Africa_TP_withLag_2019.csv") ## Country level
TP.Africa <- TP.Africa[,-c(3,16)]
TP.Africa.long <- pivot_longer(TP.Africa, cols = 3:14, names_to = "Date", values_to = "TP")
TP.Africa.long$Date <- as.Date(TP.Africa.long$Date)

## Risk Within the African Continent
Africa.risk.pie ## computed previously (see previous plot) 
Africa.risk.pie$Date <- as.Date(Africa.risk.pie$Date)

Africa.risk.pie.TP <- Africa.risk.pie %>% 
  left_join(TP.Africa.long, by = c("Dest.Country" = "COUNTRY", "Date" = "Date"))

## Now removing those months where TP <1 
Africa.West <- Africa.risk.pie.TP %>% 
  filter(TP > 1) %>% 
  filter(Region =="West Africa") %>% 
  group_by(Region,Dest.Country,Airport_State.y) %>% 
  summarise(m.annal.risk = sum(normalized_R_import))

Africa.East <- Africa.risk.pie.TP %>% 
  filter(TP > 1) %>% 
  filter(Region =="East Africa") %>% 
  group_by(Region,Dest.Country,Airport_State.y) %>% 
  summarise(m.annal.risk = sum(normalized_R_import))

Africa.Central <- Africa.risk.pie.TP %>% 
  filter(TP > 1) %>% 
  filter(Region =="Central Africa") %>% 
  group_by(Region,Dest.Country,Airport_State.y) %>% 
  summarise(m.annal.risk = sum(normalized_R_import))

Africa.North <- Africa.risk.pie.TP %>% 
  filter(TP > 1) %>% 
  filter(Region =="North Africa") %>% 
  group_by(Region,Dest.Country,Airport_State.y) %>% 
  summarise(m.annal.risk = sum(normalized_R_import))

Africa.Southern <- Africa.risk.pie.TP %>% 
  filter(TP > 1) %>% 
  filter(Region =="Southern Africa") %>% 
  group_by(Region,Dest.Country,Airport_State.y) %>% 
  summarise(m.annal.risk = sum(normalized_R_import))


# Merge with the shapefile
#Rename countries if needed 

spdf_africa_West <- spdf_africa_prov %>% 
  left_join(Africa.West, by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "Airport_State.y" ))
#spdf_africa_riskfrAsia2$Continent <- ifelse(is.na(spdf_africa_riskfrAsia2$Continent), "Asia", spdf_africa_riskfrAsia2$Continent)

spdf_africa_East <- spdf_africa_prov %>% 
  left_join(Africa.East, by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "Airport_State.y" ))

spdf_africa_Central <- spdf_africa_prov %>% 
  left_join(Africa.Central, by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "Airport_State.y" ))

spdf_africa_North <- spdf_africa_prov %>% 
  left_join(Africa.North, by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "Airport_State.y" ))

spdf_africa_South <- spdf_africa_prov %>% 
  left_join(Africa.Southern, by= c("ADM0_NAME" = "Dest.Country","ADM1_NAME" = "Airport_State.y" ))

combined.Africa.prop <- rbind(spdf_africa_West,spdf_africa_East,spdf_africa_Central,
                              spdf_africa_North,spdf_africa_South)
library(scatterpie)
valid  <- sf::st_make_valid(combined.Africa.prop)
center2 <- st_centroid(valid) ## to get mid points of polygons 
center2$Lat <- st_coordinates(center2)[, 2]
center2$Long <- st_coordinates(center2)[, 1]
center2 <- st_drop_geometry(center2)
center2$total <- length(na.omit(center2$Continent))

center2<-subset(unique(center2),!is.na(m.annal.risk))

center2<-center2 %>% group_by(ADM1_NAME) %>%
  mutate(total.annual.risk=sum(m.annal.risk)) %>%
  ungroup()

center2<-center2 %>% group_by(Region) %>%
  mutate(proportion.risk=m.annal.risk/total.annual.risk) %>%
  ungroup()

center2$prop_West<-0
center2$prop_East<-0
center2$prop_Central<-0
center2$prop_Northen<-0
center2$prop_Southern<-0


center2$prop_West <- ifelse(center2$Region == "West Africa", 
                           center2$proportion.risk,
                           0)

center2$prop_East <- ifelse(center2$Region == "East Africa", 
                           center2$proportion.risk,
                           0)


center2$prop_Central <- ifelse(center2$Region == "Central Africa", 
                              center2$proportion.risk,
                              0)

center2$prop_Northen <- ifelse(center2$Region == "North Africa", 
                              center2$proportion.risk,
                              0)

center2$prop_Southern <- ifelse(center2$Region == "Southern Africa", 
                               center2$proportion.risk,
                               0)


# Replace NA with 0 in 'prop_asia' and 'prop_south_america'
center2$prop_West[is.na(center2$prop_West)] <- 0
center2$prop_East[is.na(center2$prop_East)] <- 0
center2$prop_Central[is.na(center2$prop_Central)] <- 0
center2$prop_Northen[is.na(center2$prop_Northen)] <- 0
center2$prop_Southern[is.na(center2$prop_Southern)] <- 0

# Convert 'prop_asia' and 'prop_south_america' to numeric
center2$prop_West <- as.numeric(center2$prop_West)
center2$prop_East <- as.numeric(center2$prop_East)
center2$prop_Central <- as.numeric(center2$prop_Central)
center2$prop_Northen <- as.numeric(center2$prop_Northen)
center2$prop_Southern <- as.numeric(center2$prop_Southern)

combined.Africa.prop$custom_colour <-  ifelse(is.na(combined.Africa.prop$m.annal.risk) | combined.Africa.prop$m.annal.risk < 0, "Yes", "NO")


plot2 <- ggplot() + 
  geom_sf(data=unique(combined.Africa.prop),aes(fill= custom_colour == "Yes"),colour="#ced4da", lwd=0.2) +
  scale_fill_manual(values= c("TRUE"= "transparent", "FALSE" = "#ced4da" ),guide = "none") +
  new_scale_fill() +
  geom_sf(data=spdf_africa, fill = "NA",colour= "#ff4a2d", lwd=0.7,alpha=0.7)+
  geom_scatterpie(data=center2,aes(x=Long, y=Lat, r=0.55*(4--log10(as.numeric(total.annual.risk)))),  cols=c("prop_West", "prop_East","prop_Central","prop_Northen","prop_Southern"), 
                  alpha=0.8,color = 'black',position_jitter()) + 
  #scale_size(range = c(1, 10), name="Risk of Importation")+
  scale_fill_manual(name="Origin Country",values= c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51"),
                    labels= c("West Africa", "East Africa", "Central Africa", "Northern Africa", "Southern Africa"),guide = guide_legend(override.aes = list(size = 5),ncol=3))+
  #geom_text(data = centroids, aes(label = adm0_a3_is, x = st_coordinates(centroids)[, 1], 
  #                                    y = st_coordinates(centroids)[, 2]), nudge_y = 0.1, size = 2.5,color="#2d00f7",fontface = 'bold')+
  #scale_size_area(max_size = 12, name = "Risk of Importation")+
  # Rename the size legend for geom_point
  theme_minimal()+
  #theme_ipsum(axis_title_size = 15, base_size = 15, subtitle_size = 18)+
  theme(plot.background = element_rect(color='white', fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=24,face = "bold"),
        strip.text = element_text(size = 30, face="bold"),
        legend.box= "vertical",
        legend.position = "bottom",
        legend.text = element_text(size = 23, face="bold"),
        legend.title = element_text(size = 23, face="bold"),
        legend.margin = margin(t = -1, r = 0, b = 0, l = 0),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  xlab(" ") + ylab(" ") +
  ggtitle("TP > 1")
plot2

################# Timeline including African risk 

Risks_rev_barplot <- fread("Review/Risk_inter.csv")
Risks_rev_barplot$Date <- as.Date(Risks_rev_barplot$Date)
Risks_rev_barplot$REGION
Risks_rev_barplot2 <- Risks_rev_barplot %>% 
  group_by(Dest.Country,Date,CONTINENT) %>% 
  summarise(sumRisk=mean(sumRisk, na.rm=T),
            REGION = REGION)

Risks_rev_barplot2$Dest.Country <- with(Risks_rev_barplot2, reorder(Dest.Country, -log(sumRisk), median, na.rm=T))
Risks_rev_barplot2 <- Risks_rev_barplot2 %>% 
  filter(!Dest.Country %in% c("Reunion", "Seychelles","Cape Verde","Cabo Verde"))


##Load Transmission Potential
TP.Africa <- fread("Final/Extracted_Africa_TP_withLag_2019.csv") ## Country level
TP.Africa <- TP.Africa[,-c(3,16)]
TP.Africa.long <- pivot_longer(TP.Africa, cols = 3:14, names_to = "Date", values_to = "TP")
TP.Africa.long$Date <- as.Date(TP.Africa.long$Date)

## Joining the TP to the risk 
Risks_rev_timeline <- Risks_rev_barplot2 %>% 
  left_join(TP.Africa.long, by = c("Dest.Country" = "COUNTRY", "Date" = "Date")) %>% 
  unique()

Risks_rev_timeline$sumRisk_Max <- Risks_rev_timeline$sumRisk/max(Risks_rev_timeline$sumRisk)
Risks_rev_timeline$TP_Max <- Risks_rev_timeline$TP/max(Risks_rev_timeline$TP, na.rm=T)

Risks_rev_timeline_sumcontinent <- Risks_rev_timeline %>% 
  group_by(Date, Dest.Country) %>% 
  summarise(megasumrisk_max = sum(sumRisk_Max))


## Groupings for plots for supplementary

g2 <- c("Tunisia", "Ivory Coast","Ghana","Cameroon","Central African Republic","Equatorial Guinea","South Sudan",
        "Namibia","Zambia","Algeria","Sudan")
g3 <- c("Gabon","Democratic Republic of the Congo","Mauritania","Guinea","Madagascar","Malawi","Zimbabwe","Botswana")
g4 <- c("Togo","Sierra Leone","Burundi","Benin","Burkina Faso","Guinea Bissau","Djibouti","Eritrea","Congo","Liberia")
g5 <- c("Swaziland","Libya","Comoros")
g5.2<- c("Mali","Niger","Somalia","Gambia")
gfree <- c("Sao Tome and Principe", "Lesotho","Chad","Mozambique","Mali", "Niger", "Somalia", "Gambia")
gmain <- c("Angola", "Egypt", "Ethiopia", "Kenya", "Mauritius", "Morocco", "Nigeria", "Rwanda", "Senegal", "South Africa" , "Tanzania", "Uganda")



pdf("Review/timeline_all_S6E.pdf", width=25, height=17)
Risks_rev_timeline %>% 
  #filter(CONTINENT != "Africa") %>% 
  #filter(!Dest.Country %in% c("Reunion", "Seychelles") ) %>% 
  #filter(!Dest.Country %in% c( "Mauritius", "Angola","Egypt","Ethiopia", "Kenya",
  #           "Morocco", "Nigeria", "Rwanda", "South Africa", "Uganda","Senegal", "Tanzania", "Cape Verde")) %>% 
  filter(Dest.Country %in% g2 ) %>% 
  ggplot() +
  geom_col(aes(x=Date, y = TP_Max, fill= "Transmission Potential"),alpha=0.8) +
  scale_fill_manual(values = c("#ccdbfd"), name= " ") +  # Define color and legend title
  geom_line(aes(x=Date, y = sumRisk_Max*10,color = CONTINENT),linewidth=1) +
  #geom_line(aes(x = Date, 
   #             y = ifelse(CONTINENT %in% c("Asia", "South America"), sumRisk_Max * 100, sumRisk_Max*10), 
    #            color = CONTINENT), 
     #       linewidth = 1)+
  geom_line(data=Risks_rev_timeline_sumcontinent %>% filter(Dest.Country %in% g2),
            aes(x=Date, y = megasumrisk_max*10, colour="Total Risk"),linetype="dashed",linewidth=1) +
  scale_color_manual(values = c("#f77f00","#325070","#ff4a2d","black"), name= "Risk from") + 
  scale_y_continuous(sec.axis = sec_axis(~ ./10, name = "Risk of Importation")) +
  facet_wrap(~Dest.Country,scale="free_y",nrow=4)+
  theme_minimal()+
  #theme_ipsum( axis_title_size=20) +
  theme(axis.text.x = element_text(size = 19, angle = 25, hjust = 1,family = "sans"),
        axis.text.y = element_text(size = 19, hjust = 1,family = "sans"),
        axis.title.y = element_text(vjust=0.5),
        axis.title.y.right = element_text(vjust=1),
        axis.title = element_text(size=20,family = "sans"),
        strip.text = element_text(size= 19,family="sans",face = "bold", margin = margin(b = 20)),
        legend.position = "top",
        legend.background = element_rect( colour= "transparent",fill = "transparent"),
        legend.text =element_text(size=25,family = "sans",face="bold"),
        legend.title = element_text(size=27)) +
  labs(title = " ", x = "Date", y = "Mean Index P")
dev.off()

#"#f77f00"
