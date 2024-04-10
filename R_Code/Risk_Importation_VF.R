## Loading Libraries
install.packages("moments")
library(corrr, moments)

library(maps)
library(geosphere)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rworldmap)
#library(plyr)
library(data.table)
library(ggthemes)
library(airportr)
library(hrbrthemes)
library(ggplot2)
library(lubridate)
library(paletteer)
library(RColorBrewer)
library(data.table)
library(readr)
library(MMWRweek)
library(scales)
library(showtext)
library(sysfonts)
library(rgeos)
library(sf)
library(rnaturalearth)
library(janitor)
library(raster)
library(viridis)
library(ggnewscale)
library(ggrepel)


### Correlation Study

#Spatial Correlation
## Is the number of cases correlated with the Population count, Vector Occurence and Transmission Potential 

## Compiled dataset _JP wwith Transmission Potential
M.Data <- fread("Final/Cases_Pop_TP_2019.csv")
names(M.Data)
M.Data.2 <- M.Data[,c(1:10,23:25)]

## Filtering fo rthose where we have monthly and district data
Data_M.D.2 <- M.Data.2 %>% 
  filter(Case_Res %in% c("D", "MD"))
Data_M.D.2$incidence <-   Data_M.D.2$District_Dengue_Cases_2019/Data_M.D.2$Population
  
hist(Data_M.D.2$District_Dengue_Cases_2019, main = "Histogram of Dengue Cases 2019", xlab = "Number of Cases")
summary(Data_M.D.2$District_Dengue_Cases_2019)
Data_M.D.2$TPxPop <- Data_M.D.2$Population*Data_M.D.2$Yearly_MeanTp_1mlag



countries2 <- unique(Data_M.D.2$Country)
correlation_matrices2 <- list()

# Loop through each country
for (country in countries2) {
  # Filter the data for the current country
  filtered_data2 <- Data_M.D.2 %>% filter(Country == country)
  
  # Calculate the correlation matrix for columns 3 to 6
  correlation_matrix2 <- cor(filtered_data2[, c(7,8)],use = "pairwise.complete.obs")
  
  # Store the correlation matrix in the list
  correlation_matrices2[[country]] <- correlation_matrix2
}

corr_df <- as.data.frame(correlation_matrices2)
corr_df <- corr_df[-1,c(1,3,5,7,9,11,13,15,17,19,21,23)]
corr_df$x <- "Dengue Cases"
corr_df_long <- pivot_longer(corr_df, cols = 1:12, names_to = "y")
corr_df_long$y <- gsub("\\.Population", "", corr_df_long$y)
corr_df_long$y <- gsub("\\.Density", "", corr_df_long$y)
corr_df_long$y <- gsub("\\.", " ", corr_df_long$y)

# Horizontal version
corr2 <- ggplot(corr_df_long, aes(x=x, y=y)) +
  geom_segment( aes(x=y, xend=y, y=0, yend=value), color="#0BC9CD") +
  geom_point(aes(x=y,y=value),color="#B0DB43", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  scale_y_continuous(minor_breaks = NULL) +  # Use scale_x_continuous instead
  ylim(c(-1,1))+
  theme(plot.background = element_rect(fill="transparent"),
        panel.background = element_rect(fill="transparent"),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())+
  labs(y= "Correlation between Dengue Cases : Population", x=" ")

#ggsave(corr2, filename = "corr2.png", height = 4, width = 5,  bg = "transparent")


 ## plot 
pdf("corrplot_output.pdf", width = 15, height = 10)  # Adjust width and height as needed

par(mfrow = c(4,3),mar=c(0.1,0.1,0.1,0.1))

# Load the required library
library(corrplot)

# Loop through each country and create corrplot.mixed
for (country in countries2) {
  # Get the correlation matrix for the current country
  correlation_matrix2 <- correlation_matrices2[[country]]
  
  colnames(correlation_matrix2) <- c("Pop", "Dengue \n Cases")
  
  # Create the corrplot.mixed for the correlation matrix
  corrplot.mixed(correlation_matrix2,
                 upper = "ellipse",
                 tl.cex = 0.85, tl.col = "black",lower.col = "black",
                 title.cex=2,
                 title = paste(" ", country),
                 line= -2,
                 mar = c(0,0,1,0))
}

dev.off()

### Temporal Correlation
###  Looking at those where is only have Monthly data

MD.data <- fread("Final/Monthly_Dengue_cases2019_TP_correlation.csv")
MD.data$Date <- as.Date(MD.data$Date)
MD.data$Country <- as.factor(MD.data$Country)

MD.data %>% 
  group_by(Date) %>% 
  summarise(Cases2019= sum(`Dengue Cases`)) %>% 
  ggplot() + 
  geom_col(aes(x=Date, y=Cases2019))


ggplot(MD.data) + 
  geom_bar(aes(x=Date, y=`Dengue Cases`,fill=Country),position = "stack",stat="identity",na.rm=T)+
  #scale_fill_manual(values = paletteer_dynamic("cartography::multi.pal", 20)) +
  scale_fill_manual(values= c("India"="#84759F","Brazil"="#F16A48","Malaysia"= "darkgreen","Bangladesh" = "darkblue","Peru"="purple","Bolivia"="#0a9396",
                                "Sri Lanka" = "#dddf00", "Cambodia"= "#abc4ff", "Colombia" = "#4d194d", "Singapore" = "#f7aef8","Thailand" = "#bce784", 
                                "Vietnam" = "#147df5", "Nicaragua" = "#7cb4aa", "Belize" = "#8d6a79") ,name= "Origin Country")+
 theme_tufte()+
  #theme_ipsum(axis_title_size = 22,axis_text_size = 18)+
  theme(axis.text = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title = element_text(size=22),
        legend.title = element_text(size=22),
        legend.key.size = unit(3, "lines"))+
  guides(color = guide_legend(override.aes = list(size = 2))) 

  
  

# List of countries
countries <- c("Brazil", "Sri Lanka","Burkina Faso", "Bangladesh", "India", "Belize", 
               "Bolivia", "Colombia", "Nicaragua", "Peru", "Singapore", "Vietnam", 
               "Thailand", "Malaysia", "Cambodia", "Mauritius")

countries <- c("Burkina Faso")
# Create an empty list to store correlation matrices
correlation_matrices <- list()

# Loop through each country
for (country in countries) {
  # Filter the data for the current country
  filtered_data <- MD.data %>% filter(Country == country)
  
  # Calculate the correlation matrix for columns 3 to 6
  correlation_matrix <- cor(filtered_data[, 3:6])
  
  # Store the correlation matrix in the list
  correlation_matrices[[country]] <- correlation_matrix
}

corr_tempo_df <- as.data.frame(correlation_matrices)
corr_tempo_df_plot <- corr_tempo_df[-1,c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61)]
corr_tempo_df_plot$x <- c("TP", "TP+1m", "TP+2m")
corr_tempo_df_plot_long <- pivot_longer(corr_tempo_df_plot, cols = 1:16, names_to = "y")
corr_tempo_df_plot_long$y <- gsub("\\.Dengue\\.Cases", "", corr_tempo_df_plot_long$y)


corr<- ggplot(corr_tempo_df_plot_long, aes(x = x, y = y, fill = value)) +
  theme_minimal()+
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  #coord_fixed() +
  scale_fill_distiller(palette = "RdBu", name= "Correlation")+
  labs(y= "Dengue Cases", x=" Transmission Potential ")

ggsave(corr, filename = "corr1.png", height = 4, width = 4,  bg = "transparent")
corr_tempo_df_plot_long %>% 
  group_by(x) %>% 
  summarise(meancorr= mean(value))

par(mfrow = c(1,1))
# Load the required library
library(corrplot)

# Loop through each country and create corrplot.mixed
for (country in countries) {
  # Get the correlation matrix for the current country
  correlation_matrix <- correlation_matrices[[country]]
  
  colnames(correlation_matrix) <- c("Dengue \n Cases", " TP" , "TP+1m", "TP+2m")
  
  
  # Create the corrplot.mixed for the correlation matrix
  corrplot.mixed(correlation_matrix,
                 upper = "ellipse",
                 tl.cex = 1.2, tl.col = "black",lower.col="black",number.cex=1.4,
                 title.cex=2,
                 title = paste(" ", country),
                 mar = c(10, 0, 4, 1))
}





#########################################################################################################
#########################################################################################################
#########################################################################################################
### RISK FLOW ANALYSIS 

African_countries <- c("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi","Cape Verde", 
                       "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros", "Congo",
                       "Democratic Republic of the Congo","Republic of the Congo","Republic of Congo","Ivory Coast","Cote D'Ivoire",
                       "Djibouti","Egypt","Equatorial Guinea","Eritrea","Swaziland","Ethiopia","Ivory Coast (Cote d'Ivoire)",
                       "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Guinea Bissau","Kenya","Lesotho","Liberia",
                       "Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco",
                       "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe","Reunion",
                       "Senegal","Seychelles","Sierra Leone","Somalia","South Africa","South Sudan",
                       "Sudan","United Republic of Tanzania","Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe")

countries_to_process <- c("Bangladesh", "Belize", "Bolivia", "Brazil", "Cambodia", "Colombia", "India", "Kenya", "Malaysia", "Nicaragua", "Peru", "Singapore", "Sri Lanka", "Thailand", "Vietnam")

#### Travel Data ####################################################################
### Reading in the travel data 
library(openxlsx)

### Creating a loop for the analysis - can be runned seperately for each country

results_list <- list()

for (country in countries_to_process) {
  
Travel.data.2019 <- read.xlsx("Data/Travel Data/sampleFromAsiaAndAmerica/WEIRDO .xlsx")
Travel.data.2019$Date <- as.Date(Travel.data.2019$Date, origin = "1899-12-30")

Travel.data.2019$Dest.Country[Travel.data.2019$Dest.Country == "Ivory Coast (Cote d'Ivoire)"] <- "Ivory Coast"
## Getting airports GPS locations 
library(airportr)

library(dplyr)
full_air <- fread("Data/Travel Data/world-airports.csv")

ff_air <- airports %>% 
  left_join(full_air, by=c("IATA" = "iata_code"))

air <- ff_air[,c(4,10,11,28,30)]
## Matching travel data to airport location 

match1 <- Travel.data.2019 %>% 
  left_join(air, by=c("Orig" = "IATA"))  ## matching for orig country 

match2 <- match1 %>% 
  left_join(air, by=c("Dest" = "IATA")) # givesus the airports with their spatial coordinates 


## Aggregating to State - that is sum of travels out of a state instead of specific airports
Travel.data.2019.GPS <- match2
Travel.data.2019.GPS$Date <- as.Date(Travel.data.2019.GPS$Date)


### Removing the same country travels as you want to compute the probability of travelling internationally, 
### you want to exclude the local flights 
### Then you Aggregate the travels from each specific state
## Change accordingly based on the Country you are working with 

Ind.travel.2019 <- Travel.data.2019.GPS %>% 
  filter(Orig.Country == country & Dest.Country!= country ) %>% #removing domestic flights 
  group_by(State_Orig_airport,Date) %>%  ## you want to aggregate travels per state and date (in this case month) 
  mutate(travelout = sum(Reported.Est..Pax)) %>% 
  filter(Dest.Country %in% African_countries) ## Lastly keep the destination just to minimize the dataset as you already captured what you need 

#### Loading the Dengue data set that was compiled
###  we propagated accordinglly based on population and transmission potentials estimates
### Dengue Cases for 2019

#ind.state.cases <- fread("Data/India/Monthly_Statewise_India_cases.csv")
ind.state.cases_all <- fread("Final/TP_for_analysis_1mlag.csv") ## Using TP instead of cases
#ind.state.cases_all <- fread("Final/Dengue_Propagated_Cases_2019.csv") ## Using cases

## Filter accordingly with regards to the country you are working with 
ind.state.cases <- ind.state.cases_all %>% 
  filter(COUNTRY == country)

names(ind.state.cases)
### converting the dataframe into a longer format to be able to combine with the travel data and run the metric 
ind.state.cases.long <- pivot_longer(ind.state.cases, cols = (10:21), names_to = "Date", values_to = "Dengue_Cases_MD")
names(ind.state.cases.long)[2] <- "State_Orig_airport" ## Rename it so it matches the column you need to join it to in the travel dataset
ind.state.cases.long$Date <- as.Date(ind.state.cases.long$Date)

## Joining Cases to Travels 
ind.state.case.travel <- Ind.travel.2019 %>% 
  left_join(ind.state.cases.long, by= c("State_Orig_airport" = "State_Orig_airport","Date" = "Date"))

## Computing the cumulated incidence - Cases/Population 
#ind.state.case.travel$cumul_pop <- ind.state.case.travel$Dengue_Cases_MD/ind.state.case.travel$Population


##Adding column of sum for except [i]
library(purrr)
library(dplyr)
library(tibble)


# Calculate the sum of cumul_pop for each State_Orig_airport
#state_cumul_pop_sum <-ind.state.cases.long %>% 
 # group_by(Date) %>% 
  #summarise(tot2 = sum(Dengue_Cases_MD, na.rm=T))

state_cumul_pop_sum <- ind.state.cases.long %>%
  group_by(Date,State_Orig_airport) %>%
    summarise(tot2 = sum(Dengue_Cases_MD,na.rm=TRUE)) ## You get the case per month for each specific state

state_cumul_pop_sum <- state_cumul_pop_sum %>% 
  group_by(Date) %>% 
  summarise(tot2 = sum(tot2 , na.rm = TRUE) ) ## You get the total number of cases for the entire country for each month

# Join the total_cumul_pop to the original dataset
sum_by_state <- ind.state.case.travel %>%
  left_join(state_cumul_pop_sum, by = c("Date")) %>%
  group_by(Date, State_Orig_airport) %>%
  mutate(sum_except_i = tot2 - Dengue_Cases_MD) %>%
  ungroup() %>% 
  dplyr::select(-tot2)


### Same for population
state_pop_sum <- ind.state.cases.long %>%
  group_by(Date,State_Orig_airport) %>%
  summarise(pop2 = sum(Population_Density,na.rm=TRUE)) ## You get the case per month for each specific state

state_pop_sum <- state_pop_sum %>% 
  group_by(Date) %>% 
  summarise(pop2 = sum(pop2 , na.rm = TRUE) ) ## You get the total number of cases for the entire country for each month

# Join the total_cumul_pop to the original dataset
sum_by_state <- sum_by_state %>%
  left_join(state_pop_sum, by = c("Date")) %>%
  group_by(Date, State_Orig_airport) %>%
  mutate(population_except_i = pop2 - Population_Density) %>%
  ungroup() %>% 
  dplyr::select(-pop2)
# Do the same for travel
# Calculate the sum of cumul_pop for each State_Orig_airport

state_travel_sum <- ind.state.case.travel %>% 
  group_by(State_Orig_airport,Date) %>%
  summarise(Total_Travels = sum(Reported.Est..Pax)) # sum of travels out of each state for each month 

state_travel_sum <- state_travel_sum %>%
  group_by(Date) %>%
  mutate(Total_state_travel = sum(Total_Travels,na.rm=TRUE)) 


# Join the total_cumul_pop to the original dataset
sum_by_state <- sum_by_state %>%
  left_join(state_travel_sum, by = c("Date","State_Orig_airport")) %>%
  group_by(Date, State_Orig_airport) %>%
  mutate(sum_travel_except_i = Total_state_travel - Total_Travels) %>%
  ungroup() %>% 
  dplyr::select(-Total_state_travel)



## Risk of Importation into African Countries  ########
## Travel Influx - 
## Risk of case importation 
# travel influx: travel_in_month_us[i]
# cumulative incidence: cum_in[i]
# probability of traveling from 𝑖 to : travel_in_month_us[i]/travel_in_month_all[i]
#sum_by_state$R_import <- NA
#for (i in 1:nrow(sum_by_state)){
#  sum_by_state$R_import[i] <- (sum_by_state$`Reported + Est. Pax`[i] * 
#    (sum_by_state$`Cumulated Incidence`[i] / sum_by_state$Pop_2019[i]) *
#      sum_by_state$`Reported + Est. Pax`[i] / sum_by_state$travelout[i])/
#    ((sum_by_state$sum_except_i[i]/sum_by_state$Pop_2019[i])*sum_by_state$sum_travel_except_i[i])
#}

sum_by_state$R_import <- NA
for (i in 1:nrow(sum_by_state)) {
  denominator <- (sum_by_state$sum_except_i[i] * sum_by_state$population_except_i[i]) * ifelse(sum_by_state$sum_travel_except_i[i] == 0, 1,sum_by_state$sum_travel_except_i[i])
  #denominator <- ifelse(denominator == 0, 1, denominator)
  
  sum_by_state$R_import[i] <- (sum_by_state$Reported.Est..Pax[i] * 
                                 (sum_by_state$Dengue_Cases_MD[i] * sum_by_state$Population_Density[i]) *
                                 (sum_by_state$Reported.Est..Pax[i] / sum_by_state$travelout[i])) / denominator
}

shifted_data <- sum(sum_by_state$R_import,na.rm=T)
scaled_data <- (sum_by_state$R_import/ shifted_data)
sum_by_state$normalized_R_import <- scaled_data

results_list[[country]] <- sum_by_state
}

final_results_df <- do.call(rbind, results_list)
#write_csv(final_results_df, "Final/Risk_import_TP&Pop&TravelFlux_v2.csv")

final_results_df %>% 
  group_by(Date,Dest.Country,Orig.Country) %>% 
  summarise(rr = sum(normalized_R_import)) %>% 
  ggplot() +
  geom_col(aes(x=Date,y=rr,fill=Dest.Country),alpha=0.6)+
  facet_wrap( ~ Orig.Country)+
  labs()
  #geom_col(aes(x=Date, y=Reported.Est..Pax),fill="turquoise")


sum_by_state %>% 
  group_by(Date,Dest.Country) %>% 
  summarise(tt = sum(Reported.Est..Pax)) %>% 
  ggplot() +
  geom_col(aes(x=Date,y=tt,fill=Dest.Country),alpha=0.6)

sum_by_state %>% 
  group_by(Date,State_Orig_airport) %>% 
  summarise(kk = sum(Dengue_Cases_MD,na.rm=T)) %>% 
  ggplot() +
  geom_col(aes(x=Date,y=kk,fill=State_Orig_airport),alpha=0.6)

#### Save it for all the countries 
#Risk_Importation_HighIncidence <- sum_by_state

Risk_Importation_HighIncidence <- rbind(Risk_Importation_HighIncidence,sum_by_state)


#write_csv(Risk_Importation_HighIncidence,"Final/Risk_Importation_usingTP_2019.csv")



#### ###################################################################
#######################################################################
####  ###################### AFRICAN TRAVELS ###########################
### ####################################################################
#### ####################################################################
### Reading in the travel data 



results_list_Africa <- list()

for (country in African_countries) {

Africa.Travel.data <- fread("Data/Travel Data/Africa_Travels_2020.csv")
Africa.Travel.data$Date <- as.Date(Africa.Travel.data$Date)

Africa.Travel.data$Dest.Country[Africa.Travel.data$Dest.Country == "Ivory Coast (Cote d'Ivoire)"] <- "Ivory Coast"
Africa.Travel.data$Orig.Country[Africa.Travel.data$Orig.Country == "Ivory Coast (Cote d'Ivoire)"] <- "Ivory Coast"
Africa.Travel.data$Orig.Country[Africa.Travel.data$Orig.Country == "Cape Verde"] <- "Cabo Verde"
Africa.Travel.data$Orig.Country[Africa.Travel.data$Orig.Country == "Cape Verde"] <- "Cabo Verde"
Africa.Travel.data$Orig.Country[Africa.Travel.data$Orig.Country == "Reunion"] <- "Réunion"
Africa.Travel.data$Orig.Country[Africa.Travel.data$Orig.Country == "Reunion"] <- "Réunion"


### Removing the same country travels as you want to compute the probability of travelling internationally, 
### you want to exclude the local flights 
### Then you Aggregate the travels from each specific state
## Change accordingly based on the Country you are working with 
Ind.travel.2019 <- Africa.Travel.data %>% 
  filter(Orig.Country == country & Dest.Country!= country ) %>% #removing domestic flights 
  group_by(Orig.Country,Date) %>%  ## you want to aggregate travels per state and date (in this case month) 
  mutate(travelout = sum(Reported.Est..Pax)) %>% 
  filter(Dest.Country %in% African_countries) ## Lastly keep the destination just to minimize the dataset as you already captured what you need 

#### Loading the Dengue data set that was compiled
###  we propagated accordinglly based on population and transmission potentials estimates
### Dengue Cases for 2019

#ind.state.cases <- fread("Data/India/Monthly_Statewise_India_cases.csv")
ind.state.cases_all <- fread("Final/Extracted_Africa_TP_withLag_2019.csv")
ind.state.cases_all <- ind.state.cases_all[,c(1,2,4:15)]

## Filter accordingly with regards to the country you are working with 
ind.state.cases <- ind.state.cases_all %>% 
  filter(COUNTRY == country)

### converting the dataframe into a longer format to be able to combine with the travel data and run the metric 
ind.state.cases.long <- pivot_longer(ind.state.cases, cols = (3:14), names_to = "Date", values_to = "Dengue_Cases_MD")
ind.state.cases.long$Date <- as.Date(ind.state.cases.long$Date)

## if you are working with country, you want to add all the values per district 
ind.state.cases.long <- ind.state.cases.long %>% 
  group_by(COUNTRY,Date) %>% 
  summarise(Dengue_Cases_C = sum(Dengue_Cases_MD),
            Population_C = sum(Population))

## Joining Cases to Travels 
ind.state.case.travel <- Ind.travel.2019 %>% 
  left_join(ind.state.cases.long, by= c("Date"="Date", "Orig.Country"="COUNTRY"))

## Computing the cumulated incidence - Cases/Population 
ind.state.case.travel$cumul_pop <- ind.state.case.travel$Dengue_Cases_C/ind.state.case.travel$Population_C


##Adding column of sum for except [i]
library(purrr)
library(dplyr)
library(tibble)
# Assuming 'cumul_pop' is the name of the column you want to sum
# Calculate the sum of cumul_pop for each State_Orig_airport
state_cumul_pop_sum <- ind.state.case.travel %>%
  group_by(Date,Orig.Country) %>%
  summarise(total_cumul_pop = mean(Dengue_Cases_C,na.rm=TRUE)) 

state_cumul_pop_sum <- state_cumul_pop_sum %>% 
  group_by(Date) %>% 
  summarise(tot2 = sum(total_cumul_pop , na.rm = TRUE) )

# Join the total_cumul_pop to the original dataset
sum_by_state <- ind.state.case.travel %>%
  left_join(state_cumul_pop_sum, by = c("Date")) %>%
  group_by(Date, Orig.Country) %>%
  mutate(sum_except_i = tot2 - Dengue_Cases_C) %>%
  ungroup() %>% 
  dplyr::select(-tot2)

# Do the same for travel
# Calculate the sum of cumul_pop for each State_Orig_airport

state_travel_sum <- ind.state.case.travel %>% 
  group_by(Dest.Country,Orig.Country, Date) %>%
  summarise(Total_Travels = sum(Reported.Est..Pax))

state_travel_sum <- state_travel_sum %>%
  group_by(Dest.Country,Date) %>%
  summarise(Total_state_travel = sum(Total_Travels,na.rm=TRUE)) 

# Join the total_cumul_pop to the original dataset
sum_by_state <- sum_by_state %>%
  left_join(state_travel_sum, by = c("Date","Dest.Country")) %>%
  group_by(Date, Orig.Country) %>%
  mutate(sum_travel_except_i = Total_state_travel - Reported.Est..Pax) %>%
  ungroup() %>% 
  dplyr::select(-Total_state_travel)



## Risk of Importation into African Countries  ########
## Travel Influx - 
## Risk of case importation 
# travel influx: travel_in_month_us[i]
# cumulative incidence: cum_in[i]
# probability of traveling from 𝑖 to : travel_in_month_us[i]/travel_in_month_all[i]
#sum_by_state$R_import <- NA
#for (i in 1:nrow(sum_by_state)){
#  sum_by_state$R_import[i] <- (sum_by_state$`Reported + Est. Pax`[i] * 
#    (sum_by_state$`Cumulated Incidence`[i] / sum_by_state$Pop_2019[i]) *
#      sum_by_state$`Reported + Est. Pax`[i] / sum_by_state$travelout[i])/
#    ((sum_by_state$sum_except_i[i]/sum_by_state$Pop_2019[i])*sum_by_state$sum_travel_except_i[i])
#}

sum_by_state$R_import <- NA
for (i in 1:nrow(sum_by_state)) {
  denominator <- (sum_by_state$sum_except_i[i] / sum_by_state$Population_C[i]) * sum_by_state$sum_travel_except_i[i]
  denominator <- ifelse(denominator == 0, 1, denominator)
  
  sum_by_state$R_import[i] <- (sum_by_state$Reported.Est..Pax[i] * 
                                 (sum_by_state$Dengue_Cases_C[i] / sum_by_state$Population_C[i]) *
                                 sum_by_state$Reported.Est..Pax[i] / sum_by_state$travelout[i]) / denominator
}

shifted_data <- sum(sum_by_state$R_import,na.rm=T)
scaled_data <- (sum_by_state$R_import/ shifted_data)
sum_by_state$normalized_R_import <- scaled_data

results_list_Africa[[country]] <- sum_by_state
}

final_results_Africa_df <- do.call(rbind, results_list_Africa)
write_csv(final_results_Africa_df, "Final/Risk_import_TP&Pop&TravelFlux_AFRICA.csv")


final_results_Africa_df %>% 
  group_by(Date,Dest.Country,Orig.Country) %>% 
  summarise(rr = sum(normalized_R_import)) %>% 
  ggplot() +
  geom_col(aes(x=Date,y=rr,fill=Dest.Country),alpha=0.6)+
  facet_wrap( ~ Orig.Country)+
  labs()
#geom_col(aes(x=Date, y=Reported.Est..Pax),fill="turquoise")









