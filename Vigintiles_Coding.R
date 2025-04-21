library(ineq)### firstly read the library called ineq for calculating the income inequality
library(readxl)### to read the excel file
library(dplyr)
library(tidyr)
library(ggplot2)#### read library ggplot2 to produce plots regarding income distribution









Up_OECD <- read_excel ('Vigintiles.xlsx')
OECD_vigintile <- pivot_longer(Up_OECD,
                               cols= c(Australia, Austria, Belgium, Canada, Colombia,Denmark,Estonia, Finland,France,
                                       Germany, Greece, Hungary, Iceland, Israel, Italy, Japan, Korea, Latvia, Lithuania, Netherlands, New_Zealand,
                                       Portugal, Slovakia, Slovenia, Sweden, Turkey,UK, USA, Switzerland, Poland, Czech, Norway, Luxembourg, Spain, Mexico, Chile, Costa_Rica,Ireland), 
                               names_to = "Country", 
                               values_to = "postTaxIncome")
head(OECD_vigintile)
OECD_vigintile$postTaxIncome <- as.numeric(OECD_vigintile$postTaxIncome)

str(OECD_vigintile)
OECD_vigintiles <- data.frame(lapply(OECD_vigintile, function(x) if(is.numeric(x)) round(x, 4) else x))
head(OECD_vigintiles)

p90p100 <- filter(OECD_vigintile, Vigintiles %in% c('p90p95','p95p100'))
head(p90p100,20)

# Ensure Vigintiles column is character
OECD_vigintiles <- OECD_vigintiles %>%
  mutate(Vigintiles = as.character(Vigintiles))


mapping_vigintiles <-  c('p0p5' = '1st', 
                         'p5p10' = '2nd', 
                         'p10p15' = '3rd',
                         'p15p20' = '4th', 
                         'p20p25' = '5th', 
                         'p25p30' = '6th', 
                         'p30p35' = '7th',
                         'p35p40' = '8th', 
                         'p40p45' = '9th', 
                         'p45p50' = '10th',
                         'p50p55' = '11th', 
                         'p55p60' = '12th', 
                         'p60p65'='13th',
                         'p65p70' = '14th',
                         'p70p75'='15th',
                         'p75p80'='16th', 
                         'p80p85'='17th',
                         'p85p90'='18th',
                         'p90p95'='19th',
                         'p95p100'='20th')

# Ensure that the Vigintiles column is a character (or factor) before mapping
OECD_vigintiles <- OECD_vigintiles %>%
  mutate(Vigintiles = mapping_vigintiles[Vigintiles])

head(OECD_vigintiles)

vigintile_11 <- filter(OECD_vigintiles, Vigintiles == '11th')
head(vigintile_11)
country_to_regions_vigintiles <- c(
  'Canada' = 'North_America',
  'Mexico' = 'North_America',
  'USA' = 'North_America',
  "Chile" = "Latin_America",
  "Colombia" = "Latin_America",
  'Austria' = 'Central_Northern_Europe',
  "Belgium" = "Western_Europe",
  "France" = "Western_Europe",
  "Germany" = "Central_Northern_Europe",
  "Ireland" = "Western_Europe",
  "Luxembourg" = "Western_Europe",
  "Netherlands" = "Western_Europe",
  "Switzerland" = "Central_Northern_Europe",
  "UK" = "Western_Europe",
  "Denmark" = "Central_Northern_Europe",
  "Finland" = "Central_Northern_Europe",
  "Iceland" = "Central_Northern_Europe",
  "Norway" = "Central_Northern_Europe",
  "Sweden" = "Central_Northern_Europe",
  "Greece" = "South/Eastern_Europe",
  "Italy" = "South/Eastern_Europe",
  "Portugal" = "Western_Europe",
  "Spain" = "Western_Europe",
  "Czech" = "Central_Northern_Europe",
  "Estonia" = "South/Eastern_Europe",
  "Hungary" = "South/Eastern_Europe",
  "Latvia" = "South/Eastern_Europe",
  "Lithuania" = "South/Eastern_Europe",
  "Poland" = "South/Eastern_Europe",
  "Slovakia" = "South/Eastern_Europe",
  "Slovenia" = "South/Eastern_Europe",
  "Australia" = "Asia_Pacific",
  "Japan" = "Asia_Pacific",
  "Korea" = "Asia_Pacific",
  "New_Zealand" = "Asia_Pacific",
  "Israel" = "Middle_East",
  "Turkey" = "Middle_East",
  "Costa_Rica"= "Latin_America"
)
OECD_vigintiles <- OECD_vigintiles %>%
  mutate(geographical_region = country_to_regions_vigintiles[as.character(OECD_vigintiles$Country)])
head(OECD_vigintiles)
# Convert Percentile to a factor with the correct order
OECD_vigintiles$Vigintiles<- factor(OECD_vigintiles$Vigintiles, 
                                    levels = c('1st', '2nd', '3rd', '4th', '5th', 
                                               '6th', '7th', '8th', '9th', '10th','11th','12th','13th','14th','15th','16th','17th','18th','19th','20th'), 
                                    ordered = TRUE)
head(OECD_vigintiles)
dim(OECD_vigintiles)
####Exploratory Data Analysis
ggplot(OECD_vigintiles, aes(x= Country,y=postTaxIncome, fill= Country)) +
  geom_boxplot() +
  labs(title = "Post-tax National Income Distribution by Country",
       x= "Country",
       y= "Post-tax  National Income")+
  theme_minimal()+
  theme(plot.title = element_text(size = 15, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.y= element_text(size=10, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size= 12),
        legend.position = "none")

denmark_incomeshare <- filter(OECD_vigintiles, Country =='Denmark')
head(denmark_incomeshare)

vigintile_sums <- denmark_incomeshare %>%
  group_by(Vigintiles) %>%
  summarize(total_postTaxIncome = sum(postTaxIncome))

# Print the result
print(vigintile_sums)

ggplot(denmark_incomeshare, aes(x = postTaxIncome)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  labs(title = "Distribution of Income Share_Across all analysed years\nDenmark",
       x = "Income Share",
       y = "Frequency") +
  theme_minimal()+
  theme(plot.title = element_text(face='bold', hjust=0.5))

# Plotting the histogram
ggplot(denmark_incomeshare, aes(x = factor(Vigintiles), y = postTaxIncome)) +
  geom_bar(stat = "identity", fill = "blue", color = NA) +
  labs(x = "Vigintile", y = "Income Share Per Vigintile ($2023,PPP)", 
       title = "Income Distribution Across Vigintiles_Across all analysed years\nDenmark") +
  theme_minimal()+
  theme(plot.title = element_text(face='bold', hjust=0.5),
        axis.title.x = element_text(face='bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y= element_text(face= 'bold'),
        axis.text.x = element_text(face= 'bold'))

denmark_incomeshare_2022<- filter(OECD_vigintiles, Country =='Denmark' & Year==2022)

ggplot(denmark_incomeshare_2022, aes(x = factor(Vigintiles), y = postTaxIncome)) +
  geom_bar(stat = "identity", fill = "blue", color = NA) +
  labs(x = "Vigintile (1st to 20th)", y = "Frequency", 
       title = "Income Distribution Across Vigintiles_2022\nDenmark") +
  theme_minimal()+
  theme(plot.title = element_text(face='bold', hjust=0.5)
        )
summary(OECD_vigintiles$postTaxIncome)

australia <- filter(OECD_vigintiles, Country =='Australia')
summary(australia$postTaxIncome)
austria <- filter(OECD_vigintiles, Country =='Austria')
summary(austria$postTaxIncome)
belgium <- filter(OECD_vigintiles, Country =='Belgium')
summary(belgium$postTaxIncome)
canada<- filter(OECD_vigintiles, Country =='Canada')
summary(canada$postTaxIncome)
chile <- filter(OECD_vigintiles, Country =='Chile')
summary(chile$postTaxIncome)
colombia <- filter(OECD_vigintiles, Country == 'Colombia')
summary(colombia$postTaxIncome)
costarica <- filter(OECD_vigintiles, Country =='Costa_Rica')
summary(costarica$postTaxIncome)
czech <- filter(OECD_vigintiles, Country =='Czech')
summary(czech$postTaxIncome)
denmark <- filter(OECD_vigintiles, Country =='Denmark')
summary(denmark$postTaxIncome)
estonia <- filter(OECD_vigintiles, Country =='Estonia')
summary(estonia$postTaxIncome)
finland<- filter(OECD_vigintiles, Country =='Finland')
summary(finland$postTaxIncome)
france<- filter(OECD_vigintiles, Country =='France')
summary(france$postTaxIncome)
germany<- filter(OECD_vigintiles, Country =='Germany')
summary(germany$postTaxIncome)
greece<- filter(OECD_vigintiles, Country =='Greece')
summary(greece$postTaxIncome)
hungary<- filter(OECD_vigintiles, Country =='Hungary')
summary(hungary$postTaxIncome)
iceland<- filter(OECD_vigintiles, Country =='Iceland')
summary(iceland$postTaxIncome)
ireland<- filter(OECD_vigintiles, Country =='Ireland')
summary(ireland$postTaxIncome)
israel<- filter(OECD_vigintiles, Country =='Israel')
summary(israel$postTaxIncome)
italy<- filter(OECD_vigintiles, Country =='Italy')
summary(italy$postTaxIncome)
japan<- filter(OECD_vigintiles, Country =='Japan')
summary(japan$postTaxIncome)
korea<- filter(OECD_vigintiles, Country =='Korea')
summary(korea$postTaxIncome)
latvia<- filter(OECD_vigintiles, Country =='Latvia')
summary(latvia$postTaxIncome)
lithuania<- filter(OECD_vigintiles, Country =='Lithuania')
summary(lithuania$postTaxIncome)
luxembourg<- filter(OECD_vigintiles, Country =='Luxembourg')
summary(luxembourg$postTaxIncome)
luxembourg<- filter(OECD_vigintiles, Country =='Luxembourg')
summary(luxembourg$postTaxIncome)
mexico<- filter(OECD_vigintiles, Country =='Mexico')
summary(mexico$postTaxIncome)
netherland<- filter(OECD_vigintiles, Country =='Netherlands')
summary(netherland$postTaxIncome)
newzealand<- filter(OECD_vigintiles, Country =='New_Zealand')
summary(newzealand$postTaxIncome)
norway<- filter(OECD_vigintiles, Country =='Norway')
summary(norway$postTaxIncome)
poland<- filter(OECD_vigintiles, Country =='Poland')
summary(poland$postTaxIncome)
portugal<- filter(OECD_vigintiles, Country =='Portugal')
summary(portugal$postTaxIncome)
slovakia<- filter(OECD_vigintiles, Country =='Slovakia')
summary(slovakia$postTaxIncome)
slovenia<- filter(OECD_vigintiles, Country =='Slovenia')
summary(slovenia$postTaxIncome)
spain<- filter(OECD_vigintiles, Country =='Spain')
summary(spain$postTaxIncome)
sweden<- filter(OECD_vigintiles, Country =='Sweden')
summary(sweden$postTaxIncome)
switzerland<- filter(OECD_vigintiles, Country =='Switzerland')
summary(switzerland$postTaxIncome)
Turkey<- filter(OECD_vigintiles, Country =='Turkey')
summary(Turkey$postTaxIncome)
uk<- filter(OECD_vigintiles, Country =='UK')
summary(uk$postTaxIncome)
usa<- filter(OECD_vigintiles, Country =='USA')
summary(usa$postTaxIncome)
# Basic Scatter Plot with Year on the X-axis and Post-tax Income on the Y-axis
ggplot(OECD_vigintiles, aes(x = Year, y = postTaxIncome, color = as.factor(Vigintiles))) +
  geom_point(alpha = 0.6) +  # Adjust alpha for transparency if points overlap
  labs(title = "Post-tax Income Over Time by Vigintile",
       x = "Year",
       y = "Post-tax Income",
       color = "Vigintile") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  facet_wrap(~ Country, scales = "free_y") 



# Define the function to calculate Gini from a dataframe
Gini_from_vigintiles_OECD <- function(OECD_vigintiles) {
  # Ensure df is sorted by decile
  df <- OECD_vigintiles[order(OECD_vigintiles$Vigintiles), ]
  
  # Calculate cumulative shares
  cumulative_population <- seq(0.05, 1, by = 0.05)  # vigintiles from 10% to 100%
  cumulative_income <- cumsum(OECD_vigintiles$postTaxIncome)
  
  # Calculate the area under the Lorenz curve
  B <- sum((cumulative_income[-1] + cumulative_income[-length(cumulative_income)]) * diff(cumulative_population)) / 2
  G <- 1 - 2 * B
  return(G)
}
Gini_results_vigintiles<- OECD_vigintiles %>%
  group_by(Country, Year, geographical_region) %>%
  summarize(Gini = Gini_from_vigintiles_OECD(cur_data()))

print(Gini_results_vigintiles)
mean <- mean(Gini_results_vigintiles$Gini)
print(mean)

turkey_Gini_2022 <- filter(Gini_results_vigintiles, Country=='Turkey' & Year == 2022)
print(turkey_Gini_2022)
#####################################################
###Gini for all OECD Members
ggplot(Gini_results_vigintiles, aes(x = Year, y = Gini, color = Country, group= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Gini Coefficient by Country (1980-2022) \n 38 OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  ylim(0, max(Gini_results_vigintiles$Gini) * 1.1) +
  theme(plot.title = element_text(size = 20, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y= element_text(size=10, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold')) +
  scale_linetype_manual(values = rep(1:3, length.out = 38))# Repeat linetypes if needed

gini_2022 <- filter(Gini_results_vigintiles, Year==2022)
gini_1 <- filter(gini_2022, Country =='USA')
print(gini_1)

head(gini_2022,20)
overall_average_Gini <- mean(gini_2022$Gini)
print(overall_average_Gini)

uk <- filter(Gini_results_vigintiles, Country == 'UK')
head(uk, 20)

overall_average_gini_2022 <- 0.37

# Create a data frame for the overall average
average_data <- data.frame(
  Country = "OECD_Average",
  Gini = overall_average_gini_2022
)

# Combine country data with overall average data
combined_data <- rbind(gini_2022, average_data)

# Assign colors: one for the average, another for the rest
combined_data$Color <- ifelse(combined_data$Country == "OECD_Average", "red", "lightblue")

# Create the bar plot
ggplot(combined_data, aes(x = reorder(Country, Gini), y = Gini, fill = Color)) +
  geom_bar(stat = "identity", color = "black", width=1) +
  scale_fill_identity() +
  labs(title = "Gini Coefficients by Country and OECD average in 2022",
       x = "Country",
       y = "Gini Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold',size =11),
        axis.title.x = element_text(face='bold'),
        axis.title.y = element_text(face='bold'),
        axis.text.y = element_text(face='bold'),
        plot.title = element_text(face='bold'))  # Ro







Gini_ireland <- filter(Gini_results_vigintiles, Country == 'Ireland', Year %in% c(2021, 2022))
print(Gini_ireland)
###Only for 4 years
Gini_all_4 <- filter(Gini_results_vigintiles, Year %in% c(1980, 2000, 2020,2022))

ggplot(Gini_all_4, aes(x = Year, y = Gini, color = Country, group= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Gini Coefficient by Country (1980,2000,2020,2022) \n 38 OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  ylim(0, max(Gini_results_vigintiles$Gini) * 1.1) +
  theme(plot.title = element_text(size = 20, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y= element_text(size=10, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold')) +
  scale_linetype_manual(values = rep(1:3, length.out = 38))# Repeat linetypes if needed 

# Specify the year you are interested in
specific_year <- 2022

# Filter the data frame for the specific year
filtered_df <- subset(Gini_all_4, Year == specific_year)

# Find the country with the lowest Gini value
lowest_gini_country <- filtered_df[which.min(filtered_df$Gini), ]

# Print the result
print(lowest_gini_country)

#########Gini Plots based on regional of OECD Members
#####################################################
####for all Europe countries
Europe_Gini <- filter(Gini_results_vigintiles, geographical_region== 'Europe')
head(Europe_Gini,20)
Europe_unique_countries <- unique(Europe_Gini$Country)
Europe_Countries <- length(Europe_unique_countries)
print(Europe_Countries)


color_palette_Europe <- scales::hue_pal()(26)
point_shapes_Europe <- rep(c(16:20), length.out = 26)

Europe_Gini$Year <- as.numeric(Europe_Gini$Year)
# Create the plot for the country Europe
ggplot(Europe_Gini, aes(x = Year, y = Gini, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Gini Coefficient by Country (1980-2022) \nEuropean Countries_OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  theme(plot.title = element_text(size = 20, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y= element_text(size=10, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold')) +
  scale_color_manual(values = color_palette_Europe) +
  scale_linetype_manual(values = rep(1:3, length.out = 26))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_Europe)








###picking up data for South/East
South_East_Gini <- filter(Gini_results_vigintiles, geographical_region== 'South/Eastern_Europe')
head(South_East_Gini,20)
SE_unique_countries <- unique(South_East_Gini$Country)
SE_Countries <- length(SE_unique_countries)
print(SE_Countries)

SE_colors <- c(
  "Estonia" = "red",
  "Greece" = "blue",
  "Hungary" = "green",
  "Italy" = "purple",
  "Latvia" = "orange",
  "Lithuania" = "black",
  "Poland"= "darkgreen",
  "Slovakia" = "darkred",
  "Slovenia" = "#7a7243")


# Define a color palette with distinct colors
#color_palette_SE <- scales::hue_pal()(9)
point_shapes_SE <- rep(c(16:18), length.out = 9)
South_East_Gini$Year <- as.numeric(South_East_Gini$Year)

# Create the plot for the country South/ East
ggplot(South_East_Gini, aes(x = Year, y = Gini, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Gini Coefficient by Country (1980-2022) \nSouthern/Eastern_Europe OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 16, face='bold'),
        axis.title.y = element_text(size = 16, face='bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size = 15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines')) +
  scale_color_manual(values = SE_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 9))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_SE)

#install.packages('gganimate')
#install.packages('gapminder')
#install.packages('tweenr')

#library(gganimate)
#library(gapminder)
#library(tweenr)


#p<- ggplot(South_East_Gini, aes(x = Year, y = Gini, color = Country, linetype = Country, group = Country, shape= Country)) +
  #geom_point(size = 2.5) +
  #geom_line() +
  labs(title = "Gini Coefficient by Country (1980-2022) \nSouthern/Eastern_Europe OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 16, face='bold'),
        axis.title.y = element_text(size = 16, face='bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size = 15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines')) +
  scale_color_manual(values = SE_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 9))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_SE)



#p_anim <- p +
  transition_reveal(Year) +
  ease_aes('linear') +  # Smooth transitions
  labs(title = 'Gini Coefficient Over Time: {frame_along}',  # Add a title that updates with the animation
       x = 'Year', y = 'Gini Coefficient')

# Show the animation
animate(p_anim, nframes = 100, fps = 10)


# Animate and save
animate(p_anim, nframes = 100, fps = 10)
anim_save("gini_animation.gif", animation = p_anim, nframes = 100, fps = 10)






estonia <- filter(South_East_Gini, Country == 'Estonia' & Year >= 1996 & Year <= 2005)
print(estonia)
min_estonia <- min(estonia$Gini)
max_estonia <- max(estonia$Gini)
print(min_estonia)
print(max_estonia)

hungary <- filter(South_East_Gini, Country == 'Hungary' & Year >= 1980 & Year <= 1988)
print(hungary)
min_hungary <- min(hungary$Gini)
max_hungary <- max(hungary$Gini)
print(min_hungary)
print(max_hungary)

greece <- filter(South_East_Gini, Country =='Greece' & Year >= 1980 & Year <= 1990)
print(greece)
min_greece <- min(greece$Gini)
max_greece <- max(greece$Gini)
print(min_greece)
print(max_greece)

Lithuania <- filter(South_East_Gini, Country == 'Lithuania', Year==1980)
print(Lithuania)

slovenia <- filter(South_East_Gini, Country =='Slovenia' & Year >= 2014 & Year <= 2022)
print(slovenia)
min_slovenia <- min(slovenia$Gini)
max_slovenia <- max(slovenia$Gini)
print(min_slovenia)
print(max_slovenia)

italy <- filter(South_East_Gini, Country =='Italy' & Year %in% c(2021,2022))
print(italy)
min_italy <- min(italy$Gini)
max_italy <- max(italy$Gini)
print(min_italy)
print(max_italy)


################################################################################

###For Central and Northern EU
# Define a color palette
CN_colors <- c(
  "Austria" = "red",
  "Czech" = "blue",
  "Denmark" = "green",
  "Finland" = "purple",
  "Germany" = "orange",
  "Iceland" = "black",
  "Norway"= "darkgreen",
  "Sweden" = "darkred",
  "Switzerland" = "#7a7243")

CN_shape <- c(
  "Austria" = 16,
  "Czech" = 16,
  "Denmark" = 16,
  "Finland" = 16,
  "Germany" = 18,
  "Iceland" = 18,
  "Norway"= 18,
  "Sweden" = 18,
  "Switzerland" = 18
)

central_Northern_Gini <- Gini_results_vigintiles %>% 
  filter(geographical_region %in% c("Central_Northern_Europe"))
head(central_Northern_Gini,20)
CN_unique_countries <- unique(central_Northern_Gini$Country)
CN_Countries <- length(CN_unique_countries)
print(CN_Countries)

# Define a color palette with distinct colors
#color_palette_CN <- scales::hue_pal()(9)
point_shapes_CN<- rep(c(16:18), length.out = 9)
central_Northern_Gini$Year <- as.numeric(central_Northern_Gini$Year)

# Create the plot for the country Western/ Central 
ggplot(central_Northern_Gini, aes(x = Year, y = Gini, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Gini Coefficient by Country (1980-2022)\nCentral/Northern_Europe_OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 16, face='bold'),
        axis.title.y = element_text(size = 16, face ='bold'),
        axis.text.y= element_text(face= 'bold', size=15),
        axis.text.x = element_text(angle = 45, hjust = 1,face='bold', size=15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines')) +
  scale_color_manual(values = CN_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 9))+ # Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_CN)

germany <- filter(central_Northern_Gini, Country =='Germany' & Year >= 1999 & Year <= 2020)
min_germany <-min(germany$Gini)
max_germany <- max(germany$Gini)
print(min_germany)
print(max_germany)

iceland <- filter(central_Northern_Gini, Country =='Iceland' & Year >= 1980 & Year <= 1997)
min_iceland <- min(iceland$Gini)
max_iceland <- max(iceland$Gini)
print(min_iceland)
print(max_iceland)

austria <- filter(central_Northern_Gini, Country == 'Austria')
min_austria <- min(austria$Gini)
max_austria <- max(austria$Gini)
print(min_austria)
print(max_austria)

czech <- filter(central_Northern_Gini, Country == 'Czech' & Year ==1999)
print(czech)
min_czech <- min(czech$Gini)
max_czech <- max(czech$Gini)
print(min_czech)
print(max_czech)
################################################################################
###For North and Latin America

# Define a color palette
NL_colors <- c(
  "Canada" = "red",
  "Chile" = "blue",
  "Colombia" = "green",
  "Costa_Rica" = "purple",
  "Mexico" = "orange",
  "USA" = "black")

NL_America_Gini <- Gini_results_vigintiles %>% 
  filter(geographical_region%in% c('North_America','Latin_America'))
head(NL_America_Gini,20)
NLA_unique_countries <- unique(NL_America_Gini$Country)
NLA_Countries <- length(NLA_unique_countries)
print(NLA_Countries)

# Define a color palette with distinct colors
point_shapes_NLA<- rep(c(16:18), length.out = 6)
NL_America_Gini$Year <- as.numeric(NL_America_Gini$Year)
head(NL_America_Gini)

# Create the plot for the country Western/ Central 

ggplot(NL_America_Gini, aes(x = Year, y = Gini, color = Country, linetype = Country, group = interaction(Country))) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Gini Coefficient by Country (1980-2022)\nAmerican Countries_OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 16, face='bold'),
        axis.title.y = element_text(size = 16, face ='bold'),
        axis.text.y= element_text(face= 'bold', size=15),
        axis.text.x = element_text(angle = 45, hjust = 1, face='bold', size=15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16),
        legend.key.size = unit(1.5, 'lines')) +
  scale_color_manual(values = NL_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 6)) + 
  scale_shape_manual(values = point_shapes_NLA)




group1_NL<- filter(NL_America_Gini, Country %in% c('Chile','Colombia','Costa_Rica','Mexico'))
min_value_goup1_NL <- min(group1_NL$Gini)
max_value_group1_NL <- max(group1_NL$Gini)
print(min_value_goup1_NL)
print(max_value_group1_NL)
colombia_gini <- filter(NL_America_Gini, Country=='Colombia')
min_value_colombia <- min(colombia_gini$Gini)
max_value_colombia <- max(colombia_gini$Gini)
print(min_value_colombia)
print(max_value_colombia)
head(colombia_gini,20)
Col_2012_2016 <- filter(colombia_gini, Year %in% c(2012,2013,2014,2015,2016))
head(Col_2012_2016)
col_1980_1992 <- filter(colombia_gini,Year %in% c(1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992))
head(col_1980_1992,13)
colombia_gini <- filter(NL_America_Gini,Country=='Colombia', Year>=2012 & Year <= 2016)
print(colombia_gini)

CR_gini <- filter(NL_America_Gini, Country =='Costa_Rica')
head(CR_gini)
CR_2020_2022 <- filter(CR_gini,Year %in% c(2020,2021,2022))
head(CR_2020_2022)

Chile_gini <- filter(NL_America_Gini, Country =='Chile' & Year==2022)
head(Chile_gini)
min_value_chile <- min(Chile_gini$Gini)
max_value_chile <- max(Chile_gini$Gini)
print(min_value_chile)
print(max_value_chile)


mexico <- filter(NL_America_Gini, Country == 'Mexico' &Year %in% c(2020,2022))
head(mexico)
min_mexico <- min(mexico$Gini)
max_mexico <- max(mexico$Gini)
print(min_mexico)
print(max_mexico)
mexico_2022 <- filter(mexico, Year == 2022)
print(mexico_2022)
usa<- filter(NL_America_Gini, Country == 'USA', Year %in% c(2020,2021,2022))
head(usa)

canada <- filter(NL_America_Gini, Country =='Canada', Year %in% c(1980,2000,2020,2022))
head(canada)
#####################################################
####for Asia_Pacific and Middle East
asia_middle_east_Gini<- Gini_results_vigintiles%>% 
  filter(geographical_region %in% c("Asia_Pacific", "Middle_East"))

head(asia_middle_east_Gini,20)
AME_unique_countries <- unique(asia_middle_east_Gini$Country)
AME_Countries <- length(AME_unique_countries)
print(AME_Countries)

# Define a color palette
AME_colors <- c(
  "Australia" = "red",
  "Israel" = "blue",
  "Japan" = "green",
  "Korea" = "purple",
  "New_Zealand" = "orange",
  "Turkey" = "black")
 


# Define a color palette with distinct colors
#color_palette_AME<- scales::hue_pal()(6)
point_shapes_AME <- rep(c(16:18), length.out = 6)
asia_middle_east_Gini$Year <- as.numeric(asia_middle_east_Gini$Year)

# Create the plot for the country Asia_Pacific/ Middle_East 
ggplot(asia_middle_east_Gini, aes(x = Year, y = Gini, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Gini Coefficient by Country (1980-2022)\n Asia_Pacific/Middle_East OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold',hjust=0.5),
        axis.title.x = element_text(size = 16, face='bold'),
        axis.title.y = element_text(size = 16, face='bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face='bold', size= 15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines')) +
  scale_color_manual(values = AME_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 6))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_AME)

turkey <- filter(asia_middle_east_Gini, Country == 'Turkey')
min_turkey <- min(turkey$Gini)
max_turkey <- max(turkey$Gini)
print(min_turkey)
print(max_turkey)

AME_1980 <- filter(asia_middle_east_Gini, Year==2020)
min_AME <- min(AME_1980$Gini)
max_AME <- max(AME_1980$Gini)
print(min_AME)
print(max_AME)



Israel <- filter(asia_middle_east_Gini, Country == 'Israel')
min_Israel <- min(Israel$Gini)
max_Israel <- max(Israel$Gini)
print(min_Israel)
print(max_Israel)

australia <- filter(asia_middle_east_Gini, Country =='Australia')
min_australia <- min(australia$Gini)
max_australia <- max(australia$Gini)
print(min_australia)
print(max_australia)



################################################################################
####For Western_Europe

WE_Gini<- Gini_results_vigintiles %>% 
  filter(geographical_region== 'Western_Europe')

head(WE_Gini,20)
WE_unique_countries <- unique(WE_Gini$Country)
WE_Countries <- length(WE_unique_countries)
print(WE_Countries)

# Define a color palette
WE_colors <- c(
  "Belgium" = "red",
  "France" = "blue",
  "Ireland" = "green",
  "Luxembourg" = "purple",
  "Netherlands" = "orange",
  "Portugal" = "black",
  "Spain"= "darkgreen",
  "UK"= "darkred")


# Define a color palette with distinct colors
#color_palette_WE<- scales::hue_pal()(8)
point_shapes_WE <- rep(c(16:18), length.out = 8)
WE_Gini$Year<- as.numeric(WE_Gini$Year)

# Create the plot for the country WE
ggplot(WE_Gini, aes(x = Year, y = Gini, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Gini Coefficient by Country (1980-2022)\n Western Europe_OECD Members", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold', hjust=0.5),
        axis.title.x = element_text(size = 16, face='bold'),
        axis.title.y = element_text(size = 16, face='bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face='bold', size= 15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines')) +
  scale_color_manual(values = WE_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 8))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_WE)

Lu_2007 <- filter(WE_Gini, Country == 'Luxembourg', Year %in% c(2019,2020,2021,2022))
print(Lu_2007)

Ireland_2022 <- filter(WE_Gini, Country =='Ireland', Year==2022)
print(Ireland_2022)
##################################################################################################################

########Hoover Index

# Function to calculate the Hoover index from p and L values
hoover_index <- function(p_values, L_values) {
  distances <- p_values - L_values
  hoover_idx <- max(distances)
  max_distance_index <- which.max(distances)
  max_distance_p <- p_values[max_distance_index]
  
  return(list(hoover_idx = hoover_idx, max_distance_p = max_distance_p))
}

# List of countries and years
countries <- unique(OECD_vigintiles$Country)
years <- unique(OECD_vigintiles$Year)
geographical_region <- unique(OECD_vigintiles$geographical_region)

# Initialize an empty data frame to store the results
results <- data.frame(Country = character(), Year = integer(), geographical_region = character(),Hoover = numeric(), MaxDistanceP = numeric())

# Loop through each country and each year
for (country in countries) {
  for (year in years) {
    # Filter data for the current country and year
    income_data_filtered <- subset(OECD_vigintiles, Country == country & Year == year)
    
    # Calculate the Lorenz curve
    lorenz_curve <- ineq::Lc(income_data_filtered$postTaxIncome)
    
    # Extract p and L values from the Lorenz curve
    p_values <- lorenz_curve$p
    L_values <- lorenz_curve$L
    
    # Calculate Hoover index
    hoover_index_result <- hoover_index(p_values, L_values)
    hoover_index_value <- hoover_index_result$hoover_idx
    max_distance_p <- hoover_index_result$max_distance_p
    
    # Store the results
    results <- rbind(results, data.frame(Country = country, Year = year, geographical_region= geographical_region, Hoover = hoover_index_value, MaxDistanceP = max_distance_p))
  }
}
# Print the results
print(results)

head(results, 20)

Hoover_results <- data.frame(results)
head(Hoover_results,20)

# Create a named vector with the mapping
country_to_regions_Hoover <- c(
  'Canada' = 'North_America',
  'Mexico' = 'North_America',
  'USA' = 'North_America',
  "Chile" = "Latin_America",
  "Colombia" = "Latin_America",
  'Austria' = 'Central_Northern_Europe',
  "Belgium" = "Western_Europe",
  "France" = "Western_Europe",
  "Germany" = "Central_Northern_Europe",
  "Ireland" = "Western_Europe",
  "Luxembourg" = "Western_Europe",
  "Netherlands" = "Western_Europe",
  "Switzerland" = "Central_Northern_Europe",
  "UK" = "Western_Europe",
  "Denmark" = "Central_Northern_Europe",
  "Finland" = "Central_Northern_Europe",
  "Iceland" = "Central_Northern_Europe",
  "Norway" = "Central_Northern_Europe",
  "Sweden" = "Central_Northern_Europe",
  "Greece" = "South/Eastern_Europe",
  "Italy" = "South/Eastern_Europe",
  "Portugal" = "Western_Europe",
  "Spain" = "Western_Europe",
  "Czech" = "Central_Northern_Europe",
  "Estonia" = "South/Eastern_Europe",
  "Hungary" = "South/Eastern_Europe",
  "Latvia" = "South/Eastern_Europe",
  "Lithuania" = "South/Eastern_Europe",
  "Poland" = "South/Eastern_Europe",
  "Slovakia" = "South/Eastern_Europe",
  "Slovenia" = "South/Eastern_Europe",
  "Australia" = "Asia_Pacific",
  "Japan" = "Asia_Pacific",
  "Korea" = "Asia_Pacific",
  "New_Zealand" = "Asia_Pacific",
  "Israel" = "Middle_East",
  "Turkey" = "Middle_East",
  "Costa_Rica"= "Latin_America"
)

str(Hoover_results)
####Add the geography column to the dataframe
Hoover_results <- Hoover_results %>%
  mutate(geographical_region = country_to_regions_Hoover[as.character(Hoover_results$Country)])
head(Hoover_results)

#########################################################################################
###Time series of Hoover

#####################################################
#########Hoover Plots based on regional of OECD Members
#####################################################
###for all Europe
Europe_Hoover <- filter(Hoover_results, geographical_region== 'Europe')
head(Europe_Hoover,20)
color_palette_Europe <- scales::hue_pal()(26)
point_shapes_Europe <- rep(c(16:20), length.out = 26)

Europe_Hoover$Year <- as.numeric(Europe_Hoover$Year)

# Create the plot for the country Europe
ggplot(Europe_Hoover, aes(x = Year, y = Hoover, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Hoover Index by Country (1980-2022) \nEuropean Countries OECD Members", x = "Year", y = "Hoover Index") +
  theme_minimal() +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  theme(plot.title = element_text(size = 20, face = 'bold', hjust=0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y= element_text(size=10, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold')) +
  scale_color_manual(values = color_palette_Europe) +
  scale_linetype_manual(values = rep(1:3, length.out = 26))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_Europe)

####for selected 10 countries
selected_Hoover <- filter(Hoover_results, Country %in% c('Turkey','Chile','Colombia','Costa_Rica','Mexico','Spain','USA','Slovakia','Denmark','Sweden', 'Norway'))
#color_palette_Select <- scales::hue_pal()(10)
Select_colors <- c(
  "USA" = "red",
  "Chile" = "blue",
  "Colombia" = "green",
  "Costa_Rica" = "purple",
  "Mexico" = "orange",
  "Turkey" = "black",
  "Spain"= "darkgreen",
  "Slovakia" = "darkred",
  "Denmark" = "#7a7243",
  "Sweden" = "darkblue",
  "Norway"="yellow")

point_shapes_Select <- rep(c(16:18), length.out = 11)
selected_Hoover$Year<- as.numeric(selected_Hoover$Year)

# Create the plot for the selected 10 countries
ggplot(selected_Hoover, aes(x = Year, y = Hoover, color = Country, linetype = Country, group = Country, shape = Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Hoover Index by Country (1980-2022) \nSelected 10 Countries - OECD Members",
       x = "Year",
       y = "Hoover Index") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold', hjust = 0.5),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 15, face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold', size = 15),
        legend.text = element_text(size = 15),  # Increase legend text size
        legend.title = element_text(size = 16), # Increase legend title size
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", linewidth = 0.5),  # Update to `linewidth`
        panel.grid.minor = element_line(color = "gray90", linewidth = 0.25)) + # Update to `linewidth`
  scale_color_manual(values = Select_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 11)) + # Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_Select)

Den_Swe <- filter(selected_Hoover, Country %in% c('Denmark','Sweden'))

min_Swe <- min(Den_Swe$Hoover)
max_Swe <- max(Den_Swe$Hoover)
print(min_Swe)
print(max_Swe)

usa_hoover <- filter(selected_Hoover, Country %in% c('Sweden','Denmark') & Year==1980)
head(usa_hoover,20)

min_usa <- min(usa_hoover$Hoover)
max_usa <- max(usa_hoover$Hoover)
print(min_usa)
print(max_usa)


###picking up data for South/East
South_East_Hoover <- filter(Hoover_results, geographical_region== 'South/Eastern_Europe')
head(South_East_Hoover,20)
# Define a color palette with distinct colors
#color_palette_SE <- scales::hue_pal()(9)
point_shapes_SE <- rep(c(16:18), length.out = 9)
South_East_Hoover$Year<- as.numeric(South_East_Hoover$Year)

# Create the plot for the country South/ East
ggplot(South_East_Hoover, aes(x = Year, y = Hoover, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Hoover Index by Country (1980-2022) \nSouthern/Eastern_Europe OECD Members", x = "Year", y = "Hoover Index") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold', hjust=0.5),
        axis.title.x = element_text(size = 16, face ='bold'),
        axis.title.y = element_text(size = 16, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size=15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = SE_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 9))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_SE)

greece <- filter(South_East_Hoover, Country=='Greece', Year==2009)
print(greece)

lithuania <- filter(South_East_Hoover, Country == 'Lithuania', Year==2022)
print(lithuania)

Latvia <- filter(South_East_Hoover, Country =='Latvia', Year== 2005)
print(Latvia)

slovakia <- filter (South_East_Hoover, Country =='Slovakia', Year == 2022)
print(slovakia)

poland <- filter(South_East_Hoover, Country =='Poland', Year == 2022)
print(poland)
################################################################################

###For Central and Northern EU
central_Northern_Hoover <- Hoover_results %>% 
  filter(geographical_region %in% c("Central_Northern_Europe"))
head(central_Northern_Hoover,20)


# Define a color palette with distinct colors
#color_palette_CN <- scales::hue_pal()(9)
point_shapes_CN<- rep(c(16:18), length.out = 9)
central_Northern_Hoover$Year <- as.numeric(central_Northern_Hoover$Year)

# Create the plot for the country Western/ Central 
ggplot(central_Northern_Hoover, aes(x = Year, y = Hoover, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Hoover Index by Country (1980-2022)\nCentral/Northern_Europe_OECD Members", x = "Year", y = "Hoover Index") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold', hjust=0.5),
        axis.title.x = element_text(size = 16, face='bold'),
        axis.title.y = element_text(size = 16, face='bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1,face='bold', size=15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = CN_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 9))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_CN)

max_CN_hoover<- max(central_Northern_Hoover$Hoover)
min_CN_hoover <- min(central_Northern_Hoover$Hoover)
print(max_CN_hoover)
print(min_CN_hoover)

DNS<- filter(central_Northern_Hoover, Country %in% c('Denmark', 'Norway','Sweden'))
max_DNS_hoover<- max(DNS$MaxDistanceP)
min_DNS_hoover <- min(DNS$MaxDistanceP)
print(max_DNS_hoover)
print(min_DNS_hoover)

Finland<- filter(central_Northern_Hoover, Country =='Finland')
max_finland_hoover<- max(Finland$Hoover)
min_finland_hoover <- min(Finland$Hoover)
print(max_finland_hoover)
print(min_finland_hoover)

iceland <- filter(central_Northern_Hoover, Country =='Iceland', Year==2022)
print(iceland)
################################################################################
###For North and Latin America
NL_America_Hoover<- Hoover_results %>% 
  filter(geographical_region %in% c('North_America','Latin_America'))
head(NL_America_Hoover,20)


# Define a color palette with distinct colors
#color_palette_NL <- scales::hue_pal()(6)
point_shapes_NL<- rep(c(16:18), length.out = 6)
NL_America_Hoover$Year<- as.numeric(NL_America_Hoover$Year)

# Create the plot for the country Western/ Central 
ggplot(NL_America_Hoover, aes(x = Year, y = Hoover, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Hoover Index by Country (1980-2022)\n American Countries_OECD Members", x = "Year", y = "Hoover Index") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  theme(plot.title = element_text(size = 20, face = 'bold', hjust=0.5),
        axis.title.x = element_text(size = 16, face='bold'),
        axis.title.y = element_text(size = 16, face='bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size = 15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = NL_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 6))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_NL)

Can_USA <- filter(NL_America_Hoover, Country %in% c('Canada','USA'))
max_CUSA_hoover<- max(Can_USA$Hoover)
min_CUSA_hoover <- min(Can_USA$Hoover)
print(max_CUSA_hoover)
print(min_CUSA_hoover)

mexico_hoover <- filter(NL_America_Hoover, Country=='Mexico')
max_mexico_hoover<- max(mexico_hoover$Hoover)
min_mexico_hoover <- min(mexico_hoover$Hoover)
print(max_mexico_hoover)
print(min_mexico_hoover)

CR_hoover <- filter(NL_America_Hoover, Country=='Costa_Rica')
max_CR_hoover<- max(CR_hoover$Hoover)
min_CR_hoover <- min(CR_hoover$Hoover)
print(max_CR_hoover)
print(min_CR_hoover)

CC_hoover <- filter(NL_America_Hoover, Country %in% c('Chile','Colombia'))
max_cc_hoover<- max(CC_hoover$Hoover)
min_cc_hoover <- min(CC_hoover$Hoover)
print(max_cc_hoover)
print(min_cc_hoover)
####for Asia_Pacific and Middle East
asia_middle_east_Hoover<- Hoover_results %>% 
  filter(geographical_region %in% c("Asia_Pacific", "Middle_East"))

head(asia_middle_east_Hoover,20)


# Define a color palette with distinct colors
#color_palette_AM<- scales::hue_pal()(6)
point_shapes_AM <- rep(c(16:18), length.out = 6)
asia_middle_east_Hoover$Year <- as.numeric(asia_middle_east_Hoover$Year)

# Create the plot for the country Asia_Pacific/ Middle_East 
ggplot(asia_middle_east_Hoover, aes(x = Year, y = Hoover, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Hoover Index by Country (1980-2022) \nAsia_Pacific/Middle_East OECD Members", x = "Year", y = "Hoover Index") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  theme(plot.title = element_text(size = 20, face = 'bold', hjust=0.5),
        axis.title.x = element_text(size = 16, face ='bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face='bold', size = 15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = AME_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 6))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_AM)

turkey_hoover <- filter(asia_middle_east_Hoover, Country =='Turkey')
max_turkey_hoover<- max(turkey_hoover$MaxDistanceP)
min_turkey_hoover <- min(turkey_hoover$MaxDistanceP)
print(max_turkey_hoover)
print(min_turkey_hoover)

israel_hoover <- filter(asia_middle_east_Hoover, Country=='Israel', Year==1991)
print(israel_hoover)

JKA <- filter(asia_middle_east_Hoover, Country %in% c('Australia','Japan', 'Korea'))
max_JKA_hoover<- max(JKA$Hoover)
min_JKA_hoover <- min(JKA$Hoover)
print(max_JKA_hoover)
print(min_JKA_hoover)
################################################################################
####For Western_Europe

WE_Hoover<- Hoover_results %>% 
  filter(geographical_region== 'Western_Europe')

head(WE_Hoover,20)
min_hoover_WE <- min(WE_Hoover$Hoover)
max_hoover_WE <- max(WE_Hoover$Hoover)
print(min_hoover_WE)
print(max_hoover_WE)

spain_hoover <- filter(Hoover_results, Country =='Spain'& Year >=1980 & Year <=1993)
print(spain_hoover)

netherlands_hoover <- filter(Hoover_results, Country =='Netherlands'& Year >=1980 & Year <=2009)
print(netherlands_hoover)

Luxembourg_hoover <- filter(Hoover_results, Country =='Luxembourg'& Year >= 2019 & Year <= 2022)
print(Luxembourg_hoover)
# Define a color palette with distinct colors
color_palette_WE<- scales::hue_pal()(8)
point_shapes_WE <- rep(c(16:18), length.out = 8)

WE_Hoover$Year <- as.numeric(WE_Hoover$Year)

# Create the plot for the country WE
ggplot(WE_Hoover, aes(x = Year, y = Hoover, color = Country, linetype = Country, group = Country, shape= Country)) +
  geom_point(size = 2.5) +
  geom_line() +
  labs(title = "Hoover Index by Country (1980-2022)\n Western_Europe_OECD Members", x = "Year", y = "Hoover Index") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  theme(plot.title = element_text(size = 20, face = 'bold', hjust=0.5),
        axis.title.x = element_text(size = 16, face ='bold'),
        axis.title.y = element_text(size = 16, face='bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size = 15),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = WE_colors) +
  scale_linetype_manual(values = rep(1:3, length.out = 8))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_WE)
########################################################################################################


Mexico_Hoover_2022 <- filter(Hoover_results, Country=='Mexico'& Year == 2022)
print(Mexico_Hoover_2022)
#################For Mexico Plot
income_data_2022 <- filter(OECD_vigintiles, Year==2022)
# Hoover index calculation function
hoover_index_Mexico <- function(p_values_10_Mexico, L_values_10_Mexico) {
  # Calculate the vertical distances between the Lorenz curve and the line of perfect equality
  distances <- p_values_10_Mexico - L_values_10_Mexico
  # Find the maximum distance (Hoover index)
  hoover_idx <- max(distances)
  
  # Find the x position where the maximum distance occurs
  max_distance_index <- which.max(distances)
  max_distance_p <- p_values_10_Mexico[max_distance_index]
  
  # Debug print statements to verify values
  print(paste("Hoover Index (max vertical distance):", hoover_idx))
  print(paste("Position (p) at max distance:", max_distance_p))
  
  return(list(hoover_idx = hoover_idx, max_distance_p = max_distance_p))
}


# Filter data for Mexico and calculate Lorenz curve
lorenz_curve_Mexico_2022 <- ineq::Lc(income_data_2022$postTaxIncome[income_data_2022$Country == "Mexico"])

# Extract p and L values from the Lorenz curve
p_values_10_Mexico <- lorenz_curve_Mexico_2022$p
L_values_10_Mexico <- lorenz_curve_Mexico_2022$L

# Calculate Hoover index
hoover_index_result_Mexico <- hoover_index(p_values_10_Mexico, L_values_10_Mexico)
hoover_index_value_Mexico <- hoover_index_result_Mexico$hoover_idx
max_distance_p_Mexico <- hoover_index_result_Mexico$max_distance_p

print(paste("Hoover Index:", hoover_index_value_Mexico))

# Adjust plot margins to create space for the legend at the bottom
par(mar = c(5, 4, 4, 2) + 0.1)  # Increase the bottom margin

# Plot the Lorenz curve
plot(lorenz_curve_Mexico_2022, 
     main = 'Lorenz Curve of Mexico with Hoover Index Line_2022', 
     xlab = 'Cumulative share of population_equal split adults (vigintile)', 
     ylab = 'Cumulative share of income of each 5% of population/Dollar$_2023', 
     col = 'blue',
     cex.lab = 1.1,  # Increase the size of the axis labels
     cex.axis = 1.2, # Increase the size of the axis tick labels
     cex.main = 1.8) # Increase the size of the main title
# Extract the Lorenz curve data
lorenz_data_mexico <- data.frame(x = lorenz_curve_Mexico_2022$p, y = lorenz_curve_Mexico_2022$L)
head(lorenz_data_mexico)

# Add the points to the plot
points(lorenz_data_mexico$x, lorenz_data_mexico$y, pch=16, col='blue') 

# Shade the area between the Lorenz curve and the line of perfect equality
#polygon(c(p_values_10_Mexico, rev(p_values_10_Mexico)), c(L_values_10_Mexico, rev(p_values_10_Mexico)), col = 'gray')

# Add Hoover index text at the bottom-right
text(0.05, 0.95, paste("Hoover Index:", round(hoover_index_value_Mexico, 2)), col = 'black', cex = 1.2, pos = 4)


# Add Hoover index line at the point of maximum distance
abline(v = max_distance_p_Mexico, col = 'red', lwd = 2, lty = 2)  # Blue dashed line at the max distance

# Add the legend below the x-axis label
#legend("bottom", inset = c(0, -0.28), legend = c("Lorenz Curve", "Hoover Index"), 
       #col = c("blue", "red"), lty = c(1, 2), lwd = 2, xpd = TRUE, horiz = TRUE, bty ='n')

# Reset par to default
#par(mar = c(5, 4, 4, 2) + 0.1)

#######################################################################################################################
head(Gini_results_vigintiles)
head(Hoover_results)

###Checking the structure of the dataframes
str(Gini_results_vigintiles)
####changing factor into character for the variable Country
Gini_results_vigintiles$Country <- as.character(Gini_results_vigintiles$Country)
str(Gini_results_vigintiles)
str(Hoover_results)

####Merging the dataframes
Gini_Hoover <- merge(Gini_results_vigintiles,Hoover_results, by = c("Country", "Year", 'geographical_region'))
head(Gini_Hoover,20)

###Converting into the long dataframe
long_data <- Gini_Hoover %>%
  pivot_longer(cols = c(Gini, Hoover), names_to = "Index_Name", values_to = "Value")
long_data

GiHoo_USA <- filter(long_data, Country == 'USA')
head(GiHoo_USA)
GiHoo_USA$Year<- as.numeric(GiHoo_USA$Year)

ggplot(GiHoo_USA, aes(x = Year, y = Value, color = Index_Name, group = Index_Name)) +
  geom_point(size = 3, shape = 18) +
  geom_line() +
  labs(title = "Trends in Gini and Hoover Indexes: A Historical Overview\n The United States of America (1980 to 2022)", x = "Year", y = "Index Value") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.2)) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5))+
  theme(plot.title = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 17, face= 'bold'),
        axis.title.y = element_text(size = 17, face= 'bold'),
        axis.text.y= element_text(size=16, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size =16),
        legend.text = element_text(size=15, face= 'bold'),
        legend.title= element_text(size=15, face= 'bold'))
##################################################################################################################

head(Gini_Hoover)
###For Europe
Europe_GiniHoover_C <- filter(Gini_Hoover, geographical_region == 'Europe')

# Create scatter plot with linear regression line
ggplot(Europe_GiniHoover_C, aes(x = Gini, y = Hoover)) +
  geom_point(size= 2) +                             # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add linear regression line without confidence interval
  labs(title = "Linear Regression Analysis of Gini Index Vs Hoover Index\nEuropean Countries_OECD Members",
       x = "Gini Index",
       y = "Hoover Index") +
  theme_minimal()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 13, face= 'bold'),
        axis.title.y = element_text(size = 13, face= 'bold'),
        axis.text.y= element_text(size=10, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold'),
        strip.text = element_text(size = 13, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')







CN_GiniHoover_C <- filter(Gini_Hoover, geographical_region == 'Central_Northern_Europe')

# Create scatter plot with linear regression line
ggplot(CN_GiniHoover_C, aes(x = Gini, y = Hoover)) +
  geom_point(size= 2) +                             # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add linear regression line without confidence interval
  labs(title = "Linear Regression Analysis of Gini Index Vs Hoover Index\nCentral/Northern_Europe_OECD Members",
       x = "Gini Coefficient",
       y = "Hoover Index") +
  theme_minimal()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size = 13),
        strip.text = element_text(size = 13, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

Gini_Hoover_Czech<- filter(Gini_Hoover, Country == 'Czech')
head(Gini_Hoover_Czech)
m_Czech<- lm(Hoover ~ Gini, Gini_Hoover_Czech)
summary(m_Czech)

Gini_Hoover_Austria<- filter(Gini_Hoover, Country == 'Austria')
head(Gini_Hoover_Austria)
m_Austria<- lm(Hoover ~ Gini, Gini_Hoover_Austria)
summary(m_Austria)

Gini_Hoover_Denmark<- filter(Gini_Hoover, Country == 'Denmark')
head(Gini_Hoover_Denmark)
m_Denmark<- lm(Hoover ~ Gini, Gini_Hoover_Denmark)
summary(m_Denmark)

Gini_Hoover_Finland<- filter(Gini_Hoover, Country == 'Finland')
head(Gini_Hoover_Finland)
m_Finland<- lm(Hoover ~ Gini, Gini_Hoover_Finland)
summary(m_Finland)

Gini_Hoover_GM<- filter(Gini_Hoover, Country == 'Germany')
head(Gini_Hoover_GM)
m_GM<- lm(Hoover ~ Gini, Gini_Hoover_GM)
summary(m_GM)

Gini_Hoover_IC<- filter(Gini_Hoover, Country == 'Iceland')
head(Gini_Hoover_IC)
m_IC<- lm(Hoover~ Gini, Gini_Hoover_IC)
summary(m_IC)

Gini_Hoover_Nor<- filter(Gini_Hoover, Country == 'Norway')
head(Gini_Hoover_Nor)
m_Nor<- lm(Hoover ~ Gini, Gini_Hoover_Nor)
summary(m_Nor)

Gini_Hoover_Sweden<- filter(Gini_Hoover, Country == 'Sweden')
head(Gini_Hoover_Sweden)
m_Sweden<- lm(Hoover ~ Gini, Gini_Hoover_Sweden)
summary(m_Sweden)

Gini_Hoover_Switzerland<- filter(Gini_Hoover, Country == 'Switzerland')
head(Gini_Hoover_Switzerland)
m_Switzerland<- lm(Hoover ~ Gini, Gini_Hoover_Switzerland)
summary(m_Switzerland)
################################################################
W_GiniHoover_C <- filter(Gini_Hoover, geographical_region == 'Western_Europe')

# Create scatter plot with linear regression line
ggplot(W_GiniHoover_C, aes(x = Gini, y = Hoover)) +
  geom_point(size= 2) +                             # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add linear regression line without confidence interval
  labs(title = "Linear Regression Analysis of Gini Index Vs Hoover Index\nWestern_Europe_OECD Members",
       x = "Gini Coefficient",
       y = "Hoover Index") +
  theme_minimal()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold',size=13),
        strip.text = element_text(size = 13, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')


Gini_Hoover_Bel<- filter(Gini_Hoover, Country == 'Belgium')
head(Gini_Hoover_Bel)
m_Belgium<- lm(Hoover ~ Gini, Gini_Hoover_Bel)
summary(m_Belgium)

Gini_Hoover_France<- filter(Gini_Hoover, Country == 'France')
head(Gini_Hoover_France)
m_France<- lm(Hoover ~ Gini, Gini_Hoover_France)
summary(m_France)

Gini_Hoover_Ireland<- filter(Gini_Hoover, Country == 'Ireland')
head(Gini_Hoover_Ireland)
m_Ireland<- lm(Hoover~ Gini, Gini_Hoover_Ireland)
summary(m_Ireland)

Gini_Hoover_Luxembourg<- filter(Gini_Hoover, Country == 'Luxembourg')
head(Gini_Hoover_Luxembourg)
m_Luxembourg<- lm(Hoover ~ Gini, Gini_Hoover_Luxembourg)
summary(m_Luxembourg)

Gini_Hoover_Netherlands<- filter(Gini_Hoover, Country == 'Netherlands')
head(Gini_Hoover_Netherlands)
m_Netherlands<- lm(Hoover ~ Gini, Gini_Hoover_Netherlands)
summary(m_Netherlands)

Gini_Hoover_Portugal<- filter(Gini_Hoover, Country == 'Portugal')
head(Gini_Hoover_Portugal)
m_Portugal<- lm(Hoover ~ Gini, Gini_Hoover_Portugal)
summary(m_Portugal)

Gini_Hoover_Spain<- filter(Gini_Hoover, Country == 'Spain')
head(Gini_Hoover_Spain)
m_Spain<- lm(Hoover ~ Gini, Gini_Hoover_Spain)
summary(m_Spain)

Gini_Hoover_UK<- filter(Gini_Hoover, Country == 'UK')
head(Gini_Hoover_UK)
m_UK<- lm(Hoover ~ Gini, Gini_Hoover_UK)
summary(m_UK)

##############################################################
#for South and Eastern
SE_GiniHoover_C <- filter(Gini_Hoover, geographical_region == 'South/Eastern_Europe')
# Create scatter plot with linear regression line
ggplot(SE_GiniHoover_C, aes(x = Gini, y = Hoover)) +
  geom_point(size= 2) +                             # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add linear regression line without confidence interval
  labs(title = "Linear Regression Analysis of Gini Index Vs Hoover Index\nSouthern_Eastern_Europe_OECD Members",
       x = "Gini Coefficient",
       y = "Hoover Index") +
  theme_minimal()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold',size =13),
        strip.text = element_text(size = 13, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')



Gini_Hoover_Estonia<- filter(Gini_Hoover, Country == 'Estonia')
head(Gini_Hoover_Estonia)
m_Estonia<- lm(Hoover ~ Gini, Gini_Hoover_Estonia)
summary(m_Estonia)

Gini_Hoover_Greece<- filter(Gini_Hoover, Country == 'Greece')
head(Gini_Hoover_Greece)
m_Greece<- lm(Hoover ~ Gini, Gini_Hoover_Greece)
summary(m_Greece)

Gini_Hoover_Hungary<- filter(Gini_Hoover, Country == 'Hungary')
head(Gini_Hoover_Hungary)
m_Hungary<- lm(Hoover ~ Gini, Gini_Hoover_Hungary)
summary(m_Hungary)

Gini_Hoover_Italy<- filter(Gini_Hoover, Country == 'Italy')
head(Gini_Hoover_Italy)
m_Italy<- lm(Hoover ~ Gini, Gini_Hoover_Italy)
summary(m_Italy)

Gini_Hoover_Latvia<- filter(Gini_Hoover, Country == 'Latvia')
head(Gini_Hoover_Latvia)
m_Latvia<- lm(Hoover ~ Gini, Gini_Hoover_Latvia)
summary(m_Latvia)

Gini_Hoover_Lithuania<- filter(Gini_Hoover, Country == 'Lithuania')
head(Gini_Hoover_Lithuania)
m_Lithuania<- lm(Hoover ~ Gini, Gini_Hoover_Lithuania)
summary(m_Lithuania)

Gini_Hoover_Poland<- filter(Gini_Hoover, Country == 'Poland')
head(Gini_Hoover_Poland)
m_Poland<- lm(Hoover ~ Gini, Gini_Hoover_Poland)
summary(m_Poland)

Gini_Hoover_Slovakia<- filter(Gini_Hoover, Country == 'Slovakia')
head(Gini_Hoover_Slovakia)
m_Slovakia<- lm(Hoover ~ Gini, Gini_Hoover_Slovakia)
summary(m_Slovakia)

Gini_Hoover_Slovenia<- filter(Gini_Hoover, Country == 'Slovenia')
head(Gini_Hoover_Slovenia)
m_Slovenia<- lm(Hoover ~ Gini, Gini_Hoover_Slovenia)
summary(m_Slovenia)


##################################################################
#For AP and ME
APME_GiniHoover_C <- Gini_Hoover%>%
  filter(geographical_region %in% c('Asia_Pacific', 'Middle_East'))
head(APME_GiniHoover_C)
# Create scatter plot with linear regression line
ggplot(APME_GiniHoover_C, aes(x = Gini, y = Hoover)) +
  geom_point(size= 2) +                             # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add linear regression line without confidence interval
  labs(title = "Linear Regression Analysis of Gini Index Vs Hoover Index\nAsia Pacific / Middle East_OECD Members",
       x = "Gini Coefficient",
       y = "Hoover Index") +
  theme_minimal()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size =13),
        strip.text = element_text(size = 13, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

Gini_Hoover_Turkey<- filter(Gini_Hoover, Country == 'Turkey')
head(Gini_Hoover_Turkey)
m_Turkey<- lm(Hoover ~ Gini, Gini_Hoover_Turkey)
summary(m_Turkey)

Gini_Hoover_Australia<- filter(Gini_Hoover, Country == 'Australia')
head(Gini_Hoover_Australia)
m_Australia <- lm(Hoover ~ Gini, Gini_Hoover_Australia)
summary(m_Australia)

Gini_Hoover_Israel<- filter(Gini_Hoover, Country == 'Israel')
head(Gini_Hoover_Israel)
m_Israel <- lm(Hoover ~ Gini, Gini_Hoover_Israel)
summary(m_Israel)

Gini_Hoover_Japan<- filter(Gini_Hoover, Country == 'Japan')
head(Gini_Hoover_Japan)
m_Japan <- lm(Hoover ~ Gini, Gini_Hoover_Japan)
summary(m_Japan)

Gini_Hoover_Korea<- filter(Gini_Hoover, Country == 'Korea')
head(Gini_Hoover_Korea)
m_Korea <- lm(Hoover ~ Gini, Gini_Hoover_Korea)
summary(m_Korea)

Gini_Hoover_New_Zealand<- filter(Gini_Hoover, Country == 'New_Zealand')
head(Gini_Hoover_New_Zealand)
m_NZ<- lm(Hoover ~ Gini, Gini_Hoover_New_Zealand)
summary(m_NZ)

####################################################################

#for Latin and North America
NL_GiniHoover_C <- Gini_Hoover%>%
  filter(geographical_region %in% c('North_America', 'Latin_America'))

# Create scatter plot with linear regression line
ggplot(NL_GiniHoover_C, aes(x = Gini, y = Hoover)) +
  geom_point(size= 2) +                             # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add linear regression line without confidence interval
  labs(title = "Linear Regression Analysis of Gini Index Vs Hoover Index\nAmerican Countries_OECD Members",
       x = "Gini Coefficient",
       y = "Hoover Index") +
  theme_minimal()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size=13),
        strip.text = element_text(size = 13, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

#########summary lm for NL

Gini_Hoover_Canada <- filter(Gini_Hoover, Country == 'Canada')
head(Gini_Hoover_Canada)
m_Canada <- lm(Hoover ~ Gini, Gini_Hoover_Canada)
summary(m_Canada)

Gini_Hoover_Chile <- filter(Gini_Hoover, Country == 'Chile')
head(Gini_Hoover_Chile)
m_Chile <- lm(Hoover ~ Gini, Gini_Hoover_Chile)
summary(m_Chile)

Gini_Hoover_Colombia <- filter(Gini_Hoover, Country == 'Colombia')
head(Gini_Hoover_Colombia)
m_Colombia<- lm(Hoover ~ Gini, Gini_Hoover_Colombia)
summary(m_Colombia)

Gini_Hoover_CR <- filter(Gini_Hoover, Country == 'Costa_Rica')
head(Gini_Hoover_CR)
m_CR<- lm(Hoover ~ Gini, Gini_Hoover_CR)
summary(m_CR)

Gini_Hoover_Mex <- filter(Gini_Hoover, Country == 'Mexico')
head(Gini_Hoover_Mex)
m_Mex<- lm(Hoover ~ Gini, Gini_Hoover_Mex)
summary(m_Mex)

Gini_Hoover_USA <- filter(Gini_Hoover, Country == 'USA')
head(Gini_Hoover_USA)
m_USA<- lm(Hoover ~ Gini, Gini_Hoover_USA)
summary(m_USA)
#####################################################################################################################


#####Calculate changes for all OECD
OECD_changes <- filter(OECD_vigintiles, Year %in% c(1980,2022))
head(OECD_changes)

library(dplyr)
library(tidyr)

# Filter for the required years
OECD_changes <- filter(OECD_vigintiles, Year %in% c(1980, 2022))

# Aggregate to ensure unique combinations for each Vigintiles and Country
OECD_aggregated <- OECD_changes %>%
  group_by(Vigintiles, Country, Year) %>%
  summarise(postTaxIncome = mean(postTaxIncome, na.rm = TRUE), .groups = 'drop')
head(OECD_aggregated)
# Calculate changes for all OECD with additional identifiers
changes_OECD_all <- OECD_aggregated %>%
  spread(key = Year, value = postTaxIncome) %>%
  rename(InitialYear = `1980`, FinalYear = `2022`) %>%
  mutate(
    Income_Share_Change = FinalYear - InitialYear,
    Relative_Change = ((FinalYear - InitialYear) / InitialYear) * 100
  )
    

head(changes_OECD_all)

usa_change <- read_excel('USA_income.xlsx')


usa_changes <- usa_change %>% mutate(Income_Share_Change = FinalYear - InitialYear,
                                     Relative_Change = ((FinalYear - InitialYear) / InitialYear) * 100
)

head(usa_changes,20)


# Create a named vector with the mapping
country_to_regions_changes <- c(
  'Canada' = 'North_America',
  'Mexico' = 'North_America',
  'USA' = 'North_America',
  "Chile" = "Latin_America",
  "Colombia" = "Latin_America",
  'Austria' = 'Central_Northern_Europe',
  "Belgium" = "Western_Europe",
  "France" = "Western_Europe",
  "Germany" = "Central_Northern_Europe",
  "Ireland" = "Western_Europe",
  "Luxembourg" = "Western_Europe",
  "Netherlands" = "Western_Europe",
  "Switzerland" = "Central_Northern_Europe",
  "UK" = "Western_Europe",
  "Denmark" = "Central_Northern_Europe",
  "Finland" = "Central_Northern_Europe",
  "Iceland" = "Central_Northern_Europe",
  "Norway" = "Central_Northern_Europe",
  "Sweden" = "Central_Northern_Europe",
  "Greece" = "South/Eastern_Europe",
  "Italy" = "South/Eastern_Europe",
  "Portugal" = "Western_Europe",
  "Spain" = "Western_Europe",
  "Czech" = "Central_Northern_Europe",
  "Estonia" = "South/Eastern_Europe",
  "Hungary" = "South/Eastern_Europe",
  "Latvia" = "South/Eastern_Europe",
  "Lithuania" = "South/Eastern_Europe",
  "Poland" = "South/Eastern_Europe",
  "Slovakia" = "South/Eastern_Europe",
  "Slovenia" = "South/Eastern_Europe",
  "Australia" = "Asia_Pacific",
  "Japan" = "Asia_Pacific",
  "Korea" = "Asia_Pacific",
  "New_Zealand" = "Asia_Pacific",
  "Israel" = "Middle_East",
  "Turkey" = "Middle_East",
  "Costa_Rica"= "Latin_America"
)

changes_OECD_all <- changes_OECD_all %>%
  mutate(geographical_region = country_to_regions_changes[as.character(changes_OECD_all$Country)])
head(changes_OECD_all)

sorted_changes <- changes_OECD_all %>%
  arrange(Income_Share_Change)
head(sorted_changes)

changes_11 <- filter(changes_OECD_all, Vigintiles == '11th')
head(changes_11)

# Calculate the minimum and maximum values of AbsoluteChange from changes_OECD_all
min_value_change <- min(changes_OECD_all$Income_Share_Change, na.rm = TRUE)

max_value_change <- max(changes_OECD_all$Income_Share_Change, na.rm = TRUE)

###For relative change plot
min_value <- min(changes_OECD_all$Relative_Change, na.rm = TRUE)

max_value <- max(changes_OECD_all$Relative_Change, na.rm = TRUE)

####Visualising changes plots based on geographical region
#For Central and Northern
CN_Change <- filter(changes_OECD_all, geographical_region == 'Central_Northern_Europe')
head(CN_Change)

ggplot(CN_Change, aes(x = as.factor(Vigintiles), y =Income_Share_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Change in income share per vigintile between 1980 and 2022\nCentral/Northern Europe_OECD Members",
       x = "Population Decile",
       y = "Change in income share per vigintile",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_change * 1.1, max_value_change * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 17, face= 'bold'),
        axis.title.y = element_text(size = 17, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size= 15),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')


###for relative change
min_value_CN <- min(CN_Change$Relative_Change, na.rm = TRUE)

max_value_CN <- max(CN_Change$Relative_Change, na.rm = TRUE)

ggplot(CN_Change, aes(x = as.factor(Vigintiles), y =Relative_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Relative Change in income share per vigintile between 1980 and 2022\nCentral/Northern Europe_OECD Members",
       x = "Vigintile (each 5% of the total population)",
       y = "Relative Change in income share per vigintile (%)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_CN*1.1, max_value_CN * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 17, face= 'bold'),
        axis.title.y = element_text(size = 17, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size=15),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

###############################################################################
###For Western
WE_Change <- filter(changes_OECD_all, geographical_region == 'Western_Europe')

ggplot(WE_Change, aes(x = as.factor(Vigintiles), y =Income_Share_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Change in income share per vigintile between 1980 and 2022\nWestern_Europe_OECD Members",
       x = "Population Vigintile",
       y = "Change in income share per Vigintile",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_change *1.1, max_value_change * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold'),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

###for relative change
min_value_WE <- min(WE_Change$Relative_Change, na.rm = TRUE)

max_value_WE <- max(WE_Change$Relative_Change, na.rm = TRUE)

ggplot(WE_Change, aes(x = as.factor(Vigintiles), y =Relative_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Relative Change in income share per vigintile between 1980 and 2022\nWestern_Europe_OECD Members",
       x = "Vigintile (each 5% of the total population)",
       y = "Relative Change in income share per vigintile (%)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_WE *1.1, max_value_WE * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 17, face= 'bold'),
        axis.title.y = element_text(size = 17, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size = 15),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

########################################################################
#for Asia Pacific and Middle East
APME_Change <- changes_OECD_all %>%
  filter(geographical_region %in% c('Asia_Pacific','Middle_East'))

ggplot(APME_Change, aes(x = as.factor(Vigintiles), y =Income_Share_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Change in income share per Vigintile between 1980 and 2022\nAsia_Pacific/Middle_East_OECD Members",
       x = "Population Vigintile",
       y = "Change in income share (each 5% of population)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_change *1.1, max_value_change * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold'),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

###for relative change
min_value_APME <- min(APME_Change$Relative_Change, na.rm = TRUE)

max_value_APME <- max(APME_Change$Relative_Change, na.rm = TRUE)

ggplot(APME_Change, aes(x = as.factor(Vigintiles), y =Relative_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Relative Change in income share per Vigintile between 1980 and 2022\nAsia_Pacific/Middle_East_OECD Members",
       x = "Vigintile (each 5% of the total population)",
       y = "Relative Change in income share per vigintile (%)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_APME *1.1, max_value_APME * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 17, face= 'bold'),
        axis.title.y = element_text(size = 17, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size= 15),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')


######################################################################
#for Latin and North America
NL_Change <- changes_OECD_all%>%
  filter(geographical_region %in% c('Latin_America', 'North_America'))

ggplot(NL_Change, aes(x = as.factor(Vigintiles), y =Income_Share_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Change in income share per vigintile between 1980 and 2022\nNorth/Latin_America_OECD Members",
       x = "Population Vigintile",
       y = "Change in income share (each 5% of population)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_change *1.1, max_value_change * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold'),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

###for relative change
min_value_NL <- min(NL_Change$Relative_Change, na.rm = TRUE)

max_value_NL <- max(NL_Change$Relative_Change, na.rm = TRUE)

ggplot(NL_Change, aes(x = as.factor(Vigintiles), y =Relative_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Relative Change in income share per vigintile between 1980 and 2022\nAmerican Countries_OECD Members",
       x = "Vigintile (each 5% of the total population)",
       y = "Relative Change in income share per vigintile (%)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_NL *1.1, max_value_NL * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 17, face= 'bold'),
        axis.title.y = element_text(size = 17, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size = 15),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

###################################################################
##for South and Eastern

SE_Change <- filter(changes_OECD_all, geographical_region == 'South/Eastern_Europe')

ggplot(SE_Change, aes(x = as.factor(Vigintiles), y =Income_Share_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Change in income share per vigintile between 1980 and 2022\nSouthern/Eastern_Europe_OECD Members",
       x = "Population Vigintile",
       y = "Change in income share (each 5% of population)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_change *1.1, max_value_change * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold'),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

##For relative change
min_value_SE <- min(SE_Change$Relative_Change, na.rm = TRUE)

max_value_SE <- max(SE_Change$Relative_Change, na.rm = TRUE)

ggplot(SE_Change, aes(x = as.factor(Vigintiles), y =Relative_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Relative Change in income share per vigintile between 1980 and 2022\nSouthern/Eastern_Europe_OECD Members",
       x = "Vigintile (each 5% of the total population)",
       y = "Relative Change in income share per vigintile (%)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_SE *1.1, max_value_SE * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 17, face= 'bold'),
        axis.title.y = element_text(size = 17, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size = 15),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')
#########################################################################################################

###For the USA Plot
usa_income_change <- filter(changes_OECD_all, Country == 'USA')
head(usa_income_change,20)

min_value_USA <- min(usa_income_change$Relative_Change, na.rm = TRUE)

max_value_USA <- max(usa_income_change$Relative_Change, na.rm = TRUE)

####For only the country USA

ggplot(usa_income_change, aes(x = as.factor(Vigintiles), y =Income_Share_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Change in income share per vigintile between 1980 and 2022_USA",
       x = "Population Vigintile",
       y = "Change in income share (each 5% of population)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_change * 1.1, max_value_change * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(size= 15, angle = 45, hjust = 1, face= 'bold'))



ggplot(usa_income_change, aes(x = as.factor(Vigintiles), y =Relative_Change)) +
  geom_bar(stat = "identity", fill= 'blue') +
  labs(title = "Relative Change in income share per vigintile\n between 1980 and 2022_USA",
       x = "Population Vigintile",
       y = "Relative Change in income share per vigintile (%)",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(
    limits = c(min_value_USA * 1.1, max_value_USA * 1.1),  # Set y-axis limits
    breaks = scales::pretty_breaks(n = 10)  # Set breaks for better readability
  ) + 
  theme(plot.title = element_text(size = 20, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(size= 15, angle = 45, hjust = 1, face= 'bold'))
###############################################################################################################



###For checking tables of changes in income share
Estonia_income_change <- filter(changes_OECD_all, Country == 'Estonia')
head(Estonia_income_change,20)

Greece_income_change <- filter(changes_OECD_all, Country == 'Greece')
head(Greece_income_change,20)

Hungary_income_change <- filter(changes_OECD_all, Country == 'Hungary')
head(Hungary_income_change,20)

Italy_income_change <- filter(changes_OECD_all, Country == 'Italy')
head(Italy_income_change,20)

Latvia_income_change <- filter(changes_OECD_all, Country == 'Latvia')
head(Latvia_income_change,20)

Lithuania_income_change <- filter(changes_OECD_all, Country == 'Lithuania')
head(Lithuania_income_change,20)

Poland_income_change <- filter(changes_OECD_all, Country == 'Poland')
head(Poland_income_change,20)

Slovakia_income_change <- filter(changes_OECD_all, Country == 'Slovakia')
head(Slovakia_income_change,20)

Slovenia_income_change <- filter(changes_OECD_all, Country =='Slovenia')
head(Slovenia_income_change,20)

Canada_income_change <- filter(changes_OECD_all, Country =='Canada')
head(Canada_income_change,20)

Chile_income_change <- filter(changes_OECD_all, Country == 'Chile')
head(Chile_income_change,20)

Colombia_income_change <- filter(changes_OECD_all, Country == 'Colombia')
head(Colombia_income_change,20)

Costa_Rica_income_change <- filter(changes_OECD_all, Country == 'Costa_Rica')
head(Costa_Rica_income_change,20)

Mexico_income_change <- filter(changes_OECD_all, Country == 'Mexico')
head(Mexico_income_change,20)

USA_income_change <- filter(changes_OECD_all, Country == 'USA')

Australia_income_change <- filter(changes_OECD_all, Country == 'Australia')
head(Australia_income_change, 20)

Israel_income_change <- filter(changes_OECD_all, Country == 'Israel')
head(Israel_income_change,20)

Japan_income_change <- filter(changes_OECD_all, Country == 'Japan')
head(Japan_income_change,20)

Korea_income_change <- filter (changes_OECD_all, Country == 'Korea')
head(Korea_income_change,20)

New_Z_income_change <- filter (changes_OECD_all, Country == 'New_Zealand')
head(New_Z_income_change,20)

Turkey_income_change <- filter(changes_OECD_all, Country == 'Turkey')
head(Turkey_income_change,20)

Belgium_income_change <- filter(changes_OECD_all, Country == 'Belgium')
head(Belgium_income_change,20)

France_income_change <- filter(changes_OECD_all, Country == 'France')
head(France_income_change,20)

Ireland_income_change <- filter(changes_OECD_all, Country =='Ireland')
head(Ireland_income_change,20)

Luxembour_income_change <- filter(changes_OECD_all, Country == 'Luxembourg')
head(Luxembour_income_change,20)

Netherlands_income_change <- filter(changes_OECD_all, Country == 'Netherlands')
head(Netherlands_income_change,20)

Portugal_income_change <- filter(changes_OECD_all, Country == 'Portugal')
head(Portugal_income_change, 20)

Spain_income_change <- filter(changes_OECD_all,Country == 'Spain')
head(Spain_income_change, 20)

UK_income_change <- filter(changes_OECD_all,Country =='UK')
head(UK_income_change, 20)

Austria_income_change <-filter(changes_OECD_all, Country == 'Austria')
head(Austria_income_change, 20)

Czech_income_change <- filter(changes_OECD_all, Country == 'Czech')
head(Czech_income_change,20)

Denmark_income_change <- filter(changes_OECD_all, Country =='Denmark')
head(Denmark_income_change,20)

Finland_income_change <- filter(changes_OECD_all, Country =='Finland')
head(Finland_income_change,20)

Germany_income_change <- filter(changes_OECD_all, Country == 'Germany')
head(Germany_income_change,20)

Iceland_income_change <- filter(changes_OECD_all, Country == 'Iceland')
head(Iceland_income_change,20)

Norway_income_change <- filter(changes_OECD_all, Country =='Norway')
head(Norway_income_change,20)

Sweden_income_change <- filter(changes_OECD_all, Country =='Sweden')
head(Sweden_income_change,20)

Switzerland_income_change <- filter(changes_OECD_all, Country == 'Switzerland')
head(Switzerland_income_change, 20)
###########################################################################################################
###for Gini and Hoover Time series

###Converting into the long dataframe
long_data <- Gini_Hoover %>%
  pivot_longer(cols = c(Gini, Hoover), names_to = "Index_Name", values_to = "Value")
long_data

GiHoo_USA <- filter(long_data, Country == 'USA')
head(GiHoo_USA)

ggplot(GiHoo_USA, aes(x = Year, y = Value, color = Index_Name, group = Index_Name)) +
  geom_point(size = 2.5, shape = 18) +
  geom_line() +
  labs(title = "Trends in Gini and Hoover Indexes: A Historical Overview\n The United States of America (1980 to 2022)", x = "Year", y = "Index Value") +
  theme_minimal() +
  ylim(0, max(long_data$Value) * 1.1) +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=15, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold'),
        legend.text = element_text(size=15, face= 'bold'),
        legend.title= element_text(size=15, face= 'bold'))
#########################################################################################################
#For NL
OECD_American <- filter(OECD_vigintiles, Country %in% c('Canada','Chile','Colombia','Costa_Rica','Mexico', 'USA') & Year%in% c(1980,2000,2020,2022))
head(OECD_American,20)


#Function to compute Lorenz curve for a given group
compute_lorenz_curve <- function(income) {
  lc <- ineq::Lc(income)
  data.frame(p = lc$p, L = lc$L)
}

# Filter and group data, then compute Lorenz curve for each group
NL_lorenz <- OECD_American %>%
  group_by(Country, Year) %>%
  summarise(data = list(compute_lorenz_curve(as.numeric(postTaxIncome)))) %>%
  unnest(cols = c(data))

# View the results
head(NL_lorenz)
mexico <- filter(NL_lorenz, Country == 'Mexico' & Year == 1980)
head(mexico,20)
usa <- filter(NL_lorenz, Country =='USA' & Year == 1980)
head(usa,20)
usa_2022 <- filter(NL_lorenz, Country =='USA', Year == 2022)
head(usa_2022,20)
# Ensure that Year is a factor with levels in the desired order
NL_lorenz$Year <- factor(NL_lorenz$Year, levels = c("1980", "2000", "2020", "2022"))


# Plot the Lorenz curves
ggplot(NL_lorenz, aes(x = p, y = L, color = Country)) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Lorenz Curves with Equality Line\nAmerican Countries_OECD Members",
       x = "Cumulative Share of Population",
       y = "Cumulative Share of Income") +
  facet_wrap(~ Year, scales='free_x') +
  theme_minimal()+
  theme(plot.title = element_text(face = 'bold', hjust= 0.5),
        strip.text = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(face='bold', size = 13),
        axis.title.y = element_text(face='bold',size =13),
        axis.text.y= element_text(face= 'bold',size = 12),
        axis.text.x = element_text(face= 'bold', size = 12),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = NL_colors)

###For Europe
##################for Europe
Europe <- filter(OECD_vigintiles, Country %in% c('Estonia','Greece','Hungary','Italy','Latvia','Lithuania','Poland','Slovakia','Slovenia','Austria','Czech','Denmark','Finland','Germany','Iceland','Norway','Sweden','Switzerland','Belgium','France','Ireland','Luxembourg','Netherlands','Portugal','Spain','UK') & Year %in% c(1980,2000,2020,2022))

#Function to compute Lorenz curve for a given group
compute_lorenz_curve <- function(income) {
  lc <- ineq::Lc(income)
  data.frame(p = lc$p, L = lc$L)
}

# Filter and group data, then compute Lorenz curve for each group
Europe_lorenz <- Europe %>%
  group_by(Country, Year) %>%
  summarise(data = list(compute_lorenz_curve(as.numeric(postTaxIncome)))) %>%
  unnest(cols = c(data))

# View the results
head(Europe_lorenz)

# Ensure that Year is a factor with levels in the desired order
Europe_lorenz$Year <- factor(Europe_lorenz$Year, levels = c("1980", "2000", "2020", "2022"))

color_palette_Europe <- scales::hue_pal()(26)
point_shapes_Europe <- rep(c(16:20), length.out = 26)
# Plot the Lorenz curves
ggplot(Europe_lorenz, aes(x = p, y = L, color = Country,linetype = Country, shape=Country)) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Lorenz Curves with Equality Line\nEuropean Countries_OECD Members",
       x = "Cumulative Share of Population",
       y = "Cumulative Share of Income per vigintile ($ 2023,PPP)") +
  facet_wrap(~ Year, scales='free_x') +
  theme_minimal()+
  theme(plot.title = element_text(face = 'bold', hjust= 0.5),
        strip.text = element_text(size = 10, face = 'bold'),
        axis.title.x = element_text(face='bold'),
        axis.title.y = element_text(face='bold'),
        axis.text.y= element_text(face= 'bold'),
        axis.text.x = element_text(face= 'bold'))+
  scale_color_manual(values = color_palette_Europe)+
  scale_linetype_manual(values = rep(1:3, length.out = 26))+# Repeat linetypes if needed
  scale_shape_manual(values = point_shapes_Europe)














##################for SE
SE <- filter(OECD_vigintiles, Country %in% c('Estonia','Greece','Hungary','Italy','Latvia','Lithuania','Poland','Slovakia','Slovenia') & Year %in% c(1980,2000,2020,2022))

#Function to compute Lorenz curve for a given group
compute_lorenz_curve <- function(income) {
  lc <- ineq::Lc(income)
  data.frame(p = lc$p, L = lc$L)
}

# Filter and group data, then compute Lorenz curve for each group
SE_lorenz <- SE %>%
  group_by(Country, Year) %>%
  summarise(data = list(compute_lorenz_curve(as.numeric(postTaxIncome)))) %>%
  unnest(cols = c(data))

# View the results
head(SE_lorenz)

# Ensure that Year is a factor with levels in the desired order
SE_lorenz$Year <- factor(SE_lorenz$Year, levels = c("1980", "2000", "2020", "2022"))


# Plot the Lorenz curves
ggplot(SE_lorenz, aes(x = p, y = L, color = Country)) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Lorenz Curves with Equality Line\nSouthern/Eastern European_OECD Members",
       x = "Cumulative Share of Population",
       y = "Cumulative Share of Income per vigintile ($ 2023,PPP)") +
  facet_wrap(~ Year, scales='free_x') +
  theme_minimal()+
  theme(plot.title = element_text(face = 'bold', hjust= 0.5),
        strip.text = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(face='bold', size = 13),
        axis.title.y = element_text(face='bold', size = 13),
        axis.text.y= element_text(face= 'bold', size = 12),
        axis.text.x = element_text(face= 'bold', size = 12),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = SE_colors)
##########################################################################################################
CN<- filter(OECD_vigintiles, Country %in% c('Austria','Czech','Denmark','Finland','Germany','Iceland','Norway','Sweden','Switzerland') & Year %in% c(1980,2000,2020,2022))

#Function to compute Lorenz curve for a given group
compute_lorenz_curve <- function(income) {
  lc <- ineq::Lc(income)
  data.frame(p = lc$p, L = lc$L)
}

# Filter and group data, then compute Lorenz curve for each group
CN_lorenz <- CN %>%
  group_by(Country, Year) %>%
  summarise(data = list(compute_lorenz_curve(as.numeric(postTaxIncome)))) %>%
  unnest(cols = c(data))

# View the results
head(CN_lorenz)

# Ensure that Year is a factor with levels in the desired order
CN_lorenz$Year <- factor(CN_lorenz$Year, levels = c("1980", "2000", "2020", "2022"))


# Plot the Lorenz curves
ggplot(CN_lorenz, aes(x = p, y = L, color = Country)) +
  geom_line() +
  geom_point(size=1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Lorenz Curves with Equality Line\nCentral/Northern European_OECD Members",
       x = "Cumulative Share of Population",
       y = "Cumulative Share of Income per vigintile ($ 2023,PPP)") +
  facet_wrap(~ Year, scales='free_x') +
  theme_minimal()+
  theme(plot.title = element_text(face = 'bold', hjust= 0.5),
        strip.text = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(face='bold', size =13),
        axis.title.y = element_text(face='bold', size =13),
        axis.text.y= element_text(face= 'bold', size =12),
        axis.text.x = element_text(face= 'bold', size = 12),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
      scale_color_manual(values = CN_colors)
#################################################################################
AME<- filter(OECD_vigintiles, Country %in% c('Australia','Israel','Japan','Korea','New_Zealand','Turkey') & Year %in% c(1980,2000,2020,2022))

#Function to compute Lorenz curve for a given group
compute_lorenz_curve <- function(income) {
  lc <- ineq::Lc(income)
  data.frame(p = lc$p, L = lc$L)
}

# Filter and group data, then compute Lorenz curve for each group
AME_lorenz <- AME %>%
  group_by(Country, Year) %>%
  summarise(data = list(compute_lorenz_curve(as.numeric(postTaxIncome)))) %>%
  unnest(cols = c(data))

# View the results
head(AME_lorenz)

# Ensure that Year is a factor with levels in the desired order
AME_lorenz$Year <- factor(AME_lorenz$Year, levels = c("1980", "2000", "2020", "2022"))


# Plot the Lorenz curves
ggplot(AME_lorenz, aes(x = p, y = L, color = Country)) +
  geom_line() +
  geom_point(size=1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Lorenz Curves with Equality Line\nAsia Pacific and Middle East_OECD Members",
       x = "Cumulative Share of Population",
       y = "Cumulative Share of Income per vigintile ($ 2023,PPP)") +
  facet_wrap(~ Year, scales='free_x') +
  theme_minimal()+
  theme(plot.title = element_text(face = 'bold', hjust= 0.5),
        strip.text = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(face='bold', size = 13),
        axis.title.y = element_text(face='bold', size=13),
        axis.text.y= element_text(face= 'bold', size = 12),
        axis.text.x = element_text(face= 'bold', size =12),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = AME_colors)
#################################################################################
WE<- filter(OECD_vigintiles, Country %in% c('Belgium','France','Ireland','Luxembourg','Netherlands','Portugal','Spain','UK') & Year %in% c(1980,2000,2020,2022))

#Function to compute Lorenz curve for a given group
compute_lorenz_curve <- function(income) {
  lc <- ineq::Lc(income)
  data.frame(p = lc$p, L = lc$L)
}

# Filter and group data, then compute Lorenz curve for each group
WE_lorenz <- WE %>%
  group_by(Country, Year) %>%
  summarise(data = list(compute_lorenz_curve(as.numeric(postTaxIncome)))) %>%
  unnest(cols = c(data))

# View the results
head(WE_lorenz)

# Ensure that Year is a factor with levels in the desired order
WE_lorenz$Year <- factor(WE_lorenz$Year, levels = c("1980", "2000", "2020", "2022"))


# Plot the Lorenz curves
ggplot(WE_lorenz, aes(x = p, y = L, color = Country)) +
  geom_line() +
  geom_point(size=1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Lorenz Curves with Equality Line\nWestern European Countries_OECD Members",
       x = "Cumulative Share of Population",
       y = "Cumulative Share of Income per vigintile ($ 2023,PPP)") +
  facet_wrap(~ Year, scales='free_x') +
  theme_minimal()+
  theme(plot.title = element_text(face = 'bold', hjust= 0.5),
        strip.text = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(face='bold', size =13),
        axis.title.y = element_text(face='bold', size = 13),
        axis.text.y= element_text(face= 'bold', size = 12),
        axis.text.x = element_text(face= 'bold', size = 12),
        legend.text = element_text(size = 15),  # Increase the size of the legend text
        legend.title = element_text(size = 16), # Increase the size of the legend title
        legend.key.size = unit(1.5, 'lines'),
        panel.grid.major = element_line(color = "gray80", size = 0.5),  # Faded major grid lines
        panel.grid.minor = element_line(color = "gray90", size = 0.25))+
  scale_color_manual(values = WE_colors)



########################################################Trying for only one Year for only one country
UK_2022 <- filter(WE, Country=='UK'& Year==2022)
lorenz_curve_UK <- ineq::Lc(UK_2022$postTaxIncome)
print(lorenz_curve_UK)

Turkey <- filter(OECD_vigintiles, Country == 'Turkey' &Year ==2022)
head(Turkey)
lorenz_curve_Turkey <- ineq::Lc(Turkey $postTaxIncome)

# Print the Lorenz curve object to check its structure
print(lorenz_curve_Turkey)

# Plot the Lorenz curve
plot(lorenz_curve_UK, main='Lorenz Curve with Perfect Equality Line \nThe United Kingdom_2022', 
     xlab='Cumulative Share of Population (each 5% of population_Vigintile)', ylab='Cumulative Share of Income per vigintile ($ 2023,PPP)', col='red',
cex.lab = 1.1,  # Increase the size of the axis labels
cex.axis = 1.2, # Increase the size of the axis tick labels
cex.main = 1.8) # Increase the size of the main title

# Add points to the plot
# Extract the Lorenz curve data
lorenz_data <- data.frame(x = lorenz_curve_UK$p, y = lorenz_curve_UK$L)
head(lorenz_data)
# Add the points to the plot
points(lorenz_data$x, lorenz_data$y, pch=16, col='blue') 

# Plot the Lorenz curve
plot(lorenz_curve_Turkey, main='Lorenz Curve with Perfect Equality Line \n Turkey_2022', 
     xlab='Cumulative Share of Population (each 5% of population_Vigintile)', ylab='Cumulative Share of Income per vigintile ($ 2023,PPP)', col='red',
     cex.lab = 1.3,  # Increase the size of the axis labels
     cex.axis = 1.2, # Increase the size of the axis tick labels
     cex.main = 1.8) 

# Add points to the plot
# Extract the Lorenz curve data
lorenz_data_turkey <- data.frame(x = lorenz_curve_Turkey$p, y = lorenz_curve_Turkey$L)
head(lorenz_data_turkey,20)
# Add the points to the plot
points(lorenz_data_turkey$x, lorenz_data_turkey$y, pch=16, col='blue') 

Mexico_PL <- filter(OECD_vigintiles,Country =='Mexico', Year==2022)
lorenz_curve_mexico <- ineq::Lc(Mexico_PL $postTaxIncome)
print(lorenz_curve_mexico)

lithuania <- filter(OECD_vigintiles, Country =='Lithuania' & Year == 1980)
lorenz_curve_lithuania <- ineq::Lc(lithuania $postTaxIncome)
print(lorenz_curve_lithuania)


israel_lorenz <- filter(OECD_vigintiles, Country =='Israel', Year == 1991)
lorenz_curve_israel <- ineq::Lc(israel_lorenz $postTaxIncome)
print(lorenz_curve_israel)

colombia_lorenz <- filter(OECD_vigintiles, Country =='Colombia', Year == 2022)
lorenz_curve_colombia <- ineq::Lc(colombia_lorenz $postTaxIncome)
print(lorenz_curve_colombia)

CR_lorenz <- filter(OECD_vigintiles, Country =='Costa_Rica', Year == 2022)
lorenz_curve_CR <- ineq::Lc(CR_lorenz $postTaxIncome)
print(lorenz_curve_CR)

Canada_lorenz <- filter(OECD_vigintiles, Country =='Canada', Year ==2022)
lorenz_curve_can <- ineq::Lc(Canada_lorenz $ postTaxIncome)
print(lorenz_curve_can)

Mexico_lorenz <- filter(OECD_vigintiles, Country == 'Mexico', Year ==2022)
lorenz_curve_mex <- ineq::Lc(Mexico_lorenz$postTaxIncome)
print(lorenz_curve_mex)

lithuania_lorenz <- filter(OECD_vigintiles, Country =='Lithuania', Year==2022)
lorenz_curve_lithu <- ineq::Lc(lithuania_lorenz$postTaxIncome)
print(lorenz_curve_lithu)
##################################################################################

library(MASS)

##### Testing ############
head(Gini_results_vigintiles)
gini_all_2022 <- filter(Gini_results_vigintiles, Year == 2022)
summary(gini_all_2022$Gini)


# Method of moments estimates
mean_gini <- mean(gini_all_2022$Gini)
var_gini <- var(gini_all_2022$Gini)
shape1_initial <- mean_gini * ((mean_gini * (1 - mean_gini) / var_gini) - 1)
shape2_initial <- (1 - mean_gini) * ((mean_gini * (1 - mean_gini) / var_gini) - 1)

# Ensure that shape parameters are positive
shape1_initial <- max(shape1_initial, 0.1)
print(shape1_initial)
shape2_initial <- max(shape2_initial, 0.1)
print(shape2_initial)

fit <- fitdistr(gini_all_2022$Gini, "beta", start = list(shape1 = shape1_initial, shape2 = shape2_initial))

#library(fitdistrplus)
#fit <- fitdist(gini_all_2022$Gini, "beta")

# Extract fitted parameters
shape1 <- fit$estimate["shape1"]
shape1
shape2 <- fit$estimate["shape2"]
shape2


# Histogram of the data
p <- ggplot(gini_all_2022, aes(x = Gini)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "gray", color = "black", alpha = 0.7) +
  labs(title ="Fitted Beta Distribution of 2022 Gini Coefficients for OECD Countries\n (α = 4.7, β = 7.8)", x = "Gini Coefficient", y = "Probability Density Function (PDF)")+
  theme(axis.title.x = element_text(face='bold'),
        axis.title.y = element_text(face= 'bold'),
        plot.title = element_text(face='bold', hjust=0.5),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text (face= 'bold'))

# Generate fitted Beta distribution
x <- seq(0, 1, length.out = 100)
y <- dbeta(x, shape1 = fit$estimate["shape1"], shape2 = fit$estimate["shape2"])
fit_data <- data.frame(x = x, y = y)

# Add Beta distribution line to the plot
p + geom_line(data = fit_data, aes(x = x, y = y), color = "blue", size = 1)
#################################################################################

###For the year 1980
head(Gini_results_vigintiles)
gini_all_1980 <- filter(Gini_results_vigintiles, Year == 1980)
summary(gini_all_1980$Gini)


# Method of moments estimates
mean_gini_1980 <- mean(gini_all_1980$Gini)
var_gini_1980 <- var(gini_all_1980$Gini)
shape1_initial <- mean_gini_1980 * ((mean_gini_1980 * (1 - mean_gini_1980) / var_gini_1980) - 1)
shape2_initial <- (1 - mean_gini_1980) * ((mean_gini_1980 * (1 - mean_gini_1980) / var_gini_1980) - 1)

# Ensure that shape parameters are positive
shape1_1980 <- max(shape1_initial, 0.1)
shape2_1980 <- max(shape2_initial, 0.1)

fit_1980 <- fitdistr(gini_all_1980$Gini, "beta", start = list(shape1 = shape1_1980, shape2 = shape2_1980))

#library(fitdistrplus)
#fit <- fitdist(gini_all_2022$Gini, "beta")

# Extract fitted parameters
shape1_fit_1980 <- fit_1980$estimate["shape1"]
shape1_fit_1980
shape2_fit_1980 <- fit_1980$estimate["shape2"]
shape2_fit_1980


# Histogram of the data
p <- ggplot(gini_all_1980, aes(x = Gini)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "gray", color = "black", alpha = 0.7) +
  labs(title ="Fitted Beta Distribution of 1980 Gini Coefficients for OECD Countries\n (α = 4.1, β = 7.8)", x = "Gini Coefficient", y = "Probability Density Function (PDF)")+
  theme(axis.title.x = element_text(face='bold'),
        axis.title.y = element_text(face= 'bold'),
        plot.title = element_text(face='bold', hjust=0.5),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text (face= 'bold'))

# Generate fitted Beta distribution
x_1980 <- seq(0, 1, length.out = 100)
y_1980 <- dbeta(x, shape1 = fit_1980$estimate["shape1"], shape2 = fit_1980$estimate["shape2"])
fit_data_1980 <- data.frame(x = x_1980, y = y_1980)

# Add Beta distribution line to the plot
p + geom_line(data = fit_data_1980, aes(x = x_1980, y = y_1980), color = "blue", size = 1)
#######################################################################
###For the year 2000
head(Gini_results_vigintiles)
gini_all_2000<- filter(Gini_results_vigintiles, Year == 2000)
summary(gini_all_2000$Gini)


# Method of moments estimates
mean_gini_2000 <- mean(gini_all_2000$Gini)
var_gini_2000 <- var(gini_all_2000$Gini)
shape1_initial <- mean_gini_2000 * ((mean_gini_2000 * (1 - mean_gini_2000) / var_gini_2000) - 1)
shape2_initial <- (1 - mean_gini_2000) * ((mean_gini_2000 * (1 - mean_gini_2000) / var_gini_2000) - 1)

# Ensure that shape parameters are positive
shape1_2000 <- max(shape1_initial, 0.1)
shape2_2000 <- max(shape2_initial, 0.1)

fit_2000 <- fitdistr(gini_all_2000$Gini, "beta", start = list(shape1 = shape1_2000, shape2 = shape2_2000))

#library(fitdistrplus)
#fit <- fitdist(gini_all_2022$Gini, "beta")

# Extract fitted parameters
shape1_fit_2000 <- fit_2000$estimate["shape1"]
shape1_fit_2000
shape2_fit_2000 <- fit_2000$estimate["shape2"]
shape2_fit_2000

# Histogram of the data
p <- ggplot(gini_all_2000, aes(x = Gini)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "gray", color = "black", alpha = 0.7) +
  labs(title ="Fitted Beta Distribution of 2000 Gini Coefficients for OECD Countries\n (α = 5.1, β = 8.7)", x = "Gini Coefficient", y = "Probability Density Function (PDF)")+
  theme(axis.title.x = element_text(face='bold'),
        axis.title.y = element_text(face= 'bold'),
        plot.title = element_text(face='bold', hjust=0.5),
        axis.text.x = element_text(face='bold'),
        axis.text.y = element_text (face= 'bold'))

# Generate fitted Beta distribution
x_2000 <- seq(0, 1, length.out = 100)
y_2000 <- dbeta(x, shape1 = fit_2000$estimate["shape1"], shape2 = fit_2000$estimate["shape2"])
fit_data_2000 <- data.frame(x = x_2000, y = y_2000)

# Add Beta distribution line to the plot
p + geom_line(data = fit_data_2000, aes(x = x_2000, y = y_2000), color = "blue", size = 1)
###############################################################################

# Create data frames for each year with fitted Beta distribution
x_vals <- seq(0, 1, length.out = 100)

fit_data_1980 <- data.frame(x = x_vals, y = dbeta(x_vals, shape1 = 4.1, shape2 = 7.8), Year = "1980")
fit_data_2000 <- data.frame(x = x_vals, y = dbeta(x_vals, shape1 = 5.1, shape2 = 8.7), Year = "2000")
fit_data_2022 <- data.frame(x = x_vals, y = dbeta(x_vals, shape1 = 4.7, shape2 = 7.8), Year = "2022")

combined_data <- filter(Gini_results_vigintiles, Year%in% c(1980,2000,2022))
head(combined_data)

fit_data_combined <- bind_rows(
  fit_data_1980,
  fit_data_2000,
  fit_data_2022
)
head(fit_data_combined)

# Define a custom labeller function
custom_labeller <- labeller(
  Year = c(
    "1980" = "1980\n(α = 4.1, β = 7.8)",
    "2000" = "2000\n(α = 5.1, β = 8.7)",
    "2022" = "2022\n(α = 4.7, β = 7.8)"
  )
)
# Create the faceted plot
p <- ggplot(combined_data, aes(x = Gini)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "gray", color = "black", alpha = 0.7) +
  geom_line(data = fit_data_combined, aes(x = x, y = y), color = "blue", size = 1) +
  facet_wrap(~ Year, scales = "free_x", labeller = custom_labeller) +
  labs(title = "Fitted Beta Distribution of Gini Coefficients for OECD Countries",
       x = "Gini Coefficient",
       y = "Probability Density Function (PDF)") +
  theme(axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'))
print(p)
#######################################################################################################

###Finding out the correlation between unemployment rate and Gini Index
Unemployment <- read_excel('Unemployment_WorldBank.xls')
head(Unemployment,10)
Unemployment_long <- data.frame(pivot_longer(Unemployment,
                                             cols = -Country,
                                             names_to = 'Year',
                                             values_to = 'Unemployment_Rate'))
head(Unemployment_long,10)
str(Unemployment_long)

###Checking the structure of the dataframes
str(Gini_results_vigintiles)



####Merging the dataframes
Gini_Unemployment <- merge(Gini_results_vigintiles,Unemployment_long, by = c("Country", "Year"))
head(Gini_Unemployment,20)

Gini_Unemployment <- Gini_Unemployment%>%
  mutate(Gini= Gini*100)

##Handling missing values with Mean Imputation
sum(is.na(Gini_Unemployment))
colSums(is.na(Gini_Unemployment))
Gini_Unemployment <- Gini_Unemployment %>%
  group_by(Country) %>%
  mutate(Unemployment_Rate = ifelse(is.na(Unemployment_Rate), 
                                    mean(Unemployment_Rate, na.rm = TRUE), 
                                    Unemployment_Rate)) %>%
  ungroup()
head(Gini_Unemployment,20)
#############################################################################


##Conducting further analysis 
###Converting into the long dataframe
long_data_Gini_Un <- Gini_Unemployment %>%
  pivot_longer(cols = c(Gini, Unemployment_Rate), names_to = "Economic_Indicator", values_to = "Value")
long_data_Gini_Un
head(long_data_Gini_Un,20)

head(Gini_Unemployment)

###Checking min and max of Unemployment
min_unemployment_rate <- min(Gini_Unemployment$Unemployment_Rate)
max_unemployment_rate <- max(Gini_Unemployment$Unemployment_Rate)
print(min_unemployment_rate)
print(max_unemployment_rate)
################################################################################


#####Trying to plot for one country

head(Gini_Unemployment)
Gini_Un_Spain <- filter(Gini_Unemployment, Country == 'Spain')
head(Gini_Un_Spain,20)
#Gini_Un_Spain <- Gini_Un_Spain %>%
  #mutate(Gini = Gini * 100)
#head(Gini_Un_Spain)
###change Year variable into a numeric character
Gini_Un_Spain$Year <- as.numeric(as.character(Gini_Un_Spain$Year))

ggplot(Gini_Un_Spain, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate * 3, color = "Unemployment Rate", group=1), linetype = "dotdash", linewidth = 1) +
  geom_line(aes(y = Gini, color = "Gini Index", group = 1), linewidth = 1) +
  scale_y_continuous(
    name = "Gini Index (LHS scale)",
    sec.axis = sec_axis(~ . / 3, name = "Unemployment Rate (%) (RHS scale)", breaks = seq(0, 30, by = 5))
  ) +
  scale_x_continuous(breaks = seq(1980, 2022, by = 5)) +
  scale_color_manual(values = c("Unemployment Rate" = "blue", "Gini Index" = "red")) +
  labs(
    title = "Trends in Gini Index and Unemployment Rate: A Historical Overview\n Spain (1980 to 2022)",
    x = "Year"
  ) +
  theme_minimal() +
  theme(plot.title= element_text(size = 15, face= 'bold', hjust=0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.text.y.right = element_text(color = "black", size= 15),
        axis.title.y.right = element_text(color = "black", size = 15),
        axis.text.y.left = element_text(color = "black", size = 15),
        axis.text.x = element_text(angle = 45, face='bold',size= 10, hjust = 1),
        axis.title.y.left = element_text(color = "black"),
        legend.position = "bottom"
  ) 

###################################################################
###Finding out the correlation between unemployment rate and Gini Index
Unemployment <- read_excel('Unemployment_WorldBank.xls')
head(Unemployment,10)
Unemployment_long <- data.frame(pivot_longer(Unemployment,
                                             cols = -Country,
                                             names_to = 'Year',
                                             values_to = 'Unemployment_Rate'))
head(Unemployment_long,10)
str(Unemployment_long)

###Checking the structure of the dataframes
str(Gini_results_vigintiles)



####Merging the dataframes
Gini_Unemployment <- merge(Gini_results_vigintiles,Unemployment_long, by = c("Country", "Year"))
head(Gini_Unemployment,20)

#Gini_Unemployment <- Gini_Unemployment%>%
  #mutate(Gini= Gini*100)

##Handling missing values with Mean Imputation
sum(is.na(Gini_Unemployment))
colSums(is.na(Gini_Unemployment))
Gini_Unemployment <- Gini_Unemployment %>%
  group_by(Country) %>%
  mutate(Unemployment_Rate = ifelse(is.na(Unemployment_Rate), 
                                    mean(Unemployment_Rate, na.rm = TRUE), 
                                    Unemployment_Rate)) %>%
  ungroup()
head(Gini_Unemployment,20)

Gini_Unemployment_five<- filter(Gini_Unemployment, Country %in% c('Chile','Greece','Denmark','Ireland','Japan'))
Gini_Unemployment_five

# Create scatter plot with linear regression line
ggplot(Gini_Unemployment_five, aes(x = Unemployment_Rate, y = Gini)) +
  geom_point(size= 2) +                             # Add points
  geom_smooth(method = "lm", se = FALSE) +   # Add linear regression line without confidence interval
  labs(title = "Linear Regression Analysis of Gini Coefficient Vs Unemployment Rate\nSelected Five Countries_OECD Members",
       x = "Unemployment Rate (% of total labor force)",
       y = "Gini Coefficient") +
  theme_minimal()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust= 0.5),
        axis.title.x = element_text(size = 15, face= 'bold'),
        axis.title.y = element_text(size = 15, face= 'bold'),
        axis.text.y= element_text(size=13, face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face= 'bold', size =13),
        strip.text = element_text(size = 13, face = 'bold'))+
  facet_wrap(~ Country, scales = 'free_x')

Gini_Un_Chile <- filter(Gini_Unemployment_five, Country=='Chile')
m_Chile<- lm(Gini ~ Unemployment_Rate, Gini_Un_Chile)
summary(m_Chile)

Gini_Un_Japan <- filter(Gini_Unemployment, Country=='Japan')
m_Japan<- lm(Gini ~ Unemployment_Rate, Gini_Un_Japan)
summary(m_Japan)

Gini_Un_Greece <- filter(Gini_Unemployment_five, Country=='Greece')
m_Greece<- lm(Gini ~ Unemployment_Rate, Gini_Un_Greece)
summary(m_Greece)

Gini_Un_Denmark <- filter(Gini_Unemployment, Country=='Denmark')
m_Denmark<- lm(Gini ~ Unemployment_Rate, Gini_Un_Denmark)
summary(m_Denmark)

Gini_Un_Ireland<- filter(Gini_Unemployment_five, Country=='Ireland')
m_Ireland<- lm(Gini ~ Unemployment_Rate, Gini_Un_Ireland)
summary(m_Ireland)



