#Proyecto AdvStat
# Tip: Use the down arrow (v) besides the line number to collapse sections for a better experience
library(tidyverse)
library(ggplot2)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))

shrek <- read.csv("worlddata2023.csv", fileEncoding = "UTF-8")
asno <- read.csv("2021.csv", fileEncoding = "UTF-8")
print(shrek)
print(asno)


#### a. Are the data observational or experimental? ####
# The data in the countries dataset is observational,
# reflecting economic and social indicators collected without experimental intervention.
# However the Happiness dataset is experimental because the data was collected through polls.


#### b. How many observations and variables do you have? ####
dim(shrek) #195 rows, this is the number of countries
#35 columns, this is the number of parameters for each country
dim(asno) #149 rows, this is the number of countries
#20 columns, this is the number of parameters for each country


##### c. Are there missing values? #####
# Yes, but only for the countries dataset. There are specially present in:
# Armed forces size, gasoline price, minimum wage,
# labor force participation, tax revenue or unemployment rate.
count_NA <- function(dataframe) {
  na_count <- sapply(dataframe, function(x) sum(is.na(x) | x == ""))
  na_percentage <- round((na_count / nrow(dataframe)) * 100, 2)
  
  result <- data.frame(
    Column = names(na_count),
    NA_Count = na_count,
    NA_Percentage = na_percentage
  )
  
  cat("\nNANs:\n")
  return(result)
}

count_NA(shrek)
count_NA(asno)




##### d. How are the data coded? Indicate the type of each variable #####
str(shrek)
str(asno)

#### COUNTRIES DATASET ####

## Continuous Variables (Numerical)
# Birth Rate: The number of births per 1,000 people in a year.
# Fertility Rate: The average number of children born per woman over her lifetime.
# Infant Mortality: The number of infant deaths per 1,000 live births.
# Life Expectancy: The average number of years a newborn is expected to live under current mortality conditions.
# Physicians per Thousand: The number of medical doctors per 1,000 people in a country.
# Latitude: The geographical latitude of a country, measured in degrees.
# Longitude: The geographical longitude of a country, measured in degrees.

## Discrete Variables (Numerical)
# Maternal Mortality Ratio: The number of maternal deaths per 100,000 live births.

## Categorical Variables (Nominal)
# Country: The name of the country.
# Abbreviation: The abbreviated name of the country.
# Capital or Major City: The capital or the largest city of the country.
# Currency Code: The official currency code used in the country (e.g., USD, EUR).
# Largest City: The most populous city in the country.
# Official Language: The official language(s) spoken in the country.

## Categorical Variables (Ordinal)
# Population Density per Km²: The number of people per square kilometer. Although represented as text,
# it could be categorized into levels (low, medium, high).
# Agricultural Land Percentage: The proportion of land used for agriculture.
# Land Area (Km²): The total land area of the country, measured in square kilometers.
# Armed Forces Size: The number of active military personnel in a country.
# CO₂ Emissions: The total amount of carbon dioxide emitted by a country.
# Consumer Price Index (CPI): A measure that examines the average change in prices of goods and services over time.
# CPI Change: The percentage change in the Consumer Price Index over a specific period.
# Forested Area Percentage: The proportion of land covered by forests.
# Gasoline Price: The cost of one liter of gasoline in US dollars.
# Gross Domestic Product (GDP): The total economic output of a country, measured in US dollars.
# Primary Education Enrollment: The percentage of eligible children enrolled in primary education.
# Tertiary Education Enrollment: The percentage of eligible individuals enrolled in higher education institutions.
# Out-of-Pocket Health Expenditure: The percentage of total health expenses paid directly by individuals.
# Labor Force Participation: The percentage of the working-age population that is either employed or actively looking for work.
# Tax Revenue Percentage: The proportion of a country's GDP collected as taxes.
# Total Tax Rate: The percentage of income or corporate profit paid in taxes.
# Unemployment Rate: The percentage of the labor force that is unemployed and actively seeking work.
# Urban Population: The percentage of the population living in urban areas.
# Total Population: The total number of people living in a country. Although represented as text,
# it could be categorized into population size ranges.
#### HAPPINESS INDEX DATASET ####

# Categorical Variables (Nominal)
# Country name : We show the names of the countries.
# Regional indicator : Shows which region the country is in.

# Continuous Variables
# Ladder score : Ladder score of countries.
# Standard error of ladder score : Showing the error rate in the ladder score.
# upperwhisker : Upper limit of score.
# lowerwhisker : Lower limit of score.
# Logged GDP per capita : Total monetary or market value of all the finished goods
# and services produced within a country's borders in a specific time period.
# Social support : Social support scores of countries.
# Healthy life expectancy : Healthy life expectancy of countries.
# Freedom to make life choices : The freedom of people in that country to choose life.
# Generosity : Generosity rate of the country.
# Perceptions of corruption : Corruption perceptions in the country.
# Ladder score in Dystopia : Ladder score in Dystopia of the country
# (Dystopia: alternatively cacotopia or simply anti-utopia, is a community
# or society that is undesirable or frightening).
# Explained by: Log GDP per capita : Explained Log GDP per capita ratio.
# Explained by: Social support : Explained social support rate.
# Explained by: Healthy life expectancy : Explained healthy life expectancy.
# Explained by: Freedom to make life choices : Explained freedom to make life choices.
# Explained by: Generosity : Explained generosity.
# Explained by: Perceptions of corruption : Explained perceptions of corruption.
# Dystopia + residual : Dystopia + residual total rate.




##### e. What are the units of measurement? #####
# It depends on the measurement but the most common ones are: Km2 for area related data,
# number of metric tons for CO2 emissions, US dollars for economy or price related data,
# number of deaths per 1.000 or 100.000 births for the different mortality ratios.

##### Data Preparation #####

colnames(shrek)[2] = "Density (P/Km2)" # Column name was wrapped
# Add missing Country/City names (probably a encoding error by the author) 
shrek[shrek == "S�����������"]<- "Sao Tome and Principe"
shrek[shrek == "Bras���"]<- "Brasilia"
shrek[shrek == "Bogot�"]<- "Bogota"
shrek[shrek == "San Jos������"]<- "San Jose"
shrek[shrek == "Reykjav��"]<- "Reykjavik"
shrek[shrek == "Mal�"]<- "Male"
shrek[shrek == "Chi����"]<- "Chisinau"
shrek[shrek == "Asun��"]<- "Asuncion"
shrek[shrek == "S����"]<- "Sao Tome"
shrek[shrek == "Lom�"]<- "Lome"
shrek[shrek == "Nuku����"]<- "Nukualofa"
shrek[shrek == "S����"]<- "Sao Paulo"
shrek[shrek == "Statos�������"]<- "Nicosia"
shrek[shrek == "S�����"]<- "Stockholm"
shrek[shrek == "Z���"]<- "Zurich"


#### 6. Graphs ####

# 1st: Tax revenue -- GDP

# sum(is.na(shrek$Total.tax.rate)) = 0
# sum(is.na(shrek$Tax.revenue....)) = 0

data1 <- shrek[!shrek$GDP == "",] # Removing all blank spaces in GDP
data1 <- data1[!data1$Tax.revenue.... == "",] # Removing all blank spaces in Tax.Revenue

# now that we don't have any blank rows we will pass the information from string to it's respective type of data

data1$GDP <- gsub("\\$", "", data1$GDP) # Removing the '$' to convert it into numerical 
data1$GDP <- gsub(",", "", data1$GDP) # Removing the ','
data1$Tax.revenue.... <- gsub("%", "", data1$Tax.revenue....) # Removing the '%' to convert it into numerical where will be out of 100
data1 <- data1 %>% mutate(GDP = as.numeric(GDP)) # Convert GDP row into numerical
data1 <- data1 %>% mutate(Tax.revenue.... = as.numeric(Tax.revenue....)) # Converting Tax.revenue row into numerical

first <- ggplot(data1, aes(x = Tax.revenue...., y = GDP)) + geom_col(color = "blue")
first <- first + labs(title = "Relation between Tax revenue and GDP", x = "Tax revenue", y = "GDP")
first

# We don't see a clear relationship between this variables but we can see that when the Tax Revenue is too high or too low the GDP decreases considerably

# 2nd: Relation between Out of pocket Health expenditure and Infant mortality

# sum(is.na(shrek$Out.of.pocket.health.expenditure)) = 0
# sum(is.na(shrek$Infant.mortality)) = 0
# Removing all the blank spaces of the variables we need
data2 <- shrek[!shrek$Infant.mortality == "",] # To eliminate the rows were there is a blank space
data2 <- data2[!data2$Out.of.pocket.health.expenditure == "",] # To eliminate the rows were there is a blank space

# Converting needed rows into numeric ones
data2$Out.of.pocket.health.expenditure <- gsub("%", "", data2$Out.of.pocket.health.expenditure)
data2 <- data2 %>% mutate(Infant.mortality = as.numeric(Infant.mortality))
data2 <- data2 %>% mutate(Out.of.pocket.health.expenditure = as.numeric(Out.of.pocket.health.expenditure))

second <- ggplot(data2, aes( x = Out.of.pocket.health.expenditure, y = Infant.mortality))
second <- second + labs(title = "Relation between Out of pocket Health expenditure and Infant Mortality", x = "Out of pocket health expenditure", y = "Infant Mortality")
second <- second + geom_smooth(method = "lm") + geom_point(color = "green")
second

# We can see a relation between this two variable, the more high the out of pocket health expenditure is the more infant mortality

# 4th: Relation between CO2 emisions and Urban population

# Removing all the blank spaces of the variables we need

# sum(is.na(shrek$Co2.Emissions)) = 0
# sum(is.na(shrek$Urban_population)) = 0
data4 <- shrek[!shrek$Co2.Emissions == "",]
data4 <- data4[!data4$Urban_population == "",]

data4$Co2.Emissions <- gsub(",","",data4$Co2.Emissions)
data4$Urban_population <- gsub(",", "", data4$Urban_population)
# Converting needed rows into numeric ones
data4 <- data4 %>% mutate(Co2.Emissions = as.numeric(Co2.Emissions))
data4 <- data4 %>% mutate(Urban_population = as.numeric(Urban_population))

fourth <- ggplot(data4, aes(x = Co2.Emissions, y = Urban_population)) + geom_point(color = "orange")
fourth <- fourth + labs(title = "Relation between CO2 emisions and Urban Population") + geom_smooth(method =  "gam")
fourth

# The high CO2 emission is a clear consequence of a high urban population

# 5th: Relation between Population density and Land area

# Removing all the blank spaces of the variables we need


# sum(is.na(data5$`Density (P/Km2)`)) == 0
#data5 <- data5[!data5$`Density (P/Km2)` == "",]
# Converting needed rows into numeric ones
#data5$Land.Area.Km2. <- gsub(",","",data5$Land.Area.Km2.)
#data5 <- data5 %>% mutate(Land.Area.Km2. = as.numeric(Land.Area.Km2.))
#data5$`Density (P/Km2)` <- gsub(",","", data5$`Density (P/Km2)`)
#data5 <- data5 %>% mutate(`Density (P/Km2)` = as.numeric(`Density (P/Km2)`))

#five <- ggplot(data5, aes(x = Land.Area.Km2., y = `Density (P/Km2)`)) + geom_area(color = "red")
#five <- five + labs(title = "Relation between Population density and Land", x = "Area", y = "Population density")
#five

# When the Land Area is high, the Population density decreases a lot

# 6th: Relation between Population: Labor fource participation and Unemployment rate

# Removing all the blank spaces of the variables we need

data6 <- shrek[!shrek$Population..Labor.force.participation.... == "",]
data6 <- data6[!data6$Unemployment.rate == "",]

data6$Population..Labor.force.participation.... <- gsub("%","",data6$Population..Labor.force.participation....)
data6$Unemployment.rate <- gsub("%","", data6$Unemployment.rate)
# Converting needed rows into numeric ones
data6 <- data6 %>% mutate(Population..Labor.force.participation.... = as.numeric(Population..Labor.force.participation....))
data6 <- data6 %>% mutate(Unemployment.rate = as.numeric(Unemployment.rate))


six <- ggplot(data6, aes(x = Population..Labor.force.participation...., y = Unemployment.rate)) + geom_point(color = "black") + geom_smooth(method = "lm", color = "red")
six <- six + labs(title = "Relation between Labor fource participation and Unemployment Rate", x = "Labor fource participation", y = "Unemployment Rate")
six

# They have a relation, a high labor fource participation lead to a small unemployment rate

#7th: Relationship between perception of corruption and ladder score
#In the happiness dataset aren't null values nor blanck spaces so it isn't necessary to eliminate rows
seven <- ggplot(asno, aes(x = Perceptions.of.corruption, y = Ladder.score)) + geom_point(color = "blue") + geom_smooth(method = "lm", color = "black")
seven <- seven + labs(title = "Relationship between perception of corruption and ladder score", x = "Perceptions of corruption", y = "Ladder Score")
seven

#With this graph we can conclude that the corruption conceived by population affects in their happiness

#8th: Relationship between the power to make choices and ladder score
eight <- ggplot(asno, aes(x = Freedom.to.make.life.choices, y = Ladder.score)) + geom_point(color = "orange") + geom_smooth(method = "lm", color = "black")
eight <- eight + labs(title = "Relationship between the power to make choices and ladder score", x = "Freedom to make life choices", y = "Ladder Score")
eight <- eight + facet_grid(.~ Regional.indicator)
eight

#Is a strong relation between the freedom of speech and decision making with the happiness conceived
#the graph have been splitted so it can be seen that this tendency mantains within almost all world areas

#9th: Relationship between life expectancy and social support
nine <- ggplot(asno, aes(x = Healthy.life.expectancy, y = Social.support)) + geom_point(color = "green") + geom_smooth(method = "lm", color = "black")
nine <- nine + labs(title = "Relationship between life expectancy and social support", x = "Healthy life expectancy", y = "Social support")
nine

#We can see than in countries where is more social support people tend to live more

# Variables summaries
# Population Density
#shrek <- shrek[!is.na(shrek$Density..P.Km2.),]
# shrek$Density..P.Km2. <- gsub(",", "", shrek$Density..P.Km2.)
# shrek <- shrek %>% mutate(Density..P.Km2. = as.numeric(Density..P.Km2.)) 
# 
# mean(shrek$Density..P.Km2.)
# median(shrek$Density..P.Km2.)
# sd(shrek$Density..P.Km2.)
# 
# # Gasoline prices
# 
# shrek$Gasoline.Price <- gsub("\\$","", shrek$Gasoline.Price)
# shrek <- shrek[!is.na(shrek$Gasoline.Price),]
# shrek <- shrek[!shrek$Gasoline.Price == "",]
# shrek <- shrek %>% mutate(Gasoline.Price = as.numeric(Gasoline.Price))
# 
# mean(shrek$Gasoline.Price)
# median(shrek$Gasoline.Price)
# sd(shrek$Gasoline.Price)
# 
# 
# # Life expectancy
# 
# shrek <- shrek[!is.na(shrek$Life.expectancy),]
# # shrek <- shrek[!shrek$Life.expectancy = "",]
# shrek <- shrek %>% mutate(Life.expectancy = as.numeric(Life.expectancy))
# 
# mean(shrek$Life.expectancy)
# median(shrek$Life.expectancy)
# sd(shrek$Life.expectancy)
# 
# 
# # Generosity
# 
# mean(asno$Generosity)
# median(asno$Generosity)
# sd(asno$Generosity)
# min(asno$Generosity)
# max(asno$Generosity)
# 
# # Perceptions of corruption	
# mean(asno$Perceptions.of.corruption)
# median(asno$Perceptions.of.corruption)
# sd(asno$Perceptions.of.corruption)
# min(asno$Perceptions.of.corruption)
# max(asno$Perceptions.of.corruption)

model1 <- lm(Density..P.Km2. ~ Agricultural.Land.... + Land.Area.Km2. + Armed.Forces.size + Birth.Rate +
               Calling.Code + Co2.Emissions + CPI + CPI.Change.... + Currency.Code + Fertility.Rate +
               Forested.Area.... + Gasoline.Price + GDP + Gross.primary.education.enrollment.... +
               Gross.tertiary.education.enrollment.... + Infant.mortality + Life.expectancy +
               Maternal.mortality.ratio + Minimum.wage + Out.of.pocket.health.expenditure +
               Physicians.per.thousand + Population + Population..Labor.force.participation.... +
               Tax.revenue.... + Total.tax.rate + Unemployment.rate + Urban_population + Latitude +
               Longitude, data = shrek)
summary(model1)
