#Proyecto AdvStat
library(tidyverse)
library(ggplot2)

shrek <- read.csv("worlddata2023.csv", fileEncoding = "UTF-8")
print(shrek)



# a. Are the data observational or experimental?
# The data is  observational, 
# reflecting economic and social indicators collected without experimental intervention.



# b. How many observations and variables do you have?
dim(shrek) #195 indicates the number of rows
           #35 indicates the number of columns



# c. Are there missing values?
# Yes, specially in Armed forces size, gasoline price, minimum wage, 
# labor force participation, tax revenue or unemployment rate.
cat("\nNANs:\n")
count_NA <- function(dataframe) {
  sapply(dataframe, function(x) sum(is.na(x) | x == ""))  
}
count_NA(shrek)



# d. How are the data coded? Indicate the type of each variable (continuous, discrete,
# categorical, binary, etc.)
str(shrek)

## Continuous Variables (Numerical)
# Birth.Rate: Birth rate.
# Fertility.Rate: Fertility rate.
# Infant.mortality: Infant mortality per 1.000 births.
# Life.expectancy: Life expectancy.
# Physicians.per.thousand: Physicians per thousand people.
# Latitude: Latitude.
# Longitude: Longitude.

## Discrete Variables (Numerical)
# Maternal.mortality.ratio: Maternal mortality ratio per 100.000 births.

## Categorical Variables (Nominal)
# Country: Country.
# Abbreviation: Country abbreviation.
# Capital.Major.City: Capital or major city.
# Currency.Code: Currency code.
# Largest.city: Largest city.
# Official.language: Official language.

## Categorical Variables (Ordinal)
# Density..P.Km2.: Population density per square km (although represented as text, it could be an ordinal category).
# Agricultural.Land....: Percentage of agricultural land.
# Land.Area.Km2.: Land area in square km.
# Armed.Forces.size: Size of the armed forces.
# Co2.Emissions: CO2 emissions.
# CPI: Consumer Price Index (CPI).
# CPI.Change....: CPI change.
# Forested.Area....: Percentage of forested area.
# Gasoline.Price: Gasoline price.
# GDP: Gross Domestic Product.
# Gross.primary.education.enrollment....: Primary education enrollment.
# Gross.tertiary.education.enrollment....: Tertiary education enrollment.
# Out.of.pocket.health.expenditure: Percentage of out-of-pocket health expenditure.
# Population..Labor.force.participation....: Labor force participation.
# Tax.revenue....: Tax revenue percentage.
# Total.tax.rate: Total tax rate.
# Unemployment.rate: Unemployment rate.
# Urban_population: Urban population.
# Population: Total population (represented as text).



# e. What are the units of measurement?
# It depends on the measurement but the most common ones are: Km2 for area related data, 
# number of metric tons for CO2 emissions, US dollars for economy or price related data,
# number of deaths per 1.000 or 100.000 births for the different mortality ratios.





#colnames(shrek)[2] = "Density (P/Km2)" # Column name was wrapped
#shrek[shrek == "S�����������"]<- "Sao Tome and Principe"