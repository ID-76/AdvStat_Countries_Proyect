#Proyecto AdvStat
shrek <- read.csv("worlddata2023.csv", fileEncoding = "UTF-8")
print(shrek)



#a. Are the data observational or experimental?
#The data is likely observational, 
#reflecting economic and social indicators collected without experimental intervention.

#b. How many observations and variables do you have?
dim(shrek) #195 indicates the number of rows
           #35 indicates the number of columns

#c. Are there missing values?

#d. How are the data coded? Indicate the type of each variable (continuous, discrete,
#categorical, binary, etc.)
str(shrek)

## Continuous Variables (Numerical)
# Birth.Rate: Birth rate.
# Fertility.Rate: Fertility rate.
# Infant.mortality: Infant mortality.
# Life.expectancy: Life expectancy.
# Physicians.per.thousand: Physicians per thousand people.
# Latitude: Latitude.
# Longitude: Longitude.
## Discrete Variables (Numerical)
# Maternal.mortality.ratio: Maternal mortality ratio.
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