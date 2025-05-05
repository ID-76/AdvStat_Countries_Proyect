#Proyecto AdvStat
# Tip: Use the down arrow (v) besides the line number to collapse sections for a better experience
# List of required packages
packages <- c(
  "tidyverse", "ggplot2", "rstudioapi", "glue", "stringdist", "MASS",
  "dplyr", "caret", "pROC", "ca", "FactoMineR", "factoextra", "plotrix"
)

# Function to install missing packages and load all
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(packages, install_and_load))


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

#colnames(shrek)[2] = "Density (P/Km2)" # Column name was wrapped
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



shrek[shrek == ""] <- NA
clean_column <- function(col) {
  col <- gsub(",", "", col)
  col <- gsub("%", "", col)
  col <- gsub("\\$", "", col)
  col <- trimws(col)  # elimina espacios al inicio/final
  as.numeric(col)
}

# Limpiar las columnas que son <chr> pero deberían ser numéricas
cols_to_clean <- c("Density..P.Km2.", "Land.Area.Km2.", "Population", "GDP", 
                   "Urban_population", "Co2.Emissions", "Minimum.wage", "Armed.Forces.size", "Agricultural.Land....",
                   "CPI", "CPI.Change....", "Forested.Area....", "Gasoline.Price", "Gross.primary.education.enrollment....", "Gross.tertiary.education.enrollment....",
                   "Out.of.pocket.health.expenditure", "Population..Labor.force.participation....", "Tax.revenue....", "Total.tax.rate", "Unemployment.rate")

shrek <- shrek %>%
  mutate(across(all_of(cols_to_clean), clean_column))
glimpse(shrek)
head(shrek)




#### MERGE OF THE TWO DATSETS 
asno2 <- asno
colnames(asno2)[1] <- "Country"
# Merge using inner join to keep only common country names
dragona <- inner_join(shrek, asno2, by = "Country")



###################################################################
#Linear model predicting Life expectancy
model1Life <- lm(Life.expectancy ~ Density..P.Km2. + Agricultural.Land.... + Land.Area.Km2. + Armed.Forces.size + Birth.Rate +
                   Calling.Code + Co2.Emissions + CPI + CPI.Change.... + Fertility.Rate +
                   Forested.Area.... + Gasoline.Price + GDP + Gross.primary.education.enrollment.... +
                   Gross.tertiary.education.enrollment.... + Infant.mortality + Birth.Rate +
                   Maternal.mortality.ratio + Minimum.wage + Out.of.pocket.health.expenditure +
                   Physicians.per.thousand + Population + Population..Labor.force.participation.... +
                   Tax.revenue.... + Total.tax.rate + Unemployment.rate + Urban_population + Latitude +
                   Longitude, data = shrek, na.action = na.omit)

summary(model1Life)

model2Life <- lm(Life.expectancy ~ Density..P.Km2. + Infant.mortality +
                   Maternal.mortality.ratio + Minimum.wage +
                   Longitude, data = shrek, na.action = na.omit)
summary(model2Life)

model3Life<- lm(Life.expectancy ~ Density..P.Km2. + Infant.mortality +
                  Maternal.mortality.ratio + Minimum.wage, data = shrek, na.action = na.omit)
summary(model3Life)

#We consider that the model3, compared with the model2, it predicts enough better to let the predictor "longitud" in the model
anova(model3Life, model2Life)

cor(shrek$Infant.mortality, shrek$Maternal.mortality.ratio, use = "complete.obs")
#infant mortality and maternal mortality are highly correlated so I will remove the one with the highest p-value

model4Life<-lm(Life.expectancy ~ Density..P.Km2. + Infant.mortality + Minimum.wage +
                 Longitude, data = shrek, na.action = na.omit)
summary(model4Life)
confint(model4Life)

#We split the data in two, one for training and the other for testing
n <- nrow(shrek)
train_ids <- sample(1:n, size = 0.8 * n)
train_data <- shrek[train_ids, ]
test_data <- na.omit(shrek[-train_ids, ])

# Train the data with the 80%
model_train <- lm(Life.expectancy ~ Density..P.Km2. + Infant.mortality + Minimum.wage + Longitude, 
                  data = train_data, na.action = na.exclude)

#Prediction and confidence intervals
predict(model_train, newdata = test_data, interval = "prediction")
predict(model4Life, newdata = test_data, interval = "confidence")

#With this plot we can see that residuals aren't correlated 
residuals_clean <- na.omit(model4Life$residuals)
plot(residuals_clean, type = "o", main="Residuals vs. Observation Order",
     xlab="Observations", ylab="Residuals", col="green")
abline(h=0, col="black", lwd=2, lty=2)

#We can see that our model is completly linear
plot(model4Life, 1)

#Although we can see a decrease in the variance, it isn't significative enough to say that it is heteroscedasticity
ggplot(data = data.frame(Fitted = model4Life$fitted.values, Residuals = model4Life$residuals), 
       aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_smooth(color = "red") +
  geom_quantile(quantiles = c(0.05, 0.95), color = "blue", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

#The high W and p-value of the shapiro test show us that the model follows a normal distribution
shapiro.test(model4Life$residuals)
plot(model4Life,2)

#We studentized the resiudals so we can indentify clearly the outliers
stud_resids <- studres(model4Life)

plot(model4Life$fitted.values,stud_resids,
     xlab="Fitted", ylab ="Studentized Residuals")
abline (0 , 0)
outliers <- ifelse(abs(stud_resids) > 3, TRUE, FALSE)
#Now that we have identified the outliers, we are going to analyse if the model improves without them
model_clean <- update(model4Life, subset = -outliers)

#As the outliers don't look like if they were a human error, and as the model prediction 
#capacity doesn't improve with or without outliers, we are not going to eliminate them
summary(model_clean)
summary(model4Life)


###################################################################

###################################################################
#We choose Bith.Rate as the variable to predict because we did Minimum.wage and 
#followed a non-linear regression impossible to correct it with log()
modeltristeza <- lm(Birth.Rate ~ Density..P.Km2. + Agricultural.Land.... + Land.Area.Km2. + CPI +
                      Calling.Code + Life.expectancy + GDP + CPI.Change.... + Fertility.Rate +
                      Forested.Area.... + Gasoline.Price + Unemployment.rate + Gross.primary.education.enrollment.... +
                      Gross.tertiary.education.enrollment.... + Infant.mortality + Minimum.wage +
                      Maternal.mortality.ratio + Armed.Forces.size + Out.of.pocket.health.expenditure +
                      Physicians.per.thousand + Co2.Emissions + Population..Labor.force.participation.... +
                      Tax.revenue.... + Total.tax.rate + Urban_population + Population + Latitude +
                      Longitude, data = shrek, na.action = na.omit)
summary(modeltristeza)
#We remove the ones that didn't make sense with Birth.Rate and the ones that weren't related(high p-value)
model2tristeza <- lm(Birth.Rate ~ Unemployment.rate + Gross.primary.education.enrollment.... +
                       Gross.tertiary.education.enrollment....  + Maternal.mortality.ratio + Out.of.pocket.health.expenditure +
                       Physicians.per.thousand, data = shrek, na.action = na.omit)
summary(model2tristeza)
#The final stage of this model has all the related variables for Birth.Rate
model3tristeza <- lm(Birth.Rate ~ Gross.tertiary.education.enrollment....  
                     + Maternal.mortality.ratio + Physicians.per.thousand, data = shrek, na.action = na.omit)
summary(model3tristeza)

#Comparison of models
#anova(model3tristeza, model2tristeza)

#confidence 
confint(model3tristeza)
confint(model3tristeza, level = 0.99)

# Train the data with the 80%
model_train2 <- lm(Birth.Rate ~ Gross.tertiary.education.enrollment....  
                   + Maternal.mortality.ratio + Physicians.per.thousand,
                   data = train_data, na.action = na.omit)
summary(model_train2)

prediction_confidence <- predict(model_train2, newdata = test_data, interval = "confidence")
predictionx2 <- predict(model_train2, newdata = test_data, interval = "prediction")
head(prediction_confidence)
head(predictionx2)

#Residual Plot
residuals_clean <- na.omit(model_train2$residuals)

#Residuals vs. Observations Plot
plot(residuals_clean, type = "o", main="Residuals vs. Observation Order",
     xlab="Observations", ylab="Residuals", col="green")

abline(h=0, col="black", lwd=2, lty=2)

plot(model3tristeza, 1)
#The residuals vs. fitted values plot shows a curved pattern, suggesting non-linearity in the model. 
#Applying a log transformation to the predictors could help straighten the trend, 
#stabilize variance, and improve the model fit

model3tristezalog <- lm(Birth.Rate ~ log(Gross.tertiary.education.enrollment....)  
                        + log(Maternal.mortality.ratio) +
                          log(Physicians.per.thousand), data = train_data, na.action = na.omit)
plot(model3tristezalog,1)
#After applying the log transformation, the residuals now show a more random spread around zero, reducing 
#the previous curve, this suggests that the transformation improved the linearity and model fit


#This graph helps visualize the distribution of the residuals
#The red line shows the remaining patterns, while the dashed blue 
#quantile lines show how they spread out from the residuals,
#in this case it's possible to note a minor decrease in variance,
#so isn't enough to confirm heteroscedasticity
ggplot(data = data.frame(Fitted = model3tristezalog$fitted.values, Residuals = model3tristezalog$residuals), 
       aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_smooth(color = "red") +
  geom_quantile(quantiles = c(0.05, 0.95), color = "blue", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")
#We are going to verify if the model follows a normal distribution
shapiro.test(model3tristezalog$residuals)
plot(model3tristezalog,2)
#The residuals are not perfectly normal, but the residuals approximate a normal distribution 
#at the center

stud_resids2 <- studres(model3tristezalog)

plot(model3tristezalog$fitted.values,stud_resids2,
     xlab="Fitted", ylab ="Studentized Residuals")
abline (0 , 0)
outliers <- ifelse(abs(stud_resids2) > 3, TRUE, FALSE)
#The outlier is the oberserved value 172
model_clean2 <- update(model3tristezalog, subset = !outliers)
summary(model_clean2)
summary(model3tristezalog)
plot(model_clean2, 2)
shapiro.test(model_clean2$residuals)
plot(model_clean2, 1)
#We have appreciate that it's no worth it to remove the outlier because we suffer
#a fall of many variables (F-static, the W of shapiro test and R^2)

######################################################################

#Linear model predicting Ladder score
model1Free <- lm(Ladder.score ~ Density..P.Km2. + Agricultural.Land.... + Land.Area.Km2. + Armed.Forces.size + Birth.Rate +
                   Calling.Code + Co2.Emissions + CPI + CPI.Change.... + Fertility.Rate +
                   Forested.Area.... + Gasoline.Price + GDP + Gross.primary.education.enrollment.... +
                   Gross.tertiary.education.enrollment.... + Infant.mortality + Birth.Rate +
                   Maternal.mortality.ratio + Minimum.wage + Out.of.pocket.health.expenditure +
                   Physicians.per.thousand + Population + Population..Labor.force.participation.... +
                   Tax.revenue.... + Total.tax.rate + Unemployment.rate + Urban_population + Latitude +
                   Longitude + Regional.indicator + Standard.error.of.ladder.score +
                   Social.support + Logged.GDP.per.capita + Healthy.life.expectancy + Perceptions.of.corruption +
                   Generosity + Freedom.to.make.life.choices, data = dragona, na.action = na.omit)

summary(model1Free)

model2Free <- lm(Ladder.score ~ Agricultural.Land.... + Land.Area.Km2. + Birth.Rate +
                   Calling.Code + Co2.Emissions + CPI + Fertility.Rate +
                   Forested.Area.... + GDP + Gross.primary.education.enrollment.... +
                   Gross.tertiary.education.enrollment.... + Infant.mortality + Birth.Rate +
                   Maternal.mortality.ratio + Minimum.wage + Out.of.pocket.health.expenditure +
                   Physicians.per.thousand + Population + Population..Labor.force.participation.... +
                   Tax.revenue.... + Unemployment.rate + Urban_population + Latitude +
                   Longitude + Standard.error.of.ladder.score +
                   Social.support + Logged.GDP.per.capita + Healthy.life.expectancy + Perceptions.of.corruption +
                   Generosity + Freedom.to.make.life.choices, data = dragona, na.action = na.omit)
summary(model2Free)

model3Free <- lm(Ladder.score ~ Agricultural.Land.... +
                   Calling.Code + Co2.Emissions + CPI + Fertility.Rate + Gross.primary.education.enrollment.... +
                   Gross.tertiary.education.enrollment.... + Infant.mortality +
                   Maternal.mortality.ratio + Minimum.wage + Out.of.pocket.health.expenditure +
                   Population + Population..Labor.force.participation.... +
                   Tax.revenue.... + Unemployment.rate + Urban_population + Latitude +
                   Longitude + Standard.error.of.ladder.score +
                   Social.support + Logged.GDP.per.capita + Healthy.life.expectancy +
                   Generosity + Freedom.to.make.life.choices, data = dragona, na.action = na.omit)
summary(model3Free)

model4Free <- lm(Ladder.score ~ Calling.Code + Co2.Emissions + CPI + Fertility.Rate + 
                   Gross.primary.education.enrollment.... + Gross.tertiary.education.enrollment.... + Infant.mortality +
                   Maternal.mortality.ratio + Minimum.wage + Out.of.pocket.health.expenditure +
                   Population + Unemployment.rate + Urban_population +
                   Longitude + Social.support + Logged.GDP.per.capita + Healthy.life.expectancy +
                   Freedom.to.make.life.choices, data = dragona, na.action = na.omit)
summary(model4Free)

model5Free <- lm(Ladder.score ~ CPI + Fertility.Rate + Gross.tertiary.education.enrollment.... +
                   Maternal.mortality.ratio + Minimum.wage + Out.of.pocket.health.expenditure +
                   Population + Unemployment.rate + Urban_population + Longitude + Social.support +
                   Freedom.to.make.life.choices, data = dragona, na.action = na.omit)
summary(model5Free)

model6Free <- lm(Ladder.score ~ Minimum.wage + Unemployment.rate + Longitude + Social.support +
                   Freedom.to.make.life.choices, data = dragona, na.action = na.omit)
summary(model6Free)

confint(model6Free)

# We reduce data to 80%
n <- nrow(dragona)
train_ids <- sample(1:n, size = 0.8 * n)
train_data <- dragona[train_ids, ]
test_data <- na.omit(dragona[-train_ids, ])

# Train the data with the 80%
model_train <- lm(Ladder.score ~ Minimum.wage + Unemployment.rate + Longitude + Social.support +
                    Freedom.to.make.life.choices, data = train_data, na.action = na.exclude)

# Prediction and confidence intervals
predict(model_train, newdata = test_data, interval = "prediction")
predict(model6Free, newdata = test_data, interval = "confidence")

# With this plot we can see that residuals aren't correlated 
residuals_clean <- na.omit(model_train$residuals)
plot(residuals_clean, type = "o", main="Residuals vs. Observation Order",
     xlab="Observations", ylab="Residuals", col="green")
abline(h=0, col="black", lwd=2, lty=2)

# We can see that our model is completly linear
plot(model6Free, 1)

# Although we can see a decrease in the variance, it isn't significative enough to say that it is heteroscedasticity
ggplot(data = data.frame(Fitted = model6Free$fitted.values, Residuals = model6Free$residuals), 
       aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_smooth(color = "red") +
  geom_quantile(quantiles = c(0.05, 0.95), color = "blue", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

# The high W and p-value of the shapiro test show us that the model follows a normal distribution
shapiro.test(model6Free$residuals)

stud_resids <- studres(model6Free)

plot(model6Free$fitted.values,stud_resids,
     xlab="Fitted", ylab ="Studentized Residuals")
abline (0 , 0)
outliers <- ifelse(abs(stud_resids) > 3, TRUE, FALSE)
#Now that we have identified the outliers, we are going to analyse if the model improves without them
model_clean <- update(model6Free, subset = -outliers)

# As the outliers don't look like if they were a human error, and as the model prediction 
#capacity doesn't improve with or without outliers, we are not going to eliminate them
summary(model_clean)
summary(model6Free)



##### LOGISTIC REGRESSION

fiona <- dragona

# Define democratic countries (DEFINED USING CHATGPT)
democratic <- c("Albania", "Argentina", "Armenia", "Australia", "Austria", "Belgium", "Benin", 
                "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "Canada", "Chile", 
                "Colombia", "Costa Rica", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                "Dominican Republic", "Ecuador", "El Salvador", "Estonia", "Finland", "France", 
                "Georgia", "Germany", "Ghana", "Greece", "Guatemala", "Iceland", "India", 
                "Indonesia", "Israel", "Italy", "Jamaica", "Japan", "Kenya", "Latvia", "Lesotho", 
                "Liberia", "Lithuania", "Luxembourg", "Malawi", "Malaysia", "Malta", "Mauritius", 
                "Mexico", "Moldova", "Mongolia", "Montenegro", "Namibia", "Nepal", "Netherlands", 
                "New Zealand", "Nicaragua", "North Macedonia", "Norway", "Panama", "Paraguay", 
                "Peru", "Philippines", "Poland", "Portugal", "Romania", "Senegal", "Serbia", 
                "Slovakia", "Slovenia", "South Africa", "South Korea", "Spain", "Sri Lanka", 
                "Sweden", "Switzerland", "Ukraine", "United Kingdom", "United States", "Uruguay", "Zambia")

# Add binary democracy variable
fiona$Democracy_Status <- ifelse(fiona$Country %in% democratic, 1, 0)
fiona$Democracy_Status <- as.factor(fiona$Democracy_Status)  # Convert to factor

# Select initial predictors
predictors <- c("Logged.GDP.per.capita", "Gross.primary.education.enrollment....", 
                "Explained.by..Freedom.to.make.life.choices", "Perceptions.of.corruption", 
                "Armed.Forces.size", "CPI", "GDP", "Gross.primary.education.enrollment....", 
                "Gross.tertiary.education.enrollment....", "Total.tax.rate", "Urban_population" )

# Remove NA values
fiona <- na.omit(fiona[, c("Democracy_Status", predictors)])


# Correlation analysis
cor_matrix <- cor(fiona[, predictors])
print(cor_matrix)

# Split data
n_log <- nrow(fiona)
train_ids_log <- sample(1:n, size = 0.8 * n)
train_data_log <- fiona[train_ids_log, ]
test_data_log <- na.omit(fiona[-train_ids_log, ])

# Fit logistic regression model
fiona_Model1 <- glm(Democracy_Status ~ Logged.GDP.per.capita + Explained.by..Freedom.to.make.life.choices + 
                      Perceptions.of.corruption + Armed.Forces.size + CPI + GDP + Gross.primary.education.enrollment.... + 
                      Gross.tertiary.education.enrollment.... + Total.tax.rate + Urban_population, 
                    data = train_data_log, family = binomial)
summary(fiona_Model1)

# We remove the least related predictors
fiona_Model2 <- glm(Democracy_Status ~ Explained.by..Freedom.to.make.life.choices + Perceptions.of.corruption +  
                      Gross.primary.education.enrollment.... + Gross.tertiary.education.enrollment...., 
                    data = train_data_log, family = binomial)
summary(fiona_Model2)

fiona_Model3 <- glm(Democracy_Status ~ Explained.by..Freedom.to.make.life.choices  +  
                      Gross.primary.education.enrollment.... + Gross.tertiary.education.enrollment...., 
                    data = train_data_log, family = binomial)
summary(fiona_Model3)
##fiona model 2 better than model 3

# Predictions
pred_prob <- predict(fiona_Model2, newdata = test_data_log, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
pred_class <- as.factor(pred_class)

# Confusion matrix
test_data_log$Democracy_Status <- as.factor(test_data_log$Democracy_Status)
conf_matrix <- confusionMatrix(pred_class, test_data_log$Democracy_Status) 
print(conf_matrix)

# ROC curve
roc_curve <- roc(test_data_log$Democracy_Status, pred_prob)
plot(roc_curve, col="blue", main="ROC Curve")
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")


########PRINCIPAL COMPONENT ANALYSIS###################
## Data cleaning
pca_data <- dragona[, c("Country",
                        "Regional.indicator",
                        "Logged.GDP.per.capita", 
                        "Life.expectancy", 
                        "Freedom.to.make.life.choices", 
                        "Generosity", 
                        "Perceptions.of.corruption", 
                        "Ladder.score", 
                        "Population")]
names(pca_data)[names(pca_data) == "Regional.indicator"] <- "Region"
names(pca_data)[names(pca_data) == "Logged.GDP.per.capita"] <- "GDP"
names(pca_data)[names(pca_data) == "Life.expectancy"] <- "LifeExpectancy"
names(pca_data)[names(pca_data) == "Freedom.to.make.life.choices"] <- "Freedom"
names(pca_data)[names(pca_data) == "Perceptions.of.corruption"] <- "Corruption"
names(pca_data)[names(pca_data) == "Ladder.score"] <- "Happiness"

pca_data <- na.omit(pca_data)


## Data standardization
countries <- pca_data$Country
pca_num2 <- pca_data[, -1]
pca_num <- scale(pca_num2[, -1])
rownames(pca_num) <- countries




## EIGENVALUES
pca_values <- PCA(pca_num2[, -1])
pca_values$eig

pca_rho <- cor(pca_num2[, -1])
pca_eig <- eigen(pca_rho)
pca_coord <- t(solve(pca_eig$vectors) %*% t(pca_num))

t(apply(pca_num, 2, function(col) cor(pca_coord, col)))

pca_correlations <- NULL
pca_correlations <- t(apply(pca_num, 2, function(col) cor(pca_coord[, 1:2], col)))
rownames(pca_correlations) <- colnames(pca_num)



# We can now represent the correlations in a 2D circle.
plot(pca_correlations,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),pch=16,cex=0.3,
     xlab="Component 1 (46,12%)",ylab="Component 2 (18,71%)",asp=1)
grid()
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, 
       x1 = pca_correlations[, 1], 
       y1 = pca_correlations[, 2], 
       length = 0.1,
       lwd = 2)
abline(h = 0, v = 0, lty = 2)

# LABEL PLACEMENT
x_offset <- ifelse(pca_correlations[, 1] >= 0, 0.15, -0.5)
y_offset <- ifelse(pca_correlations[, 2] >= 0, 0.15, -0.15)

y_offset["GDP"] <- y_offset["GDP"] + 0.1               # move GDP up
y_offset["LifeExpectancy"] <- y_offset["LifeExpectancy"] - 0.125  # move LifeExpectancy down
y_offset["Happiness"] <- y_offset["Happiness"] - 0.13  # move happiness down
x_offset["LifeExpectancy"] <- x_offset["LifeExpectancy"] - 0.125  # move LifeExpectancy left
x_offset["GDP"] <- x_offset["GDP"] + 0.1                # move GDP right
x_offset["Population"] <- x_offset["Population"] + 0.22  # move population right
x_offset["Corruption"] <- x_offset["Corruption"] - 0.2  # move corruption left

text(pca_correlations[, 1] + x_offset, 
     pca_correlations[, 2] + y_offset, 
     labels = row.names(pca_correlations), 
     cex = 1.0,
     font = 1
)
title(main = "PCA Analisys: Variables")



##PCA with selected 20 countries
selected_countries <- c("Finland", "New Zealand", "France", "Portugal", 
                        "Belarus", "Yemen", "Spain", "Mexico", "Nigeria", "Nepal", 
                        "Pakistan", "Zimbabwe", "Tunisia", "United States", "Cuba")

pca_20 <- pca_data[pca_data$Country %in% selected_countries, ]
pca_num_3 <- pca_20[, -1]
pca_num_4 <- scale(pca_num_3[, -1])
pca_coord_1 <- t(solve(pca_eig$vectors) %*% t(pca_num_4))
rownames(pca_coord_1) <- pca_20$Country


plot(pca_correlations, xlim = c(-3.5, 3.5), ylim = c(-2, 2), 
     pch = 16, cex = 0.3, 
     xlab = "Component 1 (46,12%)", ylab = "Component 2 (18,71%)", asp = 1)
grid()
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, 
       x1 = pca_correlations[, 1], 
       y1 = pca_correlations[, 2], 
       length = 0.1,
       lwd = 2)
abline(h = 0, v = 0, lty = 2)

# LABEL PLACEMENT
x_offset <- ifelse(pca_correlations[, 1] >= 0, 0.15, -0.5)
y_offset <- ifelse(pca_correlations[, 2] >= 0, 0.15, -0.15)

y_offset["GDP"] <- y_offset["GDP"] + 0.1               # move GDP up
y_offset["LifeExpectancy"] <- y_offset["LifeExpectancy"] - 0.125  # move LifeExpectancy down
y_offset["Happiness"] <- y_offset["Happiness"] - 0.13  # move happiness down
x_offset["LifeExpectancy"] <- x_offset["LifeExpectancy"] - 0.125  # move LifeExpectancy left
x_offset["GDP"] <- x_offset["GDP"] + 0.1                # move GDP right
x_offset["Population"] <- x_offset["Population"] + 0.22  # move population right
x_offset["Corruption"] <- x_offset["Corruption"] - 0.2  # move corruption left

text(pca_correlations[, 1] + x_offset, 
     pca_correlations[, 2] + y_offset, 
     labels = row.names(pca_correlations), 
     cex = 1.6,
     font = 2
     )

# Overlay the scatter plot of individual data points
points(pca_coord_1[, 1], pca_coord_1[, 2], pch = 16, col = "blue", cex= 1.5)
text(pca_coord_1[, 1] - 0.2, pca_coord_1[, 2] + 0.1, labels = rownames(pca_coord_1), cex = 1.6)
title(main = "PCA Analysis: Selected 20 countries")





##PCA WITH CATEGORICAL VARIABLE
pca_data$Region[pca_data$Region == "Commonwealth of Independent States"] <- "Former Soviet countries"

region_colors <- c("Western Europe" = "blue", "North America and ANZ" = "red", 
                   "Middle East and North Africa" = "black", "Latin America and Caribbean" = "orange", 
                   "Central and Eastern Europe" = "green", "East Asia" = "hotpink",
                   "Southeast Asia" = "purple", "Former Soviet countries" = "grey",
                   "Sub-Saharan Africa" = "brown", "South Asia"  = "yellow") 
point_colors <- region_colors[pca_data$Region]

region_icons <- c("Western Europe" = 15, "North America and ANZ" = 23, 
                   "Middle East and North Africa" = 16, "Latin America and Caribbean" = 23, 
                   "Central and Eastern Europe" = 15, "East Asia" = 17,
                   "Southeast Asia" = 17, "Former Soviet countries" = 8,
                   "Sub-Saharan Africa" = 16, "South Asia"  = 17) 
point_icons <- region_icons[pca_data$Region]


# DUALITY #
# Create the scatter plot with vectors
plot(pca_correlations, xlim = c(-3.5, 3.5), ylim = c(-2, 2), pch = 16, cex = 0.3,
     xlab = "Component 1 (46,12%)",ylab="Component 2 (18,71%)", asp = 1)
draw.circle(0, 0, radius = 1)
arrows(x0 = 0, y0 = 0, x1 = pca_correlations[, 1], y1 = pca_correlations[, 2], length = 0.1, lwd = 2.5)
abline(h = 0, v = 0, lty = 2)
# LABEL PLACEMENT
x_offset <- ifelse(pca_correlations[, 1] >= 0, 0.15, -0.5)
y_offset <- ifelse(pca_correlations[, 2] >= 0, 0.15, -0.15)

y_offset["GDP"] <- y_offset["GDP"] + 0.1               # move GDP up
y_offset["LifeExpectancy"] <- y_offset["LifeExpectancy"] - 0.125  # move LifeExpectancy down
y_offset["Happiness"] <- y_offset["Happiness"] - 0.13  # move happiness down
x_offset["LifeExpectancy"] <- x_offset["LifeExpectancy"] - 0.125  # move LifeExpectancy left
x_offset["GDP"] <- x_offset["GDP"] + 0.1                # move GDP right
x_offset["Population"] <- x_offset["Population"] + 0.22  # move population right
x_offset["Corruption"] <- x_offset["Corruption"] - 0.2  # move corruption left

text(pca_correlations[, 1] + x_offset, 
     pca_correlations[, 2] + y_offset, 
     labels = row.names(pca_correlations), 
     cex = 1.6,
     font = 2
)

# Overlay the scatter plot of individual data points
points(pca_coord[, 1], pca_coord[, 2], pch = point_icons, col = point_colors, bg = point_colors, cex = 1.4)
text(pca_coord[, 1], pca_coord[, 2] + 0.08, labels = rownames(pca_num), cex = 0.6)
title(main = "PCA Analysis: 140 countries")

# 1. Draw legend with fill only, no labels
legend(x = -4.1, y = 2.27, 
       legend = rep("", length(region_colors)),  # No text
       fill = region_colors, 
       title = NULL, 
       cex = 0.6, 
       y.intersp = 0.7,
       bty = "n")

# 2. Manually add text with more control
legend_coords <- cbind(
  x = rep(-3.65, length(region_colors)),          # Adjust X position as needed
  y = seq(2.035, 2.4 - 0.6 * (length(region_colors) - 1), by = -0.1926)  # Y positions match y.intersp
)

region_names <- names(region_colors)

# Loop to place each label
for (i in seq_along(region_names)) {
  text(x = legend_coords[i, 1], 
       y = legend_coords[i, 2], 
       labels = region_names[i], 
       cex = 1.0,              # Bigger text
       font = 2,               # Bold
       adj = 0)                # Left-aligned
}






########CORRESPONDENCE ANALYSIS#######################################################
dragona2 <- dragona
head(dragona2)
hist(dragona2$Life.expectancy, breaks = 100, main = "Distribución Life expectancy", xlab = "Life expectancy", col = "lightblue")

quantile(dragona2$Life.expectancy, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
# Low = Life.expectancy < 70
# 
# Medium = Life.expectancy 70–79
# 
# High = Life.expectancy > 79

# Looking at the histogram we have decided to cut like this, because we see clearly different groups.
dragona2$LifeExp.Category <- cut(dragona2$Life.expectancy,
                                breaks = c(-Inf, 70, 79, Inf),
                                labels = c("Low", "Medium", "High"),
                                right = FALSE)

table(dragona2$LifeExp.Category)
# Here we got the results of how many countries belongs to each category.

contingency_table <- table(dragona2$Regional.indicator, dragona2$LifeExp.Category)

print(contingency_table)

# We can suspect that are regions that has lots of countries in one category 
# which could mean that the contigency table is dependent.

# Test chi-square to obtain the independence table
chi_result <- chisq.test(contingency_table)

expected_table <- chi_result$expected

print(expected_table)

# This table show to us what would happend if the variables were not related. 
# The comparation:

observed <- contingency_table
expected <- chi_result$expected
round(observed - expected, 1)

#Seeing the comparison between the contingency and independence tables
#we can conclude that is a clear dependence. 
#Specifically in the Sub-Saharan African region are much more 
#countries with low life expectancy  (+22.0) than they would be if they were independent. 
#On the other hand, the opposite happens with the Western Europe Region, 
#were is much more high life expectancy (+14.9) than it should.

chi_result

#The low p-value show us that it is a extremly 
#high correlation between regional indicator and the life expectancy

caLR <- ca(contingency_table)


plot(caLR, invisible =" col ")



#We plot row and column profiles on the plane defined by the eigenvectors, 
#so we can see how the categories are reñated between
summary(caLR)

