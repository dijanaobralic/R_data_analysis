library(sqldf)
library(psych)
library(data.table)
library(car)
library(ggplot2)
library(plyr)
library(pastecs)
library(pscl)

# Read the whole file into R

flights <- read.csv("On_Time_On_Time_Performance_2016_8.csv")

# flights.dt <- fread("filename) 
# nrow(flights)
# takes less time to read


# Get rid of all other origin airports but SFO
## Note: SQL cannot take variables that contain ".", use "_" instead

SFO_flights <- sqldf("SELECT * FROM flights WHERE Origin = 'SFO'")

# Check all distinct delay codes

sqldf("SELECT DISTINCT DepartureDelayGroups FROM SFO_flights ORDER BY DepartureDelayGroups ASC ")

## Note: -2 and -1 are related to early flights tehrefore they don't represent actual delay codes

# Remove on-time flights 
## Flights that have DepDel15 0 are on-time flights

DelayedFlights <- sqldf("SELECT CarrierDelay AS Carrier, WeatherDelay AS Weather,
NASDelay AS NAS, SecurityDelay AS Security, LateAirCraftDelay AS LateAircraft,
DepDel15, DepDelayMinutes, DepartureDelayGroups
                            FROM SFO_flights WHERE DepDel15 = 1")



delay.carrier <- sqldf("SELECT Carrier FROM DelayedFlights
                     WHERE Carrier !='0'")

delay.weather <- sqldf("SELECT Weather FROM DelayedFlights
                     WHERE Weather != 0")

delay.NAS <- sqldf("SELECT NAS FROM DelayedFlights
                     WHERE NAS != 0")

delay.security <- sqldf("SELECT Security FROM DelayedFlights
                     WHERE Security != 0")

delay.lateAircraft <- sqldf("SELECT LateAircraft FROM DelayedFlights
                     WHERE LateAircraft != 0")


carrier <- stat.desc(delay.carrier)
weather <- stat.desc(delay.weather)
NAS <- stat.desc(delay.NAS)
security <- stat.desc(delay.security)
lateAircraft <- stat.desc(delay.lateAircraft)

delay.reasons <- cbind(carrier, weather, NAS, security, lateAircraft)

#__________________________________________Q2___________________________________
# Is there a significant difference between carriers
## Check what are all distinct airlines in this data set

sqldf("SELECT DISTINCT Carrier FROM SFO_flights") 

### Note: there are 10 different airlines in the dataset


# Flight is DELAYED if actual departure time > 15 min than scheduled departure time

df1 <- sqldf("SELECT Carrier, COUNT(DepDel15) AS DelayedFlights 
                               FROM SFO_flights WHERE DepDel15 = 1 GROUP BY Carrier")

df2 <- sqldf("SELECT Carrier, COUNT(DepDel15) AS TotalFlights
                             FROM SFO_flights GROUP BY Carrier")


total_and_delayed <- merge(df1,df2)



# Calculate percentage of delayed flights by each airline in the dataset
total_and_delayed$PercentDelayed <- (total_and_delayed$DelayedFlights/total_and_delayed$TotalFlights)*100

format(total_and_delayed$PercentDelayed, (digits = 3))

## F9 (Frontier) has the higest percetage of delayed flights

# Calculate average delayed time per carrier

df3 <- sqldf("SELECT Carrier, AVG(DepDelayMinutes) AS AverageDelay
             FROM SFO_flights WHERE DepDel15 = 1 GROUP BY Carrier")

 SFO_flights.info<- merge(total_and_delayed, df3)


Delayed_Flights_Info <- merge(total_and_delayed, df3)


# dataForANOVA <- sqldf("SELECT DepDelayMinutes, Carrier
#                            FROM SFO_flights WHERE DepDel15 = ")

dataForANOVA <- sqldf("SELECT Carrier, DepDelayMinutes
                      FROM SFO_flights WHERE DepDel15 = 1")

# factor Carrier column (10 levels)

dataForANOVA$Carrier <- as.factor(dataForANOVA$Carrier)

# prepare data for anova

SFO_flights.fit <- lm(DepDelayMinutes ~ Carrier, dataForANOVA)

# run anova analysis 

SFO_flights.anova <- aov(SFO_flights.fit)

SFO_flights.anova.summary <- summary(SFO_flights.anova)

# where is the difference?

SFO_flights.tukey <- TukeyHSD(SFO_flights.anova, conf.level = 0.95)

print(SFO_flights.tukey)


# Homogenity of variences 

SFO_flights.levene <- leveneTest(SFO_flights.fit)

StressTreatment.levene<-leveneTest(SFO_flights.fit)


# Plots


SFO_flights.plot <- ggplot(dataForANOVA)+
  stat_qq(aes(sample = DepDelayMinutes, colour = factor(Carrier))) +
  guides(col=guide_legend(title = "Carrier" ))
# Predicted variable: DepDel15 (logistic regression)


#____________________________________________Q3__________________________-
# Predictor variables: DayOfWeek, Airline 

sqldf("SELECT DISTINCT DepDel15 FROM SFO_flights")

dataForLR <- sqldf("SELECT Carrier, DayOfWeek, DistanceGroup, DepDel15 FROM SFO_flights WHERE DepDel15 != 'NA'")

#hist(dataForLR$Delayed) # 0 or 1
#hist(dataForLR$Day) # 1 - 7
#hist(dataForLR$Distance) # 0- 12

# create indicators for Airline

dataForLR$CarrierAA <- ifelse(dataForLR$Carrier == 'AA', 1, 0)

dataForLR$CarrierAS <- ifelse(dataForLR$Carrier == 'AS', 1, 0)

dataForLR$CarrierB6 <- ifelse(dataForLR$Carrier == 'B6', 1, 0)

dataForLR$CarrierDL <- ifelse(dataForLR$Carrier == 'DL', 1, 0)

dataForLR$CarrierUA <- ifelse(dataForLR$Carrier == 'UA', 1, 0)

dataForLR$CarrierOO <- ifelse(dataForLR$Carrier == 'OO', 1, 0)

dataForLR$CarrierVX <- ifelse(dataForLR$Carrier == 'VX', 1, 0)

dataForLR$CarrierWN <- ifelse(dataForLR$Carrier == 'WN', 1, 0)

dataForLR$CarrierF9 <- ifelse(dataForLR$Carrier == 'F9', 1, 0)

# assamble a new data frame with explanatory variables
dataForLR <- data.frame(cbind(Delayed =dataForLR$DepDel15, 
                                  Day = dataForLR$DayOfWeek,
                                  Distance = dataForLR$DistanceGroup,
                                  AA = dataForLR$CarrierAA,
                                  AS = dataForLR$CarrierAS,
                                  B6 = dataForLR$CarrierB6,
                                  DL = dataForLR$CarrierDL,
                                  UA = dataForLR$CarrierUA,
                                  OO = dataForLR$CarrierOO, 
                                  VX = dataForLR$CarrierVX,
                                  WN = dataForLR$CarrierWN,
                                  F9 = dataForLR$CarrierF9))

# Model fitting

train <- dataForLR[1:7500, ]
test <- dataForLR[7500:15973,]

m1 <- glm(formula = Delayed~., family= binomial, data = train)
m1.summary <- summary(m1)

# use McFadden R squared to assses the model fit
## measuers likeliehood and falls between 0 and 1
m1.McFadden.R2 <- pR2(m1)

m2.dataForLR <- data.frame(cbind(Delayed =dataForLR$DepDel15, 
                                 Distance = dataForLR$DistanceGroup,
                                 AA = dataForLR$CarrierAA,
                                 AS = dataForLR$CarrierAS,
                                 B6 = dataForLR$CarrierB6,
                                 DL = dataForLR$CarrierDL,
                                 UA = dataForLR$CarrierUA,
                                 OO = dataForLR$CarrierOO, 
                                 VX = dataForLR$CarrierVX,
                                 WN = dataForLR$CarrierWN,
                                 F9 = dataForLR$CarrierF9))

m2 <- glm(formula = Delayed~., family = binomial, data = train)
m2.summary <- summary(m2)

m2.McFadden.R2 <- pR2(m2)


destination.info <-sqldf("SELECT Dest, DestCityName,  Distance, DistanceGroup FROM SFO_flights GROUP BY Dest ORDER BY Distance ASC")