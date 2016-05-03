setwd('C:/Users/yadavps/Desktop/Data Science/Chapter 7/Linear Regression/Mini-Project Linear Regression/Data Files/dataSets')

states.data <- readRDS('C:/Users/yadavps/Desktop/Data Science/Chapter 7/Linear Regression/Mini-Project Linear Regression/Data Files/dataSets/states.rds')

View(states.data)

#Model 1
states.energy.metro <- subset((states.data), select = c("metro","energy"))
View(states.energy.metro)
states.energy.metro <- na.omit(states.energy.metro)
summary(states.energy.metro)
cor(states.energy.metro)
states.energy.metro
plot(states.energy.metro)

energyMetro1 <- lm(energy ~ metro,data = states.energy.metro)
summary(energyMetro1)

#Model2

statesMoreData <- subset(states.data, select = c("metro","energy","pop","area"))
statesMoreData <- na.omit(statesMoreData)
summary(statesMoreData)
plot(statesMoreData)
cor(statesMoreData,method = "pearson")

model2 <- lm(energy ~ metro + pop + area, data = statesMoreData)
plot(model2)
summary(model2)

# model 3 with region
statesRegion <- subset(states.data, select = c("metro","energy","pop","area","region"))
statesRegion <- na.omit(statesRegion)

model3 <- lm(energy ~ metro + pop + area + region, data = statesRegion)
plot(model3)
summary(model3)