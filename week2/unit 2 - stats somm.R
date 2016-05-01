#############################################
# Working through the course lecture examples
#############################################

wine = read.csv("wine.csv")

# Identify the structure of the wine data set
str(wine)

# Get some summary stats
summary(wine)

# Let's make a univariate prediction
model1 = lm(Price ~ AGST, data = wine)

summary(model1)

# Let's compute the Sum of Squared Errors (SSE)
model1$residuals

SSE = sum(model1$residuals^2)
SSE

# Now let's make a multivariate prediction
model2 = lm(Price ~ AGST + HarvestRain, data = wine)

summary(model2)

SSE2 = sum(model2$residuals^2)
SSE2

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)

summary(model3)

SSE3 = sum(model3$residuals^2)
SSE3

# Quick Question
summary( lm( Price ~ HarvestRain + WinterRain, data = wine) )


# Remove france population for our new model
model4 = lm( Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)

summary(model4)

# Quick question
summary( lm( Price ~ HarvestRain + WinterRain, data = wine) )

# Compute the correlation between WinterRain and Price
cor(wine$WinterRain, wine$Price)

# Now for Age and FrancePop
cor(wine$Age, wine$FrancePop)

# How about all variables?
cor(wine)
lattice::splom(wine)

model5 = lm( Price ~ AGST + HarvestRain + WinterRain, data = wine )
summary(model5)

# Quick question
cor(wine$HarvestRain, wine$WinterRain)

# Lets do some testing!
WT = read.csv("wine_test.csv")
str(WT)

predict_test = predict(model4, newdata = WT)
predict_test

# Let's meausre how well the model performs on the test data with R squared
SSEp = sum( (WT$Price - predict_test)^2 )
SSTp = sum( (WT$Price - mean(wine$Price) )^2 )
Rsq = 1 - SSEp / SSTp
Rsq

80.8814 + 0.1058(RD)