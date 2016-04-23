## Basic data anlaysis

# Read in the USDA data
USDA = read.csv("USDA.csv")

# Get a sense of the USDA nutritional data
str(USDA)

# Get some summary stats
summary(USDA)

# Take a look at the sodium vector
USDA$Sodium

# Find the row with the highest sodium level
which.max(USDA$Sodium)

# Identify the name of the item with the highest sodium
USDA$Description[which.max(USDA$Sodium)]

# Find the high sodium foods
HighSodium = subset(USDA, Sodium > 10000)

# How many observations do we have?
nrow(HighSodium)

# What items are they?
HighSodium$Description

# Where does caviar fall on this list of sodium
match("CAVIAR", USDA$Description)

# What is the sodium level of caviar?
USDA$Sodium[match("CAVIAR", USDA$Description)]

# Summary stats for sodium
summary(USDA$Sodium)

# Standard deviation of sodium
sd(USDA$Sodium, na.rm = TRUE)

## Now lets do some plotting

# Scatter plot
plot(
  USDA$Protein, USDA$TotalFat,
    xlab = "Protein",
    ylab = "Fat",
    main = "Protein vs Fat",
    col  = "red"
)

# Histogram
hist(
  USDA$VitaminC,
    xlab = "Vitamin C (mg)",
    main = "Histogram of Vitamin C Levels",
    xlim = c(0, 100),
    breaks = 2000
)

# Box plots
boxplot(
  USDA$Sugar,
    main = "Boxplot of Sugar Levels",
    ylab = "Sugar (g)"
)

## Adding variables

# Check if first entry is greater than the mean of sodium
USDA$Sodium[1] > mean(USDA$Sodium, na.rm = TRUE)

# How about the 50th food?
USDA$Sodium[50] > mean(USDA$Sodium, na.rm = TRUE)

# How about all foods?
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
str(HighSodium)

# Let's make this a new column
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))

# Let's do the same for protein, carbs and fat
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))

## Let's make some summary tables

table(USDA$HighSodium)

# Let's make a table of high sodium and high fat
table(USDA$HighSodium, USDA$HighFat)

# What if we want to compute some averages
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)

# How about some max values
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm = TRUE)

# How about some summary values
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm = TRUE)
