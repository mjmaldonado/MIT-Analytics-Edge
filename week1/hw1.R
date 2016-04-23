
#############################################################################################
#
# Author: Michael Maldonado
# Create Date: 2016-04
# Description: This the the first homework assignment for the MIT Analytics Course on edX.org
#
#############################################################################################


########################################
# Homework Problem Set 1
#######################################

# Week 1 hw
# Using Chicago auto theft data
crime_data = read.csv("mvtweek1.csv")

## Section 1

# 1.1.1 How many obersvations are in the dataset?
nrow(crime_data)

# 1.1.2 How many variables are in the dataset?
ncol(crime_data)

# 1.1.3 What is the maximum value of the variable "ID"?
max( (crime_data$ID) ) 

# 1.1.4 What is the minimum value of "Beat"?
min( (crime_data$Beat) )

# 1.1.5 How many obervations have value TRUE in the Arrest variable?
nrow( subset(crime_data, (Arrest) == TRUE) )

# 1.1.6 How many observations have a LocationDescription value of Alley?
nrow( subset(crime_data, (LocationDescription) == "ALLEY") )

## Section 2

# 1.2.1 What format are the entries of the Data variable?
crime_data$Date[1]

# 1.2.2 Date Conversion
# What is the month and year of the median data in our dataset?
DateConvert = as.Date(strptime( (crime_data$Date), "%m/%d/%y %H:%M") )

median(DateConvert)

# 1.2.3 Which month did the fewest motor vehicle thefts occur?
(crime_data$Month) = months(DateConvert)

(crime_data$Weekday) = weekdays(DateConvert)

(crime_data$Date) = DateConvert

table( (crime_data$Month) ) 

# 1.2.4 Which weekday did the most motor vehicle thefts occur?
table( (crime_data$Weekday) )

# 1.2.5 Which month has the largest number of motor vehicle thefts for which an arrest was made?
table( (crime_data$Month), (crime_data$Arrest) )

## Section 3 - Visualization

# 1.3.1 Make a histogram and deteremine, in general, whether it looks like crime increase or decreases from 2002 - 2012
hist( (crime_data$Date), breaks = 100)

# 1.3.2 Using a boxplot, does it look like there where
# more crimes for which arrest were made in the first
# half of the time period or the second half of  the
# time period?

boxplot( (crime_data$Date) ~ (crime_data$Arrest) )

# 1.3.3 For what proportion of motor vehicle thefts in 2001 was an arrest made?

# Grab the second row and second column for the TRUE results / Total obersvations in 2001
table( (crime_data$Arrest), (crime_data$Year) == 2001)[,2][2] / ( table( (crime_data$Arrest), (crime_data$Year) == 2001)[,2][1] + table( (crime_data$Arrest), (crime_data$Year) == 2001)[,2][2] )

# 1.3.4 For what proportion of motor vehicle thefts in 2007 was an arrest made?

# Repeat 1.3.3, but in year 2007
table( (crime_data$Arrest), (crime_data$Year) == 2007)[,2][2] / ( table( (crime_data$Arrest), (crime_data$Year) == 2007)[,2][1] + table( (crime_data$Arrest), (crime_data$Year) == 2007)[,2][2] )

# 1.3.5 For what proportion of motor vehicle thefts in 2012 was an arrest made?

# Repeat 1.3.3, but in year 2012
table( (crime_data$Arrest), (crime_data$Year) == 2012)[,2][2] / ( table( (crime_data$Arrest), (crime_data$Year) == 2012)[,2][1] + table( (crime_data$Arrest), (crime_data$Year) == 2012)[,2][2] )


## Section 4 - Popular Locations

# 1.4.1 Which locations are the top five locations for motor vehicle
# thefts, excluding the "Other" category?
sort( table( (crime_data$LocationDescription) ) )

# 1.4.2 Create a subset of the top 5 locations, by observations, how many observations are there?
TopLocations = c( "STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")

top5 =  subset( crime_data, (LocationDescription) %in% TopLocations )

nrow(top5)

# 1.4.3 Which location has a much higher arrest rate than the other locations?
(top5$LocationDescription) = factor( (top5$LocationDescription) )

table( (top5$Arrest), (top5$LocationDescription) )[2,] / ( table( (top5$Arrest), (top5$LocationDescription) )[1,] + table( (top5$Arrest), (top5$LocationDescription) )[2,] )

# 1.4.4 On which day of the week do the most motor vehicle thefts at gas stations happen?
table( (top5$Weekday), (top5$LocationDescription) ==  "DRIVEWAY - RESIDENTIAL")

# Remove data prior to the next exercise
rm(crime_data, top5)

########################################
# Homework Problem Set 2
########################################

# Using stock market data for GE, IBM, Procter and Gamble, Coca Cola, and Boeing

# Read in the data
IBM = read.csv('IBMStock.csv')
GE  = read.csv('GEStock.csv')
PG  = read.csv('ProcterGambleStock.csv')
CC  = read.csv('CocaColaStock.csv')
BO  = read.csv('BoeingStock.csv')

# Convert dates
(IBM$Date) = as.Date( (IBM$Date), "%m/%d/%y")
(GE$Date)  = as.Date( (GE$Date), "%m/%d/%y")
(PG$Date)  = as.Date( (PG$Date), "%m/%d/%y")
(CC$Date)  = as.Date( (CC$Date), "%m/%d/%y")
(BO$Date)  = as.Date( (BO$Date), "%m/%d/%y")

# 2.1.1 Find the number of observations in one of the datasets using str or summary
str(IBM)

# 2.1.2 What is the earliest year in our datasets
# 2.1.3 What is the latest year in our datsets
# 2.1.4 What is the mean stock price of IBM over this time period
summary(IBM)

# 2.1.5 What is the minimum stock price of GE over this time period?
summary(GE)

# 2.1.6 What is the max stock price of Coca-Cola over this time period?
summary(CC)

# 2.1.7 What is the median stock price of Boeing over this time period?
summary(BO)

# 2.1.8 What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd( (PG$StockPrice) )

## Section 2.2 - Visualization

# 2.2.1 - Track Coca-Colas stock price over time
plot(
  (CC$Date), (CC$StockPrice),
    type = "l"
)

# 2.2.2 - Add Procter and Gamble to the mix and determine how the tech bubble burst
# of 2000 had a greater impact
lines(
  (PG$Date), (PG$StockPrice),
    col = "red",
    lty = 2
)

abline( v = as.Date( c("2000-03-01") ), lwd = 2 )

# 2.2.3 Around 1983, one company's stock was increasing while the other was increasing
# Which is which?
# Which generally had lower values?
abline( v = as.Date( c("1983-01-01") ), lwd = 2, col = "blue" )

## Section 2.3 - Visualizing Stock Dynamics 1995 - 2005

# 2.3.1 Which stock fell the most right after the technology bubble burst in March 2000?
# 2.3.2 Which stock reaches the highest value in the time period 1995-2005?
# 2.4.3 Which companies saw a decreasing trend in their stock price to due
# a global stock market crash in October of 1997

# Start with Coca-Cola
plot(
  (CC$Date)[301:432], (CC$StockPrice)[301:432],
    type = "l",
    col  = "red",
    ylim = c(0,210)
)

# Add GE, BO, PG, and IBM
lines( (PG$Date)[301:432], (PG$StockPrice)[301:432], col = "sea green", lty = 3 )
lines( (GE$Date)[301:432], (GE$StockPrice)[301:432], col = "blue", lty = 4 )
lines( (BO$Date)[301:432], (BO$StockPrice)[301:432], col = "purple", lty = 5 )
lines( (IBM$Date)[301:432], (IBM$StockPrice)[301:432], col = "black", lty = 2 )

# 2.4.3 code
abline( v = as.Date( c("1997-09-01") ), lwd = 2 )
abline( v = as.Date( c("1997-11-01") ), lwd = 2 )

## Section 2.4 - Monthly trends

# 2.4.1 For IBM, in which months has IBM historically had a higher stock price (on average)?
tapply( (IBM$StockPrice), months((IBM$Date)), mean, na.rm = TRUE ) < mean((IBM$StockPrice), na.rm = TRUE)

# 2.4.2 Repeat the tapply from 2.4.1 for all other companies
tapply( (GE$StockPrice), months((GE$Date)), mean, na.rm = TRUE )
tapply( (CC$StockPrice), months((CC$Date)), mean, na.rm = TRUE )
tapply( (BO$StockPrice), months((BO$Date)), mean, na.rm = TRUE )
tapply( (PG$StockPrice), months((PG$Date)), mean, na.rm = TRUE )

# Remove the datasets prior to the next exercise
rm(GE, CC, BO, PG, IBM)

########################################
# Homework Problem Set 3
########################################

# Working with the Current Population Survey (CPS) data
CPS = read.csv('CPSData.csv')

## Section 3.1

# 3.1.1 How many interviewees are in the dataset?
str(CPS)

# 3.1.2 What is the most common industry of employment for those  who have been interviewed?
sort( summary((CPS$Industry)) )

# 3.1.3 Which state has the fewest and largest number of interviewees?
sort( summary((CPS$State)) )

# 3.1.4 What proportion of interviewees are citizens of the United States?
sum( table((CPS$Citizenship))[c(1,2)] )  / sum( table((CPS$Citizenship)) )

# 3.1.5 For which races are there at least 250 interviewees in the CPS dataset of Hispanice ethnicity?
table((CPS$Race), (CPS$Hispanic)) > 250

# Section 3.2 Evaluating missing values

# 3.2.1 Which variables have at least one interviewee with a missing (NA) value?
summary(CPS)

# 3.2.2 Determine if there is a pattern in missing data between Region, Sex, Age, or Citizenship variables and Married
table( (CPS$Region), is.na((CPS$Married)) ) # Region
table( (CPS$Sex), is.na((CPS$Married)) ) # Sex
table( (CPS$Age), is.na((CPS$Married)) ) # Age
table( (CPS$Citizenship), is.na((CPS$Married)) ) # Citizenship

# 3.2.3 How many states had all interviewees living in a non-metropolitan area?
table( (CPS$State), is.na((CPS$MetroAreaCode)) )

# 3.2.4 Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table( (CPS$Region), is.na((CPS$MetroAreaCode)) )

# 3.2.5 Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%
tapply( is.na((CPS$MetroAreaCode)), (CPS$State), mean )

## Section 3.3 - Integrating Metropolitan Area Data
MAM = read.csv('MetroAreaCodes.csv')
CCodes = read.csv('CountryCodes.csv')

# 3.3.1; 3.3.2 How many observations are in each dataset?
nrow(MAM); nrow(CCodes)

# Join the reference tables
CPS = merge(
  CPS, MAM,
    by.x = "MetroAreaCode",
    by.y = "Code",
    all.x = TRUE #Left Join
)

# What is the name of the vairable that was added to the data frame?
str(CPS)

# How many havea  missing value?
table( is.na((CPS$MetroArea)) )

# 3.3.3 Which MSA has the largest number of interviewees?
sort( table((CPS$MetroArea)) )

# 3.3.4 Which MSA has the highest proportion of Hispanic ethnicity?
sort( tapply( (CPS$Hispanic), (CPS$MetroArea), mean ) )

# 3.3.5 Determine the number of MSAs which have a greater than 20% Asian interviewees
tapply( (CPS$Race) == "Asian", (CPS$MetroArea), mean )

# 3.3.6 Determine the MSA which had the smallest proportion of interviewees who received no high school diploma
sort( tapply( (CPS$Education) == "No high school diploma", (CPS$MetroArea), mean, na.rm = TRUE ), decreasing = TRUE )

## Section 3.4 - Integrating Country of Birth Data
CPS = merge(
  CPS, CCodes,
    by.x = "CountryOfBirthCode",
    by.y = "Code",
    all.x = TRUE #Left Join
)

# 3.4.1 What is the name of the variable added to the CPS data frame by this merge operation?
str(CPS)

# How many interviewees have a missing value for the new country of birth variable?
table( is.na((CPS$Country)) )

# 3.4.2 Among all interviewees born outside of North America, which country was the most common place of birth?
sort( table((CPS$Country)) )

# 3.4.3 What poportion of the interviewees from the
# "New York-Northern New Jersey-Long Island, NY-NJ-PA" MSA have a country of
# birth outside of the US?
tapply( (CPS$Country) != "United States", (CPS$MetroArea) == "New York-Northern New Jersey-Long Island, NY-NJ-PA", mean, na.rm = TRUE )

# 3.4.4 Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? 
sort( tapply((CPS$Country) == "India", (CPS$MetroArea), sum, na.rm = TRUE) )

# Brazil?
sort( tapply((CPS$Country) == "Brazil", (CPS$MetroArea), sum, na.rm = TRUE) )

# Somalia?
sort( tapply((CPS$Country) == "Somalia", (CPS$MetroArea), sum, na.rm = TRUE) )

# Remove the data for the next exercise
rm(CCodes, CPS, MAM)

###########################################
# Homework Problem Set 4 - This is optional
###########################################

## Section 4.1 - Loading and summarizing the dataset - AnonymityPoll.csv
Poll = read.csv('AnonymityPoll.csv')

# 4.1.1 How many people participated in the poll
nrow(Poll)

# 4.1.2
# How many interviewees responded that they use a smartphone?
# How many interviewees responded that they don't use a smartphone?
table( (Poll$Smartphone) )

# How many interviewees did not respond to the question, resulting in a mssing value, or NA, in the summary() output?
summary( (Poll$Smartphone) )

# 4.1.3
# Which states are in the Midwest census region?
table( (Poll$State), (Poll$Region) == "Midwest" )

# Which was the state in the South census region with the largest number of interviewees?
table( (Poll$State), (Poll$Region) == "South" )

## Section 4.2

# 4.2.1
# How many interviewees reported not having used the Internet and not having used a smartphone?
# How many interviewees reported having used the Internet and having used smartphone?
# How many interviewees reported having used the Internet but not having used a smartphone?
# How many interviewees reported having used smartphone but not having used the Internet?
table( (Poll$Smartphone), (Poll$Internet.Use) )

# 4.2.2
# How many interviewees have a missing avlue for their Internet use?
table( is.na((Poll$Internet.Use)) )
# or
summary( (Poll$Smartphone) )

# 4.2.3 How many interviewees are in the new data frame of which only includes interviewees who reported Internet use or who reported smartphone use
limited = subset( Poll, (Internet.Use) == 1 | (Smartphone) == 1 )

nrow(limited)

## Section 4.3

# 4.3.1 Which variables have missing values in the limited data frame?
summary( limited )

# 4.3.2 What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
mean( (limited$Info.On.Internet) )

# 4.3.3
# How many interviewees reported a value of 0 for Info.On.Internet?
nrow( subset( limited, (Info.On.Internet) == 0 ) )

# How many interviewees reported the max value of 11 for Info.On.Internet?
nrow( subset( limited, (Info.On.Internet) == 11 ) )

# 4.3.4 What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet?
table( (limited$Worry.About.Info) )[2] / sum(table( (limited$Worry.About.Info) ) )
# or
summary( (limited$Worry.About.Info) )

# 4.3.5 What porportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet?
summary( (limited$Anonymity.Possible) )

# 4.3.6 What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet?
summary( (limited$Tried.Masking.Identity) )

# 4.3.7 What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
summary( (limited$Privacy.Laws.Effective) )

## Section 4.4 - Relating Demographcis to Polling Results

# 4.4.1 Build a histogram of the age of interviewees.  What is the best represented age group in the population?
hist( (limited$Age) )

# 4.4.2 What is the largest number of interviewees that have exactly the same value in their Age variable AND the same value in their Info.On.Internet variable?
# In other words, what is the largest number of overlapping points in the plot plot(limited$Age, limited$Info.On.Internet)?
plot(
  (limited$Age), (limited$Info.On.Internet),
    xlab = "Age",
    ylab = "Information on Internet",
    main = "Relationship between Age and Information on Internet"
)

max( table( (limited$Age), (limited$Info.On.Internet) ) )

# 4.4.3 To avoid points covering each other up, we can use the jitter() function on the values we pass to the plot function. Experimenting with the command jitter(c(1, 2, 3)),
# what appears to be the functionality of the jitter command?
jitter( c(1,2,3) )

# 4.4.4 Now, plot Age against Info.On.Internet with plot(jitter(limited$Age), jitter(limited$Info.On.Internet)). What relationship to you observe between Age and Info.On.Internet?
plot(
  jitter((limited$Age)), jitter((limited$Info.On.Internet)),
  xlab = "Age",
  ylab = "Information on Internet",
  main = "Relationship between Age and Information on Internet"
)

# 4.4.5
# What is the average Info.On.Internet value for smartphone users?
# What is the average Info.On.Internet value for non-smartphone users?
tapply( (limited$Info.On.Internet), (limited$Smartphone), summary )

# 4.4.6
# What proportion of smartphone users who answered the Tried.Masking.Identity question of tried masking their identity when using the internet?
# What proportion of non-smartphone users who answered the Tried.Masking.Identity question of tried masking their identity when using the internet?
tapply( (limited$Tried.Masking.Identity), (limited$Smartphone), summary )
