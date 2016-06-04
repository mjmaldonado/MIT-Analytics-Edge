############################################
#
# Data visualization
#
############################################

# We will be using data from the World Health Organization

# clear some space
rm( list = ls() )

# read in the data
who = read.csv('WHO.csv')

# take a look at the structure of the data
str(who)

# let's make a quick scatter plot
plot(who$GNI, who$FertilityRate)

# now let's try the same plot with ggplot2
library(ggplot2)

# remember we need at least three things to make a plot
# data
# an aesthetic object
# a geometric object

scatterplot = ggplot(who, aes(x = GNI, y = FertilityRate))

# now we need to tell ggplot which geometric objects to use for the scatter plot
scatterplot + geom_point()

# let's add blue triangles
scatterplot + geom_point(color = "blue", size = 3, shape = 17)

# let's try another option
scatterplot + geom_point(color = "darkred", size = 3, shape = 8)

# now let's add a title to our plot
scatterplot + geom_point(color = "darkred", size = 3, shape = 20) + ggtitle("Fertility Rate vs. Gross National Income")

# now let's save our plot to a file
fertilityGNIplot = scatterplot + geom_point(color = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")

pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()

# now let's color the points by region
ggplot(who, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

# let's now color the points based on the life expectancy
ggplot(who, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

# let's take a look at a different plot now
ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()

# let's perform a tranformation of the fertility rate to turn this into a linear relationship
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()

# now let's build a model using this relationship
model = lm(Under15 ~ log(FertilityRate), data = who)
summary(model)

# and incorporate this back into the graph
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")

# let's add a 99% confidence interval instead
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

# let's change the color of the regression line
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", color = "orange")

# quick question
# color the points by Region, which region has a very low fertility rate
ggplot(who, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer(palette = "Dark2")


###############################################
#
# Chicago Crime Visualization
#
###############################################

rm( list = ls() )

# read in the data
mvt = read.csv('mvt.csv', stringsAsFactors = FALSE)

# take a look at the structure of the data
str(mvt)

# turn the date field into a date data type
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")

mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

str(mvt)

# make a table of the crime frequency by weekday
weekday.counts = as.data.frame( table(mvt$Weekday) )

str(weekday.counts)

# now we're ready to make our plot
ggplot(weekday.counts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))

# let's order the Var1 variable to be an ordered factor
weekday.counts$Var1 = factor(weekday.counts$Var1, ordered = TRUE, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Friday", "Saturday"))

# now let's try our plot again
ggplot(weekday.counts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))

# now let's change the x and y labels of our plot
ggplot(weekday.counts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), alpha = 1) + xlab("Day of the Week") + ylab("Total More Vehicle Thefts")

# quick question - what do the "linetype" and "alpha" commands do to the line plot
# linetype changes the line type (dashed, solid, etc)
# alpha changes the color of the line (closer to 0 is lighter)

# now let's add the hour of the day
day.hour.counts = as.data.frame( table(mvt$Weekday, mvt$Hour) )

str(day.hour.counts)

# convert the hour variable to a numeric
day.hour.counts$Hour = as.numeric(as.character(day.hour.counts$Var2))

# now plot the data
ggplot(day.hour.counts, aes(x = Hour, y = Freq, color = Var1), size = 2) + geom_line(aes(group = Var1))

# let's visualize this data as a heat map
day.hour.counts$Var1 = factor(day.hour.counts$Var1, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(day.hour.counts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# let's clean up the graph a little bit
ggplot(day.hour.counts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV Thefts") + theme(axis.title.y = element_blank())

# we can also change the color scheme
ggplot(day.hour.counts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV Thefts", low = "white", high = "red") + theme(axis.title.y = element_blank())

# now let's do some geographical mapping
install.packages('maps')
install.packages('ggmap')

library(maps)
library(ggmap)

chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

# now let's aggregate based on the lat/long
lat.lon.counts = as.data.frame( table(round(mvt$Longitude,2),round(mvt$Latitude,2)) )

# take a look at our data
str(lat.lon.counts)

# let's convert our lat and lon to numeric data types
lat.lon.counts$Long = as.numeric(as.character(lat.lon.counts$Var1))
lat.lon.counts$Lat  = as.numeric(as.character(lat.lon.counts$Var2))

ggmap(chicago) + geom_point(data = lat.lon.counts, aes(x = Long, y = Lat, color = Freq, size = Freq))

# let's change the color scheme
ggmap(chicago) + geom_point(data = lat.lon.counts, aes(x = Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low = "yellow", high = "red")

# we can also use geom_tile to make a traditional heat map
ggmap(chicago) + geom_tile(data = lat.lon.counts, aes(x = Long, y = Lat, alpha = Freq, fill = "red"))

# our map is currently plotting motor vehicle theft our in the water, which seems a little odd.
# let's take care of this behavior
lat.lon.counts2 = subset(lat.lon.counts, Freq > 0)

ggmap(chicago) + geom_tile(data = lat.lon.counts2, aes(x = Long, y = Lat, alpha = Freq, fill = "red"))

# how many observations did we remove?
nrow(lat.lon.counts) - nrow(lat.lon.counts2)

#################################################
#
# Murders in the US
#
#################################################

rm( list = ls() )

# read in the data (which is provided by the U.S. Census Bureau and the FBI)
murders = read.csv('murders.csv')

str(murders)

# load a map of the US
US.map = map_data("state")
ggplot(US.map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

# now we need to make sure the state names match between the murder data frame and the US.map data frame
murders$region = tolower(murders$State)

# now we can join the data frames together
murder.map = merge(US.map, murders, by = "region")

str(murder.map)

ggplot(murder.map, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")

# let's create a murder rate variable
murder.map$MurderRate = (murder.map$Murders / murder.map$Population) * 100000

# now let's plot based on the murder rate
ggplot(murder.map, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")

# now let's make limit the data that returns
ggplot(murder.map, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend", limits = c(0,10))

# quick question
# which of the following states has the highest gun ownership rate?
ggplot(murder.map, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")
