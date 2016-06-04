##########################################
#
# Week 6 Recitation - Bar Charts and World Maps
#
# Author: Michael Maldonado
#
##########################################

# clear some space
rm( list = ls() )

# read in the data - MIT International Student data
intl = read.csv('intl.csv')

# load ggplot2
library(ggplot2)

str(intl)

# let's make an initial bar plot
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity") + geom_text(aes(label = PercentOfIntl))
# stat = "identity" -- use the value of the y value as is

# we need to make Region into a ordered factor to plot the bars in descending order
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))

str(intl)

# we'd like to scale our data differently now, too
intl$PercentOfIntl = intl$PercentOfIntl * 100

# now let's replot our data with some improvements
ggplot(intl,aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity", fill = "dark blue") + geom_text(aes(label = PercentOfIntl), vjust = -0.4) + ylab("Percent of International Students") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

#####################################################
#
# world data mapping
#
#####################################################

rm(intl)

intl = read.csv('intlall.csv', stringsAsFactors = FALSE)

library(ggmap)

head(intl)

# tranform the NAs into 0
intl[is.na(intl)] = 0

# now load the world map
world_map = map_data("world")

str(world_map)

world_map = merge(world_map, intl, by.x = "region", by.y = "Citizenship")

str(world_map)

ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

# we have to reorder the data into the correct order
world_map = world_map[order(world_map$group, world_map$order), ]

# let's try that again
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

# it appears we have some missing data.  this is because the names for the countries don't match up exactly in both data frames, so in the merge it didn't actually merge for those countries

# we won't handle that hear, but let's take a look at another way of viewing the data
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("ortho", orientation = c(20,30,0))

###########################################
#
# Line charts
#
###########################################

rm( list = ls() )

hh = read.csv('households.csv')

str(hh)

library(reshape2)
# melt will take a 2D dataframe and convert it into exactly the right form for our plot in ggplot2

(melt(hh, id = "Year"))[1:10,]

ggplot(melt(hh, id = "Year"), aes(x = Year, y = value, color = variable)) + geom_line(size = 2) + geom_point(size = 5) + ylab("Percentage of Households")
