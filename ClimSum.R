#Selecting and importing all the climate csvs

setwd("C:/Users/Alex/Desktop/Bio812/Group assignment/Data/Excel/Climate")
clim <- read.csv("2016-2017.csv")

#From this csv, want to extract Nov - March, and columns for date, snow on ground, and direction and speed of max gust
#and write to new csv


library(dplyr)
clim <- clim %>% rename(Date = ï..Date.Time) #renaming date column
#only selecting data from Nov 2016 - March 2017, from first snow fall to when data was collected


clim16 <- subset(clim, Year == "2016" & Month == "11" |Month == "12")
clim17 <- subset(clim, Year == "2017" & Month == "1" | Month == "2" | Month == "3")
clim2 <- rbind(clim16, clim17) #i think this worked?


clim3 <- clim2 %>% select(Date, Snow.on.Grnd..cm., Dir.of.Max.Gust..10s.deg., Spd.of.Max.Gust..km.h.) #creating a dataset for just the variables of interest

#now what do I do with this?

#mean wind direction to add to map
winddir <- mean(clim3$Dir.of.Max.Gust..10s.deg., na.rm = TRUE)*10
median(clim3$Dir.of.Max.Gust..10s.deg., na.rm = TRUE)*10
#Mean direction = 241.9 degrees
#Median direction = 270

#selecting data when snow is present (when snow on ground =/= NA) - not sure if i should actually do this



