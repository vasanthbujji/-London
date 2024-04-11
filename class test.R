#Q1
london_crime =read.csv("london-crime-data.csv")
View(london_crime)
#structure of the dataset
str(london_crime)
# Amalgamate the day month and year variables into a new variable called Date
london_crime$Date <- paste(london_crime$day, london_crime$month, london_crime$year, sep = "-")
# Removing  the original day, month, and year variables 
london_crime <- london_crime[, !names(london_crime) %in% c("day", "month", "year")]
#updated structure of the dataset
str(london_crime)

#Q2
# Renaming the specified variables
london_crime <- london_crime[, c("borough", "major_category", "minor_category", "value", "Date")]
names(london_crime) <- c("Borough", "MajorCategory", "SubCategory", "Value", "CrimeDate")
#updated structure of the dataset
str(london_crime)

#Q3
# Converting the CrimeDate variable to type Date
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, format = "%d-%m-%Y")
# Updated structure and content of the CrimeDate variable
str(london_crime$CrimeDate)
head(london_crime$CrimeDate)

#Q4
# Create a table summarizing the borough information
borough_summary <- table(london_crime$Borough)
# Plot the chart
barplot(borough_summary, main = "Crime Summary by Borough", xlab = "Borough", ylab = "Number of Crimes")
# Borough with the highest level of crime
borough_HC <- names(borough_summary)[which.max(borough_summary)]
print(paste("Borough with the highest level of crime:", borough_HC))
# Borough with the lowest level of crime
borough_LC <- names(borough_summary)[which.min(borough_summary)]
print(paste("Borough with the lowest level of crime:", borough_LC))

#Q5
# Creating a table and summarizing the MajorCategory information
major_category_summary <- table(london_crime$MajorCategory)
# Plot the pie chart
pie(major_category_summary, main = "Major Categories of Crime", labels = paste(names(major_category_summary), "\n", major_category_summary), col = rainbow(length(major_category_summary)))
# Major category with the highest level of crimes
major_category_highest <- names(major_category_summary)[which.max(major_category_summary)]
print(paste("Major category with the highest level of crimes:", major_category_highest))
# Major category with the lowest level of crimes
major_category_lowest <- names(major_category_summary)[which.min(major_category_summary)]
print(paste("Major category with the lowest level of crimes:", major_category_lowest))

#Q6
# Creating the  new variable called Region and store the correct region for each borough
London_regions <- c("East", "North", "East","West","South","North","South","West","North","East","North","West","North","West","East","West","West","Central","Central","East","Central","Central","South","East","East","West","Central","South","Central","Central","East","Central")
# Assigning the regions 
london_crime$Region <- London_regions[match(london_crime$Borough, c("Barking and Dagenham", "Barnet", "Bexley","Brent","Bromley","Camden","Croydon","Ealing","Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea","Kingston upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond upon Thames","Southwark","Sutton","Tower Hamlets","Waltham Forest","Wandsworth","Westminster"))]
# Checking for NA values in the Region 
na_boroughs <- london_crime$Borough[is.na(london_crime$Region)]
# Print boroughs with NA region values
if(length(na_boroughs) > 0) {
  print("Boroughs with NA Region:")
  print(na_boroughs)
}
# Replacing NA regions with suitable regions
# Replacing NA for Barking and Dagenham with "East"
london_crime$Region[london_crime$Borough == "Barking and Dagenham"] <- "East"
# updated dataset structure 
str(london_crime)

#Q7
# Create a table and summarizing the number of reported crimes by region
region_crime_summary <- table(london_crime$Region)
# Ploting the number of reported crimes by region
plot(region_crime_summary, type = "b", main = "Reported Crimes by Region", xlab = "Region", ylab = "Number of Reported Crimes")
# Region with the highest number of crimes
region_HC <- names(region_crime_summary)[which.max(region_crime_summary)]
print(paste("Region with having highest number of crimes:", region_HC))
# Region with the lowest number of crimes
region_LC <- names(region_crime_summary)[which.min(region_crime_summary)]
print(paste("Region with having lowest number of crimes:", region_LC))

#Q8
# Finding the region with the highest number of crimes
region_HC <- names(region_crime_summary)[which.max(region_crime_summary)]
region_HC
# Extracting subset of data with the highest number of crimes
subset_HC <- london_crime[london_crime$Region == region_HC, ]
subset_HC
# Finding the region with the lowest number of crimes
region_LC <- names(region_crime_summary)[which.min(region_crime_summary)]
region_LC
# Extracting subset of data with the lowest number of crimes
subset_LC <- london_crime[london_crime$Region == region_LC, ]
subset_LC
# Print the major crime category for both regions
print(paste("Major crime category for region with the highest number of crimes:", unique(subset_HC$MajorCategory)))
print(paste("Major crime category for region with the lowest number of crimes:", unique(subset_LC$MajorCategory)))

#Q9
par(mfrow = c(1, 2))
# Ploting for subset with highest number of crimes
barplot(table(subset_HC$MajorCategory),
        main = paste("Major Crime Categories in", region_HC),
        xlab = "Major Crime Category",
        ylab = "Number of Reported Crimes",
        las = 2, 
        ylim = c(0, max(table(subset_HC$MajorCategory), table(subset_LC$MajorCategory))),
        col = "skyblue")
# Ploting for subset with lowest number of crimes
barplot(table(subset_LC$MajorCategory),
        main = paste("Major Crime Categories in", region_LC),
        xlab = "Major Crime Category",
        ylab = "Number of Reported Crimes",
        las = 2,
        ylim = c(0, max(table(subset_HC$MajorCategory), table(subset_LC$MajorCategory))),
        col = "lightgreen")
# Reseting graphics parameters
par(mfrow = c(1, 1))

#Q10
write.csv(london_crime, file = "london-crime-modified.csv", row.names = FALSE)
