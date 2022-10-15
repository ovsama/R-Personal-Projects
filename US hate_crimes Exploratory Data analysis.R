# This project aims at performing an exploratory data analysis of a data set  from  Bloomington Police Department (Indiana, USA) 
#cases where a hate or bias crime has been reported from 02/06/2016 to 02/25/2022.

# The dataset has been retrevied directly from the Data Gov website  (url: https://catalog.data.gov/dataset/hate-crimes).
#--------------------------------------------------------------------------------------------------------------------------------------


# Here we go !


# Setting the working directory to the location of the data set file.
setwd("C:/Users/OS/Desktop")

# Loading the data set
x <- read.csv("Hate_crimes.csv")

# View the data
View(x)

colnames(x)

#_______________________
# Summary statistics
#_______________________

str(x)
dim(x)
head(x)
tail(x)

# Check to see if there are missing data?
sum(is.na(x))

#Get a larger set of statistics (a broader overview)
install.packages("skimr")
library(skimr)
skim(x)

#____________________
# Data transformation
#____________________

x$motivation

# Here, we notice that there are 4 modalities ("Anti-Gay"/ "Anti-Homosexual Male"/ "Anti-Male Homosexual"/ "Anti-LGBT+" )
#that can be grouped in one single category: "Anti-LGBT+".

# So to make things simple, we choose to integrate the 3 modalities ("Anti-Gay"/ "Anti-Homosexual Male"/ "Anti-Male Homosexual") within
#the "Anti-LGBT+" modality.

x$motivation[7] <- "Anti-Homosexual Male"

which(x$motivation == "Anti-Gay")

x$motivation[39:41] <- "Anti-LGBT+" 

which(x$motivation == "Anti-Homosexual Male")

x$motivation[c(1,7,15,20)] <- "Anti-LGBT+"


x$offender_race
factor(x$offender_race)
table(factor(x$offender_race))

# Another anomaly detected here : The white race is represented by 2 different forms ("W" uppercase and "w" lowercase) due to the case sensitivity of R.
# We are going to correct that by transforming "w" lowercase into "W" uppercase.

which(x$offender_race == "w")
x$offender_race[c(18,25)] <- "W"


x$offense
# A third anomaly noticed regarding to the nomenclature of the types of offense.
# Some types of offense  are referred to by two slighlty different names.

which(x$offense == "Simple Assault/DC")
x$offense[31] <-  "Simple Assault"

which(x$offense == "Aggravated Battery (att)")
x$offense[23] <-  "Aggravated Battery"


#____________________________
# Quick data visualization
#____________________________

# Eventually, the visualization will allow answering some interesting questions that flood the mind right off the bat.


#Create a function that facilitates the visualization of different variables:
vis <- function(var) {
        f <- factor(var)
        barplot(table(f)) 
}


# What is the frequency of the number of victims ?
vis(x$victims)

# What are the most frequent hate crime's motivations ?
vis(x$motivation)

# The week's days where the hate crimes occur the most ?
vis(x$weekday)

# What is the gender the most affected by hate crimes ?
vis(x$victim_gender)

# What is the race of the offender in most cases ? 
vis(x$offender_race)

# What is the kind of offense the most reported ?
vis(x$offense)

# However, data visualization will not be always useful when it comes down to examning the distribution within each variable.
# In the case of a categorical variable which contains too many modalities, it would be difficult to look at the distribution using a barplot.

# For example, to find out in which places the hate crimes occur the most, we should look at the "location_type" variable that contains 18 modalities.
# A barplot of the data of this variable will be so messy.
vis(x$location_type)
# So to answer the question, we are going to build a table comprised of all the modalities with their respective frequencies.
factor(x$location_type)
levels(factor(x$location_type))
sort(table(factor(x$location_type))

     


