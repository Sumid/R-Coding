####################
##### Problem 2 ####
####################

### Load package and Dataset ###
if(!require(DataExplorer)){install.packages("DataExplorer")}
library(DataExplorer)


### Exercise A ###
# Load the dataset "cars"
cars <- read.csv("CarsPCA.csv", header = TRUE)


# Set the first column as names
rownames(cars) <- cars[,1]
cars[,1] <- NULL
cars
### Exercise B ###
# Turn the cylinder variable into a numeric variable (use the as.numeric function)
cars$Cylinders <- as.numeric(cars$Cylinders)

### Exercise C ###
# What percentage of all variables are discrete columns? 
barplots.cars <- plot_bar(cars)
barplots.cars

### Exercise D ### 
# Determine the percentage of missing variables
profile_missing(cars) # Rear.seat.room = 0.02150538, luggage.room = 0.11827957
 

### Exercise E ### 
### Replace NA`s with median value (na.rm argument makes sure median is calculated)
cars$Rear.seat.room <- replace(cars$Rear.seat.room, is.na(cars$Rear.seat.room), median(cars$Rear.seat.room, na.rm = TRUE))
cars$Rear.seat.room
cars$Luggage.room <- replace(cars$Luggage.room, is.na(cars$Luggage.room), median(cars$Luggage.room, na.rm = TRUE))
cars$Luggage.room

### If there are less than 5 missing values left, drop records (lines) with NA`s in them, and only keep the
# complete cases 

### Exercise F ### 
#	Plot the histogram of continuous variables of the dataset! 
plot_histogram(cars)

#if(!require(esquisse))(install.packages("esquisse"))
#library (esquisse)

#esquisse::esquisser()
####################
##### Problem 3 ####
####################

# You will Conduct a Principal Component Analysis on the cars dataset you worked with in exercise 2. 
# Do not reload the dataset, the modified version you created in question 2 is needed


### Exercise A ### 
# Conduct PCA analysis and name it PCA_Cars
PCA_Cars <- prcomp(cars)
PCA_Cars


### Exercise B ### 
# Explore Principal Compontents` contribution to total variance
summary(PCA_Cars)


### Exercise C ### 
# Explore variables (disciplines) contribution to  Principal Compontents
PCA_Cars$rotation

### Exercise D ### 
# Plot the first two Principal Compontents on a biplot
biplot(PCA_Cars)
