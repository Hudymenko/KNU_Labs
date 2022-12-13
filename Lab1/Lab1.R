# Preparation.
setwd("C:/Users/USER/Desktop/DataProcessingLabs/Lab1")
data <- read.csv("airquality.csv")

# Question 1. What are the column names of the data frame?
colnames(data)

# Question 2. What are the row names of the data frame?
rownames(data)

# Question 3. Extract the first 6 rows of the data frame and print them to the console
head(data)

# Question 4. How many observations (i.e. rows) are in this data frame?
nrow(data)

# Question 5. Extract the last 6 rows of the data frame and print them to the console
tail(data)

# Question 6. How many missing values are in the “Ozone” column of this data frame?
sum(is.na(data$Ozone))

# Question 7. What is the mean of the “Ozone” column in this dataset? Exclude missing values (coded as NA) from this calculation.
mean(data$Ozone, na.rm = TRUE)

# Question 8. Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90.
subset(data, data$Ozone > 31 & data$Temp > 90)

# Question 9. Use a for loop to create a vector of length 6 containing the mean of each column in the data frame (excluding all missing values).
vector <- c()
for (i in 1:6)
  vector <- append(vector, mean(data[,i], na.rm = TRUE))
vector

# Question 10. Use the apply function to calculate the standard deviation of each column in the data frame (excluding all missing values).
sapply(data, function(x) sd(x, na.rm = TRUE))

# Question 11. Calculate the mean of “Ozone” for each Month in the data frame and create a vector containing the monthly means (exclude all missing values).
monthly_means <- aggregate(data$Ozone, list(data$Month), function(x) mean(x, na.rm = TRUE))
monthly_means$x

# Question 12. Draw a random sample of 5 rows from the data frame
data[sample(nrow(data), 5),]
