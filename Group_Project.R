
library("readr")
my_data <- read.table("dataset_Facebook.csv", sep=";", header=T)


# Check how many values are missing
colSums(is.na(my_data))


# Convert cateogrical values to integers
my_data$Type <- as.numeric(as.factor(my_data$Type))


