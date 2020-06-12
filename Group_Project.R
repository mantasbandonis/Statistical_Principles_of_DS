
library("readr")
my_data <- read.table("dataset_Facebook.csv", sep=";", header=T)


# Check how many values are missing
colSums(is.na(my_data))


# Convert cateogrical values (type of facebook content) to integers
# Link = 1, Photo = 2, Status = 3, Video = 4

my_data$Type <- as.numeric(as.factor(my_data$Type))


