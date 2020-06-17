library("readr")
my_data <- read.table("insurance.csv", sep=",", header=T)

my_data_num <- my_data

# Check how many values are missing, no missing values
colSums(is.na(my_data))


# Check types of variables in the data
str(my_data)

# TO-DO Here Visualizations and descriptive statistics without numeric values




# Converting Smoker and Sex into numeric Values
# Female 1, Male 0 

my_data_num$sex <- ifelse(my_data$sex == "female", 1,0)
my_data_num$sex = as.factor(my_data_num$sex )

# Smoker 1, Non-smoker 0 

my_data_num$smoker <- ifelse(my_data$smoker == "yes", 1,0)
my_data_num$sex = as.factor(my_data_num$smoker)


# Only use this if you want the regions in numeric

my_data_num$region <- as.numeric(as.factor(my_data$Type))
