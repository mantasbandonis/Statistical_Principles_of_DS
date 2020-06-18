# Library

library("readr")
library("ISLR")
library("tibble")
library("ggplot2")
library("reshape2")
library("GGally")
library("cowplot")
library("psych")
library("factoextra")

my_data <- read.table("insurance.csv", sep=",", header=T)

my_data_num <- my_data

# Check how many values are missing, no missing values
colSums(is.na(my_data))


# Check types of variables in the data
str(my_data)


#summarize all variables
summary(my_data)




# PART 2  - Visualizations and descriptive statistics without numeric values

# 1. Correlation Between Charges and Age/BMI 

x <- ggplot(my_data, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(my_data, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


# 2. Correlation Between Charges and Sex/Children covered by insurance 

x <- ggplot(my_data, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()

y <- ggplot(my_data, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 

title <- ggdraw() + draw_label("2. Correlation between Charges and Sex / Children covered by insurance", fontface='bold')

plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

# 3. Correlation between Charges and Smoker / Region

x <- ggplot(my_data, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()

y <- ggplot(my_data, aes(region, charges)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("3. Correlation between Charges and Smoker / Region", fontface='bold')

plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))



ggplot(data = my_data,aes(region,charges)) + geom_boxplot(fill = c(2:5)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges per Region")


# Bsed from above plot, we can disclose that region of origin doesn’t have much impact with the amount of medical cost.

ggplot(data = my_data,aes(smoker,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Smoking Status")

# On the other hand, the same cannot be said with smoking status. It can be clearly deceived that smokers spends a lot more in terms of medical expenses compared to non-smokers by almost 4x.


ggplot(data = my_data,aes(sex,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Gender")

# Medical expenses doesn’t seem to be affected by gender as well.

my_data$obese <- as.factor(ifelse(my_data$bmi >=30, "yes", "no"))

ggplot(data = my_data,aes(bmi30,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Obesity")

# The idea behind deriving a new variable bmi30 is that, 30 is the bmi threshold for obesity and we all know that obesity plays a huge role in a person’s health. As we can see, although obese and non-obese people has the same median medical expenses, their average expenditure differ by almost USD 5000.


#1. Variable Age
summary(my_data$age)
#Histogram
age <- ggplot(my_data, aes(x=age))
age + geom_histogram(breaks=seq(18,64, by=2) , fill=I("lightblue"), 
                     col=I("black")) + labs(title = "Histogram of Age", x="Age in years",y="Count") + scale_x_continuous(breaks = seq(20, 65, 5))

#2. Variable sex
table(my_data$sex)
prop.table(table(my_data$sex))

#Bar chart
sex <- ggplot(my_data, aes(x=sex, y = (..count..)/sum(..count..)))
sex + geom_bar(fill=I("lightblue"), col=I("black"), alpha=1) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)  + labs(title = "Distribution of Gender", x="Gender", y="Percent") + scale_y_continuous(breaks = seq(0, 0.6, 0.1),labels = scales::percent)


#3. Variable BMI
summary(my_data$bmi)
#histogram
bmi <- ggplot(my_data, aes(x=bmi))
bmi + geom_histogram(breaks=seq(15,54, by=2) , fill=I("lightgreen"), 
                     col=I("black")) + labs(title = "Histogram of BMI", x="BMI",y="Count") + scale_x_continuous(breaks = seq(15, 55, 5))


#4. Variabe children
table(my_data$children)
prop.table(table(my_data$children))

#Bar chart
children <- ggplot(my_data, aes(x=as.factor(children), y = (..count..)/sum(..count..)))
children + geom_bar(fill=I("lightblue"), col=I("black"), alpha=1) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)  + labs(title = "Distribution of Number of Children", x="Number of Children", y="Percent") + scale_y_continuous(breaks = seq(0, 0.6, 0.1),labels = scales::percent)


#5. Variable Smoker
table(my_data$smoker)
prop.table(table(my_data$smoker))

#Bar chart
smoker <- ggplot(my_data, aes(x=smoker, y = (..count..)/sum(..count..)))
smoker + geom_bar(fill=I("lightblue"), col=I("black"), alpha=1) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)  + labs(title = "Distribution of Smoking Status", x="Person is Smoker", y="Percent") + scale_y_continuous(breaks = seq(0, 0.9, 0.2),labels = scales::percent)


#6. Variable Region
table(my_data$region)
prop.table(table(my_data$region))

#Bar chart
region <- ggplot(my_data, aes(x=region, y = (..count..)/sum(..count..)))
region + geom_bar(fill=I("lightblue"), col=I("black"), alpha=1) + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)  + labs(title = "Distribution of Region", x="Region", y="Percent") + scale_y_continuous(breaks = seq(0, 0.3, 0.05),labels = scales::percent)


#7. Variable Charges
summary(my_data$charges)
#Histogram
c <- ggplot(my_data, aes(x=charges))
c + geom_histogram(breaks=seq(0,64000, by=2000) , fill=I("red"), 
                   col=I("black")) + labs(title = "Histogram of Charges", x="Charges",y="Count")  + scale_x_continuous(breaks = seq(0, 60000, 10000))

#7.1 Charges and Age
ggplot(my_data, aes(x=age, y=charges)) + geom_point()+ labs(title="Scatter plot Charges and Age ", 
                                                            caption="",
                                                            x="Age",
                                                            y="Charges")+ geom_smooth()

#7.2 Charges and Sex
c + geom_density(aes(fill=sex), color = NA, alpha=.35) + labs(title="Density plot Charges and Sex ", 
                                                              subtitle="Density Plot Grouped by Sex",
                                                              caption="",
                                                              x="Charges",
                                                              fill="Legend")

ggplot(data = my_data,aes(sex,charges)) + geom_boxplot(fill = c(2,4), alpha=.6) +labs(title="Boxplot Charges and Sex", 
                                                                                      subtitle="Boxplot Grouped by Sex",
                                                                                      caption="",
                                                                                      x="Gender",
                                                                                      y="Charges") + coord_flip()

### 7.3 Charges and BMI
#7.3 Charges and BMI
ggplot(my_data, aes(x=bmi, y=charges)) + geom_point()+ labs(title="Scatter plot Charges and BMI ", 
                                                            caption="",
                                                            x="BMI",
                                                            y="Charges") + geom_smooth()


#7.4 Charges and Children
c + geom_density(aes(fill=as.factor(children)), color = NA, alpha=.45) + labs(title="Density plot Charges and Children ", 
                                                                              subtitle="Density Plot Grouped by Number of Children",
                                                                              caption="",
                                                                              x="Charges",
                                                                              fill="Legend")

ggplot(data = my_data,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7), alpha=.6) +labs(title="Boxplot Charges and Children", 
                                                                                                      subtitle="Boxplot Grouped by Number of Children",
                                                                                                      caption="",
                                                                                                      x="Number of Children",
                                                                                                      y="Charges") + coord_flip()

#7.5 Charges and Smoker
c + geom_density(aes(fill=smoker), color = NA, alpha=.35) + labs(title="Density plot Charges and Smoking", 
                                                                 subtitle="Density Plot Grouped by Smoking Status",
                                                                 caption="",
                                                                 x="Charges",
                                                                 fill="Legend")

ggplot(data = my_data,aes(smoker,charges)) + geom_boxplot(fill = c(2,4), alpha=.6) +labs(title="Boxplot Charges and Smoking", 
                                                                                         subtitle="Boxplot Grouped by Smoking Status",
                                                                                         caption="",
                                                                                         x="Smoking Status",
                                                                                         y="Charges") + coord_flip()

#7.6 Charges and Region
c + geom_density(aes(fill=region), color = NA, alpha=.45) + labs(title="Density plot Charges and Region ", 
                                                                 subtitle="Density Plot Grouped by Region",
                                                                 caption="",
                                                                 x="Charges",
                                                                 fill="Legend")

ggplot(data = my_data,aes(region,charges)) + geom_boxplot(fill = c(2:5), alpha=.6) +labs(title="Boxplot Charges and Region", 
                                                                                         subtitle="Boxplot Grouped by Region",                                                                                        caption="",
                                                                                         x="Region",
                                                                                         y="Charges") + coord_flip()

# Converting Smoker and Sex into numeric Values
# Female 1, Male 0 

my_data_num$sex <- ifelse(my_data$sex == "female", 1,0)
my_data_num$sex = as.numeric(my_data_num$sex)


# Smoker 1, Non-smoker 0 

my_data_num$smoker <- ifelse(my_data$smoker == "yes", 1,0)
my_data_num$smoker = as.numeric(my_data_num$smoker)


my_data_num$region <- as.numeric(as.factor(my_data$region))

# Only use this if you want the regions in numeric




# Add columns to distinguish between low charges and high charges (standard : mean(charges))

my_data_num$group <- ifelse(my_data_num$charges > mean(my_data_num$charges), "high", "low")


# Check correlation Coeffeicient

cor(my_data_num[-8])


# Correlation

cor(my_data_num[-8])[,"charges"]

ggcorr(my_data_num[-8], name = "corr", label = TRUE)+
  
  theme(legend.position="none")

# PCA Plot


my_data_pca <- my_data_num[, c(1,3,4,5,6)];


my_data_pca$smoker = as.numeric(my_data_pca$smoker)
my_data_pca$children <- as.numeric(my_data_pca$children)
my_data_pca$age <- as.numeric(my_data_pca$age)


res.pca <- prcomp(my_data_pca, scale = TRUE)

fviz_pca_biplot(res.pca, col.ind = my_data_num$group, col="black",
                
                palette = "jco", geom = "point", repel=TRUE,
                
                legend.title="Charges")




## Densety Plot / visual inspection

c <- ggplot(mdn, aes(x=charges))
c + geom_density()+
  labs(title="Density plot",subtitle="Density Plot for the charges",
       caption="Source: In R studio",
       x="Charges")
b <- ggplot(mdn, aes(x=bmi))
b + geom_density()+
  labs(title="Density plot", 
       subtitle="Density Plot for the bmi",
       caption="Source: In R studio",
       x="Bmi")


c + geom_density(aes(fill=sex), color = NA, alpha=.35) + labs(title="Density plot charges and sex ", 
                                                              subtitle="Density Plot Grouped by Number of Color",
                                                              caption="Let it go XDD",
                                                              x="Charges",
                                                              fill="# Color")
b + geom_density(aes(fill=sex), color = NA, alpha=.35) + labs(title="Density plot bmi and sex ", 
                                                              subtitle="Density Plot Grouped by Number of Color",
                                                              caption="Let it go XDD",
                                                              x="Bmi",
                                                              fill="# Color")

c + geom_density(aes(fill=smoker), color = NA, alpha=.35) + labs(title="Density plot charges and smoker", 
                                                                 subtitle="Density Plot Grouped by Number of Color",
                                                                 caption="Let it go XDD",
                                                                 x="Charges",
                                                                 fill="# Color")
b + geom_density(aes(fill=smoker), color = NA, alpha=.35) + labs(title="Density plot bmi and smoker", 
                                                                 subtitle="Density Plot Grouped by Number of Color",
                                                                 caption="Let it go XDD",
                                                                 x="Bmi",
                                                                 fill="# Color")

c + geom_density(aes(fill=children), color = NA, alpha=.35) + labs(title="Density plot charges and children", 
                                                                   subtitle="Density Plot Grouped by Number of Color",
                                                                   caption="Let it go XDD",
                                                                   x="Charges",
                                                                   fill="# Color")
b + geom_density(aes(fill=children), color = NA, alpha=.35) + labs(title="Density plot bmi and children", 
                                                                   subtitle="Density Plot Grouped by Number of Color",
                                                                   caption="Let it go XDD",
                                                                   x="Bmi",
                                                                   fill="# Color")
c + geom_density(aes(fill=region), color = NA, alpha=.35) + labs(title="Density plot charges and region", 
                                                                 subtitle="Density Plot Grouped by Number of Color",
                                                                 caption="Let it go XDD",
                                                                 x="Charges",
                                                                 fill="# Color")
b + geom_density(aes(fill=region), color = NA, alpha=.35) + labs(title="Density plot bmi and region", 
                                                                 subtitle="Density Plot Grouped by Number of Color",
                                                                 caption="Let it go XDD",
                                                                 x="Bmi",
                                                                 fill="# Color")
boxplot(mdn$charges)
boxplot(mdn[,-7])

# Method to use is a Logistic Regression!

head(mdn)
##  Split into train/test splits first.

set.seed(666)

default_idx <- sample(nrow(mdn), ceiling(nrow(mdn) / 2))    # coul allso macke 1/ 2/3 what you want
default_trn <-  Default[default_idx, ]
default_tst <- Default[-default_idx, ]

# Create the model.
mb1 <- glm(children ~ smoker + bmi, data = mdn, family = "binomial")
mg1 <- glm(charges ~ smoker + bmi, data = mdn, family = "gaussian")

summary(mb1)
summary(mg1)






# PCA 


fviz_pca_biplot(res.pca, 
                
                geom.ind = "point", 
                
                col.ind = my_data_num$group,
                
                pointsize = 2,
                
                palette = "jco",
                
                addEllipses = TRUE,
                
                label="var",
                
                col.var="black",
                
                repel=TRUE,
                
                legend.title="Charges")


daten2<-mdn[,c(-2,-4,-5,-6)]  # get out the categorical variabel

head(daten2)

ggpairs(data = daten2, columns = 1:3)
# Left and Right are highly correlated (Corr: 0.743). Correlated predictors creates problems in prediction.

# correlation matrix
cor.mat <- cor(daten2) ; cor.mat  
eig <- eigen(cor.mat) ; eig

# Extract eigen values and eigen vectors
evals <- eig$values; evecs <- eig$vectors

# sort the eigen values in descending order
evals.sorted <- sort(evals, decreasing = T)


##  Produce a scree-plot for this PCA.
plot(eig$values,
     ylab = "Eigenvalues",
     xlab = "Compunentnumber",
     main = "Scree-Plot")
abline(h=1, col="blue")

# Compute proportion of explained variances
var.exp <- evals.sorted/sum(evals)
barplot(var.exp, ylim = c(0,1), col = 'sandybrown',
        xlab = "Principal Component",
        ylab = "Explained Variances",
        axes = TRUE)
axis(1, c(0.7, 1.9, 3.1, 4.3,5.5,6.7),
     labels = c("PC1", "PC2", "PC3", "PC4","PC5","PC6"))
lines(cumsum(var.exp), type = 's', col = 'darkgreen')
legend(x = 2.5, y = 0.5, legend = c('Explained Variance', 'Cumulative Explained Variance'),
       pch = c(15,15), col = c('sandybrown', 'darkgreen'),
       bty = 'n')

var.exp # Shows the Explaind Variance     



# Transformation matrix
trmat <- eig$vectors[,1:2]

# New data
trans.data <- as.matrix(daten2.n) %*% trmat

# Print the partial new data matrix
print(head(trans.data, n = 20))

# Sex
plot(trans.data)
points(trans.data[which(mdn$sex==1),], col="blue")
points(trans.data[which(mdn$sex==0),], col="red")


# Smoker
plot(trans.data)
points(trans.data[which(mdn$smoker==1),], col="blue")
points(trans.data[which(mdn$smoker==0),], col="red")

# Children 
plot(trans.data)
points(trans.data[which(mdn$smoker==0),], col="blue")
points(trans.data[which(mdn$smoker==1),], col="red")
points(trans.data[which(mdn$smoker==2),], col="chocolate")
points(trans.data[which(mdn$smoker==3),], col="darkgoldenrod1")
points(trans.data[which(mdn$smoker==4),], col="darkmagenta")
points(trans.data[which(mdn$smoker==5),], col="darkslategray1")


# Region
plot(trans.data)
points(trans.data[which(mdn$smoker==1),], col="blue")
points(trans.data[which(mdn$smoker==0),], col="red")





# Regression

lm(charges~., data=my_data_num)  

# Smokers increases helath care costs(charges) by $14,972 per year.

# As the number of children increases, helath care costs(charges) can be increased by $432.

# –> guess: The increase in dependents can increase the cost of care such as hospital care,



# How do you make a model if you want to give a higher penalty to an obese&smoke person?


lm(charges ~ obese * smoker, data=my_data) 

# obesity increases health care costs by $865, and smoking increases health care costs by $13,386.

# But the both components are applied, (if smoking and obesity are together),

# It can be expected that medical expenses will increase the most with $19,329.
  
# By predicting health care charges using linear regression methods, it is possible to impose different insurance premiums depending on the charges.

# As a result of the model comparison above(4), by using * rather than +, the prediction of the model became more similar to reality.






