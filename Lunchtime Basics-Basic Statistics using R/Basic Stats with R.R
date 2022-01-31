###### CU Data Week ######

# Load Data
bweight <- read.csv("~/Downloads/bweight.csv")
birthwgt <- read.csv("~/Downloads/birthwgt.csv")
cars <- read.csv("~/Downloads/cars.csv")
class <- read.csv("~/Downloads/class.csv")

########## Hypothesis 1 ########################################################
# Birthweight and Smoking 
################################################################################

# Summarize the data 
library(tidyverse)

# Difference in weights 
bweight %>% group_by(MomSmoke) %>% summarise(Mean = mean(Weight), SD = sd(Weight))
# 1 = Smoking; 0  = No Smoking 

# Other categories 
library(table1)

table1(~ MomWtGain + factor(MomEdLevel) + factor(Boy) | factor(MomSmoke), data = bweight)


########## Test 1: Relationship between Low birthweight and Maternal Smoking ###
table1(~ LowBirthWgt | Smoking, data = birthwgt)

# We have some missing data, let's remove it 
birthwgt <- birthwgt %>% filter(Smoking != "")

table(birthwgt$LowBirthWgt, birthwgt$Smoking)

# Chi-Square test
chisq.test(birthwgt$LowBirthWgt, birthwgt$Smoking, correct = F)
# We can say that low birth weigh and smoking are significantly associated 

############## Test 2: Relationship between birthweight and Maternal Smoking ###

# How does the data look?
boxplot(bweight$Weight ~ bweight$MomSmoke)

# Any missingness?
summary(bweight) # No!

# What about the distribution?

## qq plot
qqnorm(bweight %>% filter(MomSmoke == 1) %>% pull(Weight), pch = 1, frame = FALSE)
qqline(bweight %>% filter(MomSmoke == 1) %>% pull(Weight), col = "steelblue", lwd =1)

qqnorm(bweight %>% filter(MomSmoke == 0) %>% pull(Weight), pch = 1, frame = FALSE)
qqline(bweight %>% filter(MomSmoke == 0) %>% pull(Weight), col = "steelblue", lwd =1)

# Not happy about that QQ plot, let's go non-parametric

wilcox.test(bweight$Weight ~ bweight$MomSmoke)
# Significant difference in birth weight 

# How is the t-test different?

# We assume normality, we assume equal variance 
var.test(bweight$Weight ~ bweight$MomSmoke)

# By default, R will assume unequal variance
t.test(bweight$Weight ~ bweight$MomSmoke, var.equal = F)


########## Hypothesis 2 ########################################################
# Cars and MPG
################################################################################

############## Test 1: Relationship between City MPG and Highway MPG ###

hist(cars$MPG_Highway - cars$MPG_City)

qqnorm(cars$MPG_Highway - cars$MPG_City, pch = 1, frame = FALSE)
qqline(cars$MPG_Highway - cars$MPG_City, col = "steelblue", lwd =1)

wilcox.test(cars$MPG_Highway, cars$MPG_City, paired = T)

# These are the same 
t.test(cars$MPG_Highway, cars$MPG_City, paired = T)
t.test(cars$MPG_Highway - cars$MPG_City)
summary(lm(cars$MPG_Highway - cars$MPG_City ~ 1))


########## Hypothesis 3 ########################################################
# Weight and Height
################################################################################

############## Test 1: Relationship between weight and height ###

# Look at the data 
plot(class$Weight ~ class$Height)

# normal?
hist(class$Weight) 
hist(class$Height)

# Fit a linear model 
LM1 <- lm(class$Weight ~ class$Height)
summary(LM1)

# Check our model
plot(LM1)
hist(LM1$residuals)

# same results
cor.test(class$Weight, class$Height)

# Not sure about the distribution?
cor.test(class$Weight, class$Height, method = "spearman")


