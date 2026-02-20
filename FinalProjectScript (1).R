
library(readxl)
tabledata <- read_excel("Cereal_Data.xlsx")
NROW(Cereal_Data)

library(psych)
Product <- tabledata$Calories +tabledata$Sodium+tabledata$Fiber+tabledata$Carbs+tabledata$Sugars
#CALORIES
#MEAN IS 105.52
#MEDIAN IS 110
#MODE IS 110
#STANDARD DEVIATION 18.77

#SUGAR
#MEAN IS 6.95
#MEDIAN  IS 6
#MODE 3
#STANDARD DEVIATION IS 4.4

# Mean of Calories 
result.mean <- mean(tabledata$Calories)
print(result.mean)

# Standard Deviation of Calories 
sd(tabledata$Calories)

#Variance of Calories 
var(tabledata$Calories)

# Range of Calories 
range(tabledata$Calories)

# Median of Calories 
median(tabledata$Calories)

#Squaring the SD of Calories 
myvar <- sd(tabledata$Calories)^2
myvar
result.mean <- mean(tabledata$Sugars)
print(result.mean)

# Standard Deviation of Sugars
sd(tabledata$Sugars)

#Variance of Sugars 
var(tabledata$Sugars)

#Range of Sugars
range(tabledata$Sugars)

#Median of Sugars
median(tabledata$Sugars)

#Standard Deviation of Sugars 
myvar <- sd(tabledata$Sugars)^2
myvar

# Assign the values from the Units column to new data table called units
Calories <- tabledata$Calories

# display the data in column format
transform(Calories)

# show the frequency of each value
# one approach for finding the Mode of the data
table(Calories)

# Summary stats min, 1Q Median, Mean, 3Q, Max of Calories 
summary(Calories)

# Find Mean Of Calories 
result.mean <- mean(Calories)
print(result.mean)

# Find Median of Calories  
median(Calories, na.rm = FALSE)

# Create a function called getmode for calculating a Mode.
getmode <- function(Calories) {
  uniqs <- unique(Calories)
  uniqs[which.max(tabulate(match(Calories, uniqs)))]
}

# Calculate the mode of Calories 
result <- getmode(Calories)
print(result)

# Alternatively you can use the Summary function to get summary stats except mode
# this displays the results horizontal
summary(Calories)

# Alternatively you can use the describe function to get more summary stats except mode
describe(Calories)

# display the stats vertically
df <- describe(Calories)
t(df)

# Assign the values from the Units column to new data table called units
Sugars <- tabledata$Sugars

# display the data in column format
transform(Sugars)

# show the frequency of each value
# one approach for finding the Mode of Sugars
table(Sugars)

# Summary stats min, 1Q Median, Mean, 3Q, Max of Sugars 
summary(Sugars)

# Find Mean individually of Sugars 
result.mean <- mean(Sugars)
print(result.mean)

# Find Median of Sugars 
median(Sugars, na.rm = FALSE)

# Create a function called getmode for calculating a Mode for Sugars
getmode <- function(Sugars) {
  uniqs <- unique(Sugars)
  uniqs[which.max(tabulate(match(Sugars, uniqs)))]
}

# Calculate the mode using the user function.
result <- getmode(Sugars)
print(result)

# Alternatively you can use the Summary function to get summary stats except mode
# this displays the results horizontal
summary(Sugars)

# Alternatively you can use the describe function to get more summary stats except mode
describe(Sugars)

# display the stats vertically for Sugars
df <- describe(Sugars)
t(df)



source("cfdv.r")
cfdvalues <- cfdv(tabledata$Sugars)

cfdvalues[1]
cfdvalues[2]
round(cfdvalues[3])

### Assign the values from the Units column to new data table called units
Sugars <- tabledata$Sugars

### display the data of Sugars 
transform(Sugars)

## show the frequency of each value of Sugar
table(Sugars)

hist( Sugars) -> Sugars # do a histogram of y and assign its info to h
h$counts <- cumsum(h$Sugars) # replace the cell freq.s by cumulative freq.s
plot( Sugars ) # plot a cumulative histogram of y

source("cfdv.r")
cfdvalues <- cfdv(tabledata$Calories)

cfdvalues[1]
cfdvalues[2]
round(cfdvalues[3])

### Assign the values from the Units column to new data table called units
Calories <- tabledata$Calories

### display the data of Calories 
transform(Calories)

## show the frequency of Calories
table(Calories)

hist( Calories) -> Calories # do a histogram of y and assign its info to h
h$counts <- cumsum(h$Calories) # replace the cell freq.s by cumulative freq.s
plot( Calories ) # plot a cumulative histogram of y
#The shape of the Calories Histogram is unimodal
#The plots tells me that the normality of the Calories Histogram is kinda there because of the one particluar peak in the middle of the data
#The shape of the Sugars Histogram is Multimodal
#The plots tells us about the sugar histogram is that theres very little normality because of the mulitple peaks
#For calories its from 100-120 
#For Sugars its form about 0-6 

# Rows read
NROW(tabledata$Calories) # show how many rows were read
# Vertical Box Plot
boxplot(tabledata$Calories,data=tabledata, main="Calories In cereal", 
        xlab="", ylab="Calories in Cereal")

boxplot(tabledata$Calories,data=tabledata, main="Calories in Cereal", 
        xlab="Calories", ylab="", horizontal = TRUE)
text(x = boxplot.stats(tabledata$Calories)$stats, labels = boxplot.stats(tabledata$Calories)$stats, y = 1.30)

#The median for calories is 110
#There is outliers in the Calories Box Plot 

NROW(tabledata$Sugars) # show how many rows were read
# Vertical Box Plot
boxplot(tabledata$Sugars,data=tabledata, main="Sugars In cereal", 
        xlab="", ylab="Calories in Cereal")

boxplot(tabledata$Sugars,data=tabledata, main="Sugars in Cereal", 
        xlab="Sugars", ylab="", horizontal = TRUE)
text(x = boxplot.stats(tabledata$Sugars)$stats, labels = boxplot.stats(tabledata$Sugars)$stats, y = 1.30)

#The median for Sugars is 6
#There is no outliers in the Sugars Box Plot

# Rows read
NROW(tabledata$Calories)
# Scatter Plot of Calories vs. Carbs
plot(tabledata$Calories, tabledata$Carbs, xlab="Calories", ylab="Carbs", main="Scatter Plot of Calories vs. Carbs", xlim=c(1,90), ylim=c(1,3500), pch=2, cex.main=1.5, frame.plot=FALSE , col="blue")

cor.test(tabledata$Calories, tabledata$Carbs, alternative = "greater", method = "pearson", conf.level = 0.95)

library("ggplot2")

ggplot(tabledata,aes(x=Calories, y=Carbs)) +
  geom_point()+
  geom_smooth(method=lm)



#There is a weak positive correlation between Calories and Carbs

n <- nrow(tabledata)

# Rows read
NROW(tabledata$Product) 
#
#
#
anova.aov = lm(tabledata$Calories ~ + tabledata$Sodium + tabledata$Fiber + tabledata$Carbs +tabledata$Sugars  , data = tabledata)
aovobj<-summary(anova.aov)
summary(anova.aov)

fstatistic = aovobj$fstatistic[1]
Rsquared = aovobj$r.squared
fstatistic
Rsquared

df2data <- df.residual(anova.aov)
df1data <- summary(anova.aov)$df[1] - 1
df1data
df2data
fcritical <- qf(.95, df1=df1data, df2=df2data) 
fcritical
cat("F-Statistic is: ",aovobj$fstatistic[1],"F Critical is: ", fcritical, "\n")
cat("R-Squared is: ",Rsquared, "\n")
library(dplyr)
if_else(fstatistic > fcritical, "Reject The Null Hypothesis that coefficients are 0 predictors", "Do Not Reject The Null Hypothesis")

NROW(tabledata$Calories)
step.model <- step(lm(tabledata$Calories~tabledata$Sodium + tabledata$Fiber + tabledata$Carbs + tabledata$Sugars,data=tabledata),direction="both")
summary(step.model)
fstat <- summary(step.model)
fstat$fstatistic[1]
#The linear equation is 21.89 + .02434x +.65917x + 3.43x + 3.94x
#The stepwise removed fiber out of the data because of it's high p value
#I chose the stepwise model because it made sure that the p value is not surpassing a certain threshold. 