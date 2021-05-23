# Read the given csv file
data <- read.csv("C:/Users/ADMIN/Desktop/Learning/Statistics and Probability/Assignment/grade.csv",)
# Extract the necessary data and store it to a new variable
new_DF = data.frame(data$G1,data$G2,data$G3, data$studytime,data$failures, data$absences, data$higher, data$age)
# Omit the missing data
new_DF = na.omit(new_DF)
# Change the data type of each column
# The below data are classify as numeric
new_DF$data.G1 = as.integer(new_DF$data.G1 )
new_DF$data.G2 = as.integer(new_DF$data.G2 )
new_DF$data.G3 = as.integer(new_DF$data.G3 )
new_DF$data.studytime = as.integer(new_DF$data.studytime)
new_DF$data.failures = as.integer(new_DF$data.failures)
new_DF$data.absences = as.integer(new_DF$data.absences)
new_DF$data.age = as.integer(new_DF$data.age)
# This is a factor data type
new_DF$data.higher = factor(new_DF$data.higher)
# attach the data
attach(new_DF)
# calculate the mean of each row
data_mean = apply(new_DF[,-c(7)],2, mean)
# calculate the median of each row
data_median  = apply(new_DF[,-c(7)],2 , median)
# calculate the SD of each row
data_sd = apply(new_DF[,-c(7)], 2, sd)
# get the min of each row
data_min = apply(new_DF[,-c(7)], 2, min)
# get the max of each row
data_max = apply(new_DF[,-c(7)], 2, max)
# merge the array into a list
x<-list(mean = data_mean, median = data_median,min = data_min, max = data_max)
# Convert the list to data.frame 
continuous_variable<- as.data.frame(x)
# classify if student want to be higher or not
classified_variable <- as.data.frame(table(new_DF$data.higher))

#Draw Histogram of G3
# Because G3 distribute from 0 to 20
# We draw histogram with 21 cells
hist(new_DF$data.G3, breaks = (0:20))

# Drawing box plot of G3 based on studytime

boxplot(new_DF$data.G3~new_DF$data.studytime, xlab = "Number of study hours",
        ylab = "G3 score", main = "G3 data based on studytime")
# drawing boxplot of G3 base of failures

boxplot(new_DF$data.G3~new_DF$data.failures, xlab = "Number of failures",
        ylab = "G3 score", main = "G3 data based on failures")
# drawing boxplot of G3 based on the student want to higher or not
boxplot(new_DF$data.G3~new_DF$data.higher, xlab = "Students want to improve their scores or not",
        ylab = "G3 score", main = "G3 data based on students' attitudes")
# plot scatterplot between G3 and G2
pairs(~new_DF$data.G3 + new_DF$data.G2, col = "blue", 
      labels = c ("G3", "G2"),
      main = "Scatter plot of G3 and G2")
# plot scatterplot between G3 and G1
pairs(~new_DF$data.G3 + new_DF$data.G1, col = "blue", 
      labels = c ("G3", "G1"),
      main = "Scatter plot of G3 and G1")
# plot scatterplot between G3 and age
pairs(~new_DF$data.G3 + new_DF$data.age, col = "blue", 
      labels = c ("G3", "age"),
      main = "Scatter plot of G3 and age")

### Question 4
# Using Linear Regression to analyze G3 according to other variables
linear_reg = lm (formula = new_DF$data.G3~data.G1 + data.G2 + data.studytime+ data.failures
                 + data.absences + data.age + data.higher, data = new_DF)
# get the information
summary(linear_reg)
# Question c
# M1 depends on all the variables
M1 = lm (formula = new_DF$data.G3~data.G1 + data.G2 +data.studytime+ data.failures
         + data.absences + data.age + data.higher, data = new_DF)
# M2 does not depend on higher
M2 = lm (formula =new_DF$data.G3 ~data.G1 + data.G2 + data.studytime+ data.failures
         + data.absences + data.age, data = new_DF)
# M3 does not depend on failure and higher
M3 = lm (formula =new_DF$data.G3 ~data.G1 + data.G2 + data.studytime
         + data.absences + data.age, data = new_DF)
# Using ANOVA to determine which model is the best
# Do not confuse this anova() to aov() which performs ANOVA test
compare = anova(M1,M2,M3, test = 'F')
# since the p value are while high
# we conclude that there is not significant difference between these 3 models
# so we choose the one with least parameters because it is cheaper in reality
# so we choose M3
# we see that with a specific value of fitted values, the residuals is around zero
# it proves that the linear model M3 is quite accurate
plot(M3$fitted.values, M3$residuals)

### Question 5 : prediction
# compare each entry with 10
percentage = new_DF$data.G3 >= 10
# organize into a table
my_table = table(percentage)
# Store the percentage into the "evaluate" variable
evaluate = prop.table(my_table)
evaluate = data.frame(evaluate)
# extract the necessary independent value in M3
new_X = data.frame(new_DF$data.G1,new_DF$data.G2, new_DF$data.studytime, new_DF$data.absences, new_DF$data.age)
## generate the value of predicted G3 by the model G3
pred_G3 = predict(M3,new_X)
# compare each entry with 10
percentage2 = pred_G3 >= 10
# organize into a table
my_table2 = table(percentage2)
# predict evaluation
evaluate2 = prop.table(my_table2)
evaluate2 = data.frame(evaluate2)
# Combined into the final result
final_result = data.frame(Status = c ("Observed", "Predicted"),
                          Passed = c(evaluate[2,2], evaluate2[2,2]), 
                          Failed = c(evaluate[1,2], evaluate2[1,2]))
# Show the result generated by our model M3
print(final_result)
