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
