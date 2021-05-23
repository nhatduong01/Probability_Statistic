dulieu <- c (88.5, 98.8, 89.6, 92.2, 92.7, 88.4, 97.5, 90.9, 94.7, 88.3, 90.4, 83.4, 87.9, 92.6, 87.8, 89.9, 84.3, 90.4, 91.6, 
             91.0, 93, 93.7, 88.3, 91.8, 90.1, 91.2, 90.7, 88.2, 94.4, 96.5, 89.2, 89.7, 89, 90.6, 88.6, 88.5, 90.4, 84.3, 
             92.3, 92.2, 89.8, 92.2, 88.3, 93.3, 91.2, 93.2, 88.9, 91.6, 87.7, 94.2, 87.4, 86.7, 88.6, 89.8,
             90.3, 91.1, 85.3, 91.1, 94.2, 88.7, 92.7, 90, 86.7, 90.1, 90.5, 90.8, 92.7, 93.3, 91.5, 93.4, 89.3, 100.3,
             90.1, 89.3, 86.7, 89.9, 96.1, 91.1, 87.6,91.8, 91, 91)
breaks = seq(83,101, by = 2.2)
print(breaks)
dulieu.cut = cut(dulieu, breaks, right = FALSE)
dulieu.frequency = table(dulieu.cut)
print(dulieu.frequency)
hist(dulieu, xlab = "Value", col = "yellow",border = "blue", breaks = 6, xlim = c(83,101))
boxplot(dulieu, ylab = "Ratings", main = "Motor fuel octane",col = "yellow")
dietdata <- read.csv("C:/Users/ADMIN/Desktop/Learning/Statistics and Probability/Assignment/DietWeigthLoss.csv", header =  T,
                     colClasses = c( "numeric","factor"))
print(dietdata)

boxplot(WeightLoss~Diet)
mice_data <- read.csv("C:/Users/ADMIN/Desktop/Learning/Statistics and Probability/Assignment/twoanova.csv", header =  T,
                      colClasses = c( "numeric","numeric","factor", "factor"))
# attach the data
attach(mice_data)
### Data visualization
# draw the histogram
hist(mice_data$activity)
# draw the boxplot for the first factor (predator_density)
boxplot(mice_data$activity~predator_density)
# draw the boxplot for the second factor (timing)
boxplot(mice_data$activity~timing)
### ANOVA testing
model1 <- aov(activity ~ predator_density * timing, data = mice_data)
summary (model1)
### The real Ex3 problem
late_student = read.csv("C:/Users/ADMIN/Desktop/Learning/Statistics and Probability/Assignment/Ex3.csv", header =  T,
                        colClasses = c("factor", "factor", "numeric"))
attach(late_student)
#Data Visulization
# the number of students on different days
boxplot(Students~late_student$Days.of.week)
# the number of studentss on different school
boxplot(Students~late_student$School)
# analyse using ANOVA
result <- aov(Students~Days.of.week*School,data = late_student)
# view the result
anova(result)



y = c(5,4,5,7,4,5,3,2,4,3,4,5,4,4,3,2)
days = factor(rep(c("Monday","Tuesday","Wednesday","Thurday"),each=4))
schools =factor(rep(c("A","B","C","D"),4))
summary(aov(y ~ days + schools))