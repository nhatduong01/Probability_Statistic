## doing multiple linear regression with Lung capacity data
lung_capacity <- read.csv("C:/Users/ADMIN/Desktop/Learning/Statistics and Probability/Assignment/LungCapData.csv",
                          colClasses = c("numeric","numeric","numeric","factor","factor","factor"), header = T)
# attach the data
attach(lung_capacity)
result = anova(formula = lung_capacity~lung_capacity$Age + lung_capacity$Height,
               data = lung_capacity)
