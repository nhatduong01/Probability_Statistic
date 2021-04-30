late_student <-read.csv("C:/Users/ADMIN/Desktop/Learning/Statistics and Probability/Assignment/Ex3.csv", header =  T, 
                      colClasses =c("factor", "numeric"))
# attach the data
attach(late_student)
#Examine the boxplot o f the data
boxplot(Students~Days)
# We are testing the hypothesis
# The null hypothesis H0 : The mean Factor level is the same for all factories
ANOVA1 = aov(Students~Days)
# Summary the information
summary(ANOVA1)

## We want to use multiple comparison to determine which mean
# may different from the others
TukeyHSD(ANOVA1, conf.level = 0.99)
# We draw the map to visualize
plot(TukeyHSD(ANOVA1, conf.level = 0.99),las =1)
