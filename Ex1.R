blood_data <-read.csv("C:/Users/ADMIN/Desktop/Learning/Statistics and Probability/Assignment/Ex1.csv", header =  T, 
                      colClasses =c("factor", "numeric"))
# attach the data
attach(blood_data)
#Examine the boxplot o f the data
boxplot(FactorLevel~Factory)
# We are testing the hypothesis
# The null hypothesis H0 : The mean Factor level is the same for all factories
ANOVA1 = aov(FactorLevel~Factory)
# Summary the information
summary(ANOVA1)

## We want to use multiple comparison to determine which mean
# may different from the others
TukeyHSD(ANOVA1, conf.level = 0.97)
# We draw the map to visualize
plot(TukeyHSD(ANOVA1, conf.level = 0.97),las =1)