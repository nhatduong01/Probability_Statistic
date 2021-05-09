# Input the data
y = c(5,4,5,7,4,5,3,2,4,3,4,5,4,4,3,2)
# classify the data to days of week
days = factor(rep(c("Monday","Tuesday","Wednesday","Thurday"),each=4))
# classify the data into school
schools =factor(rep(c("A","B","C","D"),4))
## Data visualization
# draw the boxplot relative to schools
boxplot (y~schools)
# draw the boxplot relative to days of week
boxplot(y~days)
## Using anovo to analyze
result = aov(y ~ days + schools)
# display the result
summary(result)
# compare the result using Tukey test
TukeyHSD(result, conf.level = 0.99)
# plot the difference
plot(TukeyHSD(result, conf.level = 0.99),las =1)
