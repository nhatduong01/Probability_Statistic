#import this package to use the pipe operator (%>%) to extract the data
library(dplyr)
# import the raw data
------------------------------
raw_data = read.csv("C:/Users/ADMIN/Desktop/Learning/Statistics and Probability/Assignment/overdose.csv")
# choosing variable and data cleaning
net_data = raw_data %>% select ("Age",
                                "Sex",
                                "DescriptionofInjury",
                                "Heroin",
                                "Cocaine",
                                "Fentanyl",
                                "FentanylAnalogue",
                                "Oxycodone",
                                "Oxymorphone",
                                "Ethanol",
                                "Hydrocodone",
                                "Benzodiazepine",
                                "Methadone",
                                "Amphet",
                                "Tramad",
                                "Morphine_NotHeroin",
                                "Hydromorphone")
# Data cleaning
------------------------------------
net_data = na.omit(net_data)
attach(net_data)
#Convert 2-option data into 0 and 1
net_data$Sex <- ifelse(net_data$Sex == "Female", 1, 0) # Female is 1 Male is 0
# We converse Yes to 1 and No to 0
net_data$Heroin <- ifelse(net_data$Heroin == "Y", 1, 0)
net_data$Cocaine <- ifelse(net_data$Cocaine == "Y", 1, 0)
net_data$Fentanyl <- ifelse(net_data$Fentanyl == "Y", 1, 0)
net_data$FentanylAnalogue <- ifelse(net_data$FentanylAnalogue == "Y", 1, 0)
net_data$Oxycodone <- ifelse(net_data$Oxycodone == "Y", 1, 0)
net_data$Oxymorphone <- ifelse(net_data$Oxymorphone == "Y", 1, 0)
net_data$Ethanol <- ifelse(net_data$Ethanol == "Y", 1, 0)
net_data$Hydrocodone <- ifelse(net_data$Hydrocodone == "Y", 1, 0)
net_data$Benzodiazepine <- ifelse(net_data$Benzodiazepine == "Y", 1, 0)
net_data$Methadone <- ifelse(net_data$Methadone == "Y", 1, 0)
net_data$Amphet <- ifelse(net_data$Amphet == "Y", 1, 0)
net_data$Tramad <- ifelse(net_data$Tramad == "Y", 1, 0)
net_data$Morphine_NotHeroin <- ifelse(net_data$Morphine_NotHeroin == "Y", 1, 0)
net_data$Hydromorphone <- ifelse(net_data$Hydromorphone == "Y", 1, 0)
# Turn these variables to categorical variables
net_data$Sex = factor(net_data$Sex)
net_data$DescriptionofInjury = factor(net_data$DescriptionofInjury)
net_data$Heroin = factor(net_data$Heroin)
net_data$Cocaine = factor(net_data$Cocaine)
net_data$Fentanyl = factor(net_data$Fentanyl)
net_data$FentanylAnalogue = factor(net_data$FentanylAnalogue)
net_data$Oxycodone = factor(net_data$Oxycodone)
net_data$Oxymorphone = factor(net_data$Oxymorphone)
net_data$Ethanol = factor(net_data$Ethanol)
net_data$Hydrocodone = factor(net_data$Hydrocodone)
net_data$Benzodiazepine = factor(net_data$Benzodiazepine)
net_data$Methadone = factor(net_data$Methadone)
net_data$Amphet = factor(net_data$Amphet)
net_data$Tramad = factor(net_data$Tramad)
net_data$Morphine_NotHeroin = factor(net_data$Morphine_NotHeroin)
net_data$Hydromorphone = factor(net_data$Hydromorphone)
# Data visualization
-----------------------------------------------------
# summary the age of the data
summary(Age)
# draw the histogram of Age
hist(Age, breaks = (0:100))
# draw the boxplot of Age
boxplot(net_data$Age)
# Statistics of reason of death
DescriptionofInjury = factor(DescriptionofInjury)
# Get highest death reasons
summary(DescriptionofInjury)
# Get the Sex summary
summary(net_data$Sex)
## Get the summary of the substances used
rHerorin = data.frame((prop.table(summary(net_data$Heroin))))
rCocaine = data.frame((prop.table(summary(net_data$Cocaine))))
rFentanyl = data.frame((prop.table(summary(net_data$Fentanyl))))
rFentanylAnalogue = data.frame((prop.table(summary(net_data$FentanylAnalogue))))
rOxycodone = data.frame((prop.table(summary(net_data$Oxycodone))))
rOxymorphone = data.frame((prop.table(summary(net_data$Oxymorphone))))
rEthanol = data.frame((prop.table(summary(net_data$Ethanol))))
rHydrocodone = data.frame((prop.table(summary(net_data$Hydrocodone))))
rBenzodiazepine = data.frame((prop.table(summary(net_data$Benzodiazepine))))
rMethadone = data.frame((prop.table(summary(net_data$Methadone))))
rAmphet = data.frame((prop.table(summary(net_data$Amphet))))
rTramad = data.frame((prop.table(summary(net_data$Tramad))))
rMorphine_NotHeroin = data.frame((prop.table(summary(net_data$Morphine_NotHeroin))))
rHydromorphone = data.frame((prop.table(summary(net_data$Hydromorphone))))
result = data.frame(Name = c("Heroin","Cocaine","Fentanyl", "FentanylAnalogue", "Oxycodone",
                             "Oxymorphone", "Ethanol", "Hydrocodone", "Benzodiazepine",
                             "Methadone", "Amphet","Tramad", "Morphine_NotHeroin","Hydromorphone"), 
                    Yes = c(rHerorin[2,1],rCocaine[2,1],rFentanyl[2,1],rFentanylAnalogue[2,1],rOxycodone[2,1],
                            rOxymorphone[2,1],rEthanol[2,1],rHydrocodone[2,1],rBenzodiazepine[2,1],rMethadone[2,1],
                            rAmphet[2,1],rTramad[2,1],rMorphine_NotHeroin[2,1],rHydromorphone[2,1]),
                    No = c(rHerorin[1,1],rCocaine[1,1],rFentanyl[1,1],rFentanylAnalogue[1,1],rOxycodone[1,1],
                            rOxymorphone[1,1],rEthanol[1,1],rHydrocodone[1,1],rBenzodiazepine[1,1],rMethadone[1,1],
                           rAmphet[1,1],rTramad[1,1],rMorphine_NotHeroin[1,1],rHydromorphone[1,1]))
# print the statistics
print(result)
# Get the reasons why women and man die
Women_reasons <- c()
Men_reasons <- c()
# converse to char
net_data$DescriptionofInjury = as.character(net_data$DescriptionofInjury)
for(i in 1:nrow(net_data))
{
  if(net_data[i,2]==0)
    Men_reasons = append(Men_reasons,net_data[i,3])
  if(net_data[i,2]==1)
    Women_reasons = append(Women_reasons, net_data[i,3])
}
# get the reasons why women die most
summary(as.factor(Women_reasons))
# get the reasons why men die most
summary(as.factor(Men_reasons))
