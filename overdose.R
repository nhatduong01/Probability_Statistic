#import this package to use the pipe operator (%>%) to extract the data
library(dplyr)
# import the raw data
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
# Omit NA data
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
# summary the age of the data
summary(Age)
# draw the histogram of Age
hist(Age, breaks = (0:100))
# Statistics of reason of death
DescriptionofInjury = factor(DescriptionofInjury)
# Get highest death reasons
summary(DescriptionofInjury)
## Get the summary of the substances used

