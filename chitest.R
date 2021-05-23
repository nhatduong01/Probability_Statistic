#input the data
my_table = matrix(c(71,430,1072,1609,1178,158,
                    54,324,894,1202,903,112),byrow = T, ncol = 6)
# add the column names
colnames(my_table) = c("0-1", "1-2","2-3","3-4","4-6",">6")
# add the row names
rownames(my_table) = c("40-50","50-60")
# perform Chi square test
chisq.test(my_table, correct = F)
