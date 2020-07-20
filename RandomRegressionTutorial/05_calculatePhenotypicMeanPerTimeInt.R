# calculation of a phenotypic mean within for each age group

#read in data 

data<-read.csv("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1/Stage1_DF_20170307_REDUCED.csv")

names(data)

meanLogTL<-tapply(data$LogTL, data$AGE_M, mean)
meanLogTL

df<-data.frame(AGE_M= levels(as.factor(data$AGE_M)), MeanLogTL= meanLogTL)
df

write.csv(df, "C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory2/PhenotypicMeanPerTimeInterval.csv")
