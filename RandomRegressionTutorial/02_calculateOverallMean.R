# 11.3.2017 Luise A. Seeker
#R script for the caculation of the overall mean which is needed for generating a fixed curve
# based on random regression models

library(stringr)

###########CONTROL PANEL############################################################################
setwd("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1") #set working directory

asreml_file<-"Stage1_20170621_RR_tutorial_07_quartic" #provide ASReml file name without endings

#prepare a relative number data frame, save it as .csv and provide file path and name here:
relativeNumberData<-read.csv("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory2/20170606_NuOfMeasPerFixedEffectLevel.csv")

###################################################################################################

#builds file name:
sol_file<-paste(asreml_file, ".sln", sep="")


###################################################################################################
## extract information from solution file and build data frame

#read in solution file:
solutions <- readLines(sol_file)                

#remove all underscores and replace by empty string. 
solutions_rem<-str_replace_all(solutions, "_", "")

#remove all empty strings ("\\s+") and replace by "#". str_trim() removes whitespace
#from start and end of string
solutions_trimmed<-gsub("\\s+", "#", str_trim(solutions_rem))

#split strings as "#"
solutions_split<-strsplit(solutions_trimmed, "#" , fixed=TRUE)

#build data frame with 4 columns (change if required)
solutions_df<- data.frame(matrix(unlist(solutions_split), ncol=4, byrow=T), stringsAsFactors=FALSE)

#change header names
names(solutions_df)<-solutions_df[1, ]

#remove first line (because it is duplicated at the moment)
solutions_df<-solutions_df[-1,]
########################################################################################
########################################################################################
#######################################################################################
#### calculating effect mean using solutions_df (created from .sln file)
#### and the relative number data frame (relativeNumberData)

wdf<-subset(solutions_df, solutions_df$Effect != "0.000") # removes rows with effect size = 0

#mdf<-merge(wdf, relativeNumberData, by=c("ModelTerm", "Level"), all=T)
#mdf
#check, if merging worked!

mdf<-merge(wdf, relativeNumberData, by=c("ModelTerm", "Level"))
mdf 

mdf$weightedEff<-as.numeric(paste(mdf$Effect))*mdf$RelativeN

mdf

MEAN<-sum(mdf$weightedEff)
print(paste("The total mean is", MEAN, sep=" "))

#write.csv(mdf, "test.csv")


