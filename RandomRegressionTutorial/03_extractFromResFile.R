# 11.3.2017 Luise A. Seeker
# R script for the extraction of data from a .res ASReml output file. Output is written in a 
# data frame and saved as .csv file. It can be used for calculating a fixed curve based
# on a random regression model

library(stringr)

###########CONTROL PANEL############################################################################
workDir<-"C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1" #set working directory


asreml_file<-"Stage1_20170621_RR_tutorial_09_bothQuadratic" #provide ASReml file name without endings

orderFixed<-2 #specify which order polynomial was used for the fixed effect
orderRandom<-2

#check your .res output file and tell the script in which row your header names for the fixed 
#effect are:
FirstLine<-5

#check your .res output file and tell the script in which row the last fixed effect residuals are:
lastLine<-74

#check your .res output file and tell the script in which row your header names for the random 
#effect are:
RandomFirstLine<-77

#check your .res output file and tell the script in which row the last random effect residuals are:
RandomLastLine<-146



####################################################################################################
# builds data name:

res_file<-paste(asreml_file, ".res", sep="")
###################################################################################################
setwd(workDir)
#Extracting infromation from .res file

ResCol<-orderFixed+2 #number of columns in residual file

#read in res file
res <- readLines(res_file)
res #here you can check lines and allocate numbers to "FirstLine", "LastLine" etc.
#remove all empty strings ("\\s+") and replace by "#". str_trim() removes whitespace
#from start and end of string
res_red<-gsub("\\s+", "#", str_trim(res))

#remove all rows that contain not required information:
res_red<-res_red[-(lastLine + 1: length(res_red))]
res_red<-res_red[-(1: FirstLine)]


#split strings as "#"
rsplit<-strsplit(res_red, "#" , fixed=TRUE)


#build data frame with n= ResCol<-orderFixed+2 columns
res_df<- data.frame(matrix(unlist(rsplit), ncol=ResCol, byrow=T), stringsAsFactors=FALSE)

#change header names
NumCol<-(1:ncol(res_df))
NumCol<-NumCol[1:ncol(res_df)-1]
names(res_df)<-c("AGE_M", as.character(NumCol))

output<-paste(asreml_file, "_resDF", ".csv", sep="")
write.csv(res_df, file=output)

print(paste(output, "has been saved in", workDir, sep=" "))


#########################################################################################################
#use the following for your final model. It saves the residuals without row names and column names so 
#you can directly convert it to a matrix for the calculation of the random curve later...
########################################################################################################
#save as matrix without header and age column

resM<-res_df[,-1]

write.table(resM, "C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory2/FinalModel_ResidualMatrix.csv", row.names=FALSE, col.names=FALSE, sep=",")


#################################################################################################
#you only need the following, if you fitted a polynomial of different order to your fixed and 
#random effects
#################################################################################################
#RANDOM RESIDUALS 

randomResCol<-orderRandom+2 #number of columns in residual file

#read in res file
res <- readLines(res_file)

#remove all empty strings ("\\s+") and replace by "#". str_trim() removes whitespace
#from start and end of string
res_red<-gsub("\\s+", "#", str_trim(res))

#remove all rows that contain not required information:
res_red<-res_red[-(RandomLastLine + 1: length(res_red))]
res_red<-res_red[-(1: RandomFirstLine)]


#split strings as "#"
rsplit<-strsplit(res_red, "#" , fixed=TRUE)


#build data frame with 3 columns (change if required)
res_df<- data.frame(matrix(unlist(rsplit), ncol=randomResCol, byrow=T), stringsAsFactors=FALSE)

#change header names
NumCol<-(1:ncol(res_df))
NumCol<-NumCol[1:ncol(res_df)-1]
names(res_df)<-c("AGE_M", as.character(NumCol))

output<-paste(asreml_file, "_resDF_randomEff", ".csv", sep="")
write.csv(res_df, file=output)

print(paste(output, "has been saved in", workDir, sep=" "))

