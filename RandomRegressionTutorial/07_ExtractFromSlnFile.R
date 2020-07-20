# 15.3.17 Luise A. script for extractin solutions for the calculation of random curves:


###########CONTROL PANEL###########################################################################
#look in your .sln file and enter the line in which the last fixed effect solution can be found
#(which is probably "mu")
LastFixed<-47

library(ggplot2)
library(stringr)


#read in input file

working_dir<-"C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1"
asreml_file<-"Stage1_20170621_RR_tutorial_09_bothQuadratic"       # name of the ASReml output file without any extensions

setwd(working_dir)


sol_file<-paste(asreml_file, ".sln", sep="")

solutions<-readLines(sol_file)
solutions <- sub('\\s+', '', solutions, perl=TRUE)  # remove leading spaces

#we are interested in the random effect solutions now. Therefore we are now removing the fixed effect
# solutions
solutions<-solutions[-(1:LastFixed)] 


#remove all empty strings ("\\s+") and replace by "#". str_trim() removes whitespace
#from start and end of string
solutions<-gsub("\\s+", "#", str_trim(solutions))

#split strings as "#"
solsplit<-strsplit(solutions, "#" , fixed=TRUE)

#build data frame with 4 columns (change if required)
sol_df<- data.frame(matrix(unlist(solsplit), ncol=4, byrow=T), stringsAsFactors=FALSE)

names(sol_df)<-c("Animal", "ID", "Effect", "SE_Effect" )

PolOrd<-as.data.frame(substr(sol_df$ID, 1, 1))
names(PolOrd)<-"PolyOrderPlusOne"
head(PolOrd)               

df<-data.frame(names=sol_df$ID,chr=apply(sol_df,2,nchar)[,2])

solDF<-cbind(sol_df, df)

AnID<-as.data.frame(substr(solDF$ID, 3, solDF$chr))
names(AnID)<-"ANIMAL_ID"
AnID[1:100,]

sol_DF<-cbind(solDF, PolOrd, AnID)
head(sol_DF)

#read in data file to filter for the sol_DF for animals for which we have measurements

data<-read.csv("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1/Stage1_DF_20170307_REDUCED.csv")

names(data)

animals<-data.frame(ANIMAL_ID= data$ANIMAL_ID)

SOLDF<-merge(sol_DF, animals, by="ANIMAL_ID", all=FALSE)
head(SOLDF)
nrow(SOLDF)

SOLDF<-SOLDF[!duplicated(SOLDF), ]
nrow(SOLDF) # should equal the number of individuals * the polynomial order plus 1 (for the intercept)


write.csv(SOLDF, paste(asreml_file, "RandomSol.csv", sep= "_"))
#this is saved in workingDirectory1



