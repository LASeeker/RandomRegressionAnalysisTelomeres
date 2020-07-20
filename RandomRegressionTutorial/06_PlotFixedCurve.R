# script for plotting fixed curve
#6.6.2017
#LA Seeker

library(ggplot2)

data<-read.csv("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory2/R_input_PlotFixedCurve.csv")
names(data)


SE<-abs(data$SE)
#data$Upper<-data$quadratic.estimates + abs(data$SE)
#data$Lower<-data$quadratic.estimates - abs(data$SE)



Plot1<-ggplot()+
  geom_line(data= data, aes(x=age, y=quadratic.estimates), colour="blue", size=1.5)+
  geom_line(data= data, aes(x=age, y=PhenMean))+
  geom_line(aes(x= data$age, y=data$Upper), linetype="dotted")+ ylim(-0.1, 0.15)+
  geom_line(aes(x= data$age, y=data$Lower), linetype="dotted")+  
  theme_gray(20) + xlab("Age in months") + ylab("log(RLTL)") 

Plot1

max(SE)
min(SE)

redData<-subset(data, data$age<59)


tiff( "C:/Users/lseeker/Documents/PhD/Thesis/01_THESIS_FOLDER/h_CHAPTER4_RRM/PaperSubmissionPlosOne/Revision/Resubmission/Images/Fig1.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)
     

Plot2<-ggplot()+
  geom_line(data= redData, aes(x=age, y=quadratic.estimates), colour="blue", size=1.5)+
  geom_line(data= redData, aes(x=age, y=PhenMean))+
  #geom_line(aes(x= redData$age, y=redData$Upper), linetype="dotted")+
  #geom_line(aes(x= redData$age, y=redData$Lower), linetype="dotted")+  
  theme_gray(20) + xlab("Age in months") + ylab("log(RLTL)") + ylim(-0.1, 0.15)

Plot2


dev.off()

min(abs(redData$SE))
max(abs(redData$SE))




