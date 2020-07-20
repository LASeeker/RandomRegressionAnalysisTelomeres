#20170619 print heritability curve with SE


library(ggplot2)

data<-read.csv("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory2/h2andSE.csv")

names(data)

ggplot(data, aes(x=AgeInDays, y=heritability))+ geom_line(size=2, colour= "blue") + 
  xlab("Age in months") + ylab("Heritability")+theme_gray(20)+ylim(0,1)+
  geom_line(data=data, aes(x=AgeInDays, y=Upper), linetype="dotted")+
  geom_line(data=data, aes(x=AgeInDays, y=Lower), linetype="dotted")

#20170619_heritabilityAndSE

max(data$SE)
min(data$SE)

min(data$heritability)
max(data$heritability)


# for age in month < 59
redData<-subset(data, data$AgeInDays<59)

tiff( "C:/Users/lseeker/Documents/PhD/Thesis/01_THESIS_FOLDER/h_CHAPTER4_RRM/PaperSubmissionPlosOne/Revision/Resubmission/Images/Fig3.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)

ggplot(redData, aes(x=AgeInDays, y=heritability))+ geom_line(size=2, colour= "blue") + 
  xlab("Age in months") + ylab("Heritability")+theme_gray(20)+ylim(0,1)+
  geom_line(data=redData, aes(x=AgeInDays, y=Upper), linetype="dotted")+
  geom_line(data=redData, aes(x=AgeInDays, y=Lower), linetype="dotted")

dev.off()

#20170619_heritabilityAndSE

max(redData$SE)
min(redData$SE)

min(redData$heritability)
max(redData$heritability)


