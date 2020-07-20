########WITHIN TRAIT CORRELATIONS COW DATASET#####
#######RLTL########

library(ggplot2)

setwd("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1") #set working directory
file <- read.csv ("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory2/FinalModel_ResidualMatrix.csv", header = FALSE)#residual data frame 
file2 <- as.matrix(read.csv ("BothQuadratic_VarianceCovarianceM.csv", header=FALSE)) #variance covariance matrix
head (file2)
k<-as.matrix((file2)) #making matrix format
head(k)
o<-(file)
s<-as.matrix (o)
head(o)

sk<-s %*% k

z<-t(s) #transpose
kmatrix<-sk%*%z
head(kmatrix)
oko<-kmatrix
head(oko)


dim(oko)

var <- diag(oko)


corr <- rep(1:(69*69))
dim(corr) <- c(69, 69)

for (i in 1:69 ) {
  for (j in 1:69) {
    corr[i,j] <- oko[i,j]/sqrt(var[i]*var[j])
  }
}
head (corr)


corr1<- corr [,1]
plot(corr1)

SE<-sqrt((1-corr1^2)/ (length(corr1-2)))
SE

min(corr)
max(corr)

write.csv(corr1, file = "GeneticCorr_RLTL_1.csv") #makes .csv file 

####


#data<-read.csv("GeneticCorr_RLTL_1_ForGGPLOT.csv")
#names(data)

#plot2<-ggplot(data, aes(x=AgeInDays, y=Corr))+geom_line(size=2, colour="blue")+ylim(0,1)+
#  xlab("Age in months")+ ylab("Genetic correlation")+theme_gray(20)
#plot2


#####SE


min(SE)
max(SE)


####


data<-read.csv("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1/GeneticCorr_RLTL_1.csv")
names(data)
names(data)<-c("AGE_M", "Corr")

data$SE<-SE
data$Upper<-data$Corr+data$SE
data$Lower<-data$Corr-data$SE

head(data)

plot2<-ggplot(data, aes(x=AGE_M, y=Corr))+geom_line(size=2, colour="blue")+
  xlab("Age in months")+ ylab("Genetic correlation")+theme_gray(20)+ylim(0,1.05)+
  geom_line(aes(y=Upper), linetype="dotted")+
  geom_line(aes(y=Lower), linetype="dotted")

plot2


redData<-subset(data, data$AGE_M<59)

tiff( "C:/Users/lseeker/Documents/PhD/Thesis/01_THESIS_FOLDER/h_CHAPTER4_RRM/PaperSubmissionPlosOne/Revision/Resubmission/Images/Fig4.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)

plot3<-ggplot(redData, aes(x=AGE_M, y=Corr))+geom_line(size=2, colour="blue")+
  xlab("Age in months")+ ylab("Genetic correlation")+theme_gray(20)+ylim(0,1.05)+
  geom_line(aes(y=Upper), linetype="dotted")+
  geom_line(aes(y=Lower), linetype="dotted")

plot3

dev.off()

min(redData$Corr)
max(redData$SE)



#calculate eigenvector

EV<-eigen(k)

E2<-as.matrix(EV$values)

E3<-as.matrix(EV$vectors)

0.228355E-02+3.138642e-03+1.721758e-04+3.743707e-06

0.228355E-02/0.005598112 # % explained by res variance
3.138642e-03/0.005598112 # % intercept
1.721758e-04/0.005598112 # lin
3.743707e-06/0.005598112 #quad

1.721758e-04/0.005598112 + 3.743707e-06/0.005598112

#Eigenfunction

EF<-s %*% E3
EF

DF<-as.data.frame(EF)
head(DF)

names(DF)<-c("intercept", "linear", "quadratic")

TimeInterval<-c(1:3,7:71, 75)

DF$AGE_M<-TimeInterval
head(DF)

plotD<-subset(DF, DF$AGE_M < 59)

eigenFPlot<-ggplot()+
  geom_line(data=plotD, aes(x=AGE_M, y=intercept), size=1.5)+
  geom_line(data=plotD, aes(x=AGE_M, y=linear), size=1.5, linetype="dotted")+
  geom_line(data=plotD, aes(x=AGE_M, y=quadratic), size=1.5, linetype="dashed")+
  xlab("Age in months")+ ylab("")+theme_grey(20)
  
eigenFPlot
