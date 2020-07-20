#13.03.2017 L A Seeker
#script for calculating random curves
library(ggplot2)
library(kmlShape)
library(reshape)
library(survival)
library(survminer)
###########################CONTROL PANEL###########################################################
###################################################################################################

TimePoints<-70  #enter for how many timepoints (days, months, years) you have measurements (= rows
# in .res file for fixed or random effects)

TimeInterval<-c(1:3,7:71, 75,76) #enter for which time points you have measurements. For example c(0:20)
# in this case it's for months 0-2, 6-70 and 74 and 75)

NumberOfInd<-308 #enter number of individuals (levels of your random effect)

#enter file path and file name of your .sln and .res file:
sol<-read.csv("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1/Stage1_20170621_RR_tutorial_09_bothQuadratic_RandomSol.csv")
res<-read.csv("C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1/Stage1_20170621_RR_tutorial_09_bothQuadratic_resDF.csv")
####################################################################################################
resdf<-res[-(1)] #I created my .res file with R which added a column as first column of the data frame
#which counts the rows. I don't need this column and therefore remove it here. 

names(resdf)<-c("AGE_M", "intercept", "linear", "quadratic") #if you fitted a higher order polynomial, add more 
#column names here


soldf<-sol[, (colnames(sol) %in% c("ANIMAL_ID", "PolyOrderPlusOne", "Effect","SE_Effect"))]
solInt<-subset(soldf, soldf$PolyOrderPlusOne==1)
solLin<-subset(soldf, soldf$PolyOrderPlusOne==2)
solQuad<-subset(soldf, soldf$PolyOrderPlusOne==3)

solDF<-merge(solInt, solLin, by = "ANIMAL_ID")
names(solDF)
names(solDF)<-c("ANIMAL_ID", "InterceptEffect", "InterceptSE", "x" ,  "LinearEffect", "LinearSE", "y")
#if you fitted a higher order polynomial, add more column names here
solDF<-merge(solDF, solQuad, by="ANIMAL_ID" )
names(solDF)
names(solDF)<-c("ANIMAL_ID", "InterceptEffect", "InterceptSE", "x" ,  "LinearEffect", "LinearSE", "y", 
                "QuadraticEffect", "QuadraticSE", "z")
solDF<-solDF[, (colnames(solDF) %in% c("ANIMAL_ID", "InterceptEffect", "InterceptSE", "LinearEffect",
                                       "LinearSE","QuadraticEffect", "QuadraticSE"))] 
#if you fitted a higher order polynomial, add more column names here
head(solDF)

solDF$AGE_M<-"NULL"

solDF<-solDF[rep(seq_len(nrow(solDF)), each=TimePoints),]
nrow(solDF) # should equal number of animals * number of time intervals

solDF$AGE_M<-rep(TimeInterval,NumberOfInd)

head(solDF)

#write.csv(solDF, "C:/Users/lseeker/Documents/PhD/Stage1_Experiment/09_RandomRegression/RandomCurvCalc/solForMerge.csv")

mergedDF<-merge(solDF, resdf, by="AGE_M", all.y=TRUE)

head(mergedDF)
mergedDF<-mergedDF[order(mergedDF$ANIMAL_ID, mergedDF$AGE_M),]

head(mergedDF)

mergedDF$InterceptProd<-mergedDF$InterceptEffect * mergedDF$intercept
mergedDF$InterceptProdSE<-mergedDF$InterceptSE * mergedDF$intercept
mergedDF$LinProd<-mergedDF$LinearEffect * mergedDF$linear
mergedDF$LinProdSE<-mergedDF$LinearSE * mergedDF$linear
mergedDF$QuadProd<-mergedDF$QuadraticEffect * mergedDF$quadratic
mergedDF$QuadProdSE<-mergedDF$QuadraticSE * mergedDF$quadratic

mergedDF$Sum<-mergedDF$InterceptProd+mergedDF$LinProd +mergedDF$QuadProd #this is the estimate you are after!
mergedDF$SumSE<-mergedDF$InterceptProdSE+mergedDF$LinProdSE +mergedDF$QuadProdSE # this is its standard error

head(mergedDF)

write.csv(mergedDF, "C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory2/RandomCurveEstimates.csv")

#OK, now you have your random curve estimates and the associated SE. Now you probably would like to plot them:

plot1<-ggplot(mergedDF, aes(x=AGE_M, y=Sum, group=ANIMAL_ID))+geom_line() + xlab("Age in months")+
  theme_gray(20)+ ylab("log(RLTL)")
plot1


# You will probably notice (depending on the number of animals in your study) that it is difficult to see 
# anything in this graph exept for that different animals seem to have different trajectories

# you could subgroup animals and plot each individual seperately like this:

plotData<-subset(mergedDF, mergedDF$ANIMAL_ID == c("010000001155984011F", "010000001157817711F",
                                                   "010000001159336211F"))
plotData2<-subset(plotData, plotData$AGE_M<60)
plotData2$upper<-plotData2$Sum+plotData2$SumSE
plotData2$lower<-plotData2$Sum-plotData2$SumSE

ggplot(plotData2, aes(x=AGE_M, y=Sum, group=ANIMAL_ID, colour=ANIMAL_ID))+geom_line(size=1.5)+ 
  xlab("Age in months")+ ylab("log(RLTL)")+theme_gray(20)+theme(legend.position="none")

tiff( "C:/Users/lseeker/Documents/PhD/Thesis/01_THESIS_FOLDER/h_CHAPTER4_RRM/PaperSubmissionPlosOne/Revision/Resubmission/Images/Fig2.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)

ggplot(plotData2, aes(x=AGE_M, y=Sum, group=ANIMAL_ID))+geom_line(size=1.3)+ 
  xlab("Age in months")+ ylab("log(RLTL)")+theme_gray(20)+theme(legend.position="none")

SEplot<-ggplot()+
  geom_line(data=plotData2, aes(x=AGE_M, y=Sum, group=ANIMAL_ID), size=1.3, colour="blue")+
  geom_line(data=plotData2, aes(x=AGE_M, y=upper, group=ANIMAL_ID), linetype="dotted")+
  geom_line(data=plotData2, aes(x=AGE_M, y=lower, group=ANIMAL_ID), linetype="dotted")+
  xlab("Age in months")+ ylab("log(RLTL)")+theme_gray(20)+theme(legend.position="none")
SEplot

dev.off()

group1<-mergedDF[1:966, ]

group2<-mergedDF[967:(2*966),]

plotGR1<-ggplot(group1, aes(x=(group1$AGE_M)-1, y=Sum, group=group1$ANIMAL_ID))+geom_line() +facet_wrap(~ANIMAL_ID)
plotGR1

plotGR2<-ggplot(group2, aes(x=AGE_M, y=Sum, group=ANIMAL_ID))+geom_line() +facet_wrap(~ANIMAL_ID)
plotGR2

#plotGR3<-ggplot(group3, aes(x=AGE_M, y=Sum, group=ANIMAL_ID))+geom_line() +facet_wrap(~ANIMAL_ID)
#plotGR3

#plotGR4<-ggplot(group4, aes(x=AGE_M, y=Sum, group=ANIMAL_ID))+geom_line() +facet_wrap(~ANIMAL_ID)
#plotGR4

#plotGR5<-ggplot(group5, aes(x=AGE_M, y=Sum, group=ANIMAL_ID))+geom_line() +facet_wrap(~ANIMAL_ID)
#plotGR5

#plotGR6<-ggplot(group6, aes(x=AGE_M, y=Sum, group=ANIMAL_ID))+geom_line() +facet_wrap(~ANIMAL_ID)
#plotGR6

#plotGR7<-ggplot(group7, aes(x=AGE_M, y=Sum, group=ANIMAL_ID))+geom_line() +facet_wrap(~ANIMAL_ID)
#plotGR7


############ Or you could try to cluster curves. I used the package kmlShape for this:
# create and plot clusters using kmlShape
head(mergedDF)

plotData3<-subset(mergedDF, mergedDF$AGE_M<60)
PlotDF<-data.frame(Age=plotData3$AGE_M, ANIMAL_ID=plotData3$ANIMAL_ID, RLTL=plotData3$Sum)
wideD<-reshape(PlotDF, idvar = "ANIMAL_ID", timevar = "Age", direction = "wide")

myClds <- cldsWide(data.frame(1:308,wideD))
plot(myClds)

par(ask=FALSE)
kmlShape(myClds,5) #here you can specify the number of clusters

plotTraj(myClds)

tiff( "C:/Users/lseeker/Documents/PhD/Thesis/01_THESIS_FOLDER/h_CHAPTER4_RRM/PaperSubmissionPlosOne/Revision/Resubmission/Images/Fig5.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)

plot(myClds)

dev.off()

clu<-as.data.frame((myClds["clusters"]))
clu

cluData<-cbind(wideD, clu )
names(cluData)

colnames(cluData)[ncol(cluData)] <- "cluster"

clusterData<-data.frame(ANIMAL_ID=cluData$ANIMAL_ID, cluster= cluData$cluster)

head(clusterData)

summary(clusterData$cluster)

crossLdata<-read.csv("C:/Users/lseeker/Documents/PhD/Stage1_Experiment/09_RandomRegression/May2017/ID_crossLink.csv")

mData<-merge(clusterData, crossLdata, by = "ANIMAL_ID", all=FALSE)
head(mData)
nrow(mData)
mData<-mData[!duplicated(mData$EAR_TAG),]
nrow(mData)

HerdLifeData<-read.csv("C:/Users/lseeker/Documents/PhD/Stage1_Experiment/07_Analysis/06_SurvivalPlot/20170807_Stage1_SurvPlotinput_MaxRecAge.csv")
hlDF<-data.frame(EAR_TAG= HerdLifeData$EAR_TAG, MaxRecAge= HerdLifeData$MaxRecAge, Event=HerdLifeData$Event)
head(hlDF)
nrow(hlDF)

hlDF<-hlDF[!duplicated(hlDF$EAR_TAG),]
nrow(hlDF)

#survData<- read.csv("C:/Users/lseeker/Documents/PhD/Stage1_Experiment/07_Analysis/06_SurvivalPlot/20170603_SurvivalDF.csv")
#names(survData)
#nrow(survData)

ClusterDF<-merge(hlDF, mData, by ="EAR_TAG", all=FALSE)
head(ClusterDF)
nrow(ClusterDF)

#ClusterDF$Event<-"bla"

#i=1
#for (i in (1:nrow(ClusterDF))){
#  if(ClusterDF[i, 2] == "NULL"){
#    ClusterDF[i,5]<-0
#  } else {
#    ClusterDF[i,5]<-1
# }
#  i=i+1
#}


#max(as.numeric(paste(ClusterDF$HerdLife),na.rm=TRUE), na.rm=TRUE)

#ClusterDF$SurvTime<-"blubb"

#i=1
#for (i in (1:nrow(ClusterDF))){
#  if(ClusterDF[i, 2] == "NULL"){
#    ClusterDF[i,6]<-paste(max(as.numeric(paste(ClusterDF$HerdLife),na.rm=TRUE), na.rm=TRUE)+2)
#  } else {
#    ClusterDF[i,6]<-paste(as.numeric(paste(ClusterDF[i,2])))
#  }
#  i=i+1
#}
### survival analysis
ClusterDF$SurvTimeN<-as.numeric(paste(ClusterDF$MaxRecAge))

Surv(ClusterDF$SurvTimeN,  ClusterDF$Event)

fit<-survfit(Surv(ClusterDF$SurvTimeN,  ClusterDF$Event == 1)~ClusterDF$cluster)

tiff( "C:/Users/lseeker/Documents/PhD/Thesis/01_THESIS_FOLDER/h_CHAPTER4_RRM/PaperSubmissionPlosOne/Revision/Resubmission/Images/Fig6.tiff", height = 12, width = 17, units = 'cm', compression = "lzw", res = 300)

ggsurvplot(fit,  data= ClusterDF, legend.title = "Telomere Length", 
           legend.labs= c("mild shortening", "maintenance", "mild elongation", "moderate shortening", "moderate elongation"),
           xlab= "survival in days", pval=TRUE, 
           palette=c("red3", "forestgreen", "blue2", "cyan", "hotpink2"), legend="right")

dev.off()

#, "gold", "grey72" for two more clusters


survdiff(Surv(ClusterDF$SurvTimeN, ClusterDF$Event == 1)~ClusterDF$cluster)


coxFit<-coxph(Surv(ClusterDF$SurvTimeN, ClusterDF$Event == 1)~ClusterDF$cluster)

summary(coxFit)



##### raw data

rawData<-read.csv("C:/Users/lseeker/Documents/PhD/Stage1_Experiment/07_Analysis/03_ExploreNewDF/20170412_CattleDF.csv")

rawData<-subset(rawData, rawData$AGE_Y<4)
rawPlot<-data.frame(EAR_TAG= rawData$EAR_TAG, Time=rawData$AGE_Y, RLTL=rawData$LogTL)
rawPlot<-rawPlot[with(rawPlot, order(EAR_TAG, Time)), ]
wideRaw<-reshape(rawPlot, idvar = "EAR_TAG", timevar = "Time", direction = "wide")
head(wideRaw)
nrow(wideRaw)

wideRaw<-wideRaw[complete.cases(wideRaw), ]
head(wideRaw)
nrow(wideRaw)

wideRaw<-wideRaw[,2:5]
head(wideRaw)

myClds <- cldsWide(data.frame(1:191,wideRaw[2:5]))
plot(myClds)

par(ask=FALSE)
kmlShape(myClds,3)

plotTraj(myClds)
plot(myClds)

clu<-as.data.frame((myClds["clusters"]))
clu

cluData<-cbind(wideRaw, clu )
names(cluData)

colnames(cluData)[ncol(cluData)] <- "cluster"

clusterData<-data.frame(EAR_TAG=cluData$EAR_TAG, cluster= cluData$cluster)

head(clusterData)

summary(clusterData$cluster)

survData<- read.csv("C:/Users/lseeker/Documents/PhD/Stage1_Experiment/07_Analysis/06_SurvivalPlot/20170603_SurvivalDF.csv")
names(survData)
nrow(survData)

ClusterDF<-merge(survData, clusterData, by ="EAR_TAG", all=FALSE)
head(ClusterDF)
nrow(ClusterDF)

clusterDF<-ClusterDF[!duplicated(ClusterDF$EAR_TAG),]
nrow(clusterDF)

### survival analysis

fit<-survfit(Surv(clusterDF$Time_Years,  clusterDF$Event == 1)~clusterDF$cluster)

ggsurvplot(fit,  legend.title = "Telomere Length", xlab= "Time in years", pval=TRUE)

survdiff(Surv(clusterDF$Time_Years, clusterDF$Event == 1)~clusterDF$cluster)


coxFit<-coxph(Surv(clusterDF$Time_Years, clusterDF$Event == 1)~clusterDF$cluster)

summary(coxFit)




#example kmlShape



nbLignes <- 20
trajG <- matrix(0,nbLignes,10)
for(i in 1:(nbLignes/2)){
  trajG[i,] <- dnorm(1:10,runif(1,3,8),1)*rnorm(1,10,0.1)
}
for(i in (nbLignes/2+1):nbLignes){
  trajG[i,] <- dnorm(1:10,runif(1,3,8),1)*rnorm(1,5,0.1)
}

myClds <- cldsWide(data.frame(1:20,trajG))
plot(myClds)

### kmlshape
par(ask=FALSE)
kmlShape(myClds,2)
par(ask=TRUE)
plot(myClds)

#example 2
nbLignes <- 12
trajH <- matrix(0,nbLignes,10)

for(i in 1:(nbLignes/3)){
  trajH[i,] <- pnorm(1:10,runif(1,3,8),1)*rnorm(1,10,1)
}
for(i in (nbLignes/3+1):(2*nbLignes/3)){
  trajH[i,] <- dnorm(1:10,runif(1,3,8),1)*rnorm(1,13,1)
}

for(i in (2*nbLignes/3+1):nbLignes){
  trajH[i,] <- pnorm(1:10,runif(1,3,8),1)*rnorm(1,5,0.1)
}

myClds2 <- cldsWide(data.frame(1:60,trajH))
plot(myClds2)

### kmlshape
par(ask=FALSE)
kmlShape(myClds2,3)
par(ask=TRUE)
plot(myClds2)

