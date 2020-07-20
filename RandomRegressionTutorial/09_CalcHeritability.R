#load required packages:

library(ggplot2)


#read in input file

working_dir<-"C:/Users/lseeker/Documents/PhD/RandomRegressionTutorial/WorkingDirectory1"
asreml_file<-"Stage1_20170621_RR_tutorial_09_bothQuadratic"       # name of the ASReml output file without any extensions

####################################################################################################
#### CONTROL PANEL
####
main=" "            # header of figure x
main2=" "           # header of figure y
n_traits=1
polynomial="leg"     # type of polynomial: "p" or "leg"
dims="AGE_M"       # variable name with days in milk
polyORDER=2          # order of polynomial
DIM_start=0        # first DIM ##TEL: first sampling month
DIM_end=75        # last DIM   ##TEL: last sampling month
pe='NO'
var_comp='YES'

####
#### GRAPHS
####
y_axis_tick_h2=0.01               # ticks on the y axis for heritability
y_axis_round_h2=2                # rounding of values on the y axis for heritability
y_axis_tick_var=0.1              # ticks on the y axis for genetic variance
y_axis_round_var=1               # rounding of values on the y axis for genetic variance
colorss=c("blue", "red")         # colors for the graph with gen, pe and resid variance
position_var="topleft"           # position of legend in the variance graph
DIM_step=1
DIM_points=2


asr_file<-paste(asreml_file, ".asr", sep="")
res_file<-paste(asreml_file, ".res", sep="")
cat("STARTING OPTIONS", "\n",
    "working directory: ", working_dir, "\n",
    "asr file: ", asr_file, "\n",
    "res_file: ", res_file, "\n", "\n",
    "output files:", "\n", asreml_file, "_h2.png", "\n",
    asreml_file, ".RData", "\n", "\n", sep="")

####
#### READ: polynomials, covariance matrices
####
setwd(working_dir)
polynom <- readLines(res_file)
polynom <- sub('\\s+', '', polynom, perl=TRUE)  # remove leading spaces
startp <- (grep(paste(polynomial, "\\(", dims, "\\,", polyORDER, "\\)", sep=""), polynom, fixed=FALSE))[1]
endpos<-grep("Convergence sequence of variance parameters", polynom) - 1
n_col <- 2 + polyORDER   # number of columns in the file with polynomials
#dataX <-matrix(strsplit(polynom[(startp+1):endpos], " "), ncol=n_col, byrow=TRUE)
endp=endpos-startp
dataX <-matrix(scan(res_file, skip=startp, nlines=endp), ncol=n_col, byrow=TRUE)
#dataX equals number of months (or different time scale) with measurements and thus residuals

asr <- readLines(asr_file)
#asr_start <- (grep("UnStructured", asr, fixed=FALSE))[1]
asr_start <- (grep("Model_Term", asr, fixed=FALSE))[1] +2
asr_end   <- (grep("Covariance\\/Variance\\/Correlation Matrix", asr, fixed=FALSE))[1] -1
#resid_pos <- (grep("Variance", asr, fixed=FALSE))[1:n_traits]
resid_pos <- (grep("Residual", asr, fixed=FALSE))[1:(n_traits)] 


p <- (polyORDER + 1) * n_traits 

parout1<- as.numeric(substr(asr[asr_start:(((((p*p)-p)/2)+p)+asr_start-1)],start=51,stop=64)) 
#parout1 are the values for sigma of the fixed effect with the polynomial function
parout1<-parout1[!is.na(parout1)]


if(pe=='YES'){
  parout2<- as.numeric(substr(asr[(((((p*p)-p)/2)+p)+asr_start):asr_end],start=51,stop=64))
}

####
#### DECLARE MATRICES
####
G1 <- matrix(0, nrow=p, ncol=p)                    # declaration of the G1 matrix
e  <- matrix(0, nrow=n_traits, ncol=n_traits)      # declaration of the e matrix


if(pe=='YES'){
  G2 <- matrix(0, nrow=p, ncol=p)                    # declaration of the G2 matrix
}

####                           
#### ASSIGN VALUES TO MATRICES 
####                           
k<-0
for(i in 1:p){
  for(j in 1:p){
    if(i>=j){
      k <- k+1
      G1[i,j] <- parout1[k]
      G1[j,i] <- G1[i,j]
      if(pe=='YES'){
        G2[i,j] <- parout2[k]
        G2[j,i] <- G2[i,j]
      }
    }  
  }
}
#the ideration fills the G1 (and G2) matrix with the values of parout1 (sigma of fixed effect with
# fitted polynomial). The G matrix has identical values at [2:1] and [1:2] resp. [3:2] and [2:3] etc....

ll<-0
for(i in 1:n_traits){   
  for(j in 1:n_traits){
    if(i<=j){
      ll<-ll+1
      #cat('ll', ll,'\n')
      #cat('i:',i, 'j',j,'\n')
      e[i,j]<- as.numeric(substr(asr[resid_pos[ll]],start=51,stop=64))
      e[j,i]<-e[i,j]
    }
  }
}
print(G1)

if(pe=='YES'){
  print(G2)
}
print(e)

####
#### DIVIDE THE MATRICES 
####
if(n_traits==1){
  G1_t1<-G1
  if(pe=='YES'){
    G2_t1<-G2
  }
  e_t1 <-e
}else if(n_traits==2){
  elem1 <- (p/2)                       # elements 1 : elem1 from the big matrix G1 belong to the 1st trait
  elem2 <- elem1+1                     # elements elem2 : p belong to the 2nd trait
  G1_t1    <- G1[1:elem1,1:elem1]      # genetic (animal) variance trait1
  G1_t2    <- G1[elem2:p,elem2:p]      # genetic (animal) variance trait2
  G1_t1_t2 <- G1[1:elem1,elem2:p]      # genetic covariance trait1 and trait2
  
  if(pe=='YES'){
    G2_t1    <- G2[1:elem1,1:elem1]
    G2_t2    <- G2[elem2:p,elem2:p]
    G2_t1_t2 <- G2[1:elem1,elem2:p]
  }
  e_t1     <- e[1,1]                              # residual variance MUN
  e_t2     <- e[2,2]                              # residual variance fertility trait
  e_t1_t2  <- e[1,2]                              # residual covariance MUN & fertility trait
}else if(n_traits>2){
  cat("script currently doesn't support more than 2 traits", "\n")
  break()
}

#######                    #######
####### BEGIN CALCULATIONS #######
#######                    #######
# dataX - polynomials for DIM 4 - 520
# G1 - animal 
# G2 - pe
# e - residual

days <- nrow(dataX)           #DIM_end - DIM_start, equals number of days (or months or different 
#time measure with measurements)
if(n_traits==1){
  cat("univariate analysis", "\n")
  s_var1  <- matrix(nrow=days,ncol=1)  # sire variance trait 1
  pe_var1 <- matrix(nrow=days,ncol=1)  # pe variance trait 1
  herit1  <- matrix(nrow=days,ncol=1)  # h2 trait 1
}else if(n_traits==2){
  cat("bivariate analysis", "\n")
  corr_P  <- matrix(nrow=days,ncol=1)  # phenotypic correlation
  corr_G  <- matrix(nrow=days,ncol=1)  # genetic correlation
  s_var1  <- matrix(nrow=days,ncol=1)  # sire variance trait 1
  s_var2  <- matrix(nrow=days,ncol=1)  # sire variance trait 2
  pe_var1 <- matrix(nrow=days,ncol=1)  # pe variance trait 1
  pe_var2 <- matrix(nrow=days,ncol=1)  # pe variance trait 2
  herit1  <- matrix(nrow=days,ncol=1)  # h2 trait 1
  herit2  <- matrix(nrow=days,ncol=1)  # h2 trait 2
}

for(j in 1:days){
  xtrans <- matrix(dataX[j,2:(polyORDER+2)])   # take columns 2: (polyORDER+2)
  # first polynomial is in column 2 and last is in column polyORDER +2
  # makes a transposed 1 by n+1 matrix  for every row in the .res file. n is order of polynomial
  # (n+1 because of residuals for the intercept which is polynomial of 0 order)
  x <- t(xtrans)                               
  
  ### GENETIC VAR ###
  XG1 <- x %*% G1_t1                           # %*% is used for matrix multiplications
  XG1X <- XG1 %*% xtrans
  s1 <- as.numeric(XG1X)
  s_var1[j] <- s1
  if(n_traits==2){
    XG2 <- x %*% G1_t2
    XG2X <- XG2 %*% xtrans
    s2 <- as.numeric(XG2X)
    s_var2[j] <- s2
    ### GENETIC COVAR ###
    XGsCov <- x %*% G1_t1_t2
    XGsCovX <- XGsCov %*% xtrans
    CovSire <- as.numeric(XGsCovX)
  }
  
  if(pe=='YES'){
    ### Pe variance ###
    XG1pe      <- x %*% G2_t1
    XG1peX     <- XG1pe %*% xtrans
    pe1        <- as.numeric(XG1peX)
    pe_var1[j] <- pe1
    if(n_traits==2){
      XG2pe      <- x %*% G2_t2
      XG2peX     <- XG2pe %*% xtrans
      pe2        <- as.numeric(XG2peX)
      pe_var2[j] <- pe2
      
      ### Pe covariance ###
      XGpeCov <- x %*% G2_t1_t2
      XGpeCovX <- XGpeCov %*% xtrans
      CovPe <- as.numeric(XGpeCovX)
    }
  }
  ### HERITABILITY ###
  if(pe=='YES'){
    herit1[j,1] <- s1/(s1 + pe1 + e_t1)
  }else if(pe=='NO'){
    herit1[j,1] <- s1/(s1 + e_t1)
  }
  if(n_traits==2){
    if(pe=='YES'){
      herit2[j,1] <- s2/(s2 + pe2 + e_t2)
    }else if(pe=='NO'){
      herit2[j,1] <- s2/(s2 + e_t2)
    }
    ### GENETIC CORRELATION ###
    corG <- CovSire /(s1^0.5 * s2^0.5)
    corr_G[j,1] <- corG
    
    ### PHENOTYPIC CORRELATION ###
    if(pe=='YES'){
      corP <- (CovSire + CovPe + e_t1_t2) /((s1 + pe1 + e_t1)^0.5 * (s2 + pe2 + e_t2)^0.5)
    }else if(pe=='NO'){
      corP <- (CovSire  + e_t1_t2) /((s1 + e_t1)^0.5 * (s2 + e_t2)^0.5)
      
    }
    corr_P[j,1] <- corP
  }
}

####
#### RESULTS 
####
# heritability: MIN MAX
(h2_t1_max <- max(herit1))
(h2_t1_min <- min(herit1))
extremes_h2 <- c(h2_t1_max, h2_t1_min)


# genetic variance: MIN, MAX
# max gen & pe variance
g_var1_max  <- round(max(s_var1),  digits=4)
pe_var1_max <- round(max(pe_var1), digits=2)
# min gen & pe variance
g_var1_min  <- round(min(s_var1),  digits=4)
pe_var1_min <- round(min(pe_var1), digits=2)
extremes_var <-c(g_var1_max, g_var1_min, pe_var1_max, pe_var1_min, e_t1) 
cat("VOLKER",extremes_var,"\n")


if(n_traits==2){
  (h2_t2_max <- max(herit2))
  (h2_t2_min <- min(herit2))
  extremes_h2 <- c(h2_t1_max, h2_t2_max, h2_t1_min, h2_t2_min)
  
  # Genetic correlations: MIN, MAX ###
  # max Rg
  (Rg_max<-round(max(corr_G), digits=2))
  # min Rg
  (Rg_min<-round(min(corr_G), digits=2))
  
  ### Phenotypic correlations: MIN, MAX ###
  # max Rp
  (Rp_max<-round(max(corr_P), digits=2))
  # min Rp
  (Rp_min<-round(min(corr_P), digits=2))
  extremes <- c(Rg_max, Rg_min, Rp_max, Rp_min)
  
  # genetic & permanent variance: MIN, MAX
  # max gen & pe variance
  g_var1_max <- round(max(s_var1), digits=2)
  g_var2_max <- round(max(s_var2), digits=2)
  pe_var1_max <- round(max(pe_var1), digits=2)
  pe_var2_max <- round(max(pe_var2), digits=2)
  # min gen var
  g_var1_min <- round(min(s_var1), digits=2)
  g_var2_min <- round(min(s_var2), digits=2)
  pe_var1_min <- round(min(pe_var1), digits=2)
  pe_var2_min <- round(min(pe_var2), digits=2)
  extremes_var <-c(g_var1_max, g_var2_max, 
                   g_var1_min, g_var2_min,
                   pe_var1_max, pe_var2_max,
                   pe_var1_min, pe_var2_min,				 
                   e_t1, e_t2) 
  
  ####
  #### GRAPHS
  ####
  
  # correlations
  DIMSg <- seq(DIM_start, DIM_end, DIM_step)
  data_points <- (DIM_start:length(corr_G))[seq(1, length(corr_G), DIM_points)]
  png(paste(fileout, "_cor", ".png", sep=""), width=1024, height=768, res=300, pointsize= 4)
  par(oma=c(1,1,0,0))
  plot(c(0),c(0), pch=" ", xlim=c(DIM_start, max(data_points)), ylim=c(min(extremes), max(extremes)), axes=FALSE, ann=FALSE, type='n')
  axis(2, at=round(seq(min(extremes), max(extremes), 0.1), digits=1), labels = FALSE)
  mtext(round(seq(min(extremes), max(extremes), 0.1), digits=1), 2, 1, at=round(seq(min(extremes), max(extremes), 0.1), digits=1), cex = 1.5)
  axis(1, at=DIMSg, labels = FALSE)
  mtext(DIMSg, 1, 1, at=DIMSg, cex = 1.5)
  box()
  mtext(main,  side=3, line=1, cex=2)
  mtext('DIM', side=1, line=3, cex = 2)
  mtext('r',   side=2, line=3, cex = 2, las=1)
  points(data_points, corr_G[seq(1, length(corr_G), DIM_points)],  pch=18, cex=2.5)
  points(data_points, corr_P[seq(1, length(corr_P), DIM_points)],  pch=17, cex=2.0)
  legend("bottomleft", c("genetic", "phenotypic"), title="correlations",
         pch=c(18,17), lty=1:2, pt.cex=2, cex=1.3, inset=0.02)
  dev.off()
}

# heritability
DIMSg <- seq(DIM_start, DIM_end, 5)
data_points <- (DIM_start:length(herit1))[seq(1, length(herit1), 1)]
png(paste(asr_file, "_h2", ".png", sep=""), width=1024, height=768, res=300, pointsize= 4)
par(oma=c(1,1,0,0))
#c(bottom, left, top, right)
plot(c(0),c(0), pch=" ", xlim=c(DIM_start, max(data_points)), ylim=c(min(extremes_h2), max(extremes_h2)), axes=FALSE, ann=FALSE, type='n')
axis(2, at=round(seq(min(extremes_h2), max(extremes_h2), y_axis_tick_h2), digits=y_axis_round_h2), labels=FALSE)
mtext(round(seq(min(extremes_h2), max(extremes_h2), y_axis_tick_h2), digits=y_axis_round_h2), 2, 1,
      at=round(seq(min(extremes_h2), max(extremes_h2), y_axis_tick_h2), digits=y_axis_round_h2), cex = 1.5)
axis(1, at=DIMSg, labels = FALSE)
mtext(DIMSg, 1, 1, at=DIMSg, cex = 1.5)
box()
mtext(main2, side=3, line=1, cex=2)
mtext('Months', side=1, line=3, cex = 2)
mtext(expression(h^2), 2, 2.5, cex = 2, las=3)
points(data_points, herit1[seq(1, length(herit1), 1)],  pch=18, cex=2.5)
if(n_traits==2){
  points(data_points, herit2[seq(1, length(herit2), 10)],  pch=17, cex=2.0)
  legend("bottomleft", c("lact 1", "lact 2+"), title="heritability",
         pch=c(18,17), lty=1:2, pt.cex=2, cex=1.3, inset=0.02)
}   
dev.off()

if(var_comp=='YES'){
  # gen, pe, and resid variance
  png(paste(fileout, "_var", ".png", sep=""), width=1024, height=768, res=300, pointsize= 4)
  par(oma=c(1,1,0,0))
  #c(bottom, left, top, right)
  cat("VOLKER","test",extremes_var)
  plot(c(0),c(0), pch=" ", xlim=c(DIM_start, max(data_points)), ylim=c(min(extremes_var,na.rm=TRUE), max(extremes_var,na.rm=TRUE)), axes=FALSE, ann=FALSE, type='n')
  axis(2, at=round(seq(min(extremes_var,na.rm=TRUE), max(extremes_var,na.rm=TRUE), y_axis_tick_var), digits=y_axis_round_var), labels=FALSE)
  mtext(round(seq(min(extremes_var,na.rm=TRUE), max(extremes_var,na.rm=TRUE), y_axis_tick_var), digits=y_axis_round_var), 2, 1,
        at=round(seq(min(extremes_var,na.rm=TRUE), max(extremes_var,na.rm=TRUE), y_axis_tick_var), digits=y_axis_round_var), cex = 1.5)
  axis(1, at=DIMSg, labels = FALSE)
  mtext(DIMSg, 1, 1, at=DIMSg, cex = 1.5)
  box()
  mtext(main, side=3, line=1, cex=2)
  mtext('DIM', side=1, line=3, cex = 2)
  mtext("variance", 2, 2.5, cex = 2, las=3)
# VOLKER
# I commented out the following two and replaced with the two after that
# s_var1 and pe_var1 have both 69 entries (pe="NO",var_comp='YES',n_traits=1)
# but the seq call reduces it to 7 (don't know why seq is called yet)
#   points(data_points, s_var1[seq(1, length(s_var1), 10)],    pch=17, cex=2, col=colorss[1])
#   points(data_points, pe_var1[seq(1, length(pe_var1), 10)],  pch=19, cex=2, col=colorss[1])
  points(data_points, s_var1,    pch=17, cex=2, col=colorss[1])
  points(data_points, pe_var1,  pch=19, cex=2, col=colorss[1])
  points(data_points, rep(e_t1, length(data_points)),        pch=8,  cex=1, col=colorss[1])
  ##############
  legend(position_var, c("g", "pe", "e"), col="black",
         text.col="black",
         pch=c(17,19,8), pt.cex=2, cex=1.3, inset=0.02)
  ##############
  if(n_traits==2){
    points(data_points, s_var2[seq(1, length(s_var2), 10)],    pch=17, col=colorss[2], cex=2)
    points(data_points, pe_var2[seq(1, length(pe_var2), 10)],  pch=19, col=colorss[2], cex=2)
    points(data_points, rep(e_t2, length(data_points)),        pch=8,  col=colorss[2], cex=1)
    legend(position_var, c("g", "pe", "e", "lact 1", "lact 2"), col=c(rep("black", 3), rep("white",2)),
           text.col=c(rep("black", 3), colorss),
           pch=c(17,19,8), pt.cex=2, cex=1.3, inset=0.02)
  }   
  dev.off()
  
}

write.table(herit1, paste(asr_file, "_h2_t1.txt", sep=""), sep="\t", col.names=TRUE, row.names=FALSE, quote=FALSE)
if(n_traits==2){
  write.table(herit2, paste(fileout, "_h2_t2.txt", sep=""), sep="\t", col.names=TRUE, row.names=FALSE, quote=FALSE)
  
  correlations_out <- cbind(corr_G, corr_P)
  colnames(correlations_out) <- c('genetic','phenotypic')
  write.table(correlations_out, paste(asr_file, "_corr.txt", sep=""), sep="\t", col.names=TRUE, row.names=TRUE, quote=FALSE)
}

#save.image(paste(fileout, ".RData", sep=""))
save(list=ls(), file=paste(asr_file, ".RData", sep=""), envir=environment())


df<-data.frame(1:days, herit1)
ggplot(df, aes(x=X1.days, y=herit1))+ geom_line(size=2, colour= "blue") + 
  xlab("Age in months") + ylab("Heritability")+theme_gray(20)+ylim(0,1)

df_genVar<-data.frame(1:days, s_var1)
ggplot(df_genVar, aes(x=X1.days, y=s_var1))+ geom_line(size=2, colour= "orange") + 
  xlab("Age in months") + ylab("Animal variance")+theme_gray(20)

write.table(G1, "BothQuadratic_VarianceCovarianceM.csv", row.names=FALSE, col.names=FALSE, sep=",")

#write genetic variance for each day:

write.csv(s_var1, "BothQuadratic_GeneticVariance.csv" )
