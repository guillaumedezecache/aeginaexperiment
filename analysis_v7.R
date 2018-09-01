#Last update: 27august2018

#Load requested packages
library(readxl)
library(ggplot2)

#Set working directory and upload dataset
setwd('Desktop/Studies/RA/Collective Decision Project/MELESSA/DATA/EIFFEL')
data<- read_excel("clean_data.xls")


#Calculate Plausibility per question, regardless of group 
data$PLAUSIBILITY=data$I1
for (a in 1:length(unique(data$QUESTION_CODE)))
{ 
  POSITIONi1=ecdf(data[data$QUESTION_CODE==a, ]$I1)(mean(data[data$QUESTION_CODE==a, ]$CORRECT_ANSWER))
  POSITIONi1=abs(POSITIONi1-0.5)
  POSITIONi1=-POSITIONi1
  data[data$QUESTION_CODE==a, ]$PLAUSIBILITY=POSITIONi1
}


#Calculate Plausibility per question per group
data$PLAUSIBILITY2=data$I1
for (a in 1:length(unique(data$QUESTION_CODE)))
{
  for (b in 1:length(unique(data$GROUP_NUMBER)))
  { 
  POSITIONi1=ecdf(data[(data$QUESTION_CODE==a) & (data$GROUP_NUMBER==b) , ]$I1)(mean(data[(data$QUESTION_CODE==a) & (data$GROUP_NUMBER==b) , ]$CORRECT_ANSWER))
  POSITIONi1=abs(POSITIONi1-0.5)
  POSITIONi1=-POSITIONi1
  data[(data$QUESTION_CODE==a) & (data$GROUP_NUMBER==b) , ]$PLAUSIBILITY2=POSITIONi1
  }}


#Initial confidence and plausibility
library(dplyr)
install.packages(e1071)
library(e1071)

tableconfidencexplausibility=data %>% group_by(QUESTION_CODE, GROUP_NUMBER) %>% summarise_at(vars(PLAUSIBILITY, PLAUSIBILITY2, CONF1), funs(mean, sd, skewness))

plot(ylim=c(0,10), tableconfidencexplausibility$PLAUSIBILITY_mean, tableconfidencexplausibility$CONF1_mean,  xlab='Plausibility of truth for each question', ylab='Mean initial confidence per group', col='black', pch = 16, cex = 1.3)
cor.test(tableconfidencexplausibility$PLAUSIBILITY_mean, tableconfidencexplausibility$CONF1_mean, method='spearman')

plot(ylim=c(0,5), tableconfidencexplausibility$PLAUSIBILITY_mean, tableconfidencexplausibility$CONF1_sd,  xlab='Plausibility of truth for each question', ylab='Variance (SD) in initial confidence per group', col='black', pch = 16, cex = 1.3)
cor.test(tableconfidencexplausibility$PLAUSIBILITY_mean, tableconfidencexplausibility$CONF1_sd, method='spearman')

#Skewness of confidence and 
plot(ylim=c(-2,2), tableconfidencexplausibility$PLAUSIBILITY_mean, tableconfidencexplausibility$CONF1_skewness,  xlab='Plausibility of truth for each question', ylab='Skewness in initial confidence per group', col='black', pch = 16, cex = 1.3)
cor.test(tableconfidencexplausibility$PLAUSIBILITY_mean, tableconfidencexplausibility$CONF1_skewness, method='spearman')

plot(ylim=c(0,10), tableconfidencexplausibility$PLAUSIBILITY2_mean, tableconfidencexplausibility$CONF1_mean,  xlab='Plausibility of truth for each question', ylab='Mean initial confidence per group', col='black', pch = 16, cex = 1.3)
cor.test(tableconfidencexplausibility$PLAUSIBILITY2_mean, tableconfidencexplausibility$CONF1_mean, method='spearman')

plot(ylim=c(0,5), tableconfidencexplausibility$PLAUSIBILITY2_mean, tableconfidencexplausibility$CONF1_sd,  xlab='Plausibility of truth for each question', ylab='Variance (SD) in initial confidence per group', col='black', pch = 16, cex = 1.3)
cor.test(tableconfidencexplausibility$PLAUSIBILITY2_mean, tableconfidencexplausibility$CONF1_sd, method='spearman')

#Mean initial confidence per plausibility per group
corrcoeff=as.data.frame(matrix(0, ncol = 2, nrow = 20))

for (g in 1:length(unique(tableconfidencexplausibility$GROUP_NUMBER)))
{ 
  png(paste("Mean_Initial_confidence_",g,".png",sep=" "))
  plot(ylim=c(0,10), tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$PLAUSIBILITY2_mean, tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$CONF1_mean,  xlab='Plausibility of truth for each question', ylab=paste("Mean Initial confidence for group",g,sep=""),col='black', pch = 16, cex = 1.3)
  dev.off()
  c=cor.test(tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$PLAUSIBILITY2_mean, tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$CONF1_mean, method='spearman')
  corrcoeff[[g,1]]= g
  corrcoeff[[g,2]] = c[["estimate"]][["rho"]]
}

t.test(corrcoeff$V2)

#SD in initial confidence per plausibility/group per group
corrcoeff=as.data.frame(matrix(0, ncol = 2, nrow = 20))

for (g in 1:length(unique(tableconfidencexplausibility$GROUP_NUMBER)))
{ 
  png(paste("SD_Initial_confidence_",g,".png",sep=" "))
  plot(ylim=c(0,10), tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$PLAUSIBILITY2_mean, tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$CONF1_sd,  xlab='Plausibility of truth for each question', ylab=paste("Variance (SD) in Initial confidence for group",g,sep=""),col='black', pch = 16, cex = 1.3)
  dev.off()
  c=cor.test(tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$PLAUSIBILITY2_mean, tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$CONF1_sd, method='spearman')
  corrcoeff[[g,1]]= g
  corrcoeff[[g,2]] = c[["estimate"]][["rho"]]
}

t.test(corrcoeff$V2)


#Skewness in initial confidence per plausability per group
corrcoeff=as.data.frame(matrix(0, ncol = 2, nrow = 20))

for (g in 1:length(unique(tableconfidencexplausibility$GROUP_NUMBER)))
{ 
  png(paste("Skewness_Initial_confidence_",g,".png",sep=" "))
  plot(ylim=c(-2,2), tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$PLAUSIBILITY2_mean, tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$CONF1_skewness,  xlab='Plausibility of truth for each question', ylab=paste("Skewness Initial confidence for group",g,sep=""),col='black', pch = 16, cex = 1.3)
  dev.off()
  c=cor.test(tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$PLAUSIBILITY2_mean, tableconfidencexplausibility[tableconfidencexplausibility$GROUP_NUMBER==g,]$CONF1_skewness, method='spearman')
  corrcoeff[[g,1]]= g
  corrcoeff[[g,2]] = c[["estimate"]][["rho"]]
}



