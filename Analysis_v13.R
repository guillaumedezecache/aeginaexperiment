########################################### GETTING READY

#PACKAGES
library(readxl)
library(lme4)
library(ggplot2)

#UPLOAD DATA
##SET THE WORKING DIRECTORY
setwd('/Users/guillaume/SpiderOak Hive/WORK/AeginaWisdomofCrowds/Data/Munich/Analysis/')
##read the datafile
data <- read_excel("data.xls")

#NORMALIZATION
##ADD 1 TO ALL VARIABLES OF INTEREST TO ALLOW FOR LOG TRANSFORMATIONS
data$I1<-data$I1+1
data$I2<-data$I2+1
data$CORRECT_ANSWER<-data$CORRECT_ANSWER+1

##APPLY LOG10 TRANSFORMATIONS
data$log10_I1<-log10(data$I1)
data$log10_I2<-log10(data$I2)
data$log10_CORRECT_ANSWER<-log10(data$CORRECT_ANSWER)

##CREATE TWO VARIABLES = ERROR AT I1 AND I2 IN LOG10 SPACE FOR EACH PARTICIPANT
data$log10_I1_ERROR<-abs(data$log10_I1-data$log10_CORRECT_ANSWER)
data$log10_I2_ERROR<-abs(data$log10_I2-data$log10_CORRECT_ANSWER)

# #SUBSETTING THE DATA
# ##DISCUSSED VS NON DISCUSSED
# data_discussed <- data[ which(data$DISCUSSED==1), ]
# data_nondiscussed <- data[ which(data$DISCUSSED==0), ]
# ##ELEVATOR VS EIFFELTOWER QUESTIONS
# data_eiffel <- data[ which(data$QUESTION_TYPE==1), ]
# data_elevator <- data[ which(data$QUESTION_TYPE==0), ]




########################################### INDEX OF BRACKETABILITY

#INDEX OF BRACKETABILITY (b-index)
##CREATE A DEDICATED DATAFRAME
BINDEXdf<-as.data.frame(matrix(0, ncol = 12, nrow = 1))

##FOR EACH QUESTION...
for (a in 1:length(unique((data$QUESTION_CODE))))
{ data_subset <- data[data$QUESTION_CODE==a, ]

##...FIND THE PERCENTILE OF THE CORRECT RESPONSE IN THE DISTRIBUTION OF PARTICIPANTS' ANSWERS AT I1
bindexlist = list()
percentilea<-ecdf(data_subset$log10_I1)
bindex<-percentilea(mean(data_subset$log10_CORRECT_ANSWER))

##...AND STORE IT IN A LIST CALLED bindexlist AND THEN IN A DATAFRAME CALLED BINDEXdf
bindexlist[[length(bindexlist)+1]] = bindex
BINDEXdf[[a]] <- bindex
}
BINDEXdf



########################################### WISDOM OF CROWDS

#WISDOM OF CROWDS FOR EACH QUESTION AT I1: WITHIN GROUP SAMPLING
##FOR EACH QUESTION, THE DATASET IS SUBSETTED
for (a in 1:length(unique((data$QUESTION_CODE))))
  {
    data_subset <- data[data$QUESTION_CODE==a, ]

##AND WE GET THE MEAN RESPONSE PER GROUP
    data_subset_pergrp=(aggregate(data_subset$log10_I1, list(data_subset$GROUP_NUMBER), mean))

##WE CREATE A LIST TO STORE VALUES FOR THE WISDOM OF CROWDS, AND ANOTHER LIST TO STORE THE SEM VALUES
    WOC=list()
    WOCsd=list()
    
##FOR EACH GROUP SIZE...
    for (m in 1:5)

##WE SET THE MAX NUMBER OF NON-REDUNDANT COMBINATIONS FOR EACH GROUP SIZE (m), AND STORE IT IN A VARIABLE CALLED 'MAXI'. IT CAN HAVE A MAX VALUE OF 190, CORRESPONDING TO NON-REDUNDANT COMBINATION OF TWO ELEMENTS IN A SET OF 20 ELEMENTS 
      { ifelse((choose(20,m)>190), yes=(maxi=190), no=(maxi=(choose(20,m))))
##WE STORE ALL THE POTENTIAL COMBINATIONS OF m IN A VARIABLE CALLED sample, WE TAKE THE MEAN OF ALL COMBINATIONS AND THEN SUBSTRACT THE CORRECT ANSWER TO IT
        sample=combn(data_subset_pergrp$x, m)
        sample=colMeans(sample)
        sample=abs(sample-mean(data_subset$log10_CORRECT_ANSWER))
        
##WE SAMPLE maxi VALUES, STORE THEIR MEAN IN THE WOC LIST, AND THEIR SD IN THE WOCsd list
        sample=sample(sample, maxi, replace=FALSE)
        WOC[[length(WOC)+1]] = mean(sample)
        WOCsd[[length(WOCsd)+1]] = sd(sample)/sqrt(maxi)
        WOC=as.numeric(WOC)
        WOCsd=as.numeric(WOCsd)}
    
##WE PLOT THE VALUES CONTAINED IN WOC (+/- WOCsd) /!\ WOCsd is in fact 'sem' of WOC
        WOCdf=data.frame(WOC,WOCsd)
        p<- ggplot(WOCdf, aes(x=c(1:5), y=WOC)) + 
          geom_line() +
          geom_point()+
          geom_errorbar(aes(ymin=WOC-WOCsd, ymax=WOC+WOCsd), width=.2) +
          labs(title=a, x="Crowd size", y = "error at i1") +
          theme_classic() +
          scale_color_manual(values=c('#999999','#E69F00'))
         ggsave(p,filename=paste("WOCPLOTi1_",a,".png",sep=""))

##BEFORE CLOSING THE LOOP FOR THIS QUESTION, WE STORE THE VALUES OF WOC & WOCsd FOR I1 (ONE PER QUESTION)
         
        assign(paste("WOCi1_", a, sep=""),WOC)
        assign(paste("WOCi1sd_", a, sep=""),WOCsd)
         
}


#WISDOM OF CROWDS FOR EACH QUESTIONS AT I2: WITHIN GROUP SAMPLING (SAME AS FOR I1) - IDEALLY WE COULD DO A LOOP HERE TO AVOID REPEATING
for (a in 1:length(unique((data$QUESTION_CODE))))
{
  data_subset <- data[data$QUESTION_CODE==a, ]
  data_subset_pergrp=(aggregate(data_subset$log10_I2, list(data_subset$GROUP_NUMBER), mean))
  
  WOC=list()
  WOCsd=list()
  for (m in 1:5)
  { ifelse((choose(20,m)>190), yes=(maxi=190), no=(maxi=(choose(20,m))))
    sample=combn(data_subset_pergrp$x, m)
    sample=colMeans(sample)
    sample=abs(sample-mean(data_subset$log10_CORRECT_ANSWER))
    sample=sample(sample, maxi, replace=FALSE)
    WOC[[length(WOC)+1]] = mean(sample)
    WOCsd[[length(WOCsd)+1]] = sd(sample)/sqrt(maxi)
    WOC=as.numeric(WOC)
    WOCsd=as.numeric(WOCsd)}
  
  WOCdf=data.frame(WOC,WOCsd)
  p<- ggplot(WOCdf, aes(x=c(1:5), y=WOC)) + 
    geom_line() +
    geom_point()+
    geom_errorbar(aes(ymin=WOC-WOCsd, ymax=WOC+WOCsd), width=.2) +
    labs(title=a, x="Crowd size", y = "error at i2") +
    theme_classic() +
    scale_color_manual(values=c('#999999','#E69F00'))
  ggsave(p,filename=paste("WOCPLOTi2_",a,".png",sep=""))
  
  assign(paste("WOCi2_", a, sep=""),WOC)
  assign(paste("WOCi2sd_", a, sep=""),WOCsd)
  
}


##PLOT WOC I1 AND I2 ON SAME GRAPHS FOR EACH QUESTION
for (i in 1:12)
{   WOC1 = paste('WOCi1_',i,sep='') 
    WOC2 = paste('WOCi2_',i,sep='')
  
  png(paste("WOCPLOTi1vsi2_",i,".png",sep=""))
   plot(get(WOC1), xlab="crowd size", ylab="error", type="l", col="red", main=i, 
        ylim=c(min(get(WOC1), get(WOC2)), max(get(WOC1), get(WOC2))))
    lines(get(WOC2), col="blue")
   legend('topright', col=c('red','blue'), lty=1,legend=c('i1', 'i2'), bty='n')
   
   dev.off()
}

########################################### ERROR AND CONFIDENCE BETWEEN I1 AND I2
b=1
pp=length(unique(data_subset$PARTICIPANT_NUMBER))
x=c(1,2)
for (b in 1:length(unique((data$QUESTION_CODE))))
  
{ 
  data_subset <- data[data$QUESTION_CODE==b, ]
  data_subset_MEANER<-c(mean(data_subset$log10_I1_ERROR), mean(data_subset$log10_I2_ERROR))
  data_subset_SDER<-c(sd(data_subset$log10_I1_ERROR)/sqrt(pp), sd(data_subset$log10_I2_ERROR)/sqrt(pp))
  p<-qplot(x, data_subset_MEANER)+geom_errorbar(aes(x=x, ymin=data_subset_MEANER-data_subset_SDER, ymax=data_subset_MEANER+data_subset_SDER), width=0.25)
  p<-p + theme_bw()
  p<-p + ggtitle(b) +xlab("Stages") + ylab("Error +/- SEM")
  p<-p + theme(axis.text.x = element_blank(),
  axis.ticks.x= element_blank())
  ggsave(p,filename=paste("PLOTERROR",b,".png",sep=""))
}

##I1 VS I2 CONFIDENCE FOR ALL QUESTIONS
b=1
pp=length(unique(data_subset$PARTICIPANT_NUMBER))
x=c(1,2)
for (b in 1:length(unique((data$QUESTION_CODE))))
  
{ 
  data_subset <- data[data$QUESTION_CODE==b, ]
  data_subset_MEANCF<-c(mean(data_subset$CONF1), mean(data_subset$CONF2))
  data_subset_SDCF<-c(sd(data_subset$CONF1/sqrt(pp)), sd(data_subset$CONF2)/sqrt(pp))
  p<-qplot(x, data_subset_MEANCF)+geom_errorbar(aes(x=x, ymin=data_subset_MEANCF-data_subset_SDCF, ymax=data_subset_MEANCF+data_subset_SDCF), width=0.25)
  p<-p + theme_bw()
  p<-p + ggtitle(b) +xlab("Stages") + ylab("Confidence +/- SEM")
  p<-p + theme(axis.text.x = element_blank(),
               axis.ticks.x= element_blank())
  ggsave(p,filename=paste("PLOTCONFIDENCE",b,".png",sep=""))
}


########################################### ERROR AND CONFIDENCE BETWEEN I1, I2 DISCUSSED AND I2 NON DISCUSSED

##ERROR REDUCTION AFTER GROUP DISCUSSION (Discussed & Non-Discussed) 
for (a in length(unique(data$QUESTION_CODE)))
{datasubset<-subset(data,QUESTION_CODE==a)
data_disc<-subset(datasubset,DISCUSSED==1)
data_nondisc<-subset(datasubset,DISCUSSED==0)

png(paste("WOCPLOTDISCUSSIONEFFECT_",a,".png",sep=""))
boxplot(datasubset$log10_I1_ERROR, data_disc$log10_I2_ERROR, data_nondisc$log10_I2_ERROR, ylab="Error",names=c("i1","i2 non-disc", "i2 disc"), main=a)
dev.off()
}


###########################################  AGGREGATION RULES
#AGGREGATION RULES
#GROUP ANSWER ERROR (R0)
data$group_answer<-as.numeric(data$'GROUP ANSWER')
data$norm_group_answer<-log10(data$group_answer)
data$norm_group_answer_error<-abs(data$norm_group_answer-data$log10_CORRECT_ANSWER)
R0<-mean(data$norm_group_answer_error,na.rm=TRUE)

#AGGREGATION RULE 1: Mean
R1 <-mean(data$norm_I1_ERROR,na.rm=TRUE)

##CHECKING: ALSO WORKS IF ADD WEIGHT OF 0.2 to each individual answer
data$weight_mean <- rep(c(.2,.2,.2,.2,.2))
R1 <-weighted.mean(data$norm_I1_ERROR,data$weight_mean)


#Aggregation Rule 2: Median
##Sort individual answers within each group and for each question
data$sorted_I1 <- ave(data$I1, list(data$GROUP_NUMBER,data$QUESTION_CODE), FUN = sort)
##Add column: normalize sorted answers
data$sorted_norm_I1<-log10(data$sorted_I1)
##Add column: compute error of normalized & sorted answers
data$sorted_norm_I1_ERROR<-abs(data$sorted_norm_I1-data$log10_CORRECT_ANSWER)
#Add column with weights (median = 3rd value equal to 1, the rest equal to 0)
data$weight_median <- rep(c(0,0,1,0,0))
R2 <-weighted.mean(data$sorted_norm_I1_ERROR,data$weight_median)


#Aggregation Rule 3: Soft Median.
##Add column with weights (soft median = median equal to .5, second and forth values equal to .25)
data$weight_softmedian <- rep(c(0,.25,.5,.25,0))
R3 <- weighted.mean(data$sorted_norm_I1_ERROR,data$weight_softmedian)

#Aggregation Rule 4: Robust Average #for every group, compute mean, and average only those that differ from the mean by 4 orders of magnitude
##Compute group average (average of individual answers)
data$group_average <- ave(data$I1, list(data$GROUP_NUMBER,data$QUESTION_CODE))
##Add column: normalized group averages
data$norm_group_average <- log10(data$group_average)
#Add column with weights (robust average = if individual answer is <= 4, then it is given a value of 1, otherwise 0)
data$weight_robustaverage <- ifelse(((abs(data$I1 - data$group_average))<= 4),"1", "0")
R4 <-weighted.mean(data$norm_I1_ERROR,data$weight_robustaverage)

##Aggregation Rule 5: Confidence-weighted average rule, divide individual confidence by total within group confidence, and then weight each individual answer by the
##ADD COLUMN WITH AVERAGE CONFIDENCE PER GROUP & PER QUESTION
data$total_confidence<-ave(data$CONF1, list(data$GROUP_NUMBER,data$QUESTION_CODE),FUN = sum)
#Add column with weights for individual confidence
data$weight_conf<-(data$CONF1/data$total_confidence)
R5 <-weighted.mean(data$norm_I1_ERROR,data$weight_conf)


#Aggregation Rule 6: Expert Rule, w = 1 to the most confident
##Add column and sort individual confidence ratings
data$sorted_confidence <- ave(data$CONF1, list(data$GROUP_NUMBER,data$QUESTION_CODE), FUN = sort)
##Add weight of 1 to individual with highest level of confidence
data$weights_expert <- rep(c(0,0,0,0,1))
R6 <-weighted.mean(data$norm_I1_ERROR,data$weights_expert)

#Aggregation Rule 7: Resistance to social influence
##Create column with the average of i2 within each group
data$group_averageI2 <- ave(data$I2, list(data$GROUP_NUMBER,data$QUESTION_CODE))
#Add column with weights
data$weight_resist<-((abs(data$group_averageI2-data$group_average + 32)^-1)/(data$I2-data$I1+32))

R7<-weighted.mean(data$norm_I1_ERROR,data$weight_resist)


##PLOT EIFFEL AGGREGATION RULES

AR_dataframe_within = data.frame(rules=c("DATA","R1", "R2", "R3", "R4", "R5","R6","R7"), mean = c(R0_within,R1_within,R2_within,R3_within,R4_within,R5_within,R6_within,R7_within), lower=c(0.07314574,0.1534856,0.1229398,0.13144,0.1030816,0.1386584,0.1467729,0.1409909), upper=c(0.08065176,0.1712849,0.1367627,0.1463203,0.1160102,0.1556611,0.1649457,0.1532641))
ggplot() + geom_errorbar(data=AR_dataframe_within, mapping=aes(x=rules, ymin=upper, ymax=lower), width=0.2, size=1, color="blue")                        

##PLOT ELEVATOR AGGREGATION RULES
AR_dataframe_outside = data.frame(rules=c("DATA","R1", "R2", "R3", "R4", "R5","R6","R7"), mean = c(R0_outside,R1_outside,R2_outside,R3_outside,R4_outside,R5_outside,R6_outside,R7_outside), lower=c(0.2490182,0.3095929,0.2930279,0.2943246,0.2600963,0.2971068,0.2894377,0.2758877), upper=c(0.2683814,0.3321239,0.3086492,0.3112488,0.2725931,0.3190539,0.3073885,0.2954644))
ggplot() + geom_errorbar(data=AR_dataframe_outside, mapping=aes(x=rules, ymin=upper, ymax=lower), width=0.2, size=1, color="orange")                        



#RULES APPLIED TO MACRON VS RWANDA QUESTION


#GROUP ANSWER ERROR (R0)
R0_Macron <-mean(Macron$norm_group_answer_error,na.rm=TRUE)
R0_Rwanda <-mean(Rwanda$norm_group_answer_error,na.rm=TRUE)


R0_Macron_SE<- ((sd(Macron$norm_group_answer_error,na.rm=TRUE))/sqrt(100))  
R0_Rwanda_SE<- ((sd(Rwanda$norm_group_answer_error,na.rm=TRUE))/sqrt(100))  

#AGGREGATION RULE 1: Mean
R1_Macron <-mean(Macron$norm_I1_ERROR,na.rm=TRUE)
R1_Rwanda <-mean(Rwanda$norm_I1_ERROR,na.rm=TRUE)

R1_Macron_SE<- ((sd(Macron$norm_I1_ERROR,na.rm=TRUE))/sqrt(100))
R1_Rwanda_SE<- ((sd(Rwanda$norm_I1_ERROR,na.rm=TRUE))/sqrt(100))  

#Aggregation Rule 2: Median
R2_Macron <-weighted.mean(Macron$sorted_norm_I1_ERROR,Macron$weight_median)
R2_Rwanda <-weighted.mean(Rwanda$sorted_norm_I1_ERROR,Rwanda$weight_median)


R2_Macron_SE<- ((wt.sd(Macron$sorted_norm_I1_ERROR,Macron$weight_median))/sqrt(100))
R2_Rwanda_SE<- ((wt.sd(Rwanda$sorted_norm_I1_ERROR,Rwanda$weight_median))/sqrt(100))

#Aggregation Rule 3: Soft Median
R3_Macron <- weighted.mean(Macron$sorted_norm_I1_ERROR,Macron$weight_softmedian)
R3_Rwanda <- weighted.mean(Rwanda$sorted_norm_I1_ERROR,Rwanda$weight_softmedian)

R3_Macron_SE<- ((wt.sd(Macron$sorted_norm_I1_ERROR,Macron$weight_softmedian))/sqrt(100))
R3_Rwanda_SE<- ((wt.sd(Rwanda$sorted_norm_I1_ERROR,Rwanda$weight_softmedian))/sqrt(100))

#Aggregation Rule 4: Robust Average
R4_Macron <-weighted.mean(Macron$norm_I1_ERROR,Macron$weight_robustaverage)
R4_Rwanda <-weighted.mean(Rwanda$norm_I1_ERROR,Rwanda$weight_robustaverage)

R4_Macron_SE<- ((wt.sd(Macron$norm_I1_ERROR,as.numeric(Macron$weight_robustaverage)))/sqrt(100))
R4_Rwanda_SE<- ((wt.sd(Rwanda$norm_I1_ERROR,as.numeric(Rwanda$weight_robustaverage)))/sqrt(100))

##Aggregation Rule 5: Confidence-weighted average rule
R5_Macron <-weighted.mean(Macron$norm_I1_ERROR,Macron$weight_conf)
R5_Rwanda <-weighted.mean(Rwanda$norm_I1_ERROR,Rwanda$weight_conf)

R5_Macron_SE<- ((wt.sd(Macron$norm_I1_ERROR,Macron$weight_conf))/sqrt(100))
R5_Rwanda_SE<- ((wt.sd(Rwanda$norm_I1_ERROR,Rwanda$weight_conf))/sqrt(100))

#Aggregation Rule 6: Expert Rule
R6_Macron <-weighted.mean(Macron$norm_I1_ERROR,Macron$weights_expert)
R6_Rwanda <-weighted.mean(Rwanda$norm_I1_ERROR,Rwanda$weights_expert)

R6_Macron_SE<- ((wt.sd(Macron$norm_I1_ERROR,Macron$weights_expert))/sqrt(100))
R6_Rwanda_SE<- ((wt.sd(Rwanda$norm_I1_ERROR,Rwanda$weights_expert))/sqrt(100))


#Aggregation Rule 7: Resistance to social influence

R7_Macron <-weighted.mean(Macron$norm_I1_ERROR,Macron$weight_resist)
R7_Rwanda <-weighted.mean(Rwanda$norm_I1_ERROR,Rwanda$weight_resist)

R7_Macron_SE<- ((wt.sd(Macron$norm_I1_ERROR,Macron$weight_resist))/sqrt(100))
R7_Rwanda_SE<- ((wt.sd(Rwanda$norm_I1_ERROR,Rwanda$weight_resist))/sqrt(100))


##PLOT MACRON AGGREGATION RULES

AR_dataframe_Macron = data.frame(rules=c("DATA","R1", "R2", "R3", "R4", "R5","R6","R7"), mean = c(R0_Macron,R1_Macron,R2_Macron,R3_Macron,R4_Macron,R5_Macron,R6_Macron,R7_Macron), lower=c(0.02276202,0.0541151,0.03435116,0.04147316,0.03189371,0.0479717,0.05392339,0.08634485), upper=c(0.02740593,0.06479002,0.04052269,0.04988932,0.03735663,0.05822579,0.06468519,0.1012536))
ggplot() + geom_errorbar(data=AR_dataframe_Macron, mapping=aes(x=rules, ymin=upper, ymax=lower), width=0.2, size=1, color="blue")                        

##PLOT RWANDA AGGREGATION RULES
AR_dataframe_Rwanda = data.frame(rules=c("DATA","R1", "R2", "R3", "R4", "R5","R6","R7"), mean = c(R0_Rwanda,R1_Rwanda,R2_Rwanda,R3_Rwanda,R4_Rwanda,R5_Rwanda,R6_Rwanda,R7_Rwanda), lower=c(0.5469369,0.5167786,0.4939648,0.5037353,0.490889,0.5220629,0.4672787,0.3809972), upper=c(0.6242749,0.5891049,0.5347846,0.5509206,0.5237164,0.5880085,0.5237715,0.4586655))
ggplot() + geom_errorbar(data=AR_dataframe_Rwanda, mapping=aes(x=rules, ymin=upper, ymax=lower), width=0.2, size=1, color="orange")                        

#-----------------------------------
#Calculate Mean Absolute Deviation between I2 & C

mean(abs(Macron$I2-mean((na.omit(Macron$group_answer)))))
mean(abs(Rwanda$I2-mean((na.omit(Rwanda$group_answer)))))
