library(dplyr)
library(ggplot2)
 
# 1. Import Dataset and understand the data
setwd("C:/Users/Ashish/Desktop/GL/Day3/Homework")
d = read.csv("PL_XSELL_201708.csv")
metadata = read.csv("PL_XSELL_METADATA.csv")
str(d)

# 2.Perform Exploratory Data Analysis

## What if I want the percentile distribution for all the fields
apply(d[,sapply(d, is.numeric)], 
      2, quantile, 
      probs=c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
      na.rm=T)

boxplot(d$BALANCE , 
        main= "Balance Box Plot" ,
        xlab = "Overall Base"
)
d1 = d
## Typically we floor and cap the variables at P1 and P99. 
## Let us cap the Balance variable at P99.

d1$BALANCE = ifelse(d1$BALANCE > 3412762 ,3412762, d1$BALANCE)
d1$TOT_NO_OF_L_TXNS = ifelse(d1$TOT_NO_OF_L_TXNS > 80, 80,d1$TOT_NO_OF_L_TXNS)
d1$NO_OF_ATM_DR_TXNS = ifelse(d1$NO_OF_ATM_DR_TXNS > 5, 5 ,d1$NO_OF_ATM_DR_TXNS)
d1$NO_OF_NET_DR_TXNS = ifelse(d1$NO_OF_NET_DR_TXNS > 10 , 10 , d1$NO_OF_NET_DR_TXNS)
d1$NO_OF_MOB_DR_TXNS = ifelse(d1$NO_OF_MOB_DR_TXNS > 5 ,5 ,d1$NO_OF_MOB_DR_TXNS)
d1$AMT_ATM_DR = ifelse(d1$AMT_ATM_DR > 70000 , 70000 , d1$AMT_ATM_DR)

#apply(d1[,sapply(d1, is.numeric)], 
#      2, quantile, 
#      probs=c(0.01, 0.05, 0.1, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
#      na.rm=T)

summary(d1)
str(d1)

d1$ACC_OP_DATE = as.Date(d1$ACC_OP_DATE , "%m/%d/%Y")
d1$NO_OF_DAYS = as.numeric(Sys.Date()-d1$ACC_OP_DATE)

#head(d1$NO_OF_DAYS)

#str(d1)

d2 = d1[, !names(d1)%in% "ACC_OP_DATE"]

# 3. Build Hypothesis


#Probability of a TARGET is as a function of the predictor variable(s)

################################################################################
# 4. Check Hypothesis using Information Value

library(devtools)
library(woe)

iv.plot.summary(iv.mult(d2[,!names(d2) %in% c("CUST_ID")],
                        "TARGET",TRUE, verbose = TRUE))

iv <- iv.mult(d2[,!names(d2) %in% c("CUST_ID")],
              "TARGET",FALSE)

iv

#################################################################################
#5. Check Hypothesis using Visualization
install_github("K2Analytics/BinaryTargetViz")
library(BinaryTargetViz)
??BinaryTargetViz
x <- sapply(sapply(ls(), get), is.data.frame)
m <- data.frame(names(x)[(x == TRUE)])
BinaryTargetViz(m)

###################################################################################
#6 . Do necessary Variable Transformation.
 
str(d2)
View(metadata)
# Let's check multicolinearity between variables related to No of transactions
cor(d2[,12:19])
# Let's check multicolinearity between variables related to Amount of transactions
cor(d2[,21:27])
# Let's check multicolinearity between variables related to Avg Amount of transactions
cor(d2[,28:36])
# Let's check multicolinearity between variables related to Avg Amount and Amount of transactions
cor(d2[,21:36])

######
## Running Regression Process to check which variable is useful
#mylog = glm(TARGET ~+AMT_OTH_BK_ATM_USG_CHGS+NO_OF_OW_CHQ_BNC_TXNS,data = mydata.dev, family = "binomial")

d2$NO_TXN = d2$NO_OF_BR_CSH_WDL_DR_TXNS+d2$NO_OF_L_CR_TXNS+d2$NO_OF_CHQ_DR_TXNS
d2$AMT_DR = d2$AVG_AMT_PER_CHQ_TXN+d2$AVG_AMT_PER_NET_TXN+d2$AVG_AMT_PER_ATM_TXN+d2$AVG_AMT_PER_MOB_TXN

mylog = glm(TARGET ~+NO_TXN+AMT_DR+FLG_HAS_CC+LEN_OF_RLTN_IN_MNTH+
               GENDER+BALANCE+OCCUPATION+SCR+HOLDING_PERIOD
            ,data = d2, family = "binomial")
summary(mylog)

library(car)
vif(mylog)
#cor(d2[,c(21,22,23,24,25,26,32,33,34,35,36)])

## We need to treat Occupation & Gender Variable

pp <- as.data.frame.matrix(table(d2$OCCUPATION, d2$TARGET))
pp$total <- (pp$`0` + pp$`1`)
pp$rrate <- round(pp$`1` * 100 / (pp$`0` + pp$`1`), 3)
pp

d2$DV_OCC = ifelse ( d2$OCCUPATION %in% c("SAL", "PROF"), "SAL-PROF",
                             ifelse (
                               d2$OCCUPATION %in% c("SELF-EMP", "SENP"), "SELF_EMP-SENP",
                               d2$OCCUPATION
                             )
)
table(d2$DV_OCC)

pp2 <- as.data.frame.matrix(table(d2$GENDER ,d2$TARGET))
pp2$total <- (pp2$`0` + pp2$`1`)
pp2$rrate <- round(pp2$`1` * 100 / (pp2$`0` + pp2$`1`), 3)
pp2

d2$DV_G = ifelse ( d2$GENDER %in% c("F","O"), "F-O",
                           ifelse (
                             d2$GENDER %in% c("M"), "M",
                             d2$GENDER
                           )
)
table(d2$DV_G)
####################################################################################
# 7.Split dataset in Dev-Val-Hold Out samples.
mydata = d2[, names(d2)%in% c("CUST_ID", "NO_TXN","AMT_DR","FLG_HAS_CC","LEN_OF_RLTN_IN_MNTH",
              "DV_G","BALANCE","DV_OCC","SCR","HOLDING_PERIOD","random","TARGET")]
View(mydata)
mydata$random <- runif(nrow(mydata), 0, 1)
mydata.dev <- mydata[which(mydata$random <= 0.6),]
mydata.val <- mydata[which(mydata$random > 0.6 
                           & mydata$random <= 0.8 ),]
mydata.hold <- mydata[which(mydata$random > 0.8),]
nrow(mydata)
nrow(mydata.dev)
nrow(mydata.val)
nrow(mydata.hold)
sum(mydata$TARGET) / nrow(mydata)
sum(mydata.dev$TARGET)/ nrow(mydata.dev)
sum(mydata.val$TARGET)/ nrow(mydata.val)
sum(mydata.hold$TARGET)/ nrow(mydata.hold)

####################################################################################
#8.Build Logistics Regression Model on Dev.
# Model

mylog1 = glm(TARGET ~+NO_TXN+AMT_DR+FLG_HAS_CC+
               DV_G+BALANCE+DV_OCC+SCR+HOLDING_PERIOD
             ,data = mydata.dev, family = "binomial")
summary(mylog1)


#mylog2 = glm(TARGET ~+FLG_HAS_CC+
#           BALANCE+DV_OCC+SCR+HOLDING_PERIOD
#             ,data = mydata.dev, family = "binomial")
#summary(mylog2)

# 9. Ensure No Multi Collinearity between variables.

vif(mylog1)
#vif(mylog2)

#predict = predict(mylog2, newdata = mydata.dev ,type = "response")
#table(mydata.dev$TARGET,predict >0.5)
#(8834+1)/(8834+1+0+1245)

predict1 = predict(mylog1, newdata = mydata.dev, type = "response")
table(mydata.dev$TARGET, predict1 > 0.5)
##################################################################################
#10 .Check Model Performance Measures.

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}


## Rank Ordering Test
## Calculating the probabilities and create deciles
View(mydata.dev)
mydata.dev$prob <- predict(mylog1, mydata.dev, type="response")
mydata.dev$deciles <- decile(mydata.dev$prob)

class(mydata.dev)
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)

tmp_DT = data.table(mydata.dev)
rank <- tmp_DT[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,3);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),3);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),3);
rank$ks <- percent(abs(rank$cum_rel_resp - rank$cum_rel_non_resp));
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

############ Goodness of Fit: ##############
# A function to do the Hosmer-Lemeshow test in R.
# R Function is due to Peter D. M. Macdonald, McMaster University.
head(mydata.dev)
ttt <- data.table(mydata.dev)
library(sqldf)
sqldf('select deciles, count(1) as cnt_cust,
      sum(TARGET) as cnt_resp,
      sum(prob) as est_rep
      from ttt
      group by deciles'
)

hosmerlem <- function (y, yhat, g = 10) 
  {
    cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
                                                             1, 1/g)), include.lowest = T)
    obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
    expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
    chisq <- sum((obs - expect)^2/expect)
    P <- 1 - pchisq(chisq, g - 2)
    c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
  }

hl_gof = hosmerlem(mydata.dev$TARGET, mydata.dev$prob )
hl_gof


#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE***#
concordance=function(y, yhat)
{
  Con_Dis_Data = cbind(y, yhat) 
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}
#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE ENDS***#

concordance_output = concordance(mydata.dev$TARGET, mydata.dev$prob)
concordance_output

mydata.dev$Class = ifelse (mydata.dev$prob>0.2, 1, 0)
table(mydata.dev$TARGET, mydata.dev$Class)
##library(sqldf)
##sqldf('select deciles, count(1) as cnt, 
##    sum(Target) as Obs_Resp, 
##    count(Target==0) as Obs_Non_Resp, 
##    sum(prob) as Exp_Resp,
##    sum(1-prob) as Exp_Non_Resp 
##    from test
##    group by deciles
##    order by deciles desc')



############ GINI Index ##############
#install.packages("ineq")
library(ineq)
gini = ineq(mydata.dev$prob, type="Gini")
gini



### Calculating AUC using ROC Curve and KS for the model
#install.packages("ROCR")
library(ROCR)
pred <- prediction(mydata.dev$prob, mydata.dev$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf, col="green", lwd=2, main="ROC Curve")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
KS
auc

####### MODEL VALIDATION #########
# 11.Validate the Model 

predict1 = predict(mylog1, newdata = mydata.dev, type = "response")
table(mydata.dev$TARGET, predict1 > 0.5)

#library(devtools)
install_github("K2Analytics/logisticmodelcreation")
library(plotly)
library(logisticmodelcreation)
x <- sapply(sapply(ls(), get), is.data.frame)
m <- data.frame(names(x)[(x == TRUE)])
logisticmodelcreation(m)

########################################################################################
# 12.Finally check model performance on Hold Out. 

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## Rank Ordering Test
## Calculating the probabilities and create deciles
View(mydata.hold)
mydata.hold$prob <- predict(mylog1, mydata.hold, type="response")
mydata.hold$deciles <- decile(mydata.hold$prob)

#class(mydata.dev)
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)

tmp_DT_hold = data.table(mydata.hold)
rank_hold <- tmp_DT_hold[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
rank_hold$rrate <- round (rank_hold$cnt_resp / rank_hold$cnt,3);
rank_hold$cum_resp <- cumsum(rank_hold$cnt_resp)
rank_hold$cum_non_resp <- cumsum(rank_hold$cnt_non_resp)
rank_hold$cum_rel_resp <- round(rank_hold$cum_resp / sum(rank_hold$cnt_resp),3);
rank_hold$cum_rel_non_resp <- round(rank_hold$cum_non_resp / sum(rank_hold$cnt_non_resp),3);
rank_hold$ks <- percent(abs(rank_hold$cum_rel_resp - rank_hold$cum_rel_non_resp));
rank_hold$rrate <- percent(rank_hold$rrate)
rank_hold$cum_rel_resp <- percent(rank_hold$cum_rel_resp)
rank_hold$cum_rel_non_resp <- percent(rank_hold$cum_rel_non_resp)

View(rank_hold)

############ Goodness of Fit: ##############
# A function to do the Hosmer-Lemeshow test in R.
# R Function is due to Peter D. M. Macdonald, McMaster University.
head(mydata.hold)
ttt_hold <- data.table(mydata.hold)
library(sqldf)
sqldf('select deciles, count(1) as cnt_cust,
      sum(TARGET) as cnt_resp,
      sum(prob) as est_rep
      from ttt_hold
      group by deciles')

hosmerlem <- function (y, yhat, g = 10) 
{
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
                                                           1, 1/g)), include.lowest = T)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}

hl_gof_hold = hosmerlem(mydata.hold$TARGET, mydata.hold$prob )
hl_gof_hold


#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE***#
concordance=function(y, yhat)
{
  Con_Dis_Data = cbind(y, yhat) 
  ones = Con_Dis_Data[Con_Dis_Data[,1] == 1,]
  zeros = Con_Dis_Data[Con_Dis_Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}
#***FUNCTION TO CALCULATE CONCORDANCE AND DISCORDANCE ENDS***#

concordance_output_hold = concordance(mydata.hold$TARGET, mydata.hold$prob)
concordance_output_hold

mydata.hold$Class = ifelse (mydata.hold$prob>0.2, 1, 0)
table(mydata.hold$TARGET, mydata.hold$Class)
##library(sqldf)
##sqldf('select deciles, count(1) as cnt, 
##    sum(Target) as Obs_Resp, 
##    count(Target==0) as Obs_Non_Resp, 
##    sum(prob) as Exp_Resp,
##    sum(1-prob) as Exp_Non_Resp 
##    from test
##    group by deciles
##    order by deciles desc')



############ GINI Index ##############
#install.packages("ineq")
library(ineq)
gini_hold = ineq(mydata.hold$prob, type="Gini")
gini_hold

### Calculating AUC using ROC Curve and KS for the model
#install.packages("ROCR")
library(ROCR)
pred_hold <- prediction(mydata.hold$prob, mydata.hold$TARGET)
perf_hold <- performance(pred_hold, "tpr", "fpr")
plot(perf_hold, col="green", lwd=2, main="ROC Curve")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

KS_hold <- max(attr(perf_hold, 'y.values')[[1]]-attr(perf_hold, 'x.values')[[1]])
auc_hold <- performance(pred_hold,"auc"); 
auc_hold <- as.numeric(auc_hold@y.values)
KS_hold
auc_hold
