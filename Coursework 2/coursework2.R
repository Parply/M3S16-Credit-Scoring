#required libs
library("ggplot2")#plotting
library("reshape2")#reshaping dataframes
library("Hmisc")#pvalues
library("dplyr")#pipe operator
library("InformationValue")#information value
library("forcats")#combining factors
library("rpart")#tree models
library("MASS")#stepAIC

set.seed(1729)#keep results of runs the same


str(D1)#see variable types
summary(D1)#summarise dataframe

par(mfrow=c(3,3))#make 3x3 space in plotting device
for (i in 1:ncol(D1)){
  try(hist(as.numeric(D1[which(!is.na(D1[,i])),i]),main = sprintf("Histogram of %s",colnames(D1)[i]),breaks = 30))#plot histograms of variables
  
}


D1 <- D1[!is.na(D1$avg_cur_bal),]#remove if avg_cur_bal is NA

d <- D1 %>% group_by(addr_state) %>% tally()#tally number of each state 
d <- d[order(d$n),]#order by size
d %>% print(n = Inf)#print


info <- c()#create place to store info values
addr <- D1$addr_state
for (i in 1:50){#loop for total number of states
  info <- c(info, IV(addr,D1$def_flag))#store and calculate information value
  addr <- fct_collapse(D1$addr_state, Other=paste(d$addr_state[1:i]))#collect smallest remaining state factor into the other factor
  
}
plot(1:50,info)#plot IV

D1_trans <- D1#create dataframe of transformed predictors

D1_trans$addr_state <- fct_collapse(D1$addr_state, Other=paste(d$addr_state[1:7]))#create addr_state other factor
D1_trans$addr_state <- WOE(D1_trans$addr_state,D1_trans$def_flag)#calculate their WOE

D1_trans$term <- as.factor(D1$term)#set as factor
D1_trans$purpose_p <- as.factor(D1$purpose_p)#set purpose as factor typw
D1_trans$grade <- as.factor(D1$grade)#set grade as factor type

#removal of unused levels
D1_trans$home_ownership <- factor(D1$home_ownership)
D1_trans$verification_status <- factor(D1$verification_status)
D1_trans$initial_list_status <- factor(D1$initial_list_status)
D1_trans$issue_d <- factor(D1$issue_d)

#scale predictors by log
D1_trans$loan_amnt <- log(D1$loan_amnt)
D1_trans$int_rate <- log(D1$int_rate)
D1_trans$annual_inc <- log(D1$annual_inc+1)
D1_trans$revol_bal <- log(D1$revol_bal+1)
D1_trans$avg_cur_bal <- log(D1$avg_cur_bal+1)
D1_trans$revol_bal <- log(D1$avg_cur_bal+1) 
D1_trans$delinq_amnt <- log(D1$delinq_amnt+1)
D1_trans$mo_sin_old_rev_tl_op <- log(D1$mo_sin_old_rev_tl_op+1)
D1_trans$mo_sin_rcnt_rev_tl_op <- log(D1$mo_sin_rcnt_rev_tl_op+1)
D1_trans$mo_sin_rcnt_tl <- log(D1$mo_sin_rcnt_tl+1)
D1_trans$pub_rec_bankruptcies <- log(D1$pub_rec_bankruptcies+1)
D1_trans$pub_rec <- log(D1$pub_rec+1)
D1_trans$mort_acc <- log(D1$mort_acc+1)
D1_trans$num_accts_ever_120_pd <- log(D1$num_accts_ever_120_pd+1)
D1_trans$num_actv_bc_tl <- log(D1$num_actv_bc_tl+100)
D1_trans$num_actv_rev_tl <- log(D1$num_actv_rev_tl+100)
D1_trans$num_bc_sats <- log(D1$num_bc_sats+100)
D1_trans$open_acc <- log(D1$open_acc+1)
D1_trans$total_acc <- log(D1$total_acc+1)
D1_trans$total_rev_hi_lim <- log(D1$total_rev_hi_lim+1)

#imputation of emp_length_p by regression
D1_trans_wo_NA <- D1_trans[which(!is.na(D1_trans$emp_length_p)),]#create dataframe with no NA for emp_length_p
D1_trans_NA <- D1_trans[which(is.na(D1_trans$emp_length_p)),]#create dataframe where emp_length_p is na

pvals_emp <- rcorr(as.matrix(sapply(D1_trans_wo_NA,as.numeric)))$P[5,-5] #calculate pvals 
sig_emp <- pvals_emp[which(pvals_emp<0.05)]#find predictors significant to emp_length_p

model_emp1 <- glm(as.formula(paste("emp_length_p~",paste(names(sig_emp),collapse = '+'))),data = D1_trans_wo_NA)#create model to predict emp_length_p

D1_trans_NA$emp_length_p <- round(predict(model_emp1,D1_trans_NA))#round values of predicted emp_length_p
D1_trans_NA$emp_length_p[which(D1_trans_NA$emp_length_p>10)] <- 10#set values above 10 to 10
D1_trans_NA$emp_length_p[which(D1_trans_NA$emp_length_p<0)] <- 0#set values below 0 to 0

D1_trans <- rbind(D1_trans_NA,D1_trans_wo_NA)[sample(nrow(D1_trans)),]#recreate data frame

D1_trans$emp_length_p <- as.numeric(D1_trans$emp_length_p)#set as numeric

D1_trans$dummy <- D1_trans$emp_length_p#create dummy variable
D1_trans$dummy[which(D1_trans$dummy != 10)] <- 0#0 if not 10+
D1_trans$dummy[which(D1_trans$dummy == 10)] <- 1#1 if 10+
D1_trans$dummy <- as.factor(D1_trans$dummy)#set as factor type

D1_trans_wo_NA$dummy <- D1_trans_wo_NA$emp_length_p
D1_trans_wo_NA$dummy[which(D1_trans_wo_NA$dummy != 10)] <- 0#0 if not 10+
D1_trans_wo_NA$dummy[which(D1_trans_wo_NA$dummy == 10)] <- 1#1 if 10+
D1_trans_wo_NA$dummy <- as.factor(D1_trans_wo_NA$dummy)#set as factor type


par(mfrow=c(3,3))#make 3x3 space in plotting device
for (i in 1:ncol(D1_trans)){
  hist(as.numeric(D1_trans[,i]),main = sprintf("Histogram of transformed %s",colnames(D1_trans)[i]))#plot histograms of variables
  
}





D1_num <- data.frame(lapply(D1_trans,as.numeric))#convert all to numeric type

correlation <- cor(D1_num) #calculate correlations
melted_cor <- melt(correlation) #use melt to reshape correlation matrix into a dataframe for ggplot
melted_cor[is.na(melted_cor)] <- 0#set NA's as zero



#create a heatmap of the correlation matrix in ggplot
corplot <- ggplot(data=melted_cor, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + xlab("") + ylab("") +ggtitle("Correlation Heatmap") #create plot using geom_tile and assaign labels and title
corplot + geom_text(aes(Var2, Var1, label = round(value,2)),
                    color = "black", size = 2) + theme(axis.text.x = element_text(angle = 90),
                                                       plot.title = element_text(hjust = 0.5)) + scale_fill_gradientn(colours = rainbow(20)) #modify the text angle, colour and add corelation values onto each tile

pvals <- rcorr(as.matrix(D1_num))$P #calculate pvals
pvals_def <- pvals[1,-1]#keep only the values relating to def
sig <- pvals_def[pvals_def < 0.05]#remove values that are not significant with alpha=0.05

names(sig)#significant predictors

sig_formula <- as.formula(paste("def_flag~",paste(names(sig),collapse = '+')))#create formula consisting of just significant predictors


roc <- function(r,p,s="",plot=T){#make function to calculate roc and auc
  yav <- rep(tapply(r, p, mean), table(p))
  rocx <- cumsum(yav)
  rocy <- cumsum(1 - yav)
  area <- sum(yav * (rocy - 0.5 * (1 - yav)))
  x1 <- c(0, rocx)/sum(r)#calculate FPR
  y1 <- c(0, rocy)/sum(1 - r)#Calculate TPR
  auc <- area/(sum(r) * sum(1 - r))#Calculate AUC
  if(plot==T){
    plot(x1,y1,"l", main=s, xlab="FPR", ylab="TPR")#plot
  }
  return(list(x1=x1,y1=y1,auc=auc))
}

#create models for step function
model_def1_full <- glm(def_flag~.,data = D1_trans,family = binomial)#create model of all predictors
model_def1_empty <- glm(sig_formula,data = D1_trans,family = binomial)#create model of just significant predictors
#use step AIC starting with sig predictors to add and remove values to improve the model based on AIC 
stepAIC(model_def1_empty,list(lower=formula(model_def1_empty),upper=formula(model_def1_full)),direction = "both")#step() in base R works in the same way


aucglm <- matrix(NA,nrow = 5,ncol=2)#create place to store aucs
colnames(aucglm) <- c("auc_all","auc_sig")#name columns

for (i in 1:5){#bootstrap 5 times
  bootsample <- sample(nrow(D1_trans),nrow(D1_trans), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans), bootsample)#create validation data indicies
  
  model <- glm(def_flag~.,data = D1_trans[bootsample,])#model all predictors
  model2 <- glm(sig_formula,data = D1_trans[bootsample,])#model sig predictors
  
  #create predictions
  predictions <- predict(model, D1_trans[outofbag,])
  predictions2<- predict(model, D1_trans[outofbag,])
  
  #calculate and store AUCs
  aucglm[i,1] <-roc(D1_trans$def_flag[outofbag],predictions,plot=F)$auc
  aucglm[i,2] <- roc(D1_trans$def_flag[outofbag],predictions2,plot=F)$auc
  
}
summary(aucglm)#summarise AUCs




aucglm_seg <- matrix(NA,nrow = 5,ncol=2)#create place to store AUCs segmented model
colnames(aucglm_seg) <- c("auc_NA","auc_wo_NA")#name columns

for (i in 1:5){#bootstrap 5 times
  bootsample <- sample(nrow(D1_trans_NA),nrow(D1_trans_NA), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans_NA), bootsample)#create validation data indicies
  
  #create NA model
  model_NA <- glm(as.formula(paste("def_flag~",paste(names(sig)[which(names(sig) != "emp_length_p" & names(sig) != "dummy")],collapse = '+'))),data = D1_trans_NA[bootsample,])
  
  #create predictions
  predictions_NA <- predict(model_NA, D1_trans_NA[outofbag,])
  
  aucglm_seg[i,1] <-roc(D1_trans_NA$def_flag[outofbag],predictions_NA,plot=F)$auc#calculate and store AUC
  
  #not NA
  bootsample <- sample(nrow(D1_trans_wo_NA),nrow(D1_trans_wo_NA), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans_wo_NA), bootsample)#create validation data indicies
  
  #create model for not NA
  model_other <- glm(sig_formula,data = D1_trans_wo_NA[bootsample,])
  
  #create predictions
  predictions_other <- predict(model_other, D1_trans_wo_NA[outofbag,])
  
  aucglm_seg[i,2] <-roc(D1_trans_wo_NA$def_flag[outofbag],predictions_other,plot=F)$auc#calculate and store AUC
}
summary(aucglm_seg)#summarise AUCs



auctree <- matrix(NA, nrow = 5,ncol = 6)#create place to store AUC
colnames(auctree) <- c("auc_unpruned","auc_pruned","auc_NA","auc_NA_pruned","auc_wo_NA","auc_wo_NA_pruned")#name columns
for (i in 1:5){#bootstrap 5 times
  #unsegmented
  bootsample <- sample(nrow(D1_trans),nrow(D1_trans), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans), bootsample)#create validation data indicies
  
  fit <- rpart(sig_formula,data = D1_trans[bootsample,],control = rpart.control(minsplit = 2,minbucket = 1, cp=0.001))#create tree
  pfit<- prune(fit, cp=0.001)#prune tree
  
  prediction <- predict(fit,newdata = D1_trans[outofbag,])#predict
  pprediction <- predict(pfit,newdata = D1_trans[outofbag,])#predict pruned
  
  #calculate and store AUCs
  auctree[i,1] <- roc(D1_trans$def_flag[outofbag],prediction,plot=F)$auc
  auctree[i,2] <- roc(D1_trans$def_flag[outofbag],pprediction,plot=F)$auc
  
  #NA
  bootsample <- sample(nrow(D1_trans_NA),nrow(D1_trans_NA), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans_NA), bootsample)#create validation data indicies
  
  #create tree
  fit_NA <- rpart(as.formula(paste("def_flag~",paste(names(sig)[which(names(sig) != "emp_length_p" & names(sig) != "dummy")],collapse = '+'))),data = D1_trans_NA[bootsample,],control = rpart.control(minsplit = 2,minbucket = 1, cp=0.001))
  pfit_NA<- prune(fit_NA, cp=0.001)#prune
  
  #predict
  prediction <- predict(fit_NA,newdata = D1_trans_NA[outofbag,])
  pprediction <- predict(pfit_NA,newdata = D1_trans_NA[outofbag,])
  
  #calculate and store AUC
  auctree[i,3] <- roc(D1_trans_NA$def_flag[outofbag],prediction,plot=F)$auc
  auctree[i,4] <- roc(D1_trans_NA$def_flag[outofbag],pprediction,plot=F)$auc
  
  #wo NA
  bootsample <- sample(nrow(D1_trans_wo_NA),nrow(D1_trans_wo_NA), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans_wo_NA), bootsample)#create validation data indicies
  
  #create tree
  fit_wo_NA <- rpart(sig_formula,data = D1_trans_wo_NA[bootsample,],control = rpart.control(minsplit = 2,minbucket = 1, cp=0.001))
  pfit_wo_NA <- prune(fit_wo_NA, cp=0.001)#prune
  
  #predict
  prediction <- predict(fit_wo_NA,newdata = D1_trans_wo_NA[outofbag,])
  pprediction <- predict(pfit_wo_NA,newdata = D1_trans_wo_NA[outofbag,])
  
  #calculate and store AUC
  auctree[i,5] <- roc(D1_trans_wo_NA$def_flag[outofbag],prediction,plot=F)$auc
  auctree[i,6] <- roc(D1_trans_wo_NA$def_flag[outofbag],pprediction,plot=F)$auc
}
summary(auctree)#summarise AUCs

#plot tree of unsegmented model
par(oma=c(0,0,2,0))
plot(fit, uniform=TRUE, 
     main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


aucint <- matrix(NA, nrow=5,ncol=3)#create place to store AUCs
colnames(aucint) <- c("auc_int","auc_int_NA","auc_int_wo_NA")#name columns
for (i in 1:5){#bootstrap 5 times
  #unsegmented model
  bootsample <- sample(nrow(D1_trans),nrow(D1_trans), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans), bootsample)#create validation data indicies
  
  #create model with interaction terms
  model_int <- glm(as.formula(paste("def_flag~",paste(names(sig),collapse = '+'),"+int_rate*annual_inc+int_rate*revol_bal")),data = D1_trans[bootsample,])
  
  predictions <- predict(model_int, D1_trans[outofbag,])#predict
  
  aucint[i,1] <- roc(D1_trans$def_flag[outofbag],predictions,plot=F)$auc#calculate and store AUC
  
  #NA
  bootsample <- sample(nrow(D1_trans_NA),nrow(D1_trans_NA), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans_NA), bootsample)#create validation data indicies
  
  #create model
  model_int_NA <- glm(as.formula(paste("def_flag~",paste(names(sig)[which(names(sig) != "emp_length_p" & names(sig) != "dummy")],collapse = '+'),"+int_rate*annual_inc+int_rate*revol_bal")),data = D1_trans_NA[bootsample,])
  
  predictions <- predict(model_int_NA, D1_trans_NA[outofbag,])#predict
  
  aucint[i,2] <- roc(D1_trans_NA$def_flag[outofbag],predictions,plot=F)$auc#calculate and store AUC
  
  #verified
  bootsample <- sample(nrow(D1_trans_wo_NA),nrow(D1_trans_wo_NA), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans_wo_NA), bootsample)#create validation data indicies
  
  #create model
  model_int_wo_NA <- glm(as.formula(paste("def_flag~",paste(names(sig),collapse = '+'),"+int_rate*annual_inc+int_rate*revol_bal")),data = D1_trans_wo_NA[bootsample,])
  
  predictions <- predict(model_int_wo_NA, D1_trans_wo_NA[outofbag,])#predict
  
  aucint[i,3] <- roc(D1_trans_wo_NA$def_flag[outofbag],predictions,plot=F)$auc#calculate and store AUC
  
  
}
summary(aucint)#summarise AUC


#question 2

#split transformed data frame by loan term
D1_trans_36 <- D1_trans[which(D1_trans$term=="36"),]
D1_trans_60 <- D1_trans[which(D1_trans$term=="60"),]

aucglm_term <- matrix(NA,nrow = 5,ncol=2)#create place to store AUCs
colnames(aucglm_term) <- c("auc_36","auc_60")#name columns
for (i in 1:5){#bootstrap 5 times
  #term=36
  bootsample <- sample(nrow(D1_trans_36),nrow(D1_trans_36), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans_36), bootsample)#create validation data indicies
  
  #create model
  model_36 <- glm(as.formula(paste("def_flag~",paste(names(sig)[which(names(sig) != "term")],collapse = '+'))),data = D1_trans_36[bootsample,])
  
  predictions_36 <- predict(model_36, D1_trans_36[outofbag,])#predict
  
  aucglm_term[i,1] <-roc(D1_trans_36$def_flag[outofbag],predictions_36,plot=F)$auc#calculate and store AUC
  
  #term=60
  bootsample <- sample(nrow(D1_trans_60),nrow(D1_trans_60), replace = T)#take sample indicies
  outofbag <- setdiff(1:nrow(D1_trans_60), bootsample)#create validation data indicies
  
  #create model
  model_60 <- glm(as.formula(paste("def_flag~",paste(names(sig)[which(names(sig) != "term")],collapse = '+'))),data = D1_trans_60[bootsample,])
  
  predictions_60 <- predict(model_60, D1_trans_60[outofbag,])#predict
  
  aucglm_term[i,2] <-roc(D1_trans_60$def_flag[outofbag],predictions_60,plot=F)$auc#calculate and store AUC
  
  
}
summary(aucglm_term)#summarise



IV(D1_trans$term,D1_trans$def_flag)#IV term++


#create plot comparing all

AUC <- cbind(aucglm,aucglm_seg,auctree,aucint,aucglm_term)
auc_melted <- melt(AUC)#melt to reshape and turn to data frame

#create bar plot of AUCs
ggplot(auc_melted, aes(x=Var2, y=value, fill=Var2)) + geom_boxplot(colour=rainbow(13)) + coord_flip()  + theme(legend.position="none") + xlab("Model") + ylab("AUC") + ggtitle("Comparison of AUCs") #crate boxplot of all techniques mses

summary(model_int)









































































