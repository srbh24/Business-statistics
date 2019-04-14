setwd("C:/Users/Saurabh/Downloads")

#library(xlsx)
library(dplyr)
library(readxl)
library("MASS")
library(car)
library(caret)

ads_creation <- function(Dataset)
{
  #Converting name to lower case
  Dataset$CAND_NAME <- tolower(Dataset$CAND_NAME)
  Dataset$first_name <- gsub("([A-Za-z]+).*", "\\1", Dataset$CAND_NAME)
  
  Dataset$AC_NAME <- tolower(Dataset$AC_NAME)
  Dataset$NameConst <- paste0(Dataset$first_name,":",Dataset$AC_NAME)
  Dataset$PARTYABBRE <- tolower(Dataset$PARTYABBRE)
  Dataset$NameParty <- paste0(Dataset$first_name,":",Dataset$PARTYABBRE)

   cand_merge1 <- merge(Dataset, cand_list_clean[,!names(cand_list_clean) %in% c("NameParty","first_name")],
                       by = "NameConst", all.x = T)
  
  cand_merge2 <- merge(Dataset, cand_list_clean[,!names(cand_list_clean) %in% c("NameConst","first_name")],
                       by = "NameParty", all.x = T)
  
  cand_merge_tot <- rbind(cand_merge1[,!names(cand_merge1) %in% c("NameConst","first_name","NameParty")],
                          cand_merge2[,!names(cand_merge2) %in% c("NameConst","first_name","NameParty")])
  
  cand_merge_tot <- cand_merge_tot[!duplicated(cand_merge_tot),]
  cand_merge_tot <- cand_merge_tot[complete.cases(cand_merge_tot),]
  cand_merge_tot_1 <- cand_merge_tot[cand_merge_tot$AC_NAME==cand_merge_tot$Constituency & cand_merge_tot$PARTYABBRE==cand_merge_tot$Party,]
  
  cand_merge_tot_2 <- cand_merge_tot[cand_merge_tot$AC_NAME==cand_merge_tot$Constituency & cand_merge_tot$PARTYABBRE!=cand_merge_tot$Party,]
  cand_merge_tot_2 <- cand_merge_tot_2[cand_merge_tot_2$CAND_NAME==cand_merge_tot_2$Candidate,]
  
  cand_merge_all <- rbind(cand_merge_tot_1,cand_merge_tot_2)
  cand_merge_all$CAND_NAME[duplicated(cand_merge_all$CAND_NAME)]
  
  #-----------------------------------------------------------------------------------------
  
  cand_merge <- merge(Dataset, cand_list_clean[,!names(cand_list_clean) %in% c("NameConst","first_name","NameParty")],
                      by.x = "CAND_NAME",by.y = "Candidate", all.x = T)
  cand_merge$Candidate <- NA
  cand_merge <- cand_merge[,!names(cand_merge) %in% c("NameConst","first_name","NameParty")]
  
  missing_cand <- cand_merge[cand_merge$CAND_NAME %in% cand_merge$CAND_NAME[!cand_merge$CAND_NAME %in% cand_merge_all$CAND_NAME],]
  missing_cand <- missing_cand[!is.na(missing_cand$Education),]
  
  cand_merge_all <- rbind(cand_merge_all,missing_cand)
  cand_merge_all <- cand_merge_all[,!names(cand_merge_all) == "Candidate"]
  cand_merge_all <- cand_merge_all[complete.cases(cand_merge_all),]
  
  #---------------------------------------------------------------------------------------
  
  cand_final <- cand_merge_all[,names(cand_merge_all) %in% c("YEAR","DIST_NAME",
                                                             "AC_NO",  "AC_NAME","AC_TYPE","CAND_NAME","CAND_SEX",
                                                             "CAND_AGE","PARTYABBRE","TOTVOTPOLL","POSITION",
                                                             "Criminal Case","Education","Total Assets","Liabilities")]
  
  #---------------------------------------------------------------------------------------
  
  cand_final_1 <- cand_final %>%
    rowwise() %>%
    mutate(total_assets = as.numeric(gsub(",", "", strsplit(`Total Assets`, "\\s+")[[1]][2])),
           liabilities = as.numeric(gsub(",", "", strsplit(Liabilities, "\\s+")[[1]][2])))
  
  cand_final_1 <- cand_final_1[,!names(cand_final_1) %in% c("Total Assets","Liabilities")]
  
  
  #---------------------------------------------------------------------------------------
  factor_data <- data.frame(sapply(cand_final_1[,c("DIST_NAME","AC_TYPE","CAND_NAME","CAND_SEX","PARTYABBRE","Education")], FUN = as.factor))
  
  names(cand_final_1)[colnames(cand_final_1) %in% c("DIST_NAME","AC_TYPE","CAND_NAME","CAND_SEX","PARTYABBRE","Education")] <- c("DIST_NAME.x","AC_TYPE.x","CAND_NAME.x","CAND_SEX.x","PARTYABBRE.x","Education.x")
  cand_final_2 <- cbind(cand_final_1,factor_data)
  
  cand_final_2
}

#-----------------------------------------------------------------------------------------
#Reading the candidates details
cand_list <- read_excel("Candidates_v2.xlsx")
summary(cand_list)
names(cand_list)
dim(cand_list)

names(cand_list)[colnames(cand_list)=="Candidate???"] <- "Candidate"
cand_list_clean <- cand_list[complete.cases(cand_list),]
cand_list_clean$Candidate <- tolower(cand_list_clean$Candidate)

cand_list_clean$first_name <- gsub("([A-Za-z]+).*", "\\1", cand_list_clean$Candidate)
cand_list_clean$Constituency <- tolower(cand_list_clean$Constituency)
cand_list_clean$NameConst <- paste0(cand_list_clean$first_name,":",cand_list_clean$Constituency)

cand_list_clean$Party <- tolower(cand_list_clean$Party)
cand_list_clean$NameParty <- paste0(cand_list_clean$first_name,":",cand_list_clean$Party)

#----------------------------------------------------------------------------------------


#reading the election results from 2012
cand_res_2012 <- read_excel("AE2012_8913.xls", sheet = 

#filtering for Goa
cand_res_2012_goa <- cand_res_2012 %>% filter(ST_NAME == "Goa")
cand_train <- ads_creation(cand_res_2012_goa)
must_convert<-sapply(cand_train,is.factor)
M2<-data.frame(sapply(cand_train[,must_convert],unclass))

train_out<-cbind(cand_train[,!names(cand_train)%in% c("DIST_NAME.x","AC_TYPE.x","CAND_NAME.x","CAND_SEX.x","PARTYABBRE.x","Education.x","DIST_NAME","AC_TYPE","CAND_NAME","CAND_SEX","PARTYABBRE","Education")],M2)   
mapping<-cbind(cand_train[,names(cand_train)%in% c("DIST_NAME.x","AC_TYPE.x","CAND_NAME.x","CAND_SEX.x","PARTYABBRE.x","Education.x")],M2)

#removing totvotpoll becuase this is also a dependant variable
train_out <- train_out[,!names(train_out) %in% c("AC_NAME","YEAR","TOTVOTPOLL")]

train_out$POSITION <- if_else(train_out$POSITION==1,1,0)

mapping_dist <- mapping[,c("DIST_NAME.x","DIST_NAME")]
mapping_dist <- mapping_dist[!duplicated(mapping_dist),]

mapping_ac <- mapping[,c("AC_TYPE.x","AC_TYPE")]
mapping_ac <- mapping_ac[!duplicated(mapping_ac),]

mapping_name <- mapping[,c("CAND_NAME.x","CAND_NAME")]
mapping_name <- mapping_name[!duplicated(mapping_name),]

mapping_sex <- mapping[,c("CAND_SEX.x","CAND_SEX")]
mapping_sex <- mapping_sex[!duplicated(mapping_sex),]

mapping_party <- mapping[,c("PARTYABBRE.x","PARTYABBRE")]
mapping_party <- mapping_party[!duplicated(mapping_party),]

mapping_edu <- mapping[,c("Education.x","Education")]
mapping_edu <- mapping_edu[!duplicated(mapping_edu),]
#----------------------------------------------------------------------------------------
set.seed(100)
model_1 = glm(POSITION ~ ., data = train_out, family = "binomial")
summary(model_1) #AIC 4150.1....31 coeff..nullDev 5699.5...resDev 4102.1

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

#Excluding cand.sex
model_3<- glm(formula = POSITION ~ total_assets+PARTYABBRE , family = "binomial", data = train_out) 

summary(model_3) 

vif(model_3) 

#-----------------------------------------------------------------------------------------
#Reading the candidates details for 2017
cand_list <- read_excel("Candidates_2017.xlsx")
summary(cand_list)
names(cand_list)
dim(cand_list)
names(cand_list)[colnames(cand_list)=="Candidate???"] <- "Candidate"
cand_list_clean <- cand_list[complete.cases(cand_list),]
cand_list_clean$Candidate <- tolower(cand_list_clean$Candidate)

cand_list_clean$first_name <- gsub("([A-Za-z]+).*", "\\1", cand_list_clean$Candidate)
cand_list_clean$Constituency <- tolower(cand_list_clean$Constituency)
cand_list_clean$NameConst <- paste0(cand_list_clean$first_name,":",cand_list_clean$Constituency)

cand_list_clean$Party <- tolower(cand_list_clean$Party)
cand_list_clean$NameParty <- paste0(cand_list_clean$first_name,":",cand_list_clean$Party)

#---------------------------------------------------------------------------------------

cand_res_2017 <- read_excel("LA 2017.xls", sheet = 1)
summary(cand_res_2017)
names(cand_res_2017)
dim(cand_res_2017)

names(cand_res_2017)[colnames(cand_res_2017) == "TOTALVALIDVOTESPOLLED"] <- "TOTVOTPOLL"

#filtering for Goa
cand_res_2017_goa <- cand_res_2017 %>% filter(ST_NAME == "Goa")
cand_test <- ads_creation(cand_res_2017_goa)
must_convert<-sapply(cand_test,is.factor)
M2<-cand_test[,!must_convert]

test_out<- merge(M2,mapping_ac,by = "AC_TYPE.x",all.x = T)
test_out<- merge(test_out,mapping_dist,by = "DIST_NAME.x", all.x = T)
test_out<- merge(test_out,mapping_edu,by = "Education.x", all.x = T)
test_out<- merge(test_out,mapping_name,by = "CAND_NAME.x", all.x = T)
test_out<- merge(test_out,mapping_party,by = "PARTYABBRE.x", all.x = T)
test_out<- merge(test_out,mapping_sex,by = "CAND_SEX.x", all.x = T)

test_out <- test_out[,!names(test_out)%in% c("DIST_NAME.x","AC_TYPE.x","CAND_NAME.x","CAND_SEX.x","PARTYABBRE.x","Education.x")]
test_out <- test_out[!is.na(test_out$PARTYABBRE),]


#removing totvotpoll becuase this is also a dependant variable
test_out <- test_out[,!names(test_out) %in% c("AC_NAME","YEAR","TOTVOTPOLL")]

test_out$POSITION <- if_else(test_out$POSITION==1,1,0)


test_pred = predict(model_3, type = "response", 
                    newdata = test_out[,!names(test_out)%in% "POSITION"])


# Let's see the summary 

summary(test_pred)

test_out$prob <- test_pred
View(test_out)
# Let's use the probability cutoff of 50%.

test_pred_churn <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_churn <- factor(ifelse(test_out$POSITION==1,"Yes","No"))


table(test_actual_churn,test_pred_churn)


#######################################################################
test_pred_churn <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

library(e1071)

test_conf <- confusionMatrix(test_pred_churn, test_actual_churn, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_churn, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_churn <- factor(ifelse(test_pred >=0.2, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_churn, test_actual_churn, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test_out)



#----------------------------------------------------------------------------------------
#---------------------------This code is for vote share------------------------------------
#----------------------------------------------------------------------------------------

# Lets load the library in which stepAIC function exists
# install.packages("MASS")
library(MASS)

elec_sum_2012 <- read_excel("AE2012_8913.xls", sheet = 2)
summary(elec_sum_2012)
names(elec_sum_2012)
dim(elec_sum_2012)

elec_sum_2012_goa <- elec_sum_2012 %>% filter(ST_NAME == "Goa")

elec_sum_2012_goa <- elec_sum_2012_goa[,names(elec_sum_2012_goa) %in% c("AC_NO","TOT_VOTERS","TOT_ELECTORS","POLL_PERCENT")]

must_convert<-sapply(cand_train,is.factor)
M2<-data.frame(sapply(cand_train[,must_convert],unclass))
mapping<-cbind(cand_train[,names(cand_train)%in% c("DIST_NAME.x","AC_TYPE.x","CAND_NAME.x","CAND_SEX.x","PARTYABBRE.x","Education.x")],M2)

train_out<-cbind(cand_train[,!names(cand_train)%in% c("DIST_NAME.x","AC_TYPE.x","CAND_NAME.x","CAND_SEX.x","PARTYABBRE.x","Education.x","DIST_NAME","AC_TYPE","CAND_NAME","CAND_SEX","PARTYABBRE","Education")],M2) 

#removing totvotpoll becuase this is also a dependant variable
train_out <- train_out[,!names(train_out) %in% c("AC_NAME","YEAR")]

train_out$POSITION <- if_else(train_out$POSITION==1,1,0)

elect_mer <- merge(train_out,elec_sum_2012_goa,by="AC_NO")
elect_mer$Vote_share <- elect_mer$TOTVOTPOLL/elect_mer$TOT_VOTERS

train <- elect_mer[,!names(elect_mer) %in% c("TOT_ELECTORS","POLL_PERCENT","TOTVOTPOLL","TOT_VOTERS","POSITION")]

# Build model 1 containing all variables
model_1 <-lm(Vote_share~.,data=train)
summary(model_1)
#######

step <- stepAIC(model_1, direction="both")


step

# Let's execute this model here, 
model_2 <- lm(formula = Vote_share ~ AC_NO + `Criminal Case` + total_assets + 
                DIST_NAME + CAND_SEX + PARTYABBRE, data = train)
# Let us look at the summary of the model
summary(model_2)


vif(model_2)

# Let's execute this model here, 
model_3 <- lm(formula = Vote_share ~ AC_NO + `Criminal Case` + total_assets + 
                + CAND_SEX + PARTYABBRE, data = train)
# Let us look at the summary of the model
summary(model_3)


vif(model_3)

# Let's execute this model here, 
model_4 <- lm(formula = Vote_share ~ `Criminal Case` + total_assets + 
                + CAND_SEX + PARTYABBRE, data = train)
# Let us look at the summary of the model
summary(model_4)


vif(model_4)

# Let's execute this model here, 
model_5 <- lm(formula = Vote_share ~ total_assets + 
                + CAND_SEX + PARTYABBRE, data = train)
# Let us look at the summary of the model
summary(model_5)


vif(model_5)

#------------------------------------------------------------------------------------
elec_sum_2017 <- read_excel("LA 2017.xls", sheet = 2)

elec_sum_2017_goa <- elec_sum_2017 %>% filter(ST_NAME == "Goa")

elec_sum_2017_goa <- elec_sum_2017_goa[,names(elec_sum_2017_goa) %in% c("AC_NO","TOTAL VOTES POLLED")]

#removing totvotpoll becuase this is also a dependant variable
must_convert<-sapply(cand_test,is.factor)
M2<-cand_test[,!must_convert]

test_out<- merge(M2,mapping_ac,by = "AC_TYPE.x",all.x = T)
test_out<- merge(test_out,mapping_dist,by = "DIST_NAME.x", all.x = T)
test_out<- merge(test_out,mapping_edu,by = "Education.x", all.x = T)
test_out<- merge(test_out,mapping_name,by = "CAND_NAME.x", all.x = T)
test_out<- merge(test_out,mapping_party,by = "PARTYABBRE.x", all.x = T)
test_out<- merge(test_out,mapping_sex,by = "CAND_SEX.x", all.x = T)

test_out <- test_out[,!names(test_out)%in% c("DIST_NAME.x","AC_TYPE.x","CAND_NAME.x","CAND_SEX.x","PARTYABBRE.x","Education.x")]
test_out <- test_out[!is.na(test_out$PARTYABBRE),]

test_out$POSITION <- if_else(test_out$POSITION==1,1,0)

elect_mer_17 <- merge(test_out,elec_sum_2017_goa,by="AC_NO")
elect_mer_17$Vote_share <- elect_mer_17$TOTVOTPOLL/elect_mer_17$`TOTAL VOTES POLLED`

test <- elect_mer_17[,!names(elect_mer_17) %in% c("TOTAL VOTES POLLED","POSITION","YEAR","AC_NAME","TOTVOTPOLL")]
test <- test[!is.na(test$total_assets),]
Predict_1 <- predict(model_5,test[,!names(test) %in% "Vote_share"])
test$test_vote_share <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$Vote_share,test$test_vote_share)
rsquared <- cor(test$Vote_share,test$test_vote_share)^2
rsquared