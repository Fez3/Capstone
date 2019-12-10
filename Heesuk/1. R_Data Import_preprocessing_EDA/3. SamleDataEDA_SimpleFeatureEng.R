# Library 
library(dplyr)
library(ggplot2)
library(caret)
library(lubridate)
library(zoo)
library(scales)
library(gridExtra)
library(tidyr)
library(mice)
library(corrplot)
library(randomForest)
library(Hmisc)
library(DMwR)

analy_dir <- "~/kaykwak/1. Class/00. Capstone project/Lending club analysis/Anlaysis/dataset"
setwd(analy_dir)

## Data loading ##
all.data.impute <- read.csv("all.data.imputed.csv", na.strings="")

names(all.data.impute)[names(all.data.impute) == "new_data_kwak.issue_d"] <- c("issue_d")

all.data.impute$issue_d <- as.Date(as.yearmon(all.data.impute$issue_d, "%Y-%m-%d"))
prop.table(table(all.data.impute$loan_status))

######################### Class #######################
sapply(all.data.impute, class)

######################### Correlation ##########################
all.data.impute.num <- all.data.impute[sapply(all.data.impute, is.numeric)]

## Corr Analysis (among Xs)  # Only for numeric variables
#### SD is Zero (Value is only "0" :  out_prncp  out_prncp_inv policy_code  num_tl_120dpd_2m)
#### No need for corr : id, X, wt
del.var <- c("out_prncp", "out_prncp_inv", "policy_code", "num_tl_120dpd_2m", "id", "X", "wt")
sapply(all.data.impute.num, sd)
all.data.impute.num[del.var] <- NULL
all.data.impute[del.var] <- NULL
summary(all.data.impute.num)

# num.analy2$out_prncp <- NULL
corr.x.result <- cor(all.data.impute.num)
write.csv(corr.x.result, "corr.x.result.csv")

## Corr Analysis (X vs. Y)  
# library(CorrToolBox)
library(polycor)
library(gdata)
all.data.impute.num.Y <- cbind(all.data.impute.num, all.data.impute$loan_status)
names(all.data.impute.num.Y)[names(all.data.impute.num.Y) == "all.data.impute$loan_status"] <- c("loan_status")
levels(all.data.impute.num.Y$loan_status)
prop.table(table(all.data.impute.num.Y$loan_status))

corr_result <- c()
for (i in 1:(ncol(all.data.impute.num.Y)-1)) {
  corr <- polyserial(all.data.impute.num.Y[,i], all.data.impute.num.Y$loan_status)
  corr_result <- append(corr_result, paste(names(all.data.impute.num.Y[i]), corr, sep=" : "))
  print(paste(i, names(all.data.impute.num.Y[i])))
}
corr_result
write.csv(corr_result, "corr_XY.csv")

corr.xy <- read.csv("corr_XY_plot.csv")
corr.xy <- corr.xy %>% arrange(., desc(corr.y)) %>% 
  mutate(., colour = ifelse(corr.y >= 0, "positive.corr","negative.corr"))

ggplot(corr.xy, aes(x=reorder(x.variable,corr.y), y=corr.y)) + 
  geom_bar(stat="identity", aes(fill = colour)) + coord_flip() +   
  theme(plot.title=element_text(hjust=0.3, face="bold"), axis.text=element_text(size=6)) + 
  xlab("X variables") + ylab("Correlation with Y") +
  scale_fill_manual(values = c("positive.corr" = "darkblue", "negative.corr" = "darkred")) +
  ggtitle("[ Corrreation between Y and numeric X (polyserial) ]") 

# abs(Corr.) over 0.5
corr.xy %>% filter(., abs(corr.y) >= 0.5) %>% 
ggplot(., aes(x=reorder(x.variable,corr.y), y=corr.y)) + 
  geom_bar(stat="identity", aes(fill = colour)) + coord_flip() +   
  geom_text(data=subset(corr.xy, abs(corr.y)>=0.5),aes(label=round(corr.y,2)),
            fontface="bold", vjust=0, hjust=-0.1) +
  scale_fill_manual(values = c("positive.corr" = "darkblue", "negative.corr" = "darkred")) +
  theme( legend.position = "bottom", 
         plot.title=element_text(face="bold", hjust=0.5), 
        axis.text=element_text(size=10, face="bold"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  xlab("X variables") + ylab("Correlation with Y") +
 ggtitle("[ Corrreation between Y and numeric X (polyserial) ]") 

summary(all.data.impute)
write.csv(all.data.impute, "all.data.impute2.csv")

############################ Data after imputation ##########################
## Just before removing highly correlated variables 
write.csv(all.data.impute2, "all.data.impute.csv")

sum(is.na(all.data.impute))
sapply(all.data.impute, FUN = function(x) sum(is.na(x)))

############################## Note ####################################
# Numeric correlation (Removal for correlation issue)
## Issue group & process 
### 4 : loan_amnt,	installment, funded_amnt,	funded_amnt_inv  
####     ==> remove 2 (funded_amnt, funded_amnt_inv) : Followed Yan's comment (but Keep 2, check later)
### 3 : total_pymnt	total_pymnt_inv	total_rec_prncp
####     ==> remove 2 (total_pymnt_inv	total_rec_prncp) : Followed Yan's comment 
### 2 : num_rev_tl_bal_gt_0	num_actv_rev_tl
####     ==> remove 1 (num_actv_rev_tl) : Followed Yan's comment 
### 3 : num_il_tl	num_op_rev_tl	num_rev_accts (No correlation issue, but remove(not important))
####     ==> remove 3 (num_il_tl	num_op_rev_tl	num_rev_accts) : Followed Yan's comment 
### 2 : out_prncp, out_prncp_inv (Already removed --> SD is 0)
####     ==> remove 2 (out_prncp, out_prncp_inv) : Followed Yan's comment 
### 1 : num_rev_tl_bal_gt_0 (Keep this variable)
####     ==> not remove : Just remove num_actv_rev_tl (High correlation with num_rev_tl_bal_gt_0)

## + Others for numeric variables (Kwak)
### fico_range_low	fico_range_high
####     ==> remove 2 & create 2 new variables : mean(fico_range_low, fico_range_high), 
####                                             range(fico_range_high - fico_range_low)
### 2 : num_sats	open_acc (Correlation : 0.9959)
####     ==> remove 1 (num_sats) : little lower corr with Y
### 2 : recoveries	collection_recovery_fee (Correlation : 0.966)
####     ==> remove 1 (collection_recovery_fee) : little lower corr with Y
### 2 : tot_cur_bal	tot_hi_cred_lim (Correlation : 0.975)
####     ==> remove 1 (tot_hi_cred_lim) : distribution is more skewed 
### 3 : ttotal_bal_il	total_bal_ex_mort	total_il_high_credit_limit (Correlation : 0.961)
####     ==> remove 2 (total_bal_ex_mort	total_il_high_credit_limit) : distribution is more skewed 

## + Others for category variables (refer to "excel file")
all.data.impute.factor <- all.data.impute[sapply(all.data.impute, is.factor)]
# category_result <- apply(all.data.impute.factor,MARGIN=2,table) 


#############################################################################
############################# Feature Enginering #############################
# 1. Feature Creation
## Numerical variable 
### (1) fico 
all.data.impute.2 <- 
  all.data.impute.2 %>%
  mutate(., fico_range_mean = (fico_range_low + fico_range_high)/2)
## Range(fico_range_high-fico_range_low) doesn't mean (99.99% is "4", rest "5")

## Categorical variable
### (2) sub_grade
subgrade <- read.csv("subgrade.csv", na.strings = "")
all.data.impute.2 <- left_join(all.data.impute.2, subgrade, by="sub_grade")
# "sub_grade_num" column created

### (3) emp_length
all.data.impute.2$emp_length_num <- all.data.impute.2$emp_length
all.data.impute.2$emp_length_num <- gsub("< 1",0,all.data.impute.2$emp_length_num)
all.data.impute.2$emp_length_num <- gsub('[+]',"",all.data.impute.2$emp_length_num)
all.data.impute.2$emp_length_num <- gsub(" years","",all.data.impute.2$emp_length_num)
all.data.impute.2$emp_length_num <- gsub(" year","",all.data.impute.2$emp_length_num)

all.data.impute.2$emp_length_num <- as.numeric(all.data.impute.2$emp_length_num )
## Need to impute again

# 2. Feature Deletion
corr.del <- c("funded_amnt", "funded_amnt_inv", "num_sats", "total_pymnt_inv",
              "total_rec_prncp", "collection_recovery_fee", "tot_hi_cred_lim", "total_bal_ex_mort",
              "total_il_high_credit_limit", "num_actv_rev_tl",
              "num_il_tl",	"num_op_rev_tl",	"num_rev_accts",
              "fico_range_low", "fico_range_high")
factor.del <- c('sub_grade', 'emp_length', 'pymnt_plan',
                'hardship_flag','emp_title','url','desc','title','zip_code',
                'earliest_cr_line','last_pymnt_d','last_credit_pull_d','issue_d',
                'verification_status_joint','sec_app_earliest_cr_line',
                'hardship_type','hardship_reason','hardship_status','hardship_start_date',
                'hardship_end_date','payment_plan_start_date','hardship_loan_status',
                'debt_settlement_flag_date','settlement_status','settlement_date',
                'next_pymnt_d')

all.data.impute.2[corr.del] <- NULL
all.data.impute.2[factor.del] <- NULL

sapply(all.data.impute.2, FUN = function(x) sum(is.na(x)))

numeric.df.impute <- all.data.impute.2[sapply(all.data.impute.2, is.numeric)]
factor.df.impute <- all.data.impute.2[sapply(all.data.impute.2, is.factor)]

## New Imputation for emp_length_num
Sys.time()
numeric.impute <- mice(numeric.df.impute, seed=1234, m=4)
numeric.impute
final.df.numeric <- complete(numeric.impute,1)
sum(is.na(final.df.numeric))


## Merge
final.df <- cbind(final.df.numeric, loan_status=factor.df.impute[,"loan_status"])

# setNames(data.frame(b), c('u1', 'u2')))

sum(is.na(final.df))
write.csv(final.df, "final.df.csv")


################################ retry (after initialization) ###########################
final.df <- read.csv("final.df.csv", na.strings="")
colnames(final.df)
final.df.numeric <- final.df[sapply(final.df, is.numeric)]
final.df.factor <- final.df[sapply(final.df, is.factor)]

######### Corr Analysis (X vs. Y) #########
# library(CorrToolBox)
library(polycor)
library(gdata)

final.df.numeric.Y <- cbind(final.df.numeric, loan_status=final.df[,"loan_status"])
class(final.df.numeric.Y$loan_status)

levels(final.df.numeric.Y$loan_status)
prop.table(table(final.df.numeric.Y$loan_status))

corr_result <- c()
for (i in 1:(ncol(final.df.numeric.Y)-1)) {
  corr <- polyserial(final.df.numeric.Y[,i], final.df.numeric.Y$loan_status)
  corr_result <- append(corr_result, paste(names(final.df.numeric.Y[i]), corr, sep=" : "))
  print(paste(i, names(final.df.numeric.Y[i])))
}
corr_result
write.csv(corr_result, "corr_XY2.csv")

corr.xy <- read.csv("corr_XY_plot2.csv")
corr.xy <- corr.xy %>% arrange(., desc(corr.y)) %>% 
  mutate(., colour = ifelse(corr.y >= 0, "positive.corr","negative.corr"))

ggplot(corr.xy, aes(x=reorder(x.variable,corr.y), y=corr.y)) + 
  geom_bar(stat="identity", aes(fill = colour)) + coord_flip() +   
  theme(plot.title=element_text(hjust=0.3, face="bold"), axis.text=element_text(size=6)) + 
  xlab("X variables") + ylab("Correlation with Y") +
  scale_fill_manual(values = c("positive.corr" = "darkblue", "negative.corr" = "darkred")) +
  ggtitle("[ Corrreation between Y and numeric X (polyserial) ]") 

# abs(Corr.) over 0.5
corr.xy %>% filter(., abs(corr.y) >= 0.5) %>% 
  ggplot(., aes(x=reorder(x.variable,corr.y), y=corr.y)) + 
  geom_bar(stat="identity", aes(fill = colour)) + coord_flip() +   
  geom_text(data=subset(corr.xy, abs(corr.y)>=0.5),aes(label=round(corr.y,2)),
            fontface="bold", vjust=0, hjust=-0.1) +
  scale_fill_manual(values = c("positive.corr" = "darkblue", "negative.corr" = "darkred")) +
  theme( legend.position = "bottom", 
         plot.title=element_text(face="bold", hjust=0.5), 
         axis.text=element_text(size=10, face="bold"), 
         plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  xlab("X variables") + ylab("Correlation with Y") +
  ggtitle("[ Corrreation between Y and numeric X (polyserial) ]") 


## Corr Analysis (among Xs)  # Only for numeric variables
sapply(final.df.numeric, sd)
corr.x.result <- cor(final.df.numeric)
write.csv(corr.x.result, "corr.x.result_after.clean.csv")

