analy_dir <- "~/kaykwak/1. Class/00. Capstone project/Lending club analysis/Anlaysis/dataset"

setwd(analy_dir)

library(dplyr)
library(ggplot2)
data=read.csv("accepted_2007_to_2018Q4.csv")

# Remove all incomplete loans, and other status (40 "default", etc) 
data=data[which(data$loan_status %in% c("Fully Paid","Charged Off")),] #2260701 obs-> 1345310 obs

# Look at distributions of loan sizes and dates
Amount<-data %>% ggplot(., x=loan_amnt)+geom_histogram(bins=15, aes(x=loan_amnt, fill=term),col="black")
Month<-data %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()

# Put loan issue dates in a recognizable form for as.date function
data$issue_d=as.Date(paste("01-",as.character( data$issue_d),sep=""), format="%d-%b-%Y")

# Visualize a single year of loans
data2016=data[which(data$issue_d> "2015-12-30" & data$issue_d<"2016-12-30"),]
Month16<-data2016 %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()


#268,559 charged Off to 1,076,751 Fully paid
# 19.96% charged off. Sampling weights 0.2 and 0.8 to get equal sample sizes
nrow(data[which(data$loan_status =="Fully Paid"),])
nrow(dataC=data[which(data$loan_status =="Charged Off"),])

data<-data %>% mutate(., wt=0.2)
data[which(data$loan_status =="Charged Off"),"wt"]<-0.8
nsample=200000
samp_idx <- sample(seq_len(nrow(data)), nsample, prob=data$wt)
new_data <- data[samp_idx, ]

# Look at sample statistics
AmountS<-new_data %>% ggplot(., x=loan_amnt)+geom_histogram(bins=15, aes(x=loan_amnt, fill=term),col="black")
MonthS<-new_data %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()
MonthS16<-new_data[which(new_data$issue_d> "2015-12-30" & new_data$issue_d<"2016-12-30"),] %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()
nrow(new_data[which(new_data$loan_status =="Fully Paid"),])
nrow(new_data[which(new_data$loan_status =="Charged Off"),])

# 107,000 fully (53.5%)
# 93,000 charged off (46.5% --> 46.1%)

prop.table(table(new_data$loan_status))


# NA Ratio
na <- function(x){sum(is.na(x))/length(x)*100}
na_ratio <- data.frame(apply(new_data, 2, na))

na_ratio <- cbind(var.nm = rownames(na_ratio), na_ratio)
rownames(na_ratio) <- 1:nrow(na_ratio)
colnames(na_ratio) = c("var.na", "var.na.ratio")
na_ratio_over.70 <- na_ratio[na_ratio$var.na.ratio > 70,1]
## Manuallyn proceed afterward

remove.many.na <- c('member_id','mths_since_last_record','mths_since_last_major_derog','annual_inc_joint',
'dti_joint','mths_since_recent_bc_dlq','revol_bal_joint','sec_app_fico_range_low','sec_app_fico_range_high',
'sec_app_inq_last_6mths','sec_app_mort_acc','sec_app_open_acc','sec_app_revol_util',
'sec_app_open_act_il','sec_app_num_rev_accts','sec_app_chargeoff_within_12_mths','sec_app_collections_12_mths_ex_med',
'sec_app_mths_since_last_major_derog','deferral_term','hardship_amount','hardship_length',
'hardship_dpd','orig_projected_additional_accrued_interest','hardship_payoff_balance_amount','hardship_last_payment_amount',
'settlement_amount','settlement_percentage','settlement_term')

new_data_kwak <- new_data
new_data_kwak[remove.many.na] = NULL
sapply(new_data_kwak, FUN = function(x) sum(is.na(x)))
sapply(new_data_kwak, class)

numeric.new.data <- new_data_kwak[sapply(new_data_kwak, is.numeric)]
factor.data <- new_data_kwak[sapply(new_data_kwak, is.factor)]
# date.data <- new_data_kwak[sapply(new_data_kwak, is.Date)]

# Imputation by MI (MICE)
library(mice)
library(VIM)

# impute.data <- mice(new_data_kwak, seed=1234, m=5)
# impute.data
# new_data_impute <- complete(impute.data,1)

# Numeric only
# impute.data <- mice(numeric.new.data, seed=1234, m=5)
# impute.data
# num.data.impute <- complete(impute.data,1)

Sys.time()
impute.data2 <- mice(numeric.new.data, seed=1234, m=1)
impute.data2
num.data.impute <- complete(impute.data2,1)
sapply(num.data.impute, FUN = function(x) sum(is.na(x)))
sapply(factor.data, FUN = function(x) sum(is.na(x)))
sum(is.na(num.data.impute))

prop.table(table(factor.data$loan_status))

all.data.imputed <- cbind(num.data.impute, factor.data, new_data_kwak$issue_d)
write.csv(all.data.imputed, "all.data.imputed.csv")

prop.table(table(all.data.imputed$loan_status))

summary(new_data)
new_data.tmp <- new_data[sapply(new_data, is.factor)]
summary(new_data.tmp)
