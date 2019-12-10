analy_dir <- "~/kaykwak/1. Class/00. Capstone project/Lending club analysis/Anlaysis/dataset"

setwd(analy_dir)
list.files()

# Data Loading
accepted <- read.csv("accepted_2007_to_2018Q4.csv")
rejected <- read.csv("rejected_2007_to_2018Q4.csv")

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


## Data status varification
dim(accepted)
summary(accepted)
sapply(accepted, class)
glimpse(accepted)

# Factor -> Date (Only two important columns)
accepted$issue_d_todate <- as.Date(as.yearmon(accepted$issue_d, "%b-%Y"))
accepted$last_pymnt_d_todate <- as.Date(as.yearmon(accepted$last_pymnt_d, "%b-%Y"))


###################### Data Pre-processing & Quick EDA ######################
# (0) Data cleaning for some fields
## important column extraction     (TBD # included all variables related with "hardship","settlement")
imp_var = c('acc_now_delinq','addr_state','annual_inc','application_type','avg_cur_bal','chargeoff_within_12_mths',
            'delinq_2yrs', 'delinq_amnt','dti','emp_length','funded_amnt','funded_amnt_inv','grade','home_ownership',
            'id','installment','int_rate','issue_d_todate','loan_amnt','loan_status','num_accts_ever_120_pd',
            'num_bc_tl','num_il_tl','num_rev_accts','pct_tl_nvr_dlq','pub_rec','pub_rec_bankruptcies','purpose',
            'revol_bal','sub_grade','tax_liens','term','tot_cur_bal','tot_hi_cred_lim','total_acc','total_bal_ex_mort',
            'total_bc_limit','total_cu_tl','total_il_high_credit_limit','total_pymnt','hardship_flag','hardship_type',
            'hardship_reason','hardship_status','hardship_start_date','hardship_end_date',
            'payment_plan_start_date','hardship_loan_status',
            'debt_settlement_flag','debt_settlement_flag_date','settlement_status',
            'settlement_date','last_pymnt_d_todate','desc','all_util','fico_range_high',
            'fico_range_low','inq_fi','last_fico_range_high','last_fico_range_low','max_bal_bc',
            'num_actv_bc_tl','num_actv_rev_tl','num_op_rev_tl','open_acc','out_prncp','title',
            'settlement_amount','settlement_percentage', 'mort_acc','revol_util')

new_accept <- accepted[,imp_var]
colSums(is.na(new_accept))

## Too many NAs (over 70% : 1,582,491) : member_id, hardship_length, hardship_amount, hardship_dpd,
##           hardship_payoff_balance_amount, hardship_last_payment_amount, settlement_amount,
##           settlement_percentage, settlement_term, sec_app_fico_range_low, sec_app_fico_range_high,
##           settlement_amount.1, settlement_percentage.1, settlement_term.1 

## loan_status check
new_accept %>% group_by(., loan_status) %>% summarise(., n())

new_accept$loan_status_new <- new_accept$loan_status
new_accept$loan_status_new[new_accept$loan_status_new == 
                           "Does not meet the credit policy. Status:Charged Off"] <- "Charged Off"
new_accept$loan_status_new[new_accept$loan_status_new == 
                           "Does not meet the credit policy. Status:Fully Paid"] <- "Fully Paid"
new_accept %>% group_by(., loan_status_new) %>% summarise(., n())

## loan_status check
### issue_d NA (33 records) --> remove
sum(is.na(new_accept$issue_d_todate))
new_accept <- new_accept %>% filter(., !is.na(issue_d_todate))


####### Total Dataset analysis  #######

# (1) Loan grade by year(Issue year) 

## grade distribution (bar)
ggplot(data=new_accept) + geom_bar(aes(x=grade), stat='count')
ggplot(data=new_accept) + geom_bar(aes(x=loan_status_new), stat='count')
ggplot(data=new_accept) + geom_bar(aes(x=grade, fill=loan_status_new))
ggplot(data=new_accept) + geom_bar(aes(x=loan_status_new, fill=grade))

analy_accept <- 
  new_accept %>% filter(., loan_status_new == "Fully Paid" | loan_status_new == "Charged Off" )

colSums(is.na(analy_accept))

# Unnecessary or Many NA column remove 
## member_id, loan_status, hardship_amount, hardship_dpd, hardship_length, hardship_payoff_balance_amount, 
## hardship_last_payment_amount, settlement_amount, settlement_percentage, settlement_term
drop_col <- c("member_id", "loan_status", "hardship_amount", "hardship_dpd", "hardship_length", "hardship_payoff_balance_amount", 
              "hardship_last_payment_amount", "settlement_amount", "settlement_percentage", "settlement_term",
              "Issue_d","last_pymnt_d","issue_d")
analy_accept[drop_col] <- NULL   # Finally 67 Variables, 1,348,059 records

## emp_length check (emp_length_num column creation)
analy_accept %>% group_by(., emp_length) %>% summarise(., n())
analy_accept_recent <- analy_accept %>% filter(., year(issue_d_todate) >= '2013') 




######################### Guick EDA with all records ##########################

# 1. Generalized Analysis 
## 1) Distributions (Count)
### (1) Nb. of count by year (issued count)
### (1-1) all data
ggplot(data=new_accept) + geom_bar(aes(x=year(issue_d_todate)), stat='count') +
  xlab("Issue Year") + ggtitle("Count by year (for all issued data)") +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  scale_y_continuous(labels=comma)

### (1-2) Fully paid
count_F <- analy_accept %>% filter(., loan_status_new == "Fully Paid") %>% 
  ggplot(.) + geom_bar(aes(x=year(issue_d_todate)), stat='count') +
  xlab("Issue Year") + ggtitle("Count by year ('fully paid')") +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  scale_y_continuous(labels=comma)

### (1-3) Charged off 
count_C <- analy_accept %>% filter(., loan_status_new == "Charged Off") %>% 
  ggplot(.) + geom_bar(aes(x=year(issue_d_todate)), stat='count', fill="darkblue") +
  xlab("Issue Year") + ggtitle("Count by year ('Charged Off')") +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  scale_y_continuous(labels=comma, limits = c(0, 300000))

grid.arrange(count_F, count_C, nrow = 1)
# ggsave("Number of issued data_year_status.png")

### (2) Nb. of count by year by portion (Fully Paid vs. Charged off)
count_comp <- analy_accept %>% group_by(year(issue_d_todate), loan_status_new) %>% 
  summarise(., status_count = n()) %>% 
  mutate(percentage = status_count / sum(status_count)) 

count_2vari <- ggplot(count_comp, aes(x=`year(issue_d_todate)`, y=status_count, fill=loan_status_new)) + 
  geom_bar(stat="identity") +
  xlab("Issue Year") + ggtitle("Count by year ('fully paid vs. charged off')") +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  scale_y_continuous(labels=comma) + 
  geom_text(data=subset(count_comp, `year(issue_d_todate)` >= 2012), size=4, 
            aes(label=paste(round(percentage*100, digits=0),"%",sep="")),
            position=position_stack(vjust=0.5))

count_r_2vari <- ggplot(count_comp, aes(x=`year(issue_d_todate)`, y=status_count, fill=loan_status_new)) + 
  geom_bar(stat="identity", position="fill") +
  xlab("Issue Year") + ggtitle("Count by year ('fully paid vs. charged off')") +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(size=4, aes(label=paste(round(percentage*100, digits=0),"%",sep=""), y=percentage), 
            position=position_stack(vjust=0.5))

grid.arrange(count_2vari, count_r_2vari, nrow = 1)


## 2) Loan Amount view
### (1) Average loan_amt by year (Charge-off)
analy_accept %>% 
  group_by(., issue_year=year(issue_d_todate), loan_status=loan_status_new) %>% 
  summarise(., amt_avg = mean(loan_amnt)) %>% 
  
  ggplot(., aes(x=issue_year, y=amt_avg, fill=loan_status)) + 
  geom_bar(stat="identity", position ="dodge") +
  xlab("Issue Year") + ylab("Average amt") +
  ggtitle("Average Loan amount by status by year ") +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values = c("red", "darkgray"))

ggsave("Avg loan by year_status.png")


### (2) Loan_amt distribution by status by year (Box plot, recent year)
analy_accept %>% filter(., year(issue_d_todate) >= 2013) %>% 
  ggplot(., aes(x=as.factor(year(issue_d_todate)), y=loan_amnt, 
                fill=loan_status_new)) +
  scale_y_continuous(name = "Loan Amount") + 
  ggtitle("Loan amount distribution by year/status") +
  scale_fill_manual(values = c("red", "darkgray"))

ggsave("Box plot.png")



## 3) Loan grade vs. loan status 
### (1) 
ggplot(data=new_accept) + geom_bar(aes(x=grade), stat='count')  # referential
ggplot(data=new_accept) + geom_bar(aes(x=grade, fill=loan_status_new)) +
  ggtitle("Grade vs. Loan status") +
  scale_y_continuous(labels=comma)
ggsave("Grade_Loan_Status (by grade).png")
ggplot(data=new_accept) + geom_bar(aes(x=loan_status_new), stat='count') # referential
ggplot(data=new_accept) + geom_bar(aes(x=loan_status_new, fill=grade)) # referential

##### Additional (Grade vs. status - for all data by %)
all.grade.vs.status <- new_accept %>% 
  select(., grade, loan_status_new) %>% 
  group_by(., grade, loan_status_new) %>% summarise(., count_grade=n()) %>% 
  mutate(., count_grade_percent = count_grade / sum(count_grade))

ggplot(all.grade.vs.status, aes(x=grade, y=count_grade, fill=loan_status_new)) + 
  geom_bar(stat="identity") + xlab("Grade") +
  ggtitle("Grade vs. Loan status (Percentage)") + 
  geom_text(
    size=3, aes(label=paste(round(count_grade_percent*100, digits=0),"%",sep="")), 
    # y=percentage, 
    position=position_stack(vjust=0.5), fontface = "bold")

ggsave("Grade vs. Loan status (add Percent).png")


ggplot(all.grade.vs.status, aes(x=grade, y=count_grade, fill=loan_status_new)) + 
  geom_bar(stat="identity", position="fill") + xlab("Grade") +
  ggtitle("Grade vs. Loan status (Percentage)") + 
  geom_text(
    size=3, aes(label=paste(round(count_grade_percent*100, digits=0),"%",sep=""),
                y=count_grade_percent),
    position=position_stack(vjust=0.5),
    fontface = "bold")

ggsave("Grade vs. Loan status (Percentage).png")


## loan_status_analy (with grade.. Only for "Fully Paid","Charged Off") 
grade.vs.status <- analy_accept %>% 
  select(., grade, loan_status_new) %>% 
  group_by(., loan_status_new, grade) %>% summarise(., count_grade=n()) %>% 
  mutate(., count_grade_percent = count_grade / sum(count_grade))

ggplot(grade.vs.status, aes(x=loan_status_new, y=count_grade, fill=grade)) + 
  geom_bar(stat="identity") + xlab("Loan Status") +
  ggtitle("Grade by Loan status") + 
  geom_text(
    size=3, aes(label=paste(round(count_grade_percent*100, digits=0),"%",sep="")), 
    # y=percentage, 
    position=position_stack(vjust=0.5), fontface = "bold")

ggsave("Grade_Loan_Status (by status).png")



## grade rate distribution (by year)
grade_comp <- analy_accept %>% 
  group_by(issue_year=year(issue_d_todate), grade, loan_status_new) %>% 
  summarise(., grade_status_count = n()) %>% 
  mutate(grade_percentage = grade_status_count / sum(grade_status_count)) 

par(mfrow = c(2,2))
## Fully paid by year (with grade.. for "Fully Paid") 
g3 <-
  ggplot(subset(grade_comp,loan_status_new=="Fully Paid"),
         aes(x=issue_year, y=grade_status_count, fill=grade)) + 
  geom_bar(stat="identity", position="fill") +
  ggtitle("Grade ratio for 'Fully Paid' by year") +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  scale_y_continuous(labels=scales::percent) 

## Charged off by year (with grade.. for "Charged off") 
g4 <- 
  ggplot(subset(grade_comp,loan_status_new=="Charged Off"),
         aes(x=issue_year, y=grade_status_count, fill=grade)) + 
  geom_bar(stat="identity", position="fill") +
  ggtitle("Grade ratio for 'Charged Off' by year") +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) +
  scale_y_continuous(labels=scales::percent) 

grid.arrange(g3, g4, nrow = 1)



## 4) Interest Rate vs. loan status 

analy_accept %>% filter(., year(issue_d_todate) >= 2013) %>% 
  ggplot(., aes(x=as.factor(year(issue_d_todate)), y=int_rate, 
                fill=loan_status_new)) +
  geom_boxplot() + scale_x_discrete(name = "Loan Issue year") +
  scale_y_continuous(name = "Interest Rate") + 
  ggtitle("Loan Interest Rate by year/status") +
  scale_fill_manual(values = c("red", "darkgray"))

ggsave("int_Box_plot_by_year.png")


analy_accept %>% filter(., year(issue_d_todate) >= 2013) %>% 
  ggplot(., aes(x=loan_status_new, y=int_rate, fill=loan_status_new)) +
  geom_boxplot() + scale_x_discrete(name = "Loan Status") +
  scale_y_continuous(name = "Interest Rate") + 
  ggtitle("Loan Interest Rate by status") +
  scale_fill_manual(values = c("red", "darkgray"))

ggsave("int_Box_plot_status.png")


## 5) Interest Rate vs. grade

analy_accept %>% filter(., year(issue_d_todate) >= 2013) %>% 
  ggplot(., aes(x=as.factor(year(issue_d_todate)), y=int_rate, 
                fill=grade)) +
  geom_boxplot() + scale_x_discrete(name = "Loan Issue year") +
  scale_y_continuous(name = "Interest Rate") + 
  ggtitle("Loan Interest Rate by year/grade")

ggsave("int_Box_plot_by_grade_by_year.png")


analy_accept %>% filter(., year(issue_d_todate) >= 2013) %>% 
  ggplot(., aes(x=grade, y=int_rate,  fill=grade)) +
  geom_boxplot() + scale_x_discrete(name = "Grade") +
  scale_y_continuous(name = "Interest Rate") + 
  ggtitle("Loan Interest Rate by grade")

ggsave("Loan Interest Rate by grade.png")



######################### Correlation analysis (X vs. X, X vs. Y) ###################

#### Simple imputation ( Median ) ##### 
clean1.recent <- analy_accept_recent
clean1.recent$avg_cur_bal[is.na(clean1.recent$avg_cur_bal)] <- mean(clean1.recent$avg_cur_bal, na.rm=T)
clean1.recent$dti[is.na(clean1.recent$dti)] <- mean(clean1.recent$dti, na.rm=T)
clean1.recent$num_rev_accts[is.na(clean1.recent$num_rev_accts)] <- mean(clean1.recent$num_rev_accts, na.rm=T)
clean1.recent$pct_tl_nvr_dlq[is.na(clean1.recent$pct_tl_nvr_dlq)] <- mean(clean1.recent$pct_tl_nvr_dlq, na.rm=T)
clean1.recent$total_cu_tl[is.na(clean1.recent$total_cu_tl)] <- mean(clean1.recent$total_cu_tl, na.rm=T)
clean1.recent$all_util[is.na(clean1.recent$all_util)] <- mean(clean1.recent$all_util, na.rm=T)
clean1.recent$inq_fi[is.na(clean1.recent$inq_fi)] <- mean(clean1.recent$inq_fi, na.rm=T)
clean1.recent$max_bal_bc[is.na(clean1.recent$max_bal_bc)] <- mean(clean1.recent$max_bal_bc, na.rm=T)
clean1.recent$revol_util[is.na(clean1.recent$revol_util)] <- mean(clean1.recent$revol_util, na.rm=T)

sapply(clean1.recent, FUN = function(x) sum(is.na(x)))
sapply(clean1.recent,class)

######################### Class Check  #############################
sapply(clean1.recent,class)

######################### Correlation ##########################
## Corr Analysis (among Xs)  # Only for numeric variables
num.analy <- clean1.recent[sapply(clean1.recent, is.numeric)]
num.analy2 <- cbind(num.analy, clean1.recent$loan_status_new)
names(num.analy2)[names(num.analy2) == "clean1.recent$loan_status_new"] <- c("loan_status_new")

var.cor <- cor(subset(num.analy2, select=-loan_status_new),  use="complete.obs")
## Standard deviation is "0" for sum_fileld 
sapply(subset(num.analy2, select=-loan_status_new),sd)
# unique value of "out_prncp" is "0" --> delete the column
num.analy2$out_prncp <- NULL
var.cor <- cor(subset(num.analy2, select=-loan_status_new),  use="complete.obs")
corrplot.mixed(var.cor, number.cex = 0.4, tl.cex=0.5, tl.pos = "lt", order = "hclust")
ggsave("corrplot_Xs.png")
corr.x.result <- cor(var.cor)
write.csv(corr.x.result, "corr.x.result.csv")


## Corr Analysis (X vs. Y)  
# library(CorrToolBox)
library(polycor)
library(gdata)

levels(num.analy2$loan_status_new)
num.analy2$loan_status_new <- factor(num.analy2$loan_status_new)
num.analy2.X <- subset(num.analy2, select = -c(loan_status_new))

prop.table(table(num.analy2$loan_status_new))
polyserial(num.analy2$acc_now_delinq, num.analy2$loan_status_new)

polyserial(num.analy2.X[,2], num.analy2$loan_status_new)

corr_result <- c()
for (i in 1:ncol(num.analy2.X)) {
  corr <- polyserial(num.analy2.X[,i], num.analy2$loan_status_new)
  corr_result <- append(corr_result, paste(names(num.analy2.X[i]), corr, sep=" : "))
  print(names(num.analy2.X[i]))
}

corr_result
