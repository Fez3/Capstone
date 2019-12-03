
library(dplyr)
library(ggplot2)
library(tidyr)
data=read.csv("accepted_2007_to_2018Q4.csv")

# Remove all incomplete loans, and other status (40 "default", etc) 
data=data[which(data$loan_status %in% c("Fully Paid","Charged Off")),] #2260701 obs-> 1345310 obs

# Look at distributions of loan sizes and dates
Amount<-data %>% ggplot(., x=loan_amnt)+geom_histogram(bins=15, aes(x=loan_amnt, fill=term),col="black")
Month<-data %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()

Grade<-data %>% ggplot(.,aes(x=grade, fill=loan_status))+geom_bar()

# Put loan issue dates in a recognizable form for as.date function
data$issue_d=as.Date(paste("01-",as.character( data$issue_d),sep=""), format="%d-%b-%Y")

# Visualize a single year of loans
data2016=data[which(data$issue_d> "2015-12-30" & data$issue_d<"2017-12-30"),]
# Checking how date as factor affects output
#data2016$issue_d=as.factor(format(data2016$issue_d,format="%b-%Y" ))
Month16<-data2016 %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()


#268,559 charged Off to 1,076,751 Fully paid
# 19.96% charged off. Sampling weights 0.2 and 0.8 to get equal sample sizes
nrow(data[which(data$loan_status =="Fully Paid"),])
nrow(dataC=data[which(data$loan_status =="Charged Off"),])

data<-data %>% mutate(., wt=0.2)
data[which(data$loan_status =="Charged Off"),"wt"]<-0.8
nsample=200000
set.seed(0)
samp_idx <- sample(seq_len(nrow(data)), nsample, prob=data$wt)
new_data <- data[samp_idx, ]

# Look at sample statistics
AmountS<-new_data %>% ggplot(., x=loan_amnt)+geom_histogram(bins=15, aes(x=loan_amnt, fill=term),col="black")
MonthS<-new_data %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()
MonthS16<-new_data[which(new_data$issue_d> "2015-12-30" & new_data$issue_d<"2016-12-30"),] %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()
GradeS<-new_data %>% ggplot(.,aes(x=grade, fill=loan_status))+geom_bar()
nrow(new_data[which(new_data$loan_status =="Fully Paid"),])
nrow(new_data[which(new_data$loan_status =="Charged Off"),])


# Extracting key parameters from the complete data set of completed loans
# Probability of default PD(grade,year)=sum[grade, year](Charged Off/nloans)
# Loss given default LGD and exposure at default EAD will be estimated at 80%*[remaining balance]

PDtable<-data %>% group_by(., grade, loan_status, as.factor(format(data$issue_d,format="%Y" ))) %>% summarise(., n=n()) %>% spread(loan_status, n, fill=0)
colnames(PDtable)=c("grade", "Year", "def", "paid")
PDtable<-PDtable %>% mutate(., "PD"=100*def/(def+paid))

loss<-data %>% group_by(., grade, loan_status, as.factor(format(data$issue_d,format="%Y" ))) %>% summarise(., "loss"=mean(100*((loan_amnt-0.99*total_pymnt)/loan_amnt)),n=n()) %>% spread(loan_status,n,fill=0)
PDtable["loss"]<-loss$loss[loss$`Charged Off`>0]
PDtable["gain"]<-loss$loss[loss$`Charged Off`==0]
PDtable["expected_profit"]=-(PDtable$PD*PDtable$loss/100+(100-PDtable$PD)*PDtable$gain/100)


# Observe the rate of return for later years
PDtable %>% group_by(., grade)%>%summarise(., "ROIC"=mean(expected_profit)) %>% ggplot(.,aes(x=grade, y=ROIC))+geom_bar(stat="identity")
PDtable %>% group_by(., Year)%>%summarise(., "ROIC"=mean(expected_profit)) %>% ggplot(.,aes(x=Year, y=ROIC))+geom_bar(stat="identity")
