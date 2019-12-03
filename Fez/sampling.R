
library(dplyr)
library(ggplot2)
library(tidyr)
data=read.csv("loans_clean_complete.csv")

# Remove all incomplete loans, and other status (40 "default", etc) 
#data=data[which(data$loan_status %in% c("Fully Paid","Charged Off")),] #2260701 obs-> 1345310 obs

# Put loan issue dates in a recognizable form for as.date function
#data$issue_d=as.Date(paste("01-",as.character( data$issue_d),sep=""), format="%d-%b-%Y")

#268,559 charged Off to 1,076,751 Fully paid
# 19.96% charged off. Sampling weights 0.2 and 0.8 to get equal sample sizes
nrow(data[which(data$loan_status =="Fully Paid"),])
nrow(data[which(data$loan_status =="Charged Off"),])

data<-data %>% mutate(., wt=0.19)
data[which(data$loan_status =="Charged Off"),"wt"]<-0.81
nsample=200000
set.seed(1)
samp_idx <- sample(seq_len(nrow(data)), nsample, prob=data$wt)
new_data <- data[samp_idx, ]


nrow(new_data[which(new_data$loan_status =="Fully Paid"),])
nrow(new_data[which(new_data$loan_status =="Charged Off"),])

new_data$loan_status=as.character(new_data$loan_status)
new_data[new_data["loan_status"]=="Fully Paid","loan_status"]=0
new_data[new_data["loan_status"]=="Charged Off","loan_status"]=1

nrow(new_data[which(new_data$loan_status ==0),])
nrow(new_data[which(new_data$loan_status ==1),])
new_data=new_data[,-ncol(new_data)]
write.csv(new_data, "complete_sample.csv")

# Training set of 90% test set of 10%
nsample=180000
set.seed(1)
train  <- sample(seq_len(nrow(new_data)), nsample)
test = (-train)
testdata = new_data[test,]
traindata=new_data[train,]

nrow(testdata[which(testdata$loan_status ==0),])
nrow(testdata[which(testdata$loan_status ==1),])

nrow(traindata[which(traindata$loan_status ==0),])
nrow(traindata[which(traindata$loan_status ==1),])

write.csv(testdata, "test.csv")
write.csv(traindata, "train.csv")





