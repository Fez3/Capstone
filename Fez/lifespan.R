library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(tidyverse)
data=read.csv("accepted_2007_to_2018Q4.csv")

Amount<-data %>% ggplot(., x=loan_amnt)+geom_histogram(bins=15, aes(x=loan_amnt, fill=term),col="black")
Month<-data %>% ggplot(.,aes(x=issue_d, fill=term))+geom_bar()

Grade<-data %>% ggplot(.,aes(x=grade, fill=loan_status))+geom_bar()

# Put loan issue dates in a recognizable form for as.date function
data$issue_d=as.Date(paste("01-",as.character( data$issue_d),sep=""), format="%d-%b-%Y")
# Subset by date range and look at loan status distribution
dataQ12014=data[data$issue_d %between% c("2014-01-02", "2014-04-01"),]
dataQ12013=data[data$issue_d %between% c("2013-01-02", "2013-04-01"),]

Status<-dataQ12014 %>% ggplot(., x=loan_amnt)+geom_histogram(bins=15, aes(x=loan_amnt, fill=loan_status),col="black")
Status2<-dataQ12013 %>% ggplot(., x=loan_amnt)+geom_histogram(bins=15, aes(x=loan_amnt, fill=loan_status),col="black")


# Normalize the data to the number of incoming loans
newclients<-data %>% group_by(., issue_d, loan_status, term) %>% summarise(stati=length(loan_status)) %>% spread(loan_status,stati)
total<-data %>% group_by(., issue_d) %>% summarise(.,num_loans=length(issue_d))
newclients<-merge(x=newclients, y=total, by="issue_d")
newclients=newclients[-452,]
newclients[is.na(newclients)]<-0

newk<-data %>% group_by(., issue_d, loan_status) %>% summarise(stati=length(loan_status))
newk=newk[-631,]
newk[is.na(newk)]<-0
newk_frac=copy(newk)
newk_frac<- merge(x=newk_frac, y=total, by="issue_d", all.x =  TRUE)
newk_frac$stati=newk_frac$stati/newk_frac$num_loans
newk_frac[newk_frac$loan_status==unique(newk_frac$loan_status)[1],"loan_status"]<-unique(newk_frac$loan_status)[4]
newk_frac[newk_frac$loan_status==unique(newk_frac$loan_status)[2],"loan_status"]<-unique(newk_frac$loan_status)[3]
newk_frac[newk_frac$loan_status==unique(newk_frac$loan_status)[7],"loan_status"]<-unique(newk_frac$loan_status)[4]


total<-data %>% group_by(., issue_d, term) %>% summarise(.,num_loans=length(issue_d))
newkt1<-data %>% group_by(., issue_d, loan_status, term) %>% summarise(stati=length(loan_status))
newkt1=newkt1[-1024,]
newkt1[is.na(newkt1)]<-0
newkt2=newkt1[newkt1$term==" 60 months",]
newkt1=newkt1[newkt1$term==" 36 months",]
newk_fract1<- merge(x=newkt1, y=total[total$term==" 36 months",], by="issue_d", all.x =  TRUE)
newk_fract1$stati=newk_fract1$stati/newk_fract1$num_loans
newk_fract1[newk_fract1$loan_status==unique(newk_fract1$loan_status)[1],"loan_status"]<-unique(newk_fract1$loan_status)[4]
newk_fract1[newk_fract1$loan_status==unique(newk_fract1$loan_status)[2],"loan_status"]<-unique(newk_fract1$loan_status)[3]
newk_fract1[newk_fract1$loan_status==unique(newk_fract1$loan_status)[7],"loan_status"]<-unique(newk_fract1$loan_status)[1]
newk_fract2<- merge(x=newkt2, y=total[total$term==" 60 months",], by="issue_d", all.x =  TRUE)
newk_fract2$stati=newk_fract2$stati/newk_fract2$num_loans
newk_fract2[newk_fract2$loan_status==unique(newk_fract2$loan_status)[2],"loan_status"]<-unique(newk_fract2$loan_status)[1]
newk_fract2[newk_fract2$loan_status==unique(newk_fract2$loan_status)[2],"loan_status"]<-unique(newk_fract2$loan_status)[3]
newk_fract2[newk_fract2$loan_status==unique(newk_fract2$loan_status)[7],"loan_status"]<-unique(newk_fract2$loan_status)[1]


newk_frac %>% ggplot(.,aes(x=issue_d,y=stati, fill=loan_status, color=loan_status))+geom_bar(stat = "identity")+labs(x="Date of Issue", y="Fractional Composition")+xlim(c(min(newk_frac$issue_d),max(newk_frac$issue_d)))+ylim(0,1)+theme(axis.line = element_line(colour = "black"),
                                                                                                                                                                                                                                           panel.grid.major = element_blank(),
                                                                                                                                                                                                                                           panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                           panel.border = element_blank(),
                                                                                                                                                                                                                                          panel.background = element_blank()) + ggtitle("Status of Loans by 2019")+theme(plot.title = element_text(hjust = 0.5))
plot1<-newk_fract1 %>% ggplot(.,aes(x=issue_d,y=stati, fill=loan_status, color=loan_status))+geom_bar(stat = "identity")+labs(x="Date of Issue", y="Fractional Composition")+xlim(c(min(newk_frac$issue_d),max(newk_frac$issue_d)))+ylim(0,1)+theme(axis.line = element_line(colour = "black"),
                                                                                                                                                                                                                                           panel.grid.major = element_blank(),
                                                                                                                                                                                                                                           panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                           panel.border = element_blank(),
                                                                                                                                                                                                                                           panel.background = element_blank()) + ggtitle("36 Month Loans by 2019")+theme(plot.title = element_text(hjust = 0.5))
plot2<-newk_fract2 %>% ggplot(.,aes(x=issue_d,y=stati, fill=loan_status, color=loan_status))+geom_bar(stat = "identity")+labs(x="Date of Issue", y="Fractional Composition")+xlim(c(min(newk_frac$issue_d),max(newk_frac$issue_d)))+ylim(0,1)+theme(axis.line = element_line(colour = "black"),
                                                                                                                                                                                                                                           panel.grid.major = element_blank(),
                                                                                                                                                                                                                                           panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                           panel.border = element_blank(),
                                                                                                                                                                                                                                           panel.background = element_blank()) + ggtitle("60 Month Loans by 2019")+theme(plot.title = element_text(hjust = 0.5))




require(gridExtra)

grid.arrange(plot1, plot2, ncol=2)





