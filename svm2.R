library(e1071)
#library(ggplot2)
bank_data = read.csv("bank.csv",head=TRUE,sep=";")
bank_loan_process<-bank_data
bank_data_sorted<-ifelse(bank_loan_process$loan=="yes",1,0)
bank_loan_process$loan<-bank_data_sorted
bank_data_sorted<-ifelse(bank_loan_process$housing=="yes",1,0)
bank_loan_process$housing<-bank_data_sorted
bank_data_sorted<-ifelse(bank_loan_process$marital=="married",1,ifelse(bank_loan_process$marital=="single",2,3))
bank_loan_process$marital<-bank_data_sorted
education_unique<-unique(bank_loan_process$education)
bank_loan_process$education=as.numeric(match(bank_loan_process$education,education_unique))
job_unique<-unique(bank_loan_process$job)
bank_loan_process$job=as.numeric(match(bank_loan_process$job,job_unique))
contact_unique<-unique(bank_loan_process$contact)
bank_loan_process$contact=as.numeric(match(bank_loan_process$contact,contact_unique))
default_unique<-unique(bank_loan_process$default)
bank_loan_process$default=as.numeric(match(bank_loan_process$default,default_unique))
poutcome_unique<-unique(bank_loan_process$poutcome)
bank_loan_process$poutcome=as.numeric(match(bank_loan_process$poutcome,poutcome_unique))
#y_unique<-unique(sort(bank_loan_process$y))
#bank_loan_process$y=as.numeric(match(bank_loan_process$y,y_unique))
month_unique<-unique(sort(bank_loan_process$month))
bank_loan_process$month=as.numeric(match(bank_loan_process$month,month_unique))
index <- 1:nrow(bank_loan_process)
test_data_index <- sample(index, trunc(length(index)/3))
testdata <- bank_loan_process[test_data_index,]
traindata <- bank_loan_process[-test_data_index,]
model <- svm(y ~ ., data = traindata, cost = 4, gamma = .5)
pred <- predict(model, testdata[,-17])

plot(pred)
pred_table=table(pred , true = testdata[,17])
print(pred_table)
plot(pred_table)
error=mean(pred==testdata[,"y"])

#mytunedsvm <- tune.svm(y ~ ., data = traindata, gamma = 2^(-1:1), cost = 2^(2:4))
#summary(mytunedsvm)
#plot (mytunedsvm, transform.x=log10, xlab=expression(log[10](gamma)), ylab="C")
print(error)
