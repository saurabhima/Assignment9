normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

power_data = read.table("household_power_consumption.txt",head=TRUE,sep=";")
power_data_for_process=power_data
power_data_for_process=power_data_for_process[complete.cases(power_data_for_process),]
print(nrow(power_data))
print(nrow(power_data_for_process))
attach(power_data_for_process)
power_data_for_process$Date<-NULL
power_data_for_process$Time<-NULL
power_data_for_process <- na.omit(power_data_for_process) # listwise deletion of missing
power_data_for_process1=data.matrix(power_data_for_process,rownames.force = NA)
#power_data_for_process2=power_data_for_process1[1:10000,]
power_data_for_process2 = as.data.frame(sapply(power_data_for_process, as.numeric))

#power_data_for_process2 <- scale(power_data_for_process1) # standardize variables 
k_val <- readline(prompt="Please Enter Value of K: ")
col_name <- readline(prompt="Please Enter Column Details: ")
col_name=unlist(strsplit(col_name,','))
col_name=as.numeric(col_name)
col_vect=c(col_name)
col_vect=col_vect-2
colnames(power_data_for_process2) <- c(3:9)
power_data_for_process3=power_data_for_process2[,col_vect]
kval_vect=c(5:k_val)
time_vect=c()
error_vect=c()

for(x in 5:k_val){
  t=system.time(kc <- kmeans(power_data_for_process2, x))
  error=kc$tot.withinss
  t=c(t)
  print(t)
  t=t[2]
  error_vect <- c(error_vect, error)
  time_vect <- c(time_vect, t)
}

plot(kval_vect,error_vect)
plot(kval_vect,time_vect)
error_vect1=normalize(error_vect)


