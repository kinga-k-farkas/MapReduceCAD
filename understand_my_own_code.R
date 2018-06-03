source("clean_evaluate_cusum_results_function.R")
source("training_data_finder_function.R")
delta<-3


ts<-rnorm(80,5,1.4)
ts<-c(ts,rnorm(20,50,0.2), rnorm(70, 4.9, 2))

mean(ts)
sd(ts)
plot(ts, type="l")
training_data_object <- training_data_finder(ts)
training_data<-training_data_object$training_data
start_val<-training_data_object$segment_starting_point
segment_length<-training_data_object$segment_length
end_val<-segment_length + start_val-1

training_in_ts<-rep(NA, length(ts))
training_in_ts[start_val:end_val]<-ts[start_val:end_val]

plot(ts, type="l", main="Plot of time series and its training set in red")
points(training_in_ts, y = NULL, type = "p", col="red")
#creating the CUSUM chart
x_bar<-mean(training_data)
#print(paste("the mean is:", x_bar))
sigma<-sd(training_data)
#print(paste("the standard dev. is:", sigma))
k<-training_data_object$k+delta
H<-training_data_object$H




output<-evaluate_cusum_results(ts, x_bar, sigma,H,k, type="lower", 5)
str(output)
anomalies<-rep(NA, length(ts))
anomalies[output$lower_anomaly]<-ts[output$lower_anomaly]
anomalies[output$upper_anomaly]<-ts[output$upper_anomaly]
plot(ts, type="l", main="Plot of time series, its training set and anomalies")
points(training_in_ts, y = NULL, type = "p", col="red")
points(anomalies, y=NULL, type="p", col="purple", pch=20)


#################################


setwd("~/AmazonEMRandRHadoop")
df<-read.csv("thruput_df.csv")
str(df)
df[1,1:10]
df[2, 1:10]
x1<-unname(unlist(c(df[1,])))
str(x1)
setwd("~/AmazonEMRandRHadoop/anomalyDetection")
source("clean_cad_function.R")
source("clean_anomaly_finder_function.R")
source("clean_evaluate_cusum_results_function.R")
input_ts<-c(3,ts)
output<-cad(input_ts,delta=3, lambda=5,type= "upper",number_of_windows=19)
anomalies<-rep(NA, (length(input_ts)-1))
anomalies[output[[2]]]<-input_ts[(output[[2]]+1)]
plot(input_ts[2:length(input_ts)], type="l", main="Plot of time series and anomalies")
points(anomalies, y=NULL, type="p", col="purple", pch=20)
plot(x1[2:500], type="l")
output<-cad(x1[1:500],delta=3, lambda=5,type= "lower",number_of_windows=8)
output
unique(output[[2]])
danomalies<-rep(NA, 499)
anomalies[output[[2]]]<-x1[(output[[2]]+1)]
plot(x1[150:350], type="l", main="Plot of time series and anomalies")
points(anomalies[150:350], y=NULL, type="p", col="purple", pch=20)
