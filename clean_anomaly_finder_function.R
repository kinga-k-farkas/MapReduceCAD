#this function takes a time series of length of at least 75
#and the type of anomaly to look for: "lower" or "upper"
#and looks for anomalies using the statistical process control chart, the CUSUM chart
#TEMPORARY: the input should not be much longer than 365 
#FINAL VERSION: the input can be any length longer than 74
# Added an i as an input indicating the window number the detector is being applied
#to. It is used to save the graphs to separate files.
#function(seq,xbar,sigma, H, k, type= "upper",lambda)
anomaly_finder <- function(time_series,delta,lambda, type="upper", number_of_windows, the_window_number, the_row_number){
      #temporary version
      require(qcc)
      require(ggplot2)
      require(zoo)
      n<-length(time_series)
      stop_now<-FALSE
      source("training_data_finder_function.R")
      source("clean_evaluate_cusum_results_function.R")
      training_data_object <- training_data_finder(time_series)
      if (length(training_data_object$training_data) == 0) {
            stop_now<-TRUE
            return(list(upper=NULL, lower=NULL))
      }
      else {
            
            training_data<-training_data_object$training_data
            start_val<-training_data_object$segment_starting_point
            segment_length<-training_data_object$segment_length
            end_val<-segment_length + start_val-1
            training_in_ts<-rep(NA, n)
            training_in_ts[start_val:end_val]<-time_series[start_val:end_val]
            x_bar<-mean(training_data)
            sigma<-sd(training_data)
            k<-training_data_object$k+delta
            H<-training_data_object$H
            anomaly_indices<-
            evaluate_cusum_results(time_series,x_bar,sigma,H,k,type,lambda)
            lower_anomaly<-NULL
            upper_anomaly<-NULL
            if (length(anomaly_indices$upper_anomaly)!= 0){
                  upper_anomaly<-anomaly_indices$upper_anomaly + the_window_number
            }
           
            if (length(anomaly_indices$lower_anomaly) != 0){
                  lower_anomaly<-anomaly_indices$lower_anomaly + the_window_number
            }

            return(list(upper=upper_anomaly, lower=lower_anomaly))
            
      }
}