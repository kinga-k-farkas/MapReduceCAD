#this function takes a daily time series with length greater than 
#365, the year, month, and day of its first entry
#the type of anomaly to be searching for: lower, upper
#with the default value being upper
#delta the value to be added to the value to the CUSUM parameter k
#its default value is 3
#lambda is the minimum length of the anomalous subsequence
#the code should detect, with the defalut being 5
#and looks for anomalies using moving windows


cad<-function(sqnce,delta=3, lambda=5,type= "upper",number_of_windows=10){
      source("clean_anomaly_finder_function.R")
      #source("result_plotter_function_generic.R")
      the_row_number<-sqnce[1]
      ts<-sqnce[2:length(sqnce)]
      n<-length(ts)
      window_length<-n - number_of_windows +1
      abs_anomaly_indices<-c()
      for (i in 1:number_of_windows){
            x<-ts[i:(i+window_length-1)]
            obj<-anomaly_finder(x,delta, lambda, type, number_of_windows,i,the_row_number ) 
            if (length(obj$upper) != 0){
                 abs_anomaly_indices<-c(abs_anomaly_indices, obj$upper)
            }
            if (length(obj$lower) != 0){
                  abs_anomaly_indices<-c(abs_anomaly_indices, obj$lower) 
            }
            
            
      }
   
      return(list(the_row_number,abs_anomaly_indices))


      
}