#this function takes a time series, a mean, a standard dev., a k value, an H value
#lambda the minimum length of the anomalous subsequence
#and the type of anomaly to be identified: "lower" or "upper"
#and returns the indices of the anomalies

evaluate_cusum_results<-function(seq,xbar,sigma, H, k, type= "upper",lambda){
      options(expressions=10000)
      require(qcc)
      require(dplyr)
      source("clean_find_turning_points_function.R")
      source("clean_find_intervals_function.R")
      hi_sum_anomaly_indices<-c()
      low_sum_anomaly_indices <-c()
      upper_anomaly_indices<-NULL
      lower_anomaly_indices<-NULL
      
      object<-cusum(seq,sizes=1, center=xbar, std.dev=sigma, 
                        decision.interval=H, se.shift=k,restore.par = TRUE, 
                        plot=FALSE)
      low_sums<-object$neg
      hi_sums<-object$pos
      upper_viol_index<-object$violations$upper
      lower_viol_index<-object$violations$lower
      lower_viol<-low_sums[lower_viol_index]  #seq. of lower sum out of bound values
      upper_viol<-hi_sums[upper_viol_index] #seq. of upper sum out of bound values
      if (length(upper_viol_index)>0){
            high_sum_turning_pts<-find_turning_pts(hi_sums)
            if (high_sum_turning_pts[1]!= 1) high_sum_turning_pts<-c(1,high_sum_turning_pts)
            hi_sum_df<-find_intervals(hi_sums,high_sum_turning_pts, type)
      }
      if (length(lower_viol_index)>0){
            low_sum_turning_pts<-find_turning_pts(low_sums)
            if (low_sum_turning_pts[1]!= 1) low_sum_turning_pts<-c(1,low_sum_turning_pts)
            low_sum_df<-find_intervals(low_sums,low_sum_turning_pts, type)
      }
      
      

      if (type == "lower"){
            ################ FINDING LOWER ANOMALIES ##############
            #finding the indices of the decreasing terms for the upper violations seq.
            if (length(upper_viol_index)>0){
                  dummy_df<-filter(hi_sum_df, sign=="decreasing" & (left_index-right_index)>lambda)
                  if (nrow(dummy_df)>0){
                        for (i in (1:nrow(dummy_df))){
                             # if (sum(is.na(df[i,]))==0){
                                    dummy_interval<-c(dummy_df[[1]][i]:dummy_df[[2]][i])
                                    dummy_interval<-dummy_interval[dummy_interval %in% upper_viol_index]
                                    if (length(dummy_interval) > lambda){
                                          hi_sum_anomaly_indices<-c(hi_sum_anomaly_indices,dummy_interval)
                                    }
                              #}
                        }
                  }
                  else hi_sum_anomaly_indices<-NULL
            }
            else {
                  hi_sum_anomaly_indices<-NULL
            }
            
            #finding the indices of the decreasing terms for the lower violations seq.
            if (length(lower_viol_index)>0){
                  dummy_df<-filter(low_sum_df, sign=="decreasing" & (left_index-right_index)>lambda)
                  if (nrow(dummy_df)>0 ){
                        for (i in (1:nrow(dummy_df))){
                              #if (sum(is.na(df[i,]))==0){
                                    dummy_interval<-c(dummy_df[[1]][i]:dummy_df[[2]][i])
                                    dummy_interval<-dummy_interval[dummy_interval %in% lower_viol_index]
                                    if (length(dummy_interval) > lambda){
                                          low_sum_anomaly_indices<-c(low_sum_anomaly_indices,dummy_interval)
                                    }
                              #}
                        }
                  }
                  else low_sum_anomaly_indices<-NULL
            }
            else {
                  low_sum_anomaly_indices<-NULL
            }
      lower_anomaly_indices<-c(low_sum_anomaly_indices, hi_sum_anomaly_indices)
      }

      
      
      else {
            ################ FINDING UPPER ANOMALIES ##############
            #finding the indices of the increasing terms for the upper violations seq.
            if (length(upper_viol_index)>0){
                  dummy_df<-filter(hi_sum_df, sign=="increasing" & (left_index-right_index)>lambda)
                  print(dummy_df)
                  if (nrow(dummy_df)>0){
                        for (i in (1:nrow(dummy_df))){
                              if (sum(is.na(df[i,]))==0){
                                    dummy_interval<-c(dummy_df[[1]][i]:dummy_df[[2]][i])
                                    dummy_interval<-dummy_interval[dummy_interval %in% upper_viol_index]
                                    if (length(dummy_interval) > lambda){
                                          hi_sum_anomaly_indices<-c(hi_sum_anomaly_indices,dummy_interval)
                                    }
                              }
                        }
                  }
                  else hi_sum_anomaly_indices<-NULL
            }
            else {
                  hi_sum_anomaly_indices<-NULL
            }
            #finding the indices of the increasing terms for the lower violations seq.
            if (length(lower_viol_index)>0){
                  dummy_df<-filter(low_sum_df, sign=="increasing" & (left_index-right_index)>lambda)
                  print(dummy_df)
                  if (nrow(dummy_df)>0){
                        for (i in (1:nrow(dummy_df))){
                              if (sum(is.na(df[i,]))==0){
                                    dummy_interval<-c(dummy_df[[1]][i]:dummy_df[[2]][i])
                                    dummy_interval<-dummy_interval[dummy_interval %in% lower_viol_index]
                                    if (length(dummy_interval) > lambda){
                                          low_sum_anomaly_indices<-c(low_sum_anomaly_indices,dummy_interval)
                                    }
                              }
                        }      
                  }
                  else low_sum_anomaly_indices<-NULL
            }
            else {
                  low_sum_anomaly_indices<-NULL
            }
            upper_anomaly_indices<-c(hi_sum_anomaly_indices, low_sum_anomaly_indices)
      }
      return(list(upper_anomaly=upper_anomaly_indices, lower_anomaly=lower_anomaly_indices))
}