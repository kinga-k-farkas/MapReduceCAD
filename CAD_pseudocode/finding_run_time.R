#Determining the run time for some stats functions

#loading the libraries

library(moments)
library(qcc)

#reading in a file with test data
setwd("~/Dropbox/CADTests")
df<-read.csv("Vietnam2013DecMinRTT.csv")
x<-df$min_rtt
length(x)  #10049

setwd("~/AmazonEMRandRHadoop")
write.csv(x, "cusum_test_data.csv", row.names = FALSE)

#plotting x
hist(x, breaks=100)
d<-density(x, adjust=3)
plot(d)
hist(x, prob=TRUE,breaks=100, col="light grey", main = "Cusum Test Data Histogram and Density Plots")
lines(density(x), col="blue", lwd=2) # add a density estimate with defaults
lines(density(x, adjust=3), lty="dotted", col="dark red", lwd=2) 



#checking out my cusum function
my_cusum<-function(x,m, sd, dec.int, shift ){
      n <- length(x)
      z <- (x - m)/(sd)
      ldb <- -dec.int
      udb <- dec.int
      z.f <- z - shift/2
      cusum.pos <- rep(NA, n)
      cusum.pos[1] <- max(0, z.f[1])
      for (i in 2:n) {
            cusum.pos[i] <- max(0, cusum.pos[i - 1] + z.f[i])
      }
      z.f <- z + shift/2
      cusum.neg <- rep(NA, n)
      cusum.neg[1] <- max(0, - z.f[1])
      for (i in 2:n) {
            cusum.neg[i] <- max(0, cusum.neg[i - 1] - z.f[i])
      }
      cusum.neg <- -cusum.neg
      violations <- list(lower = which(cusum.neg < ldb),
                         upper = which(cusum.pos > udb))
      object<-list()
      object$pos <- cusum.pos
      object$neg <- cusum.neg
      object$violations <- violations
      return(object)
      
}
my_cusum(x, m=312, sd =  88.69, dec.int=5, shift=1)
system.time(my_cusum(x, m=312, sd =  88.69, dec.int=5, shift=1))
# user  system elapsed 
# 0.027   0.002   0.030 

system.time(cusum(x,center=312, std.dev=88.69, decision.interval = 5, se.shift = 1, plot = FALSE ))
# user  system elapsed 
# 0.039   0.000   0.039 

#CHECKING my_cusum VALUES 
q <- cusum(x,center=312, std.dev=88.69, decision.interval = 5, se.shift = 1, plot = FALSE )
myq<-my_cusum(x, m=312, sd =  88.69, dec.int=5, shift=1)
sum(myq$neg == q$neg) == length(q$neg)
sum(myq$pos == q$pos) == length(q$pos)
sum(myq$violations$lower == q$violations$lower)==length(q$violations$lower)
sum(myq$violations$upper == q$violations$upper) == length(q$violations$upper)

#creating csv file with my_cusum output

df_sums <- data.frame ("low_sums" = myq$neg, "hi_sums"=myq$pos)
head(df_sums)
write.csv(df_sums, "CUSUM_output_hi_low_sums.csv", row.names = FALSE)
write.csv(myq$violations$lower, "CUSUM_output_lowerViolations.csv", row.names = FALSE)
write.csv(myq$violations$upper, "CUSUM_output_upperViolations.csv", row.names = FALSE)





x1 <- rnorm(50000, mean=2.1, sd=2)
x2 <- rnorm(300000, mean=0.5, sd = 5.6)
x3 <-  rnorm(100000, mean = 4.1, sd = 1.01)
x<- c(x1,x2, x3)

write.csv(x, "stats_runtest_data.csv", row.names = FALSE)
#plotting x
hist(x)
d<-density(x, adjust=3)
plot(d)
hist(x, prob=TRUE, col="light grey", main = "Test Data Histogram and Density Plots", ylim = c(0, 0.2))
lines(density(x), col="blue", lwd=2) # add a density estimate with defaults
lines(density(x, adjust=4), lty="dotted", col="dark red", lwd=2) 
skewness(x)
kurtosis(x)
system.time(skewness(x))
system.time(kurtosis(x))
