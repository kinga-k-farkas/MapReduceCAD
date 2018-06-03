#! /usr/bin/env Rscript
# map.R



input <- file("stdin", "r")

while(length(x <- readLines(input, n=1, warn=FALSE)) > 0) {
      # in case of empty lines
      # more sophisticated defensive code makes sense here
      if(nchar(x) == 0) break
      y<-unlist(strsplit(x, "\t"))
      rnmb<-as.numeric(y[1])
      v<-as.numeric(y[2:length(y)])
      # find the max value
      mx<-max(v)
      # find its location in the timeseries
      loci<-rnmb-1+which.max(v)
      cat(loci, "\t", mx, "\n")
      
}
close(input)