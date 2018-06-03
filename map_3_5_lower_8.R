#! /usr/bin/env Rscript
# map_3_5_lower_8.R



input <- file("stdin", "r")

while(length(x <- readLines(input, n=1, warn=FALSE)) > 0) {
      # in case of empty lines
      # more sophisticated defensive code makes sense here
      if(nchar(x) == 0) break
      y<-unlist(strsplit(x, "\t"))
      rnmb<-as.numeric(y[1])
      v<-c(y, as.numeric(y[2:length(y)]))
      source("clean_cad_function.R")
      output<-cad(v,delta=3, lambda=5,type= "lower",number_of_windows=8)
      if (!is.null(output[[2]])) {
            for (i in output[[2]]) {
                  cat(i, "\t", 1, "\n") 
            }
      }
      
}
close(input)