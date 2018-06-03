#! /usr/bin/env Rscript
# reduce.R

input <- file("stdin", "r")

# initialize variables that keep
# track of the state

is_first_line <- TRUE
loci<--8
counter<-1
while(length(line <- readLines(input, n=1, warn=FALSE)) > 0) {
      line <- unlist(strsplit(line, "\t"))
      
      # current line belongs to previous
      # line's key pair
      if(!is_first_line && 
         loci == line[1]) {
            counter <- counter + 1 
      }
      # current line belongs either to a
      # new key pair or is first line
      else {
            # new key pair - so output the last
            # key pair's result
            if(!is_first_line) {
                  # language / 2-gram / count
                  cat(loci,"\t",counter,"\n")
                  counter<-1
            } 
           
            # initialize state trackers
            loci <- line[1]
      
            is_first_line <- FALSE
      }
}

# the final record
cat(loci,"\t",counter,"\n")

close(input)