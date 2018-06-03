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
      object$type <- "cusum"
      object$pos <- cusum.pos
      object$neg <- cusum.neg
      object$dec.int <- dec.int
      object$shift <- shift
      object$violations <- violations
      return(object)
                         
}
      
      
data(pistonrings)
attach(pistonrings)
q <- cusum(diameter, center = 50, std.dev =0.01001461, decision.interval = 4, se.shift = 2)
summary(q)
sum(q$data != diameter)
sum(q$data != q$statistics)
str(q)
my_q <- my_cusum(diameter, m=50, sd=0.01001461, dec.int = 4, shift = 2)


sum(q$violations$upper == my_q$violations$upper) == length(q$violations$upper)
sum(q$pos == my_q$pos) == length(q$pos)
which(q$pos != my_q$pos)
which(q$pos != 0)
sum(q$neg == my_q$neg) == length(q$neg)
length(q$neg)
length(my_q$neg)
wrong <- which(q$neg != my_q$neg)
q$neg[wrong]
my_q$neg[wrong]
off_by<-abs(q$neg[wrong] -my_q$neg[wrong])
stats <- paste("stats.", "xbar.one", sep = "")
problems <- which(q$neg != my_q$neg)
q$neg[problems]
my_q$neg[problems]
stats <- do.call(stats, list(diameter, rep(1,length(diameter))))
median(diameter)
mean(diameter)
huh <- stats.xbar.one(diameter, rep(1, length(diameter)))
sum(huh$statistics == diameter)
sizes <- rep(5,nrow(pistonrings))
adat <- qcc.groups(diameter, sample)

std.dev <- do.call(sd.xbar.one, list(diameter, rep(1, length(diameter)),NULL))
sd.xbar.one(diameter, rep(1,length(diameter)), "SD")
