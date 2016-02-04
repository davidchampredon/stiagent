library(plyr)
vt1 <- vector()
vt3 <- vector()
nb1 <- vector()
nb3 <- vector()
vt1.start <- vector()
vt3.start <- vector()
nb1.start <- vector()
nb3.start <- vector()
r1 <- vector()
r3 <- vector()

f.cum <- function(varname,scen,mc) {
	dfsim = as.data.frame(all.scen[[scen]][[mc]]$df_sim)
	t <- dfsim$time
	
	N <- 1 #dfsim$nNewBorn+1
	delta.t <- 1
	w <- which(abs(t-delta.t)<=(t[2]-t[1]))
	lag.1yr <- w[length(w)]
	
	tt <- t[(lag.1yr+1):length(t)]
	
	tmp <- dfsim[,varname]/N
	vt <- diff(tmp,lag=lag.1yr)
	
	df <- data.frame(tt,vt)
	df$yr <- floor(df$tt)
	df2 <- ddply(df,"yr",summarize,m=mean(vt))
	
	return(df2)	
}

f.inc <- function(varname,scen,mc) {
	dfsim = as.data.frame(all.scen[[scen]][[mc]]$df_sim)
	t <- dfsim$time
	
	N <- 1 #dfsim$nNewBorn+1
	delta.t <- 1
	
	df <- data.frame(t,vt=dfsim[,varname])
	df$yr <- floor(df$t)
	df2 <- ddply(df,"yr",summarize,m=sum(vt))
	
	return(df2)	
}

tstart <- 30
shift <- 0

for (mc in 1:30) {
	tmp <- f.cum(1,mc,varname = "mtctHIV")
	tmp2 <- f.inc(1,mc,varname = "nNewBorn")
	tmp2 <- subset(tmp2, yr>0)
	
	
	
	vt1.start[mc] <- tmp$m[tstart]
	nb1.start[mc] <- tmp2$m[tstart]+1
	vt1[mc] <- tmp$m[nrow(tmp)-shift]
	nb1[mc] <- tmp2$m[nrow(tmp2)-shift]+1

	tmp <- f.cum(3,mc,varname = "mtctHIV")
	tmp2 <- f.inc(3,mc,varname = "nNewBorn")
	tmp2 <- subset(tmp2, yr>0)
	
	rho <- tmp$m/tmp2$m
	if (mc==1) plot(rho,typ="l")
	if (mc>1) lines(rho)
	
	vt3.start[mc] <- tmp$m[tstart]
	nb3.start[mc] <- tmp2$m[tstart]
	vt3[mc] <- tmp$m[nrow(tmp)-shift]
	nb3[mc] <- tmp2$m[nrow(tmp2)-shift]
	
	r1[mc] <- (vt1[mc]/nb1[mc])/(vt1.start[mc]/nb1.start[mc])
	r3[mc] <- (vt3[mc]/nb3[mc])/(vt3.start[mc]/nb3.start[mc])
}

mean(r1)
mean(r3)
boxplot(r1,r3)

# plot(vt1$mtct,typ="s")
# lines(vt3$mtct,col="red",typ="s")
# 
