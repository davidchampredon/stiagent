# TEMPORARY SCRIPT FOR CALIBRATION

# NEEDS CLEAN UP !

DIR_CALIB = "../CALIBRATION/"


# Target age distribution
target = read.csv(paste0(DIR_CALIB,"calib_AgeDistribution_target.out"), header=FALSE)
# Age distribution from model
res = read.csv(paste0(DIR_CALIB,"calib_AgeDistribution_all.out"), header=FALSE)
res = na.omit(res)
# Parameters (all 7 of them) and last column is value of fct to minimize
prm = read.csv(paste0(DIR_CALIB,"calib_AgeDistribution_all_param.out"), header=FALSE)

agebreaks = target[,1]
target = t(target[,2])

prm = na.omit(prm)

f.min = min(prm$V8)
prm.min = which(prm$V8==f.min)
arg.prm.min = prm[prm.min,]

par(mfrow=c(1,1))

if (1)
{
  plot(x=agebreaks, y=target, 
       ylim=c(0,0.40),
       pch=16, col="red", cex=2)
  
  for (i in 1:nrow(res))
    lines(x=agebreaks,y=res[i,],type="l", col=rgb(0,1-0.5*i/nrow(res),0))
  
  # highlight the best match
  lines(x=agebreaks,y=res[prm.min,],type="o", col="blue",lwd=3)
  grid()
}

if(0){
mc = read.csv("mc.out", header=FALSE)
par(mfrow=c(1,1))
boxplot(mc,ylim=c(0,0.25))
points(x=1:ncol(mc),y=target,pch=16,col="red",cex=3)
grid()
}

if(0){
sensi = read.csv("sensi.out", header=FALSE)
barplot(t(sensi))
}