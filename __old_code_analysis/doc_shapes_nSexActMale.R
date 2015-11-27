################################################
#     Functional forms defining 
#     the reduction of males' sexual acts
################################################


# Parameters values

param = read.csv("in_paramSexActivity.csv", header=FALSE)
names(param) = c("name","value")

pop.features = read.csv("in_populationFeatures.csv", header=TRUE)

# Shared plot parameters
par(mfrow=c(3,2))


myplot <- function(xdata, ydata, title)
{
  mylwd= 12
  mytype = "l"
  
  plot(x=xdata, y=ydata, ylim=c(0,1),
       lwd= mylwd, type=mytype,
       main=title,
       xlab="",ylab="Sexual Activity Reduction Factor")
  grid()  
}

# --- Male's age ---

age.min = pop.features[pop.features$PARAMETER=="minSexAge",2]
age.max = pop.features[pop.features$PARAMETER=="maxSexAge",2]

age = seq(from=age.min, to=age.max, by=0.5)

a.peak = param[param$name=="reduceSexActRateAge1",2]
sigma.age = param[param$name=="reduceSexActRateAge2",2]
q = param[param$name=="reduceSexActRateAge3",2]

h.age = exp(-((age-a.peak)/sigma.age)^q)

myplot(age,h.age,"Age")



# --- Male's Risk group ---

eps = param[param$name=="reduceSexActRateRiskGroupEpsilon",2]
maxrisk = pop.features[pop.features$PARAMETER=="maxRiskGroup",2]
riskgrp = seq(from=0,to=maxrisk,by=1)

h.risk = (1-eps)*riskgrp/maxrisk + eps

myplot(riskgrp,h.risk,"Risk Group")

# --- STI symptoms ---

eps.sti.m = param[param$name=="reduceSexActRatesymptomSTImale",2]
eps.sti.f = param[param$name=="reduceSexActRatesymptomSTIfemale",2]

sympt = c(0,1)

h.sti.m =c(1,eps.sti.m)
h.sti.f =c(1,eps.sti.f)

plot(x=sympt, y=h.sti.m, pch=16,cex=3, 
     ylim=c(0,1), 
     main="STI symptoms", xlab="",ylab="Sexual Activity Reduction Factor")
points(x=sympt, y=h.sti.f, pch=17,cex=3)
legend(x="bottomleft",pch=c(16,17),legend=c("Male","Female"),)
grid()

# --- AIDS progression ---

sigma.aids = param[param$name=="reduceSexActRateAIDSsigma",2]
q.aids = param[param$name=="reduceSexActRateAIDSq",2]

aids.duration = seq(from=0, to=5, by=0.1)

h.aids = exp(-(aids.duration/sigma.aids)^q.aids)

myplot(aids.duration,h.aids,"AIDS progression")


# --- Number of current partners ---

cp = param[param$name=="reduceSexActRateNpartner",2]

npartners = c(0:7)

h.np = 2/(1+exp(-cp*npartners))-1

myplot(npartners, h.np, "Number of Partners")


