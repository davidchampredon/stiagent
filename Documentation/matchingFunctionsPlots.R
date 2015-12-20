library(MASS)
##### AGES, GAPS #####

m.a = 20
v.a = 6

m.g = 5
v.g = 1

rho = -0.3 

M = matrix(c(v.a,rho*sqrt(v.a*v.g),rho*sqrt(v.a*v.g),v.g),nrow=2)
print(M)


f.age <-function(a,g, M)
{
  X = matrix(c(a-m.a,g-m.g),nrow=2)
  
  adj = 0.01
  z = t(X) %*% ginv(M) %*% X
  
  return(exp(-z[1,1]*adj))
}

par(mfrow=c(1,2))
yy=c()
gg = c()
a = 20
for (g in seq(from=-20,to=20,by=0.1))
{
  gg = c(gg,g)
  yy = c(yy,f.age(a,g,M))
}
plot(x=gg,y=yy, typ="o", ylim=c(0,1), main="Age matching parameters\n(fixed female age)")

yy=c()
g = 5
aa = c()
for (a in seq(from=10,to=65,by=0.5))
{
  aa = c(aa,a)
  yy = c(yy,f.age(a,g,M))
}
plot(x=aa,y=yy, typ="o", ylim=c(0,1),main="Age matching parameters\n(fixed age gap)")



##### RISK GROUP #####

rmax = 3
a0 = 0.01 #0.15
a1 = 0.01 #0.09

f.risk <- function(rf,rm)
{
    return(exp(-a0*(2*rmax-(rf+rm))-a1*(rf-rm)^2))
}

rf=0

rm= seq(from=0, to=rmax, by=1)
plot(x=rm,y=f.risk(rf,rm),typ="l",ylim=c(0,1), main="Risk matching parameters")
for (rf in 1:rmax)
lines(x=rm,y=fct(rf,rm),lwd=rf)


### SPOUSAL ####


expGauss <-function(x,m,v)
{
  return(exp(-(x-m)^2/(2*v^2)))
}
par(mfrow=c(1,2))
m.age.f = 16
var.age.f = 7
age = seq(from=0,to=65,by=0.5)
yy = expGauss(age, m.age.f, var.age.f)
plot(x=age, y=yy,type="o",main="Spousal progression")

m.gap = 5
var.gap = 3
gap = seq(from=-20,to=20,by=0.2)
yy = expGauss(gap,m.gap,var.gap)
plot(x=gap, y=yy,type="o",main="Spousal progression - gap ")

# Difference of age gaps between new candidate spouse 
# and smallest age gap among existing spouses

par(mfrow=c(1,1))

m.diff.ageGap = 5
var.diff.ageGap = 3
ageGap = seq(from=-10,to=10,by=0.25)
yy = expGauss(ageGap, m.diff.ageGap, var.diff.ageGap)
plot(x=ageGap, y=yy,type="o",main="Spousal progression - Diff age gaps candidate vs. existing")
grid()

# Duration of partnership before spousal progession
par(mfrow=c(1,1))

k1 = 1
k2 = 0.05
t = seq(from=0, to=30, by=0.1)
dd = k1*exp(-k2*t^2)
plot(x=t,y=dd,typ="l",lwd=3)
grid()

# Reduction of sexual activity
age = seq(from=0, to=100, by = 0.5)
amin = 15
sigma = 40
q = 4
yy = exp(-((age-amin)/sigma)^q)
plot(x=age, y=yy, ylim=c(0,1),main="Reduction of sexual activity - age factor")

# Preference of partner type when distributing sex acts

riskgrp = seq(from=0, to=3, by=1)
c1 = 0.5
c2 = 1
rmax = 3

epsilon = c1*exp(-c2*(rmax-riskgrp))

plot(x=riskgrp,y=epsilon,
     main="Preference for sex worker - risk group factor",
     typ="o", ylim=c(0,1),lwd=3)
grid()

