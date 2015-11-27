birth = read.csv("births.out", header=FALSE)
deaths = read.csv("deaths.out", header=FALSE)
death.indiv = read.csv("death_individuals.out", header=FALSE)

names(birth) = c("time","size","births","lambda","brate","rate")
names(deaths) = c("time","n","popSize")
names(death.indiv) = c("time","uid","age","HIV","probaDeath")

par(mfrow=c(2,2))


 ### BIRTH PROCESS ###
birth$eff.brate = birth$births/birth$size
birth=na.omit(birth)
head(birth)

mx = max(birth$rate,birth$eff.brate)

plot(x=birth$time, y=birth$rate,ylim=c(0,mx), 
     main="Birth rate: Effective vs. expected",
     col="red",typ="l")
lines(x=birth$time, y=birth$eff.brate, pch=16, typ="o")
m.eff = mean(birth$eff.brate)
abline(a=m.eff,b=0)

est.size = birth$size[1]*(1+(birth$rate[1]))^birth$time
mx = max(birth$size,est.size)
plot(x=birth$time, y=birth$size, typ="o",pch=16,
     ylim=c(0,mx),
     main="Population size: Actual vs. naive estimate",
     xlab="time",ylab="Population size")
grid()
points(x=birth$time,y=est.size, col="red")

### DEATH PROCESS ###

deaths$eff.rate = deaths$n/deaths$popSize
mea.death = mean(deaths$eff.rate)
plot(x=deaths$time, y=deaths$eff.rate, typ="o", pch="+",
     main="Effective death rate")
abline(a=mea.death,b=0)


boxplot(age~time,data=death.indiv,
        main = "Age of death", col="gray",
        xlab="time",ylab="Age")
