### FUNCTIONS FOR SIMULATION ###

###     DEMOGRAPHICS   ###


# Global variables

par(mfrow=c(3,3))
mylwd = 6

# ====== Population size =======

plot.population <- function(df)
{
  ### Time series of population
  
  plot(x=df$time, y=df$nAlive, 
       typ="l", lwd=mylwd, col="darkgreen",
       main = "Population", xlab="Time (years)", ylab="Living Individuals")
  grid()
  
  rng = range(df$nRskGrp0/df$nAlive,df$nRskGrp1/df$nAlive,df$nRskGrp2/df$nAlive)
  
  ### Time series of population
  ### by risk groups
  
  plot(x=df$time, y=df$nRskGrp0/df$nAlive, col="blue",
       main = "Risk Group Proportion", typ="l", lwd=2,
       ylim = rng, 
       xlab="Time (years)", ylab="Living Individuals")
  lines(x=df$time, y=df$nRskGrp1/df$nAlive, lwd=2)
  lines(x=df$time, y=df$nRskGrp2/df$nAlive,col="red", lwd=2)
  par(xpd=TRUE)
  legend(x = "topleft", 
         legend = paste("risk group",0:2), lwd=2,
         col=c("blue","black","red"))
  grid()
  par(xpd=FALSE)
  
  ### Growth rate
  
  n=nrow(df)
  
  growth = rep(x=0,times=n-1)
  
  for (i in 2:n)
  {
    growth[i]=(df$nAlive[i]-df$nAlive[i-1])/(df$time[i]-df$time[i-1])/df$nAlive[i-1]
  }
  
  lg = 100
  time2 = round(seq(from=1,to=n, length=lg),digits=0)
  thetime = df$time[time2]
  growth2 = growth[time2]
  
  plot(x=thetime, y=growth2, typ="l", col="blue",
       main="Annual Growth Rate (sampled)", xlab="Time (years)", ylab="Growth rate")
  abline(a=0,b=0,col="gray",lwd=2)
  grid()
}






# ====== PARTNERSHIPS =======

plot.partnerships <- function(simul)
{
  # Total number of partnerships

  rng=range(simul$nPartn,simul$nSp)
  
  plot(x=simul$time,y=simul$nPartn, typ="l", lwd=mylwd,
       ylim=rng,
       main = "Number of partnerships", 
       xlab="Time (years)", ylab="")
  
  lines(x=simul$time,y=simul$nSp,typ="l", col="red",lwd=mylwd)
  legend(x="topleft",legend=c("Total","Spousal"),col=c("black","red"),lwd=mylwd)
  grid()
  
  # Proportion of partnerships that are spousal ones
  
  prop.sp = simul$nSp/simul$nPartn
  
  plot(x=simul$time, y=prop.sp,typ="l",ylim=c(0,1),lwd=mylwd,
       main="Proportion of Spousal Partnerships", ylab="") 
  grid()
  
  # Ratio #partnership / (population size/2)
  prop.Partn = simul$nPartn/(simul$nAlive/2)
  plot(x=simul$time, y=prop.Partn,typ="l",lwd=mylwd,
       main="Ratio of Partnerships vs Half Population size", ylab="") 
  abline(a=1,b=0)
  grid()
}

plot.partnerships.prop <- function(simul,agegaps)
{
    
    # Total number of partnerships
    themin=min(simul$nPartn/simul$nAlive,simul$nSp/simul$nAlive)
    themax=max(simul$nPartn/simul$nAlive,simul$nSp/simul$nAlive)
    
    plot(x=simul$time,y=simul$nPartn/simul$nAlive,
         typ="l", lwd=mylwd,
         ylim=c(0,1),
         main = "Number of partnerships", 
         xlab="Time (years)", ylab="")
    
    lines(x=simul$time,y=simul$nSp/simul$nAlive,typ="l", col="red",lwd=mylwd)
    legend(x="topleft",legend=c("Total","Spousal"),col=c("black","red"),lwd=mylwd)
    grid()
    
    # proportion of partnerships that are spousal ones
    
    prop.sp = simul$nSp/simul$nPartn
    
    plot(x=simul$time, y=prop.sp,typ="l",ylim=c(0,1),lwd=mylwd,
         main="Proportion of Spousal Partnerships", ylab="") 
    grid()
    
    # Ratio #partnership / (population size/2)
    
    prop.Partn = simul$nPartn/(simul$nAlive/2)
    plot(x=simul$time, y=prop.Partn,typ="l",lwd=mylwd,
         main="Ratio of Partnerships vs Half Population size", ylab="") 
    abline(a=1,b=0)
    grid()
}


# AGE DISTRIBUTIONS

ageGrouping <- function(pop.ages, agebreaks)
{
  H = hist(pop.ages,breaks=agebreaks,plot=FALSE)
  
  n = (length(agebreaks)-1)
  
  age.label = rep(x=NA,times=n-1)
  
  for (i in 1:n)
    age.label[i] = paste0("Age ",agebreaks[i],"-",agebreaks[i+1])
  
  print(H$counts)
  print(age.label)
  tot = sum(H$counts)
  df = data.frame(age.group=age.label, count = H$counts,prop = H$counts/tot)
  return(df)
}

plot.ageDistribution <-function(pop, agebreaks)
{
  hist(pop$age, main="Age Distribution (all)",
       xlab="age", col="lightgray")
  pop.male = pop[pop$gender==1,]
  pop.female = pop[pop$gender==0,]
  
  # 
  plot(density(pop.male$age), main="Age distribution (male amd female)")
  lines(density(pop.female$age), col="red")
  
  # By age category
  df = ageGrouping(pop$age,agebreaks)
  print(df)
  barplot(df$prop, names.arg=df$age.group, col="darkgray")
  grid()
}


#############################


plot.CSW <-function(simul)
{
  plot(x=simul$time, y=simul$nCSW/simul$nAlive, type="l", lwd=mylwd,
       xlab = "Time", ylab="CSW Proportion",
       main="Proportion of CSW in Population")
  grid() 
}



##########    STI    ###############


plot.STI_prev_timeseries <- function(simul, stinames)
{
  # stinames = vector of STI names to be plotted, i.e c("HIV","Ct")
  
  # Total population
  popTot = simul$nAlive
  
  # Max for plotting scale
  themax = min(1,max(simul[,stinames])/min(popTot))
    
  for (i in 1:length(stinames))
  {
    if (i==1)
      plot(x=simul$time, y=simul[,stinames[i]]/popTot,
           main = "Global Prevalence",
           ylim=c(0,1.0), col=i,typ="l",lwd=mylwd,
           xlab = "Time", ylab="STI prevalence",
           bty='L')
    
    if (i>1)
      lines(x=simul$time, y=simul[,stinames[i]]/popTot,
            col=i, lwd=mylwd)
  }
  legend(x="topleft",bg="n",
         legend=stinames,lwd=mylwd,col=c(1:length(stinames)),
         bty="n",cex=0.7)
  grid()
}


plot.STIprev_riskgroup <- function(simul,sti.name)
{
  idx = which(names(simul)==sti.name)
  prev = simul[,idx]/simul$nAlive
  
  simul$ratio_0 <- simul$nRskGrp0/simul$nAlive
  simul$ratio_1 <- simul$nRskGrp1/simul$nAlive
  simul$ratio_2 <- simul$nRskGrp2/simul$nAlive
  
  xrng = range(simul$ratio_0,simul$ratio_1,simul$ratio_2)
  
  plot(x=simul$ratio_0, y=prev, typ="o",
       main = paste(sti.name,"prevalence and risk groups"),
       xlim=xrng, xlab="Proportion", ylab="Prevalence")
  points(x=simul$ratio_1, y=prev,pch="1",typ="o",col="orange")
  points(x=simul$ratio_2, y=prev, pch="2",typ="o",col="red")
}



plot.STIprev_CSW <- function(simul,sti.name)
{
  idx = which(names(simul)==sti.name)
  prev = simul[,idx]/simul$nAlive
  
  plot(x=simul$nCSW/simul$nAlive, y=prev, typ="o",
       main = paste(sti.name,"prevalence and CSWs"),
       xlab="CSW Proportion", ylab="Prevalence",pch=16)
  points(x=simul$nCSW[1]/simul$nAlive[1], y=prev[1],
         pch=1,col="red",cex=2)
  text(x=simul$nCSW[1]/simul$nAlive[1],y=prev[1],
       labels = "start",pos=3)
}
