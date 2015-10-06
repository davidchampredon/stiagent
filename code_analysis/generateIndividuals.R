#############################################
### GENERATE INDIVIDUAL OF A POPULATION
### TO BE USED IN C++ CODE
#############################################

library(ggplot2)

message("Generating initial individuals... ")

### Read population features from a file
param = read.csv("in_populationFeatures.csv", header=TRUE)

pop.total = param[param$PARAMETER=="size",2]
prop.females = param[param$PARAMETER=="proportionFemales",2]
prop.csw = param[param$PARAMETER=="proportionCSW",2]

pop.female = round(prop.females*pop.total, digits=0) #non-CSW females
pop.csw = round(prop.csw*pop.total,digits=0)
pop.male = pop.total-pop.female-pop.csw

# Max number of concurrent partners (both sexes)
max.conc.partners = param[param$PARAMETER=="maxConcPartners",2]

# Youngest age of sexual debut
min.sex.age = param[param$PARAMETER=="minSexAge",2]
# oldest age for having more than 3 concurrent partners
max.conc.age = param[param$PARAMETER=="maxSexAge",2]


# Proba partner was spouse
proba.partnerSpouse = param[param$PARAMETER=="probaPartnerSpouse",2]

# Proba divorced
proba.divorce = param[param$PARAMETER=="probaDivorce",2]

# Proba widow
proba.widow = param[param$PARAMETER=="probaWidow",2]

# Proportion male circumcised
prop.circum = param[param$PARAMETER=="proportionCircumcised",2]

# Define the age range for a given max number of concurrent partnerships
# Example:
# age.lo[1] = lowest age for being single
# age.lo[2] = lowest age for having max 1 partner
# age.lo[3] = lowest age for having max 2 concurrent partners
# etc

age.lo = rep(x=min.sex.age, times=max.conc.partners+1)
age.hi = c(60,60,60,rep(x=max.conc.age,times=max.conc.partners-2))

# Partner distribution following a Poisson law
partners.dist <-function(lambda,k)
{
  return(exp(-lambda)*(lambda^k)/factorial(k))
}

# Define the distribution of single and concurrent partners
# (dist.fme or dist.nPartners.male must be same size as age.lo)
n.partner = seq(from=0,to=max.conc.partners,by=1)

# Define the distribution shape with param lambda
# Because expo dist, the value will be the avg number of partners 
# for each gender

lambda.fem = 0.9 
lambda.male = 1.8

prop.dist.nPartners.fem = partners.dist(lambda.fem,n.partner)
prop.dist.nPartners.male = partners.dist(lambda.male,n.partner)

dist.nPartners.fem = round(prop.dist.nPartners.fem*pop.female, digits=0)#c(5,3,3,1,0)   # single, 1 partner, 2 partners, etc
dist.nPartners.male = round(prop.dist.nPartners.male*pop.male, digits=0)

# forces coherence
pop.female = sum(dist.nPartners.fem)
pop.male = sum(dist.nPartners.male)


generateIndividuals <-function(startUID,genderType,dist.maxPartner)
{
  res=data.frame()
  
  uid = seq(from=startUID,length=sum(dist.maxPartner),by=1)
  
  for(i in 1:length(dist.maxPartner))
  {
    n = dist.maxPartner[i]  
    gender = rep(x=genderType, times=n)
    
    # Age are distribution (arbitrary)
    #age = runif(n,min=age.lo[i], max=age.hi[i])
    age = 60*rbeta(n=n,shape1=1.5,shape2=5)+age.lo[i]
    age = pmin(age,rep(x=75,times=length(age)))
    
    maxPartner = rep(x=i-1,times=n)
    
    res.i = data.frame(gender,age,maxPartner)
    res = rbind(res,res.i)
  }
  res = cbind(uid,res)
  return(res)
}

# VERY IMPORTANT TO START UID AT 0
start.UID = 0
# Generates females first
res.f = generateIndividuals(start.UID,0,dist.nPartners.fem)

# Then males
res.m = generateIndividuals(start.UID+pop.female,1,dist.nPartners.male)




# Then recombine
res = rbind(res.f,res.m)
print(res)



#### Calculates Risk group ####
# RiskGroup is randomly assigned
# But if maxPartner>2 RiskGroup assumed 'High=1'
 

riskGrp = rep(x=0, times=nrow(res))

# Probability being High risk group if maxPartner<3
proba.HiRisk = param[param$PARAMETER=="proportionHiRisk",2]

riskGrp = rbinom(n=nrow(res),size=1,prob=proba.HiRisk)

riskGrp[res$maxPartner>2]=1
res$riskGroup = as.numeric(riskGrp)



#### GENRATES CSW ####
# CSW <=> RiskGroup = 9

generateCSW <-function(startUID,genderType,min.age, max.age, number)
{
  res=data.frame()
  
  uid = seq(from=startUID,length=number,by=1)
  maxPartner = rep(x=0, times=number) # maxPartner=0 : CSW cannot form long term couple
  
  gender = rep(x=genderType, times=number)
  riskGroup = as.numeric(rep(x=9, times=number))
  
  # Age are uniformly distributed
  age = runif(number, min=min.age, max=max.age)
  
  res = data.frame(uid,gender,age,maxPartner,riskGroup)
  return(res)
}

csw.min.age = param[param$PARAMETER=="minCSWage",2]
csw.max.age = param[param$PARAMETER=="maxCSWage",2]

res.csw = generateCSW(start.UID+nrow(res),0,csw.min.age,csw.max.age,pop.csw)

### ADD CSW TO POPULATION ###
res = rbind(res, res.csw)

res$riskGroup = as.numeric(res$riskGroup)


### SET LIFETIME NUMBER OF PARTNERS ####

lifetimeNpartner <-function(age, riskGroup)
{
  cst = 20
  a = 5
  lambda = (age-15)/cst*(1+riskGroup*a)
  
  ltp = rpois(n=length(age),lambda = lambda)
  
  return(ltp)  
}

res$nLifetimePartner = lifetimeNpartner(res$age, res$riskGroup)

if(0){
  plot(x=res$age,y=res$nLifetimePartner)
  g = ggplot(res, aes(x=age,y=nLifetimePartner,col=as.factor(riskGroup)))+geom_point(size=6)
  print(g)
}

### SET LIFETIME NUMBER OF SPOUSES ####

lifetimeNspouse <-function(nLfPartner)
{
  # lifetime number of spouses must be smaller than lifetime partners
  lts = rbinom(n=length(nLfPartner), size=nLfPartner, prob=proba.partnerSpouse)
  return(lts)  
}

res$nLifetimeSpouse = lifetimeNspouse(res$nLifetimePartner)

### SET DIVORCE STATUS ###

isDivorced <-function(nLfSpouse)
{
  # Can only be divorced if had at least one spouse

  div = rbinom(n=length(nLfSpouse), size=nLfSpouse, prob=proba.divorce)
  div[div>0]=1
  return(div)  
}

res$isDivorced = isDivorced(res$nLifetimeSpouse)

### SET WIDOW STATUS ####

isWidow <-function(nLfSpouse, isDivorced, age)
{
  # Can only be widowed if had at least one spouse
  
  pw = proba.widow*exp(-0.05*(65-age))
  
  wid = rbinom(n=length(nLfSpouse), size=nLfSpouse-isDivorced, prob=pw)
  wid[wid>0]=1
  return(wid)  
}


res$isWidow = isWidow(res$nLifetimeSpouse, res$isDivorced, res$age)

#print(res[res$isWidow>0,])

# Determine circumcised males (for male only)
is.circum <- rbinom(n = nrow(res.m),size = 1,prob = prop.circum)
res.m.c <- res.m
res.m.c$isCircum <- is.circum
uid.circum <- res.m.c$uid[res.m.c$isCircum==1]

res$isCircumcised <- rep(0,times=nrow(res))
res$isCircumcised[res$uid %in% uid.circum] <- 1


#### SET STIs STATUS ####

# This is a very dumb way for now
# TO DO: make a function that calibrates on existing data

n = nrow(res)


sti = read.csv("in_STIinit.csv", header=TRUE)
# WARNING: THIS ORDER IS IMPORTANT (what is expected in C++)
# c("HIV","HSV2","HPV","Ct","Ng","Tp","Hd","Bv","Tv")
stiNames = sti$STI 
stiPrevalence = sti$Prevalence
stiMaxDuration = c(7,10,10,0.25,0.25,2,0.1,0.3,0.3)
stiSympt = sti$propSymptomatic

# Set STIs durations
for (i in 1:length(stiNames))
{
  #define a priori prevalence:
  isSTIinfected = rbinom(n=n, size=1, prob=stiPrevalence[i])
  # define duration, conditional on being STI infected:
  duration = runif(n=n,min=0.01, max = stiMaxDuration[i])
  STIduration = isSTIinfected*duration
  res = cbind(res,STIduration)
}
# write informative headers
nc = ncol(res)
nc1 = nc - length(stiNames)+1
names(res)[nc1:nc] = paste0(as.character(stiNames),"duration")

# correction for STI exclusively for female (Bv, Tv)
res$Bvduration[res$gender>0]=0
res$Tvduration[res$gender>0]=0

# Set STIs symptoms
for (i in 1:length(stiNames))
{
  #define a priori prevalence:
  isSymptomatic = rbinom(n=n, size=1, prob=stiSympt[i])
  # conditional on being infected:
  isSymptomatic = (res[,nc1-1+i]>0)*isSymptomatic
  res = cbind(res,isSymptomatic)
}
# write informative headers (symptoms)
nc = ncol(res)
nc1 = nc - length(stiNames)+1
names(res)[nc1:nc] = paste0(as.character(stiNames),"symptomatic")



### FINAL STEP ####

write.csv(res,"startPopulation.csv",row.names = FALSE)

message("Initial individuals generated... ")


# DEBUG
pop0 = read.csv("pop0.out")


