####################################
# GRAPHICAL REPRESENTATION OF 
# FUNCTIONAL FORMS FOR PARAMETERS OF
# - DEMOGRAPHIC 
# - PARTNERSHIP DYNAMICS
####################################


prm.DMG = read.csv("in_paramDMG.csv")
prm.FORM = read.csv("in_paramFORM.csv",header=FALSE)
prm.DISSOL = read.csv("in_paramDISSOL.csv",header=FALSE)

age = seq(10,80,by=0.2)

# ====== DMG =========

# ====== FORM =========

formation.age.age <- function(param,x)
{
    mean.age = param[prm.FORM$V1=="formation_meanAge_female",2]
    sigma.age = param[prm.FORM$V1=="formation_varAge_female",2]
    
    mean.agegap = param[prm.FORM$V1=="formation_meanAgeGap",2]
    sigma.agegap =param[prm.FORM$V1=="formation_varAgeGap",2]
    
    correl.age.agegap = param[prm.FORM$V1=="formation_correl_Age_AgeGap",2]
    
    shape = param[prm.FORM$V1=="formation_shapeAge",2]
    
    sa2 = sigma.age*sigma.age
    sg2 = sigma.agegap*sigma.agegap
    rhoss = correl.age.agegap*sigma.age*sigma.agegap
    
    M = matrix(c(sa2,rhoss,rhoss,sg2),nrow=2)
    M = solve(M)
    
    # AGE GAP IS FIXED
    age.gap = mean.agegap
    
    tmp = numeric(length(x))
    res = numeric(length(x))
    
    for (k in 1:length(x))
    {
        z = c(x[k]-mean.age, age.gap-mean.agegap)
        tmp[k] = shape*t(z) %*% M %*% z
        res[k] = exp(-tmp[k])
    }
    return(res)
}

age.female = age
formation.proba = formation.age.age(prm.FORM,age.female)

plot(x=age.female,y=formation.proba,typ="l",
     lwd = 3,
     main="Partnership formation probability\n (age gap @ mean)")
grid()


# ====== DISSOL =========

par(mfrow=c(1,1))

dissolution.age.conc <- function(param,ncp,x.age)
{
    s = param[prm.DISSOL$V1=="dissolution_ageConcPartn",2]
    #s = 1.9
    tmp2=0
    if (ncp>1) tmp2=1
    #res =1/(1+exp(-tmp))*tmp2
    pow = 8
    one = rep(x=1,times=length(x.age))
    res =pmin(one, x.age^pow/80^pow*(s*ncp)*tmp2)
}

ncp=c(1:6)

age2=rep(x=30,times=length(age))
ncp2=2

proba.dissol2 = dissolution.age.conc(prm.DISSOL,ncp2,age2)

for (i in 1:length(ncp))
{
    proba.dissol1 = dissolution.age.conc(prm.DISSOL,ncp[i],age)
    proba.dissol=pmax(proba.dissol1,proba.dissol2)
    if (i==1) plot(x=age, y=proba.dissol,
                  typ="l", ylim=c(0,1),lwd=i,
                   main=paste("Dissolution probability \n Age and concurrent partnerships\n (age2=",age2[1],"; ncp2=",ncp2,")"))
    if (i>1) lines(x=age,  y=proba.dissol,lwd=i)
}
legend(x="topleft",legend=paste("ncp=",ncp),lwd=ncp)
grid()
    
    
    