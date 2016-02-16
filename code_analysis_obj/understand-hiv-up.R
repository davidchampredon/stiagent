library(plyr)
library(splines)
library(ggplot2)

# load("/Users/davidchampredon/Dropbox/MyStudies/Syphilis_vax/simul-results/2016-02-07/compScen_B_0.2_1_0p05_symptNoEffect.RData")
# load("/Users/davidchampredon/Dropbox/MyStudies/Syphilis_vax/simul-results/2016-02-04/compScen_A_0.2_1_0p05_nosexreduc.RData")
load("/Users/davidchampredon/GitHub/__fromearnserv/stiagent/compScen_B_0.2_1_0p05.RData")

sim0 <- all.scen[[1]]
sim <- all.scen[[2]]
# sim <- all.scen[[3]]  # <-- when 5 scenarios run
sim[[length(sim)]]


z0 <- list()
z <- list()
pop0 <- list()
pop <- list()

for( mc in 1:(length(sim)-1)){
	z0[[mc]] = as.data.frame(sim0[[mc]]$df_sim)
	z0[[mc]]$mc <- mc
	z0[[mc]]$scen <- "base"
	
	z[[mc]] = as.data.frame(sim[[mc]]$df_sim)
	z[[mc]]$mc <- mc
	z[[mc]]$scen <- "vax"
	
	
	pop0[[mc]] = as.data.frame(sim0[[mc]]$population)
	pop0[[mc]]$mc <- mc
	pop0[[mc]]$scen <- "base"
	
	pop[[mc]] = as.data.frame(sim[[mc]]$population)
	pop[[mc]]$mc <- mc
	pop[[mc]]$scen <- "vax"
}

df0 <- do.call("rbind",z0)
df <- do.call("rbind",z)
D <- rbind(df0,df)

Pdf0 <- do.call("rbind",pop0)
Pdf <- do.call("rbind",pop)
P <- rbind(Pdf0,Pdf)
P <- P[P$isalive>0,]



# - - - - - - - TIME SERIES - - - - - -  -


D$mtctHIV.inc <- c(0,diff(D$mtctHIV))
D$mtctHIV.inc[D$mtctHIV.inc<0] <- 0
D$mtctHIV.inc.birth <- D$mtctHIV.inc / D$nNewBorn
D$mtctHIV.inc.birth[D$nNewBorn] <-0 
D$tb <- round(D$time)
# "time"         "nAlive"       "nDead"        "nPartn"       "nSp"          "nFemale"      "HIV"         
# [8] "Tp"           "nHIVTp"       "nHIVTp0"      "nHIVTp1"      "nHIVTp2"      "nHIVTp9"      "nCSW"        
# [15] "nRskGrp0"     "nRskGrp1"     "nRskGrp2"     "nCircum"      "nNewBorn"     "mtctHIV"      "mtctTp"      
# [22] "HIVprev"      "Tpprev"       "HIVprevRisk0" "HIVprevRisk1" "HIVprevRisk2" "HIVprevRisk9" "TpprevRisk0" 
# [29] "TpprevRisk1"  "TpprevRisk2"  "TpprevRisk9"  "Reff_HIV"     "Reff_Tp"      "nSexActRisk0" "nSexActRisk1"
# [36] "nSexActRisk2" "nSexActRisk9" "mc"           "scen"      


g <- ggplot(D)+geom_line(aes(x=time,
							 y =  HIVprev ,
							 colour=scen))+facet_wrap(~mc)
plot(g)


g <- ggplot(D)+geom_line(aes(x=time,
							 y =  HIVprevRisk0 ,
							 colour=scen))+facet_wrap(~mc)
plot(g)
g <- ggplot(D)+geom_line(aes(x=time,
							 y =  HIVprevRisk1 ,
							 colour=scen))+facet_wrap(~mc)
plot(g)
g <- ggplot(D)+geom_line(aes(x=time,
							 y =  HIVprevRisk2 ,
							 colour=scen))+facet_wrap(~mc)
plot(g)

zz = subset(D, time >49)
zz$r = zz$HIV/ zz$nAlive
ggplot(zz)+geom_boxplot(aes(x=scen,y=r))


D2 <- ddply(D,c("tb","mc","scen"),summarize,
			n=mean(nNewBorn/nFemale))  # nNewBorn  mtctHIV
g <- ggplot(D2)+geom_line(aes(x=tb,y=n,colour=scen))+facet_wrap(~mc)
g <- g + ggtitle("new born per female per time step (avg)")
plot(g)

D2 <- ddply(D,c("tb","mc","scen"),summarize,
			n=mean(nSexActRisk1/nRskGrp1))
g <- ggplot(D2)+geom_line(aes(x=tb,y=n,colour=scen))+facet_wrap(~mc)
g <- g + ggtitle("n sex acts per indiv in risk group per time step (avg)")
plot(g)


zz2 = subset(D2, tb >=49)
ggplot(zz2)+geom_boxplot(aes(x=scen,y=n))


# - - - - - - - POPULATIONS - - - - - -  -

# [1] "uid"               "dateinpop"         "isalive"           "gender"            "age"              
# [6] "riskgroup"         "nCurrPartn"        "nCurrMaxPartn"     "nCurrSpouse"       "nLifetimePartner" 
# [11] "nLifetimeSpouse"   "circum"            "singleDur"         "age1sex"           "age1partner"      
# [16] "age1spouse"        "everVisitCSW"      "isPregnant"        "gestDur"           "nChild"           
# [21] "nSexActs_lifetime" "UIDpartner1"       "durPrtn1"          "UIDpartner2"       "durPrtn2"         
# [26] "UIDpartner3"       "durPrtn3"          "UIDpartner4"       "durPrtn4"          "UIDpartner5"      
# [31] "durPrtn5"          "HIVduration"       "HIVsympt"          "HIVtreat"          "HIVimmun"         
# [36] "HIVvaccTime"       "HIV_IC"            "Tpduration"        "Tpsympt"           "Tptreat"          
# [41] "Tpimmun"           "TpvaccTime"        "Tp_IC"  

P$HIVpos <- as.numeric(P$HIVduration>0)
P$hasChild <- as.numeric(P$nChild>0)

g <- ggplot(P)
g <- g + geom_smooth(aes(x= age,
						 y= HIVpos,
						 colour = scen),
			   method="glm", 
			   method.args = list(family = "binomial"),
			   formula=y~ns(x, 5),
			   alpha=0.1, size=2)
g <- g + facet_wrap(~gender + riskgroup, scales = "free_y")
plot(g)


g <- ggplot(P)
g <- g + geom_smooth(aes(x= age,
						 y= hasChild,
						 colour = scen),
					 method="glm", 
					 method.args = list(family = "binomial"),
					 formula=y~ns(x, 5),
					 alpha=0.1, size=2)
g <- g + facet_wrap(~riskgroup, scales = "free_y")
plot(g)



g <- ggplot(P)
g <- g + geom_histogram(aes(nCurrPartn, fill=scen),binwidth=1,position='dodge')
g <- g + facet_wrap(~gender + riskgroup, scales = "free_y")
plot(g)

g <- ggplot(P)
g <- g + geom_density(aes(log10(nSexActs_lifetime/age+1), colour=scen),size=2)
g <- g + facet_wrap(~gender+riskgroup, scales = "free_y")
plot(g)

g <- ggplot(P)
g <- g + geom_density(aes(log10(nLifetimePartner/age+1), colour=scen),size=1,adjust=1.9)
g <- g + facet_wrap(~gender+riskgroup, scales = "free") + scale_y_log10()
plot(g)


g <- ggplot(P)
g <- g + geom_density(aes(log(HIV_IC+0.0001), colour=scen),size=1)
g <- g + facet_wrap(~gender+riskgroup, scales = "free_y")
plot(g)



ddply(P,c("scen","gender"),summarize,n=mean(HIVpos))
