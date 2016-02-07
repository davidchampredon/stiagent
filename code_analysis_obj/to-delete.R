library(ggplot2)
load("/Users/davidchampredon/Dropbox/MyStudies/Syphilis_vax/simul-results/2016-02-04/compScen_B_0.2_1_0p05.RData")
# load("/Users/davidchampredon/Dropbox/MyStudies/Syphilis_vax/simul-results/2016-02-04/compScen_B_0.2_1_0p05_nosexreduc.RData")

sim0 <- all.scen[[1]]
sim <- all.scen[[3]]

z0 <- list()
z <- list()
for( mc in 1:20){
z[[mc]] = as.data.frame(sim[[mc]]$df_sim)
z[[mc]]$mc <- mc
z[[mc]]$scen <- "vax"

z0[[mc]] = as.data.frame(sim0[[mc]]$df_sim)
z0[[mc]]$mc <- mc
z0[[mc]]$scen <- "base"
}

df0 <- do.call("rbind",z0)
df <- do.call("rbind",z)
D <- rbind(df0,df)

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
							 y=HIVprevRisk9,
							 colour=scen))+facet_wrap(~mc)
plot(g)

library(plyr)
D2 <- ddply(D,c("tb","mc","scen"),summarize,n=mean(nNewBorn))  # nNewBorn  mtctHIV
g <- ggplot(D2)+geom_line(aes(x=tb,y=n,colour=scen))+facet_wrap(~mc)
plot(g)

zz = subset(D, time >49)
zz$r = zz$HIV/ zz$nAlive
ggplot(zz)+geom_boxplot(aes(x=scen,y=r))

zz2 = subset(D2, tb >=49)
ggplot(zz2)+geom_boxplot(aes(x=scen,y=n))



