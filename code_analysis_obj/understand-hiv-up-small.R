load("onescen_baseline.RData")
res.base <- res

load("onescen_vaxmass.RData")
res.vax <- res


n.mc <- length(res.base)-1

mergedf <- function(res,n.mc) {
	for (i in 1:n.mc) {
		res[[i]]$df_sim$mc <- i
		res[[i]]$df_sim$cumNewBorn <- cumsum(res[[i]]$df_sim$nNewBorn)
		
		res[[i]]$df_sim$cumnSexActRisk2 <- cumsum(res[[i]]$df_sim$nSexActRisk2)
		
		if(i==1) df <- as.data.frame(res[[i]]$df_sim)
		if(i>1) df <- rbind(df,as.data.frame(res[[i]]$df_sim))
	}
	return(df)
}

df.base <- mergedf(res.base, n.mc)
df.vax <- mergedf(res.vax, n.mc)
df.base$scen <- "base"
df.vax$scen <- "vax"

summary(df.base$HIVprev)
summary(df.vax$HIVprev)

df <- rbind(df.base,df.vax)

library(ggplot2)

# "time"           "nAlive"         "nDead"          "nPartn"         "nSp"            "nFemale"       
# [7] "HIV"            "Tp"             "nHIVTp"         "nHIVTp0"        "nHIVTp1"        "nHIVTp2"       
# [13] "nHIVTp9"        "nCSW"           "nRskGrp0"       "nRskGrp1"       "nRskGrp2"       "nCircum"       
# [19] "nNewBorn"       "nPregnantRisk0" "nPregnantRisk1" "nPregnantRisk2" "nPregnantRisk9" "mtctHIV"       
# [25] "mtctTp"         "HIVprev"        "Tpprev"         "HIVprevRisk0"   "HIVprevRisk1"   "HIVprevRisk2"  
# [31] "HIVprevRisk9"   "TpprevRisk0"    "TpprevRisk1"    "TpprevRisk2"    "TpprevRisk9"    "Reff_HIV"      
# [37] "Reff_Tp"        "nSexActRisk0"   "nSexActRisk1"   "nSexActRisk2"   "nSexActRisk9"   "mc"            
# [43] "cumNewBorn"     "scen" 

g <- ggplot(df) + geom_line(aes(x=time, y=(cumNewBorn), colour=scen)) + facet_wrap(~mc)
plot(g)


