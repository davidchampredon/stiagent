load("onescen_baseline.RData")
res.base <- res

load("onescen_vaxmass.RData")
res.vax <- res


n.mc <- length(res.base)-1

mergedf <- function(res,n.mc) {
	for (i in 1:n.mc) {
		res[[i]]$df_sim$mc <- i
		res[[i]]$df_sim$mc <- i
		if(i==1) df <- as.data.frame(res[[i]]$df_sim)
		if(i>1) df <- rbind(df,as.data.frame(res[[i]]$df_sim))
	}
	return(df)
}

df.base <- mergedf(res.base, n.mc)
df.vax <- mergedf(res.vax, n.mc)
df.base$scen <- "base"
df.vax$scen <- "vax"

df <- rbind(df.base,df.vax)

library(ggplot2)

g <- ggplot(df) + geom_line(aes(x=time,y=HIVprevRisk2,colour=scen)) + facet_wrap(~mc)
plot(g)


