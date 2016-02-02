library(plyr)
library(ggplot2)

load("/Users/davidchampredon/Dropbox/MyStudies/Syphilis_vax/simul-results/2016-02-01/compScen_A_0.2_1_0p05.RData")

scen.name <- list()
pop.scen <- list()

for (i in 1:length(all.scen)) {
	
	x <- all.scen[[i]]
	scen.name[[i]] <- x[[length(x)]]
	x[[length(x)]] <- NULL
	
	pop <- list()
	for (mc in 1:length(x)) {
		pop[[mc]] <- as.data.frame(x[[mc]]$population)
	}
	
	pop.scen[[i]] <- do.call("rbind",pop)
	pop.scen[[i]]$scen <- scen.name[[i]]
}

df <- do.call("rbind",pop.scen)
df$sa <- df$nSexActs_lifetime/(df$age-df$age1sex)
df1 <- subset(df,riskgroup==2)

df2 <- ddply(df1,c("scen","riskgroup"),#,"gender"),
			 summarize,
			 m=mean(sa),
			 qlo=quantile(sa,probs = 0.25),
			 qhi=quantile(sa,probs = 0.75))
df2


ddply(df,c("scen"),summarize,n=mean(riskgroup))
ddply(df,c("scen"),summarize,n=mean(sa))

plot(df$age,df$nSexActs_lifetime)


# g <- ggplot(df1)+geom_bar(aes(x=factor(riskgroup),
# 								 y=mean(nSexActs_lifetime+1),
# 								 colour=scen), position="dodge",stat="identity")
# plot(g)
