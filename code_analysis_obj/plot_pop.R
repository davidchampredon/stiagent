###
###   PLOT POPULATION FEATURES
###


source("utils.R")

calc.density.summ <- function(sim,outcome, categ,
							  qlo = 0.1,
							  qhi = 0.9){
	
	### CALCULATE THE MEAN DENSITY OF AN OUTCOME
	### ACROSS ALL MC, SPLIT BY CATEGORIES
	
	P.all <- get.population(sim)
	nMC <- get.nMC(sim)
	
	D <- list()
	k=1
	for(i in 1:nMC){
		P <- P.all[P.all$mc==i,c(outcome,categ)]   

		u.categ <- unique(unlist(P[,categ]))
		
		for(g in 1:length(u.categ)){
			Pg <- P[P[,categ]==u.categ[g],]
			z <- unlist(Pg[,outcome])
			h <- hist(x=z, plot = F,breaks = c(0:max(z)))
			D[[k]] <- data.frame(x=h$breaks, dens=c(h$density,0), mc=i, categ=u.categ[g])
			names(D[[k]])[names(D[[k]])=="categ"] <- categ
			k <- k+1
		}
	}
	
	D.all <- dplyr::rbind_all(D)
	
	D.summ <- ddply(D.all,c("x",categ),summarize,
					m=mean(dens),
					qLo = quantile(dens,probs=qlo),
					qHi = quantile(dens,probs=qhi))
	return(D.summ)
}

A <- calc.density.summ(sim,outcome,categ)

g <- ggplot(A)+geom_line(aes(x=x,y=m,colour=factor(gender)))
g <- g + geom_pointrange(aes(x=x+gender/15,
							 y=m,
							 ymin=qLo,
							 ymax=qHi,
							 colour=factor(gender)),
						 size=1)
plot(g)



# 
# 
# g <- ggplot(P)+geom_histogram(aes(x=nCurrPartn,y=..density..,fill=factor(Gender)),
# 							  binwidth=1,origin = -0.5,colour="gray")
# g <- g  + facet_wrap(~Gender)
# plot(g)
# 
# 
# summary(P$nCurrPartn)
