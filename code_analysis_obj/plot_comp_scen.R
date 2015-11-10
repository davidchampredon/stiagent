library(ggplot2)
library(tidyr)


# x <- summ.scen

plot.comp.scen <- function(x){
	
	### PLOT COMPARISON OF OUTCOMES B/W SCENARIOS
	###
	
	### Transforms used later on:
	x.wide <- spread(data = x, key = stat, value = val)
	x.nobase <- subset(x,scen!="baseline")
	
	### Raw numbers:
	g <- ggplot(x.wide) + geom_pointrange(aes(x=scen,y=mean,ymin=qLo,ymax=qHi,colour=sti),
										  size=2,
										  position=position_dodge(width=0.3)) 
	g <- g + facet_wrap(~response,scales = "free_y")
	g <- g + ggtitle("Mean of outcomes")
	plot(g)
	
	
	### Difference with 'baseline':
	g.diff <- ggplot(x.nobase) + geom_bar(aes(x=scen ,y=diff.baseline, fill=sti),
										  position="dodge",
										  stat="identity")
	g.diff <- g.diff + facet_wrap(~response,scales = "free_y")
	g.diff <- g.diff + ggtitle("Difference of mean of outcomes")
	plot(g.diff)
	
	g.diff2 <- ggplot(x.nobase) + geom_bar(aes(x=scen ,y=diff.baseline, fill=response),
										  position="dodge",
										  stat="identity")
	g.diff2 <- g.diff2 + facet_wrap(~sti,scales = "free_y")
	g.diff2 <- g.diff2 + ggtitle("Difference of mean of outcomes")
	plot(g.diff2)
	
	### Relative difference with 'baseline':
	g.reldiff <- ggplot(x.nobase) + geom_bar(aes(x=scen, y=reldiff.baseline, fill=sti),
											 position="dodge",
											 stat="identity")
	g.reldiff <- g.reldiff + facet_wrap(~response)
	g.reldiff <- g.reldiff + ggtitle("Relative difference of mean of outcomes")
	plot(g.reldiff)
	
	g.reldiff2 <- ggplot(x.nobase) + geom_bar(aes(x=scen, y=reldiff.baseline, fill=response),
											 position="dodge",
											 stat="identity")
	g.reldiff2 <- g.reldiff2 + facet_wrap(~sti)
	g.reldiff2 <- g.reldiff2 + ggtitle("Relative difference of mean of outcomes")
	plot(g.reldiff2)
}