####
#### READ SIMULATION OUTCOMES RUN WITH DIFFERENT SCENARIO
####
#### Created on 2015-01-15 by David Champredon
####

args <- commandArgs(trailingOnly = T)

source("analyze_scenario_FCT.R")
save.to.file <- TRUE

if(save.to.file) pdf("analyze_scenario.pdf",width=25,height=15)

d.hiv <- select.simulations()[["d.hiv"]]
d.tp <- select.simulations()[["d.tp"]]

# plot.hiv.tp(d.hiv,d.tp,"prev_final")
# plot.hiv.tp(d.hiv,d.tp,"cum_inc_final")
# plot.hiv.tp(d.hiv,d.tp,"mtct_final")

par(mfrow=c(2,3))
suffix <- ""
if(length(args)>0) suffix <- args[1]

mean.population.by.scenario(d.tp, suffix) # <-- d.tp or d.hiv, it does not matter (fix this if time)

save.mean.all.outcomes(d.tp,d.hiv,suffix)

plot.all.sceanrio.outcomes(d.tp,"Tp")
plot.all.sceanrio.outcomes(d.hiv,"HIV")

plot.all.comp.scenario(d.tp,"Tp", suffix)
plot.all.comp.scenario(d.hiv,"HIV",suffix)

plot.all.diff.comp.scenario(d.tp,"Tp", suffix)
plot.all.diff.comp.scenario(d.hiv,"HIV",suffix)

if(save.to.file) dev.off()









