##############################
### TRANSMISSION ANALYSIS  ###
##############################

DIR_OUT <- "../outputs/"
save.to.file <- TRUE

if(save.to.file) pdf("analyze_transmissions.pdf", width=20,height=15)

library(plyr)
library(ggplot2)
library(gridExtra)
source("analyze_transmissions_FCT.R")

message("WARNING: analyze_transmissions DOES NOT FILTER OUT MC ITERATIONS WITH EXTINCT STI")

theme_set(theme_bw())

### DATA IMPORT ###
transm = read.csv(paste0(DIR_OUT,"transmissions.out"),stringsAsFactors=FALSE,header = T)
transm.succ = read.csv(paste0(DIR_OUT,"transmission_success.out"),stringsAsFactors=FALSE, header = T)
transm.details = read.csv(paste0(DIR_OUT,"transmission_details.out"),stringsAsFactors=FALSE,header = T)

# Probability of transmission per sex act:
succ <- ddply(transm.succ,c("stiname"),summarize,n=sum(nSexAct),n.trans=sum(successTransm))
succ$transm.per.sexact <- succ$n.trans/succ$n
succ

# === WHO INFECTS WHO? ===

g.gender <- plot.infector.gender(transm)
g.riskgroup <- plot.infector.riskGroup(transm)
g.GI <- plot.generation.interval(transm)
g.inc.rsk <- plot.incid.risk.sti.time(transm)

grid.arrange(g.gender,g.riskgroup,
             g.inc.rsk,g.GI)


if(save.to.file) dev.off()

######################################################################
######################################################################