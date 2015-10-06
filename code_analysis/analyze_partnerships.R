
library(ggplot2);theme_set(theme_bw())
library(gridExtra)
library(plyr)

source("analyze_partnerships_FCT.R")
source("analyze_UTILS.R")
source("analyze_simulation_FCT.R")

# Path to folder where output files are
folder.out =  "../OUT/" 

# Output type
save.to.file = TRUE
if(save.to.file) pdf("analyze_partnerships.pdf",width=30,height=20)


# Number of monte carlo simulations
n.mc <- as.numeric(system(paste0("ls -l ",folder.out,"simul_mc*.out | wc -l"),intern = T))

# Number of calibration times
try(n.ct <- as.numeric(system(paste0("ls -l ",folder.out,"lftNP_f_D*mc1.out | wc -l"),intern = T)),silent = T)

# Retrieve last population from all MC iterations
last.pop.fname <- system(paste0("ls ",folder.out,"last_population_job*.out"), intern=T)
last.pop <- merge.last.pop(last.pop.fname)


# Remove MC iteration where at least one STI goes extinct:
remove.extinct <- FALSE
stinames = read.table(file = paste0(folder.out,"stinames.out"),stringsAsFactors = FALSE,quote = )$V1
simul = read.simulation.file("simul_mc")
idx.extinct <- find.MC.STI.extinction(simul,stinames)
if(length(idx.extinct)>0 & remove.extinct){
  idx.extinct <- find.MC.STI.extinction(simul,stinames)
  last.pop <- subset(last.pop,subset=!(iMC %in% idx.extinct))
}

# Partnerships
last.pop <- calc.partnership.status(last.pop)
last.pop <- calc.age.group(last.pop,4)
last.pop <- last.pop[last.pop$isAlive==1, ]


### === PLOTS ===\

g.prtn <- plot.partnership.status(last.pop)
g.visitCSW <- plot.everVisitCSW(last.pop)
g.conc <- plot.partnership.concur(last.pop, include.all=TRUE)
g.conc2 <- plot.partnership.concur(last.pop, include.all=F)
g.conc.rsk <- plot.partnership.concur.rsk(last.pop)
g.lft.prtn <- plot.lft.prtn.rsk(last.pop, include.all=FALSE)
g.lft.conc <- plot.lft.prtn.conc(last.pop)
g.spouse <- plot.spouse(last.pop)
p.prtn.dur <- plot.prtn.duration(last.pop)
p.prtn.pairs <- plot.partnership.pairs(last.pop, n.mc)
p.prtn.age <- plot.currnt.age(last.pop)
g.max.prtn <- plot.max.nConcPrtn(last.pop)

grid.arrange(g.prtn[[1]],g.prtn[[2]],
             g.conc,g.conc2,g.conc.rsk,
             g.visitCSW,
             g.lft.prtn[[1]],g.lft.prtn[[2]],g.lft.prtn[[3]],
             g.lft.conc,
             #g.spouse[[1]],
             g.spouse[[2]],
             p.prtn.dur,
             p.prtn.pairs[[1]],p.prtn.pairs[[2]],
             p.prtn.age,
             g.max.prtn)


dev.off()



