### EXPLORATORY CODE !

library(igraph)
library(ggplot2);theme_set(theme_bw())
library(gridExtra)
library(plyr)

source("analyze_UTILS.R")
source("analyze_simulation_FCT.R")
source("analyze_partnerships_FCT.R")

# Path to folder where output files are
folder.out =  "../outputs/" 

# Number of monte carlo simulations
n.mc <- as.numeric(system(paste0("ls -l ",folder.out,"simul_mc*.out | wc -l"),intern = T))

# Retrieve last population from all MC iterations
last.pop.fname <- system(paste0("ls ",folder.out,"last_population_job*.out"), intern=T)
last.pop <- merge.last.pop(last.pop.fname)

# Remove MC iteration where at least one STI goes extinct:
stinames = read.table(file = paste0(folder.out,"stinames.out"),stringsAsFactors = FALSE,quote = )$V1
simul = read.simulation.file("simul_mc")
idx.extinct <- find.MC.STI.extinction(simul,stinames)
if(length(idx.extinct)>0){
  simul <- subset(simul,subset=!(iMC %in% idx.extinct))
last.pop <- subset(last.pop,subset=!(iMC %in% idx.extinct))
}

create.edge.list <- function(Z){
  tmp0 <- cbind(Z$UID, Z$partnerUID0)
  tmp1 <- cbind(Z$UID, Z$partnerUID1)
  tmp2 <- cbind(Z$UID, Z$partnerUID2)
  tmp3 <- cbind(Z$UID, Z$partnerUID3)
  tmp4 <- cbind(Z$UID, Z$partnerUID4)
  tmp <- na.exclude(rbind(tmp0,tmp1,tmp2,tmp3,tmp4))
  
  # Remove duplicated pairs
  n <- nrow(tmp)
  x = numeric(n)
  for(i in 1:n) x[i]<-paste(sort(c(tmp[i,1],tmp[i,2])),collapse="")
  tmp <- tmp[!duplicated(x),]
  return(data.frame(from=tmp[,1],to=tmp[,2]))
}

avg.path.length <- numeric(n.mc)
diam <- numeric(n.mc)

par(mfrow=c(2,2))

for(i in 1:n.mc){
  # Retrive data for that MC iteration
  Z <- subset(last.pop,subset = (isAlive==1 & iMC==i))
  
  # Create network object
  el <- create.edge.list(Z)
  net <- graph.data.frame(el,directed = F)
  
  # Metrics
  avg.path.length[i] <- average.path.length(net)
  diam[i] <- diameter(net)  #get.diameter(net)
  
  # Because heterosexual network, transitivity must be 0
  stopifnot(transitivity(net)==0)
  
  hh <- hist(degree(net),breaks=c(1:15),plot=F)
  if(i==1) plot(x=hh$breaks[1:(length(hh$breaks)-1)], y=log10(hh$density), typ="o",
                main = "Degree Distribution",xlab="degree",ylab="Log10 Density")
  if(i>1) lines(x=hh$breaks[1:(length(hh$breaks)-1)], y=log10(hh$density), typ="o",col=i)
  
  bw <- betweenness(net, normalized = F)
  lbw <- log10(bw)
  dbw <- density(lbw)
  if(i==1) plot(dbw, main="Log10 Betweeness Distribution",xlab="")
  if(i>1) lines(dbw)
}

barplot(avg.path.length,main="avg path length")
barplot(diam,main="Diameter")



