###
### VARIOUS UTILS FUNCTIONS FOR ANALYZING SIMULATIONS
###


find.MC.STI.extinction <- function(simul,stinames){
  ### Find MC iterations where at least one STI goes extinct
  
  simul1 = melt(simul,measure.vars=stinames)
  simul1$prev = simul1$value
  # if prevalence is 0 after half horizon time
  # then it's considered extinct:
  simul2 <- subset(simul1,time>max(time)/2)
  
  df <- ddply(simul2,c("iMC"),summarize,prev.min = min(prev))
  res <- which(df$prev.min==0)
  return(res)
}
