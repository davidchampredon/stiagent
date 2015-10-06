### ===================================
###
### READS SINGLE SIMULATION OUTPUTS
### AND COMPARE THEM TO 
### CALIBRATION TARGETS
###
### Author : David Champredon
### Created: 2014-10-20
### Modified 2015-03-15 (for TpHIV needs)
###
### ===================================

source("read_singleSimul_vs_target_FCT.R")

# Path to folder where output files are
folder.calib =  "../CALIBRATION/" 
folder.out =  "../OUT/" 


# Output type
save.to.file = TRUE
if(save.to.file) pdf("singleSimul_vs_target.pdf",width=25,height=15)


### Number of monte carlo simulations
n.mc <- as.numeric(system(paste0("ls -l ",folder.out,"simul_mc*.out | wc -l"),intern = T))


### ==== RETRIEVE TARGETS ====
all.targets <- read.csv(paste0(folder.calib,"calibration_targets.csv"),header=F)
all.weights <- read.csv(paste0(folder.calib,"calibration_weights.csv"),header=F)

n.calibtimes = ncol(all.targets)
n.calibtypes = nrow(all.targets)

conv.varname <- function(varname)
{
  res = varname
  if (varname=="ageGapDist") res = "ageGapDistrib"
  return(res)
}
  

### ==== MAIN FUNCTION FOR COMPARING DISTRIBUTIONS ====

plot.model.vs.target <- function(varname, xrng=NULL, yrng=NULL)
{
  for(j in 1:n.calibtimes)
    for(i in 1:n.calibtypes)
    {
      if(substr(all.targets[i,j],1,nchar(varname))==varname)
      {
        tf = paste0(folder.calib,all.targets[i,j])
        #print(paste("DEBUG",tf))
        target = read.csv(tf, header=FALSE)
        
        model.all = data.frame()
        for (mc in 1:n.mc){
          model = read.csv(paste0(folder.out,conv.varname(varname),"_D",j-1,"mc",mc, ".out"), 
                        header=FALSE)
          model$mc <- mc
          if (mc==1) model.all = model
          if(mc>1) model.all = rbind(model.all,model)
          
        }
        
        pd = try(plot.2.distributions.mc(target,model.all,
                                         paste0(varname,"\n",all.targets[i,j]),
                                         xrng=xrng,
                                         yrng=yrng),
                 silent=T)
        if(class(pd)=="try-error") plot.2.pointvalues.mc(target,model.all,
                                                         paste0(varname,"\n",all.targets[i,j]))
        
      }
    }
}

n.targets<-sum(all.targets!="")
nn <- as.integer(sqrt(n.targets))
par(mfrow=c(nn,nn+1))


### ==== PLOTS ====

plot.model.vs.target("ageDistrib")
plot.model.vs.target("ageGapDist")

plot.model.vs.target("singleRatio")

plot.model.vs.target("age1sex_f")
plot.model.vs.target("age1sex_m")

plot.model.vs.target("lftNP_f",xrng=c(0,10))
plot.model.vs.target("lftNP_m",xrng=c(0,10))

plot.model.vs.target("ageGap1SexSpouseDistrib_f")

plot.model.vs.target("HIV_prev_age_f",yrng=c(0,0.1))
plot.model.vs.target("HIV_prev_age_m")


if(save.to.file) dev.off()