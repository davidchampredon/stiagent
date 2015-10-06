### ===================================
###
### READS SINGLE SIMULATION OUTPUTS
### AND COMPARE THEM TO 
### CALIBRATION TARGETS
###
### Author : David Champredon
### Created: 2014-10-20
###
### ===================================

source("read_singleSimul_vs_target_FCT.R")

# Path to folder where output files are
#folder =  "./" 
folder.calib =  "../CALIBRATION/" 
folder.out =  "../OUT/" 


# Choice of report according to data set

do_nLifeSexPartnerDistrib = FALSE   # TRUE if recent DHS survey
do_malesVisitCSW = FALSE  # TRUE if recent DHS survey

# Output type
save.to.file = TRUE
if(save.to.file) pdf("singleSimul_vs_target.pdf",width=20,height=15)




### ==== DEMOGRAPHICS ====

par(mfrow=c(2,2))

target.AD = read.csv(paste0(folder.calib,"calib_target_ageDistrib.csv"), header=FALSE)
AD = read.csv(paste0(folder.out,"census_ageDistribution.out"), header=FALSE)
plot.2.distributions(target.AD,AD,"Age Distribution")



### ==== PARTNERSHIPS ====

#par(mfrow=c(2,2))

target.AGD = read.csv(paste0(folder.calib,"calib_target_ageGapDistrib.csv"), header=FALSE)
AGD = read.csv(paste0(folder.out,"census_ageGapDistribution.out"), header=FALSE)
plot.2.distributions(target.AGD,AGD,"Age Gap Distribution",xrng = c(-30,30))



target.SRf = read.csv(paste0(folder.calib,"calib_target_singleRatio.csv"), header=TRUE)
target.SRf = target.SRf[target.SRf$gender=="female",2]
SRf = read.csv(paste0(folder.out,"census_ratioSingles_f.out"), header=FALSE)
plot.2.pointvalues(target.SRf,SRf,"Single Ratio (female)")


target.SRm = read.csv(paste0(folder.calib,"calib_target_singleRatio.csv"), header=TRUE)
target.SRm = target.SRm[target.SRm$gender=="male",2]
SRm = read.csv(paste0(folder.out,"census_ratioSingles_m.out"), header=FALSE)
plot.2.pointvalues(target.SRm,SRm,"Single Ratio (male)")

### ==== SEXUAL BEHAVIOUR ====

par(mfrow=c(3,2))

target.AFSD.f = read.csv(paste0(folder.calib,"calib_target_ageFirstSex_f.csv"), header=FALSE)
AFSD.f = read.csv(paste0(folder.out,"census_ageFirstSexDistribution_f.out"), header=FALSE)
plot.2.distributions(target.AFSD.f,AFSD.f,"Age First Sex Distribution (female)")

target.AFSD.m = read.csv(paste0(folder.calib,"calib_target_ageFirstSex_m.csv"), header=FALSE)
AFSD.m = read.csv(paste0(folder.out,"census_ageFirstSexDistribution_m.out"), header=FALSE)
plot.2.distributions(target.AFSD.m,AFSD.m,"Age First Sex Distribution (male)")


if (do_nLifeSexPartnerDistrib)
{
  target.NLS.f = read.csv(paste0(folder.calib,"calib_target_nLifeSexPartnerDistrib_f.csv"), header=FALSE)
  NLS.f = read.csv(paste0(folder.out,"census_nLifeSexPrtnrDistrib_f.out"), header=FALSE)
  plot.2.distributions(target.NLS.f,NLS.f,
                       "Lifetime Number of Sex Partners Distribution (female)",
                       xrng=c(0,20))
  
  target.NLS.m = read.csv(paste0(folder.calib,"calib_target_nLifeSexPartnerDistrib_m.csv"), header=FALSE)
  NLS.m = read.csv(paste0(folder.out,"census_nLifeSexPrtnrDistrib_m.out"), header=FALSE)
  plot.2.distributions(target.NLS.m,NLS.m,
                       "Lifetime Number of Sex Partners Distribution (male)",
                       xrng=c(0,20))
}


if (do_malesVisitCSW)
{
  target.MVC = read.csv(paste0(folder.calib,"calib_target_malesVisitCSW.csv"), header=FALSE)
  target.MVC = target.MVC[target.MVC$V1=="prop.paidsex",2]
  MVC = read.csv(paste0(folder.out,"census_maleVisitCSW.out"), header=FALSE)
  plot.2.pointvalues(target.MVC,MVC,"Proportion of Males Visiting CSW")
  
  target.AMVC = read.csv(paste0(folder.calib,"calib_target_malesVisitCSW_ageDistrib.csv"), header=FALSE)
  AMVC = read.csv(paste0(folder.out,"census_ageMalesVisitCSWDistribution.out"), header=FALSE)
  plot.2.distributions(target.AMVC,AMVC,
                       "Age Distribtion of Males Visiting CSW")
}

### ==== STI PREVALENCES ====

target.hiv.age.f = read.csv(paste0(folder.calib,"calib_target_HIV_prev_age_f.csv"),header=FALSE)
target.hiv.age.m = read.csv(paste0(folder.calib,"calib_target_HIV_prev_age_m.csv"),header=FALSE)

hiv.age.f = read.csv(paste0(folder.out,"HIV_prev_age_f.out"), header=FALSE)
hiv.age.m = read.csv(paste0(folder.out,"HIV_prev_age_m.out"), header=FALSE)

plot.2.distributions(target.hiv.age.f,hiv.age.f,"HIV prevalence by age (Female)")
plot.2.distributions(target.hiv.age.m,hiv.age.m,"HIV prevalence by age (Male)")


if(save.to.file) dev.off()