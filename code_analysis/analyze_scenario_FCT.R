

source("scenario_extinct_FCT.R")
DIR_OUT = "../outputs/"

trim.scenario.name <- function(filename){
  ### EXTRACT SCENARIO NAME FROM FILE NAME
  ### ASSUME NAME TEMPLATE LIKE: "in_scenario_abc.csv" ==> "abc"
  b = gregexpr(pattern ='_',filename,fixed = T)
  c = gregexpr(pattern ='.',filename,fixed = T)
  return(substring(filename,first=max(b[[1]])+1, last=max(c[[1]])-1))
}



get.scenario.names <- function(){
  sn <- read.csv(file=paste0(DIR_OUT,"scenario_names.out"), header = F)
  x <- sapply(X = sn$V2,FUN=trim.scenario.name)
  return(x)
}



plot.scenario.outcomes <- function(d, stiname, col.root.name){
  
  ### Difference of outcomes between scenarios
  ### retrieve the desired outcomes
  
  col.idx = which(x=grepl(names(d), pattern=col.root.name))
  d.select = d[,col.idx]
  n <- ncol(d.select)
  n.mc <- nrow(d.select)
  boxplot(d.select,main=paste0(stiname," ",col.root.name,"\n(mc=",n.mc,")"),xaxt="n")
  points(x=1:n, y=colMeans(d.select), pch=16, cex=2)
  
  lab <- get.scenario.names()
  if(grepl(x=col.root.name, pattern = "diff")) lab <- lab[-1]
  axis(side =1, at = 1:n, labels = lab)
}



plot.all.sceanrio.outcomes <- function(d,stiname){
  
  plot.scenario.outcomes(d,stiname,"prev_final")
  plot.scenario.outcomes(d,stiname,"cuminc_prop")	
  plot.scenario.outcomes(d,stiname,"mtct_prop")
  
  plot.scenario.outcomes(d,stiname,"diff_prev")
	plot.scenario.outcomes(d,stiname,"diff_cuminc")	
  plot.scenario.outcomes(d,stiname,"diff_mtct")
  
  #plot.scenario.outcomes(d,stiname,"cum_inc_final")
  #plot.scenario.outcomes(d,stiname,"mtct_final")
}


compare.mean <- function(d,col.root.name){
  ### Compare the mean of differences from several scenario
  
  # retrieve the desired outcomes
  col.idx = which(x=grepl(names(d), pattern=col.root.name))
  d.select = d[,col.idx]
  
  # mean of outcomes
  mm = colMeans(d.select)
  n.scenario = length(mm)
  nn = n.scenario*(n.scenario-1)/2
  
  # compare all scenarios by pair:
  comp = data.frame(s1=rep(NA,nn), 
                    s2=rep(NA,nn),
                    s1.minus.s2=rep(NA,nn))
  cnt=1
  for(i in 1:n.scenario){
    j = i+1
    while(j<=n.scenario){
      comp[cnt,1]=i
      comp[cnt,2]=j
      comp[cnt,3]=mm[i]-mm[j]
      cnt = cnt+1
      j = j+1
    }
  }
  return(comp)
}


mean.diff <- function(d,col.root.name){
  ### Compare the mean of differences from several scenario
  
  # retrieve the desired outcomes
  col.idx = which(x=grepl(names(d), pattern=col.root.name))
  d.select = d[,col.idx]
  
  # mean of differences
  md = colMeans(d.select)
  return(md)
}

mean.by.scenario <- function(d,suffix,variableName, outFileName){
  ### Compare the mean of differences from several scenario
  
  # retrieve the desired outcomes
  col.idx = which(x=grepl(names(d), pattern=variableName))
  d.select = d[,col.idx]
  
  # mean of differences
  mean.pop = colMeans(d.select)
  names(mean.pop) <- get.scenario.names()
  df <- data.frame(t(mean.pop))
  df$population <- suffix
  write.csv(df,file = paste0(outFileName,suffix,".csv"))
  return(df)
}


mean.population.by.scenario <- function(d, suffix){
  ### Compare the mean of differences from several scenario
  return(mean.by.scenario(d,suffix,"pop_final","population_size_"))
}


save.mean.all.outcomes <- function(d.tp,d.hiv,suffix){
  mean.by.scenario(d.tp,suffix,"prev_final","prevalence_Tp_mean_")
  mean.by.scenario(d.hiv,suffix,"prev_final","prevalence_HIV_mean_")
  
  mean.by.scenario(d.tp,suffix,"cuminc_prop","incidence_Tp_mean_")
  mean.by.scenario(d.hiv,suffix,"cuminc_prop","incidence_HIV_mean_")
  
  mean.by.scenario(d.tp,suffix,"mtct_prop","mtct_Tp_mean_")
  mean.by.scenario(d.hiv,suffix,"mtct_prop","mtct_HIV_mean_")
}


plot.compare <- function(dat,title=""){
  scen <- get.scenario.names()
  
  par(mar=c(5,11,4,2)) # <-- give some space for y-axis labels
  lab <- paste0(scen[dat$s1]," - ",scen[dat$s2])
  barplot(height = dat$s1.minus.s2, names.arg = lab,
          main=title, las=1, horiz=T,
          col = c(1:length(lab)))
  abline(v = 0)
}



read.scenario.sti <- function(stiname, 
                              intervFilename = NA, 
                              remove.extinct = FALSE){
  ### READ SCENARIO OUTCOMES FOR A GIVEN STI
  ### AND RETURNS A DATAFRAME
  
  # Read number of jobs run
  fname.res <- paste0("compare_scenario_",stiname,"_job")
  cmd = paste0("ls ",DIR_OUT,fname.res,"*")
  fname = system(cmd,intern = TRUE)
  njobs = length(fname)
  stopifnot(njobs>=1)
  
  # create data frame from output files
  d = data.frame()
  i <- 1
  for(f in fname){
    d2 = read.csv(f)
    d2$jobnum = rep(i,nrow(d2))
    if(njobs==1) d=d2
    if(njobs>1) d = rbind(d,d2)
    rm(d2)
    i<-i+1
  }
  d$iMC = c(1:nrow(d))
  
  # Remove simulations where STI became extincted
  # in all scenarios:
  if(remove.extinct){
    stopifnot(!is.na(intervFilename))
    if(stiname=="Tp") mc.2.remove <- mc.Tp.extinct.before.interv(intervFilename)
    if(stiname=="HIV") mc.2.remove <- mc.HIV.extinct.before.interv(intervFilename)
    if(!is.na(mc.2.remove[1])) d <- subset(d, !(iMC%in%mc.2.remove))
  }
  
  # calculate proportion from actual number of cases:
  col.pop = which(x=grepl(names(d), pattern="pop_final"))
  n.scen = length(col.pop)
  # cumulative incidence 
  # (scale by final size: not ideal, but can't think of anything else right now!)
  col.cuminc = which(x=grepl(names(d), pattern="cum_inc_final"))
  stopifnot(n.scen==length(col.cuminc))
  cuminc.prop = matrix(nrow=nrow(d),ncol=n.scen)
  for(j in 1:n.scen) cuminc.prop[,j]=d[,col.cuminc[j]]/d[,col.pop[j]]
  colnames(cuminc.prop) <- paste0("cuminc_prop_",0:(n.scen-1))
  
  # MTCT
  col.mtct = which(x=grepl(names(d), pattern="cum_inc_mtct_final"))
  stopifnot(n.scen==length(col.mtct))
  mtct.prop = matrix(nrow=nrow(d),ncol=n.scen)
  for(j in 1:n.scen) mtct.prop[,j]=d[,col.mtct[j]]/d[,col.pop[j]]
  colnames(mtct.prop) <- paste0("mtct_prop_",0:(n.scen-1))
  
  # Binds calculated proportions (:matrices) to data frame
  d = cbind(d,cuminc.prop,mtct.prop)
  
  # --- (Relative) Differences from baseline (=scenario #0) ---
  relativediff <- FALSE
  proportion <- FALSE
  if(proportion){
  diff.prev <- calc.diff(d,colname.raw="prev_final", colname.diff="diff_prev_", n.scen,relativediff)
  diff.mtct <- calc.diff(d,colname.raw="mtct_prop_", colname.diff="diff_mtct_", n.scen,relativediff)
  diff.cuminc <- calc.diff(d,colname.raw="cuminc_prop_", colname.diff="diff_cuminc_", n.scen,relativediff)
  }
  if(!proportion){
    diff.prev <- calc.diff(d,colname.raw="prev_final", colname.diff="diff_prev_", n.scen,relativediff)
    diff.mtct <- calc.diff(d,colname.raw="cum_inc_mtct_final", colname.diff="diff_mtct_", n.scen,relativediff)
    diff.cuminc <- calc.diff(d,colname.raw="cum_inc_final", colname.diff="diff_cuminc_", n.scen,relativediff)
  }
  # Binds again:
  d = cbind(d,diff.prev,diff.cuminc,diff.mtct) #,diff.cuminc.prop)
  write.csv(d,file = paste0("scenario_raw_",stiname,".csv"))
  return(d)
}


calc.diff <- function(d, colname.raw, colname.diff, n.scen, relative=FALSE){
  ### CALCULATE (RELATIVE) DIFFERENCE B/W SCENARIO AND BASELINE
  ### (assumption: BASELINE = FIRST SCENARIO OF THE LIST)
  
  col = which(x=grepl(names(d), pattern=colname.raw))
  col.base = col[1]
  diff.val = matrix(nrow=nrow(d), ncol=n.scen-1)
  for(j in 1:(n.scen-1)) {
    diff.val[,j] <- d[,col[j+1]] - d[,col.base]
    if(relative)  diff.val[,j] <- diff.val[,j]/d[,col.base]
  }
  colnames(diff.val) <- paste0(colname.diff,1:(n.scen-1))
  return(diff.val)
}



select.simulations <- function(){
  ### SELECT SIMULATIONS WHERE NO STI GOES EXTINCT
  d.hiv <- read.scenario.sti(stiname="HIV",
                             intervFilename = "interv_base_HIV.csv",
                             remove.extinct = TRUE)
  
  d.tp <- read.scenario.sti(stiname="Tp",
                            intervFilename = "interv_base_Tp.csv",
                            remove.extinct = TRUE)
  
  select.sim <- which(d.tp$iMC %in% d.hiv$iMC)
  d.hiv <- subset(d.hiv,d.hiv$iMC %in% d.tp$iMC[select.sim])
  write.csv(d.hiv,"scenario_select_HIV.csv")
  write.csv(d.tp,"scenario_select_Tp.csv")
  return(list(d.hiv=d.hiv, d.tp=d.tp))
}



plot.all.comp.scenario <- function(d, stiname, suffix=""){
  ### Plot all comparisons b/w scenario for a given STI
  ### (suffix is used when comparing differentgroup of simulations 
  ### (e.g population with high vs. low STI prevalence))
  
  outcome <- c("prev_final","cuminc_prop","mtct_prop")   #"cum_inc_final", "mtct_final"
  titles.outcome <- c("Prevalence (Final)","Cumulative Incidence","MTCT (Final)")
  scen <- get.scenario.names()
  
  suffix2 <- suffix
  if(suffix!="") suffix2 <- paste0("_",suffix)
  
  for(i in 1:length(outcome)){
    cm <- compare.mean(d,outcome[i])
    plot.compare(cm, paste(stiname,titles.outcome[i]))
    # write results in file, can be merge with other 
    # simulation groups (see 'merge_scenario_suffix.R')
    cm$label <- paste0(scen[cm$s1],"-",scen[cm$s2])
    cm$outcome <- outcome[i]
    cm$suffix <- suffix
    cm$sti <- stiname
    write.csv(cm,file=paste0("scenario_summary_",stiname,"_",outcome[i],suffix2,".csv"))
  }
}

plot.all.diff.comp.scenario <- function(d, stiname, suffix=""){
  ### Plot all _differences_ comparisons b/w scenario for a given STI
  ### (suffix is used when comparing different group of simulations 
  ### (e.g population with high vs. low STI prevalence))
  
  outcome <- c("diff_prev","diff_cuminc","diff_mtct")   #"cum_inc_final", "mtct_final"
  titles.outcome <- c("Diff from Baseline Prevalence (Final)","Diff from Baseline Prevalence Cumulative Incidence","Diff from Baseline Prevalence MTCT (Final)")
  
  scen <- get.scenario.names()[-1] # <-- WARNING: assume first one is baseline
  
  suffix2 <- suffix
  if(suffix!="") suffix2 <- paste0("_",suffix)
  
  for(i in 1:length(outcome)){
    md <- mean.diff(d,outcome[i])
    df <- data.frame(t(md))
    names(df) <- scen
    # plot.compare(cm, paste(stiname,titles.outcome[i]))
    # write results in file, can be merge with other 
    # simulation groups (see 'merge_scenario_suffix.R')
    df$outcome <- outcome[i]
    df$suffix <- suffix
    df$sti <- stiname
    write.csv(df,file=paste0("scenario_diff_summary_",stiname,"_",outcome[i],suffix2,".csv"))
  }
}




plot.hiv.tp <- function(d.hiv,d.tp,outcome){
  
  col.idx <- which(grepl(names(d.hiv),pattern=outcome))
  col.idx2 <- which(grepl(names(d.tp),pattern=outcome))
  n <- length(col.idx)
  stopifnot(n==length(col.idx2))
  par(mfrow=c(sqrt(n),sqrt(n)+1))
  
  jj <-1
  for(j in col.idx){
    plot(x=d.hiv[,j], y=d.tp[,col.idx2[jj]], 
         pch=15,
         xlab="HIV", ylab="Tp", main=paste(outcome,"scen.",jj))
    jj=jj+1
  }
}

