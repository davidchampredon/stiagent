require(ggplot2); theme_set(theme_bw())
require(gridExtra)
 require(reshape2)

save.to.file <- TRUE



read.all.scenario.suffix <- function(){
  fnames <- system("ls scenario_summary*.csv",intern = T)
  
  for(i in 1:length(fnames)){
    if(i==1) d <- read.csv(file = fnames[i],header = T)
    if(i>1) d <- rbind(d,read.csv(file = fnames[i],header = T))
  }
  return(d)
}

read.all.diff.scenario.suffix <- function(){
  fnames <- system("ls scenario_diff_summary*.csv",intern = T)
  
  for(i in 1:length(fnames)){
    if(i==1) d <- read.csv(file = fnames[i],header = T)
    if(i>1) d <- rbind(d,read.csv(file = fnames[i],header = T))
  }
  return(d)
}



plot.all.scenario.suffix <- function(){
  d <- read.all.scenario.suffix()
  g <- ggplot(d)+geom_bar(aes(x=label,y=s1.minus.s2, fill=suffix),
                          stat="identity",
                          position = "dodge")
  g <- g + facet_wrap(~outcome+sti) + xlab("")+ylab("") +coord_flip()
  g <- g + scale_fill_brewer(palette = "Blues") 
  plot(g)
}



plot.merged.scenario <- function(stiname){
  ### Plot scenario outcomes for a given STI
  ### (note: I do not use "facet_wrap" bc coord_flip combined with "scales="free_y" does not work [ggplot bug] )
  d <- read.all.scenario.suffix()
  outcomes <- (unique(d$outcome))
  g <- list()
  for(i in 1:length(outcomes)){
    
    d.sti <- subset(d,sti==stiname & outcome==outcomes[i])
    g.tmp <- ggplot(d.sti)+geom_bar(aes(x=label,y=s1.minus.s2, fill=suffix),
                                stat="identity",
                                position = "dodge")
    g.tmp <- g.tmp + facet_wrap(~outcome) + xlab("")+ylab("") + scale_fill_brewer(palette = "Blues") +coord_flip()
    g.tmp <- g.tmp + ggtitle(paste("Scenario results for",stiname,"\n outcome",outcomes[i]))
    g[[i]] <- g.tmp
  }
  return(g)
}


read.pop.size <- function(){
  fname <- system("ls population_size_*.csv",intern = T)
  for(i in 1:length(fname)){
    d.tmp <- read.csv(fname[i],header = T)
    if(i==1) d<- d.tmp
    if(i>1) d<- rbind(d,d.tmp)
  }
  return(d)
}



plot.pop.size <- function(){
  pop.size <- read.pop.size()
  pop.size2 <- melt(pop.size,id.vars = c("population"),variable.name = "scenario",value.name = "size")
  pop.size2 <- subset(pop.size2,scenario!="X")
  ggplot(pop.size2)+geom_bar(aes(x=scenario,y=size,fill=population),stat = "identity",position="dodge")+scale_fill_brewer(palette = "Blues")
}


plot.merged.diffBaseline.scenario <- function(){
  ### Plot scenario differences from baseline for a given STI
  
  pop.size <- read.pop.size()
  pop.size2 <- melt(pop.size,id.vars = c("population"),variable.name = "scenario",value.name = "size")
  
  d <- read.all.diff.scenario.suffix()
  d2 <- melt(data = d, id.vars = c("outcome","suffix","sti"),variable.name = "scenario")
  d2 <- d2[d2$scenario!="X",]
  d2$population <- d2$suffix
  g <- ggplot(d2) + geom_bar(aes(x=scenario, y=value, fill = population),
                             stat="identity",
                             position="dodge")
  g <- g + facet_wrap(~sti+outcome,scales="free_y")
  g <- g + scale_fill_brewer(palette = "Blues") + xlab("")+ylab("")
  g <- g + ggtitle(paste("Differences from Baseline\n",pop.size$baseline))
  return(g)
}


read.means <- function(rootname){
  fname <- system(paste0("ls ",rootname,"*_mean_*.csv"),intern=T)
  for(i in 1:length(fname)){
    if(i==1) d <- read.csv(fname[i],header = T)
    if(i>1) d <- rbind(d,read.csv(fname[i],header = T))
  }
  return(d)
}



clean.up.means <- function(rootname){
  x.tp <- read.means(paste0(rootname,"_Tp")) ; x.tp$sti<-"Tp"
  x.hiv <- read.means(paste0(rootname,"_HIV")) ; x.hiv$sti<-"HIV"
  x <- rbind(x.tp,x.hiv)
  x2 <- melt(x,id.vars = c("population","sti"),variable.name = "scenario",value.name = "mean")
  x2 <- x2[x2$scenario!="X",]
  return(x2)
}

plot.mean.outcome <- function(df, title){
  
  g <- ggplot(df)+geom_bar(aes(x=scenario,y=mean,fill=population),
                             position="dodge",
                             stat="identity")
  g <- g + facet_wrap(~sti, scales="free_y")
  g <- g + scale_fill_brewer(palette = "Blues") + ggtitle(title)
  plot(g)
}

plot.all.outcomes <- function()
{
  prev <- clean.up.means("prevalence")
  cuminc <- clean.up.means("incidence")
  mtct <- clean.up.means("mtct")
  
  plot.mean.outcome(prev,"Final Prevalences")
  plot.mean.outcome(cuminc,"Final Cum Incidence (normalized by final pop size)")
  plot.mean.outcome(mtct,"Final Cum MTCT (normalized by final pop size)")
}

###############  ###############  ###############  ###############
###############
###############  ###############  ###############  ###############

if(save.to.file) pdf("merged_scenario.pdf", width=15,height=10)

plot.pop.size()

g.Tp <- plot.merged.scenario("Tp")
g.HIV <- plot.merged.scenario("HIV")
grid.arrange(ncol = 3,
             g.Tp[[1]],g.Tp[[2]],g.Tp[[3]],
             g.HIV[[1]],g.HIV[[2]],g.HIV[[3]])

g.diff <- plot.merged.diffBaseline.scenario()
plot(g.diff)
plot.all.outcomes()

if(save.to.file) dev.off()