### 
### ANALYZE SCENARIOS (work in progress...)
###
### Started: 2015-10-24

library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(stiagent,lib.loc = "../Rlibrary/lib")

t0 <- Sys.time()

# path to model input files:
folder_inputs = "../inputs/"
folder_calib = "../calibration/"

# simply run the model:
if(T){
    scen.file <- "in_scenario_vaxMass.csv"
    x <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
                                       folder_calib = folder_calib,
                                       scenario_file = scen.file,
                                       MC_id = 1)
    )
}




# run in parallel using snowfall:
# library(snowfall)
# 
# n.mc <- 4
# n.cpu <- 2
# sfInit(parallel = TRUE, cpu = n.cpu)
# sfLibrary(stiagent,lib.loc = "../Rlibrary//lib")
# 
# stiagent_runsim_snowWrap <- function(i){
#     # snowfall wrap 
#     x <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
#                                        folder_calib = folder_calib,
#                                        scenario_file = scen.file,
#                                        MC_id = i))
#     return(x)
# }
# 
# ### Parallel execution:
# sfExportAll()
# idx.apply <- 1:n.mc
# system.time(res <- sfSapply(idx.apply, stiagent_runsim_snowWrap, simplify = F))
# sfStop()





# run the model several times for scenario comparison:
system.time(
    allres <- stiagent_comp_interv(params = list(folder_inputs = folder_inputs,
                                                 folder_calib = folder_calib,
                                                 jobnum = 1)
    )
)

###############################################################
###   Reformat result in data frame (from list of lists...)
###############################################################

scen.name <- unlist(allres[length(allres)])
z <- as.data.frame(allres[["HIV"]])

digit.idx <- function(x){
    ### Return the index of a vector that are digits
    xx<- substring(x, 1:nchar(x), 1:nchar(x))
    return(which(!is.na(as.numeric(xx))))
}

get.scen.name<-function(i){
    return(scen.name[i])
}


format.oneSTI <- function(z,stiname){
    ###
    ### Format the scenarios results of one STI only
    
    z2 <- melt(z)
    z2$variable <- as.character(z2$variable)
    
    # split the digit(=scenario): 
    # for exple: "prev_final1" ==> "prev_final" and "1"
    digit.idx(z2$variable[1])
    pos.digit <- mapply(FUN = digit.idx,x=z2$variable)
    z2$response <- substr(z2$variable,start = 1,stop = pos.digit-1)
    
    z2$scen <- substr(z2$variable,start = pos.digit,stop = pos.digit)
    z2$scen.name <- get.scen.name(as.numeric(z2$scen)+1) #mapply(get.scen.name,z2$scen)
    z2$scen.name <- gsub(pattern = "in_scenario_",replacement = "",x = z2$scen.name)
    z2$scen.name <- gsub(pattern = ".csv",replacement = "",x = z2$scen.name)
    
    # check if all responses have the same number of MC iterations
    tmp <- ddply(z2,"variable",summarize,nmc=length(variable))
    n.mc <- unique(tmp$nmc)
    stopifnot(length(n.mc)==1)
    
    # inset MC iteration:
    nvar <- length(unique(z2$variable))
    z2$mc <- rep(1:n.mc,nvar)
    z2$sti <- stiname
    return(z2)
}

nsti <- length(allres)-1
dtmp <- list()
for(i in 1:nsti){
    dtmp[[i]] <- format.oneSTI(as.data.frame(allres[[i]]),stiname=names(allres)[i])
}

df.allres <- dplyr::rbind_all(dtmp)
df1 <- ddply(df.allres,c("sti","scen.name","response"),summarize,m=mean(value))


### Difference of response variables values
### to "baseline" scenario:

df1$diff.baseline <- NA
df1$reldiff.baseline <- NA

for(i in 1:nrow(df1)){
    if(!grepl("baseline",df1$scen.name[i])){
        
        # Retrieve the associated value from baseline scenario   
        idx.i <- which(df1$sti==df1$sti[i] &
                           df1$response==df1$response[i] & 
                           grepl("baseline",df1$scen.name))
        # Calculate differences:
        base.i <- df1$m[idx.i]
        df1$diff.baseline[i] <- df1$m[i] - base.i
        df1$reldiff.baseline[i] <- df1$m[i]/base.i-1
    }
}

########################################
###   PLOTS
########################################

g0 <- ggplot(df1)
g <- g0 + geom_bar(aes(x=scen.name,y=m,fill=sti),stat="identity",position="dodge")
g <- g + facet_wrap(~response,scales = "free_y")
g <- g + ggtitle("Mean Values")
plot(g)

g.diff <- g0 + geom_bar(aes(x=scen.name,y=diff.baseline,fill=sti),
                        stat="identity",position="dodge")
g.diff <- g.diff + facet_wrap(~response,scales = "free_y")
g.diff <- g.diff + ggtitle("Difference of Mean Values")
plot(g.diff)

g.reldiff <- g0 + geom_bar(aes(x=scen.name,y=reldiff.baseline,fill=sti),
                           stat="identity",position="dodge")
g.reldiff <- g.reldiff + facet_wrap(~response,scales = "free_y")
g.reldiff <- g.reldiff + ggtitle("Relative Difference of Mean Values")
plot(g.reldiff)

save.image(file = "simscen.RData")

# ===================================
t1 <- Sys.time()
message(paste("time elapsed:",round(t1-t0,1),"sec"))
