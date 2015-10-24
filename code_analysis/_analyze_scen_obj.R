### 
### ANALYZE SCENARIOS (work in progress...)
###
### Started: 2015-10-24

library(dplyr)
library(ggplot2)
library(stiagent,lib.loc = "../Rlibrary//lib")

t0 <- Sys.time()

# path to model input files:
folder_inputs = "../inputs/"
folder_calib = "../calibration/"

# simply run the model:
if(FALSE){
    x <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
                                       folder_calib = folder_calib)
    )
}

# run the model several times for scenario comparison:
system.time(
    allres <- stiagent_comp_interv(params = list(folder_inputs = folder_inputs,
                                                 folder_calib = folder_calib,
                                                 jobnum = 1)
    )
)

scen.name <- unlist(allres[length(allres)])

# Reformat result in data frame (from list of lists...)

z <- as.data.frame(y[["HIV"]])


digit.idx <- function(x){
    ### Return the index of a vector that are digits
    xx<- substring(x, 1:nchar(x), 1:nchar(x))
    return(which(!is.na(as.numeric(xx))))
}

get.scen.name<-function(i){
    return(scen.name[i])
}

get.scen.name(1:2)

format.oneSTI <- function(z,stiname){
    ###
    ### Format the scenarios results of one STI only
    
    z2 <- melt(z)
    z2$variable <- as.character(z2$variable)
    
   
    
    # split the digit(=scenario): "prev_final1" ==> "prev_final" and "1"
    digit.idx(z2$variable[1])
    pos.digit <- mapply(FUN = digit.idx,x=z2$variable)
    z2$response <- substr(z2$variable,start = 1,stop = pos.digit-1)
    z2$scen <- substr(z2$variable,start = pos.digit,stop = pos.digit)
    z2$scen.name <- get.scen.name(as.numeric(z2$scen)+1) #mapply(get.scen.name,z2$scen)
    
    # TO DO: trim the scenario name
    
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

ggplot(df1)+geom_bar(aes(x=scen.name,y=m,fill=sti),stat="identity",position="dodge")+facet_wrap(~response)






t1 <- Sys.time()
message(paste("time elapsed:",round(t1-t0,1),"sec"))
