
################################################
###
### Create 'copied' input files from default ones
### by just changing the value of the specified
### parameters.
###
################################################

# Sensitivity analysis is focused
# on vaccine features, so only one file
# will be modified, but set it as variable for future flexibility...
input.dir <- "../inputs"
input.file <- "in_STI_vaccine.csv"

# Variable names in the file above
# that will be changed:
varname <- c("Tp_vaccine_fail",
			 "Tp_VRE",
			 "Tp_vacc_waneRate")

# Values of variables define above that 
# will be used for the sensitivity analysis:
val <- list(c(0, 0.5),
			c(0.5),
			c(0, 0.7))

# Check if correctly setup
stopifnot(length(val)==length(varname))


# Name of the input files defining sensi analysis param values:
sensi.filename <- function(input.file,var,newval){
	return(gsub(pattern = ".csv",
				replacement = paste0("_sensi_",var,"_",newval,".csv"),
				x = input.file))
}

# Create the new file from the default one:
create.sensi.file <- function(input.dir,
							  input.file,
							  var,
							  newval){
	x <- read.csv(paste(input.dir,input.file,sep="/"),header = F)
	x[x[,1]==var,2] <- newval
	
	input.file2 <- sensi.filename(input.file,var,newval)
	write.table(x = x, 
				file = paste(input.dir,input.file2,sep="/"),
				quote = F,row.names = F,col.names = F,sep = "," ,na = "")
}

# Create all sensi inputs files:
for(i in 1:length(varname)){
	for(j in 1:length(val[[i]])){
		create.sensi.file(input.dir,
						  input.file,
						  var = varname[i],
						  newval = val[[i]][j])
	}
}



# Create sensitivity analysis execution script:

f0 <- paste(input.dir,input.file,sep="/")
cmdline <- character()

  # Back up original file:
cmdline[1] <- paste0("cp ",f0," ",f0,".bckup")

  # write commands for every parameter sensi values:
cnt <- 2
for(i in 1:length(varname)){
	for(j in 1:length(val[[i]])){
		cmdline[cnt] <- paste0("cp ",input.dir,"/",
							   sensi.filename(input.file,
													var = varname[i],
													newval = val[[i]][j])," ",f0)
		cmdline[cnt+1] <- paste0("echo executing sensi ",varname[i]," ",val[[i]][j])
		cmdline[cnt+2] <- "../setup-param"
		cmdline[cnt+3] <- "Rscript main_comp.R $1 > main_comp_pop_$1.out &"
		cmdline[cnt+4] <- "wait $!"
		cnt <- cnt+5
	}
}
  # restore initial parameter file
  # (before changing values bc of sensi analysis):
cmdline[cnt] <- paste0("cp ",f0,".bckup ",f0)

write.table(x = cmdline, file = "dosensi",
			quote = F,row.names = F,col.names = F)
system("chmod +x dosensi")

