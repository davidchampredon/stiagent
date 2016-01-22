### 
###   Launch all R scripts in this directory
###   (should produce all figures needed)
###

# Retrieve all R scripts in this folder:
flist <- system(command = "ls *.R",intern = TRUE)

# Remove this script (else infinite loop!)
flist <- flist[!grepl("_LAUNCH_ALL",flist)]

# Define command to launch:
cmd <- paste0("Rscript ",flist," &")

for (i in 1:length(flist)){
	system(command = cmd[i],ignore.stdout = TRUE)
}
