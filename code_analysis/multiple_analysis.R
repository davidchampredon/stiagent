###################################################
###
###    LAUNCH MULTIPLE ANALYSIS
###
###################################################

system("Rscript analyze_simulation.R &")
system("Rscript analyze_epidemic.R &")
system("Rscript analyze_partnerships.R &")
system("Rscript analyze_births.R &")


prm <- read.csv("../in_simulation.csv",header = F)
save.trace.file <- as.logical(prm$V2[prm$V1=="save_trace_files"])

if(save.trace.file){
	system("Rscript analyze_sexacts.R &")
	system("Rscript analyze_transmissions.R &")
}