library(stiagent,lib.loc = "./lib")

t0 <- Sys.time()
# path to model input files:
folder_inputs = "./test-inputs/"
folder_calib = "../calibration/"

# run the model:
x <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
                                   folder_calib = folder_calib
                                   )
                     )

y <- stiagent_comp_interv(params = list(folder_inputs = folder_inputs,
                                        folder_calib = folder_calib,
                                        jobnum = 1)
)
yy1 <- as.data.frame(y[[1]])

# check if everything went well:
msg <- ifelse(is.numeric(x[[1]]),
              "==> stiagent R library seems to work.",
              "THERE IS A PROBLEM WITH stiagent LIBRARY")
message(msg)
t1 <- Sys.time()
message(paste("time elapsed:",round(t1-t0,1),"sec"))

