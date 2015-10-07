library(stiagent,lib.loc = "./lib")


# path to model input files:
folder_inputs = "../inputs/"

# run the model:
x <- rcpp_stiagent(params = list(folder_inputs=folder_inputs))

# check if everything went well:
msg <- ifelse(is.numeric(x[[1]]),"==> stiagent R library seems to work.","THERE IS A PROBLEM WITH stiagent LIBRARY")
message(msg)
