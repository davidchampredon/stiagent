
library(Rcpp)

# Input the name of the package here
mypackage <- "stiagent"


code.model.folder <- "../code_model/"

cppfiles <- system(paste0("ls ",code.model.folder,"*.cpp"),intern = TRUE)
hfiles <- system(paste0("ls ",code.model.folder,"*.h"),intern = TRUE)

# Remove "main" files:
cppfiles <- cppfiles[!grepl(pattern = "main",x = cppfiles)]

c.path <- c(cppfiles, hfiles)

# This generates all the necessary files 
# when creating an R package from scrath 
# with the view of interfacing with C++ (using Rcpp)
#?Rcpp::Rcpp.package.skeleton
Rcpp.package.skeleton(name =  mypackage,
                      example_code = FALSE,
                      author = "David Champredon",
                      cpp_files = c.path, 
                      force = TRUE
)
