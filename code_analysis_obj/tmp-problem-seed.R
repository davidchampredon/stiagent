#sim1 <- sim
#sim2<-sim

# df1 <- as.data.frame(sim1[[1]]$df_sim)
# df2 <- as.data.frame(sim2[[1]]$df_sim)
# 
# inc1 <- df1$Tp
# inc2 <- df2$Tp
# 
# plot(inc1,typ="l")
# lines(inc2,col="red")


### paths
path.stiagent.lib <- "../Rlibrary/lib"
folder_inputs <- "../inputs/"
folder_calib <- "../calibration/"

library(stiagent,lib.loc = path.stiagent.lib)

### Founder population parameters:
founder_file <- "in_populationFeatures.csv"


s1 <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
								   folder_calib = folder_calib,
								   scenario_file = "in_scenario_test.csv", # "in_scenario_test.csv"  "in_scenario_test2.csv"
								   founder_size = 333,
								   founder_femprop = 0.5,
								   founder_cswprop = 0.01,
								   displayProgress = 11,
								   MC_id = 1)
)

s2 <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
									folder_calib = folder_calib,
									scenario_file = "in_scenario_test2.csv", # "in_scenario_test.csv"  "in_scenario_test2.csv"
									founder_size = 333,
									founder_femprop = 0.5,
									founder_cswprop = 0.01,
									displayProgress = 11,
									MC_id = 1)
)

s1[["seed"]]
s2[["seed"]]

df1 <- as.data.frame(s1$df_sim)
df2 <- as.data.frame(s2$df_sim)

inc1 <- df1$Tp
inc2 <- df2$Tp

plot(inc1,typ="l")
lines(inc2,col="red")