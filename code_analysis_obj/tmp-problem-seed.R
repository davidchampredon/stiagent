options(width = 1000)

### paths
path.stiagent.lib <- "../Rlibrary/lib"
folder_inputs <- "../inputs/"
folder_calib <- "../calibration/"

library(stiagent,lib.loc = path.stiagent.lib)

### Founder population parameters:
founder_size <- 80
founder_femprop <- 0.5
founder_cswprop <- 0.01

s1 <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
								   folder_calib = folder_calib,
								   scenario_file = "in_scenario_test.csv", # "in_scenario_test.csv"  "in_scenario_test2.csv"
								   founder_size = founder_size,
								   founder_femprop = founder_femprop,
								   founder_cswprop = founder_cswprop,
								   displayProgress = 11,
								   MC_id = 1)
)

print(rep("~",100))

s2 <- stiagent_runsim(params = list(folder_inputs = folder_inputs,
									folder_calib = folder_calib,
									scenario_file = "in_scenario_test2.csv", # "in_scenario_test.csv"  "in_scenario_test2.csv"
									founder_size = founder_size,
									founder_femprop = founder_femprop,
									founder_cswprop = founder_cswprop,
									displayProgress = 11,
									MC_id = 1)
)

s1[["seed"]]
s2[["seed"]]

df1 <- as.data.frame(s1$df_sim)
df2 <- as.data.frame(s2$df_sim)

t <- df1$time
t2 <- df2$time
inc1 <- df1$Tp
inc2 <- df2$Tp

plot(x=t, y=inc1,typ="l",ylim=range(inc1,inc2))
lines(x=t2, inc2,col="red")
