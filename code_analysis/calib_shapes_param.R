# DISPLAY THE FUNCTIONAL PARAMETRIZATIONS
# OF THE LIMIT VALUES OF THE CALIBRATION PARAMETERS



plot.shapes.range.calibration <- function(filename.lo, filename.hi,
                                          param.names,
                                          function.shape,
                                          xrange,
                                          n.sample,
                                          title, xlabel)
{
  
  # PLOT THE POSSIBLE SHAPES 
  # OF PARAMETERS DEFINED BY 
  # A PARAMETRIC FUNCTION
  
  # Read files defining the 'lower' and 'upper' limits
  param.lo = read.csv(filename.lo, header=FALSE)
  param.hi = read.csv(filename.hi, header=FALSE)
  
  # number of parameters defined in both files 
  # (both must have same length)
  n.param = length(param.names)
  
  value.lo = numeric(n.param)
  value.hi = numeric(n.param)
  value.rand = matrix(nrow=n.param, ncol=n.sample)
  
  
  # Read the lower and upper limits of parameters
  for (i in 1:n.param)
  {
    value.lo[i] = param.lo[param.lo$V1==param.names[i],2]
    value.hi[i] = param.hi[param.hi$V1==param.names[i],2]
    
    # sample randomly values within limits
    value.rand[i,] = sample(seq(value.lo[i], value.hi[i], length.out=n.sample))

  }

  
  # Shapes for limit values (lower and upper only)
  shape.lo = function.shape(value.lo,xrange)
  shape.hi = function.shape(value.hi,xrange)
  
  themax = max(shape.lo,shape.hi)
  themax = min(themax,1)
  
  
  # Plots
  
  mylwd= 6
  
  plot(x=xrange, y=shape.lo, typ="l", lwd=mylwd, main=title,
       ylim=c(0,themax), ylab="", xlab=xlabel,
       las=1)
  lines(x=xrange, y=shape.hi, lwd=mylwd)
  
  for (k in 1:n.sample)
  {
    shape.rand = function.shape(value.rand[,k],xrange)
    lines(x=xrange, y=shape.rand, lwd=mylwd/3, col=rgb(0,0,1,0.20))
  }
  grid()
}


# =============================================================
#       FORMATION FUNCTIONS
# =============================================================


formation.age.age <- function(param,x)
{
  
  # ORDER IS IMPORTANT !!!
  i=1
  mean.age = param[i] ; i=i+1
  sigma.age = param[i] ; i=i+1
  
  mean.agegap = param[i] ; i=i+1
  sigma.agegap = param[i] ; i=i+1
  
  correl.age.agegap = param[i] ; i=i+1
  
  shape = param[i] ; i=i+1
  
  sa2 = sigma.age*sigma.age
  sg2 = sigma.agegap*sigma.agegap
  rhoss = correl.age.agegap*sigma.age*sigma.agegap
  
  M = matrix(c(sa2,rhoss,rhoss,sg2),nrow=2)
  M = solve(M)
  
  # AGE GAP IS FIXED
  age.gap = mean.agegap

  tmp = numeric(length(x))
  res = numeric(length(x))
  
  for (k in 1:length(x))
  {
    z = c(x[k]-mean.age, age.gap-mean.agegap)
    tmp[k] = shape*t(z) %*% M %*% z
    res[k] = exp(-tmp[k])
  }
  return(res)
}


formation.age.agegap <- function(param,x)
{
  
  # ORDER IS IMPORTANT !!!
  i=1
  mean.age = param[i] ; i=i+1
  sigma.age = param[i] ; i=i+1
  
  mean.agegap = param[i] ; i=i+1
  sigma.agegap = param[i] ; i=i+1
  
  correl.age.agegap = param[i] ; i=i+1
  
  shape = param[i] ; i=i+1
  
  sa2 = sigma.age*sigma.age
  sg2 = sigma.agegap*sigma.agegap
  rhoss = correl.age.agegap*sigma.age*sigma.agegap
  
  M = matrix(c(sa2,rhoss,rhoss,sg2),nrow=2)
  M = solve(M)
  
  # AGE IS FIXED
  age = mean.age
  
  tmp = numeric(length(x))
  res = numeric(length(x))
  
  for (k in 1:length(x))
  {
    z = c(age-mean.age, x[k]-mean.agegap)
    tmp[k] = shape*t(z) %*% M %*% z
    res[k] = exp(-tmp[k])
  }
  return(res)
}


# =============================================================
#       SPOUSAL FUNCTIONS
# =============================================================

spousal.agefemale <- function(param,x)
{
  return(exp(-(x-param[1])*(x-param[1])/2/param[2]))
}

spousal.agegap <- function(param,x)
{
  return(exp(-(x-param[1])*(x-param[1])/2/param[2]))
}


spousal.duration <- function(param,x)
{
  return(exp(-(x-param[1])*(x-param[1])/2/param[2]))
}


# =============================================================
#       DISSOLUTION FUNCTIONS
# =============================================================

dissolution.duration <- function(param,x)
{
  return(param[1]+param[2]*exp(-param[3]*x))
}

dissolution.risk <- function(param,x)
{
  # RISK GROUP OF MALE IS FIXED
  rm = 1
  
  rmax=3
  
  return(exp((param[1])*(x+rm-2*rmax)))
}


dissolution.age <- function(param,x)
{
  # MALE AGE IS FIXED
  a_m = 30
  
  return(exp(-(a_m+x-2*param[1])^2/param[2]))
}

pp = c(25,70)
xx = seq(from=0,to=80,by=0.25)
yy = dissolution.age(pp,xx)
#plot(x=xx,y=yy)



# =============================================================
#       PLOTS
# =============================================================

par(mfrow=c(2,4))


# Formation - age
param.names = c("formation_meanAge_female","formation_varAge_female",
                "formation_meanAgeGap","formation_varAgeGap",
                "formation_correl_Age_AgeGap","formation_shapeAge")
xrange = seq(from=10,to=80,by=0.5)
n.sample = 100
thetitle ="Formation - Age Female\n(Age gap = mean)"
xlabel ="Years"

plot.shapes.range.calibration("calib_param_FORM_lower.csv","calib_param_FORM_upper.csv",
                              param.names,
                              formation.age.age,
                              xrange,
                              n.sample, thetitle, xlabel)


# Formation - age gap
param.names = c("formation_meanAge_female","formation_varAge_female",
                "formation_meanAgeGap","formation_varAgeGap",
                "formation_correl_Age_AgeGap","formation_shapeAge")
xrange = seq(from=-20,to=30,by=0.5)
n.sample = 100
thetitle ="Formation - Age Gap\n(Age female = mean)"
xlabel ="Years"

plot.shapes.range.calibration("calib_param_FORM_lower.csv","calib_param_FORM_upper.csv",
                              param.names,
                              formation.age.agegap,
                              xrange,
                              n.sample, thetitle, xlabel)



# Spousal - age
param.names = c("spousalProgress_meanAge_f","spousalProgress_varAge_f")
xrange = seq(from=0,to=60,by=0.25)
n.sample = 100
thetitle = "Spousal - Age female"
xlabel = "Years"

plot.shapes.range.calibration("calib_param_SPOUSAL_lower.csv","calib_param_SPOUSAL_upper.csv",
                              param.names,
                              spousal.agefemale,
                              xrange,
                              n.sample, thetitle, xlabel)


# Spousal - age gap
param.names = c("spousalProgress_meanGap","spousalProgress_varGap")
xrange = seq(from=-20,to=30,by=0.25)
n.sample = 100
thetitle = "Spousal - Age gap"
xlabel = "Years"

plot.shapes.range.calibration("calib_param_SPOUSAL_lower.csv","calib_param_SPOUSAL_upper.csv",
                              param.names,
                              spousal.agegap,
                              xrange,
                              n.sample, thetitle, xlabel)


# Spousal - duration
param.names = c("spousalProgress_durationK1","spousalProgress_durationK2")
xrange = seq(from=0,to=15,by=0.25)
n.sample = 100
thetitle ="Spousal - Duration"
xlabel ="Years"

plot.shapes.range.calibration("calib_param_SPOUSAL_lower.csv","calib_param_SPOUSAL_upper.csv",
                              param.names,
                              spousal.duration,
                              xrange,
                              n.sample, thetitle, xlabel)






# Dissolution - duration
param.names = c("dissolution_duration_1","dissolution_duration_2","dissolution_duration_3")
xrange = seq(from=0,to=50,by=0.25)
n.sample = 100
thetitle ="Dissolution - Duration"
xlabel ="Years"

plot.shapes.range.calibration("calib_param_DISSOL_lower.csv","calib_param_DISSOL_upper.csv",
                              param.names,
                              dissolution.duration,
                              xrange,
                              n.sample, thetitle, xlabel)


# Dissolution - Age
param.names = c("dissolution_age_mean","dissolution_age_var")
xrange = seq(from=0,to=80,by=0.25)
n.sample = 100
thetitle ="Dissolution - Age\n(Male age fixed at 30)"
xlabel ="Age Female"

plot.shapes.range.calibration("calib_param_DISSOL_lower.csv","calib_param_DISSOL_upper.csv",
                              param.names,
                              dissolution.age,
                              xrange,
                              n.sample, thetitle, xlabel)

# Dissolution - risk
param.names = c("dissolution_RiskGroup_1")
xrange = seq(from=0,to=3,by=1)
n.sample = 100
thetitle ="Dissolution - Risk Group Female\n(Male RG=1)"
xlabel ="Risk group"

plot.shapes.range.calibration("calib_param_DISSOL_lower.csv","calib_param_DISSOL_upper.csv",
                              param.names,
                              dissolution.risk,
                              xrange,
                              n.sample, thetitle, xlabel)

