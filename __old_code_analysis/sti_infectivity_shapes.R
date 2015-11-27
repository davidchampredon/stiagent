#### S T I   I N F E C T I V I T Y #####

# Produces a pdf of all STI infectivity curve
# (for documentation... and check!)

# file defining STI parameter values
sti = read.csv("in_STI.csv",header=FALSE)
names(sti) = c("name","value")

### HIV ####

primary.peak.time = sti[sti$name=="HIV_peakTime_weeks",2]/52
primary.peak.VL = 10^7
primary.sd = sti[sti$name=="HIV_shape_acute",2]/52
q = sti[sti$name=="HIV_var_acute_weeks",2]

chronic.min = primary.peak.VL*sti[sti$name=="HIV_frac_chronic_acute",2]
chronic.length =sti[sti$name=="HIV_chronic_duration_yrs",2]
chronic.slope = sti[sti$name=="HIV_chronic_growth",2]
duration.AIDS = sti[sti$name=="HIV_aids_duration_yrs",2]

hiv.v1 <- function(t,peaktime,peakVL,stddev,q)
{
  return(peakVL*exp(-((t-peaktime)^q) / (stddev^q) ))
}

T.chronic <- function(peaktime, peakVL, stddev, q, chronic.min)
{
  tmp = (log(peakVL/chronic.min))^(1/q)
  return(stddev*tmp+peaktime)  
}

hiv.v2 <- function(t,peaktime,peakVL,stddev,q,
                   chronic.min,chronic.length,chronic.slope)
{

  Tc  = T.chronic(peaktime,peakVL,stddev,q,chronic.min)
  return(chronic.min*exp((t-Tc)*chronic.slope))
}

hiv.v3 <- function(t,peaktime,peak.VL,stddev,q,
                   chronic.min,chronic.length,chronic.slope,
                   duration.AIDS)
{

  Tc  = T.chronic(peaktime, peak.VL, stddev, q, chronic.min)
  
  time.AIDS = Tc+chronic.length
  
  print("time AIDS:") ; print(time.AIDS)
  
  v2 = hiv.v2(time.AIDS,peaktime,peak.VL,stddev,q,
              chronic.min,chronic.length,chronic.slope)
  
  slope.AIDS = log(peak.VL/v2)/duration.AIDS
  
  return(v2*exp((t-time.AIDS)*slope.AIDS))
}



n = 5000
time = seq(from=0, to=12, length=n)

Tc= T.chronic(primary.peak.time,primary.peak.VL,
              primary.sd,q,
              chronic.min)

T1 = rep(0,n)
T1[time<Tc] = 1

T2 = rep(0,n)
T2[time>Tc & time<Tc+chronic.length] = 1

T3 = rep(0,n)
T3[time>Tc+chronic.length] = 1

VL1 = hiv.v1(time,primary.peak.time,primary.peak.VL,primary.sd,q)

VL2 = hiv.v2(time,primary.peak.time,primary.peak.VL,primary.sd,q,
             chronic.min,chronic.length,chronic.slope)

VL3 = hiv.v3(time,primary.peak.time,primary.peak.VL,primary.sd,q,
                   chronic.min,chronic.length,chronic.slope, duration.AIDS)

VL = VL1*T1 + VL2*T2 + VL3*T3

pdf("sti_infectivity_shapes.pdf", width=10)
plot(x=time,y=log10(VL)/log10(primary.peak.VL),type="l", lwd=6, 
     main="HIV infectivity curve", 
     xlab="Time (years)", ylab="Infectivity (log10 then normalized)", ylim=c(0,1))

grid()
dev.off()