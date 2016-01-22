pdf("IC_Tv.pdf")

pseudo.beta <- function(x,alpha,beta)
{
  if(x<0 || x>1) res=0
  if (x>=0 & x<=1){
    cst<- ((alpha-1)/(alpha+beta-2))^(alpha-1)*((beta-1)/(alpha+beta-2))^(beta-1)
    res = x^(alpha-1)*(1-x)^(beta-1)/cst
  }
  return(res)
}

t.m <- seq(0,2,by=0.001)
t.f <- seq(0,2,by=0.001)

### Infectivity curves are segregated
### by gender and symptomatic status

L <- 1/52   # same for all

### MALES ###

# MALES asymptomatic
doi.m.a <- 9/12 #
shape1.m.a <- 2
shape2.m.a <- 5
reduc.m.a <- 0.5

tt.m.a<-pmax(0,t.m-L)/doi.m.a
IC.m.a <- reduc.m.a*sapply(tt.m.a,pseudo.beta, shape1.m.a, shape2.m.a)

# MALES symptomatic
doi.m.s <- 20/365
shape1.m.s <- 2
shape2.m.s <- 4

tt.m.s <-pmax(0,t.m-L)/doi.m.s
IC.m.s <- sapply(tt.m.s,pseudo.beta, shape1.m.s, shape2.m.s)


### FEMALES ###

# FEMALES asymptomatic
doi.f.a <- 2.0
shape1.f.a <- 1.7
shape2.f.a <- 4
reduc.f.a <- 0.5

tt.f.a <- pmax(0,t.f-L)/doi.f.a
IC.f.a <- reduc.f.a*sapply(tt.f.a,pseudo.beta, shape1.f.a, shape2.f.a)

# FEMALES symptomatic
doi.f.s <- 0.5
shape1.f.s <- 2
shape2.f.s <- 4

tt.f.s <-pmax(0,t.f-L)/doi.f.s
IC.f.s <- sapply(tt.f.s,pseudo.beta, shape1.f.s, shape2.f.s)


### === PLOTS ===

par(mfrow=c(2,1))

mylwd = 6
plot(t.m,IC.m.s,typ="l",lwd=mylwd,
     main = "Male",
     xlab = "Time (years)",
     ylab="Tv Infectivity Curve")

lines(t.m,IC.m.a,lty=2,lwd=mylwd)
grid()

plot(t.f,IC.f.s,typ="l",lwd=mylwd,
     main="Female",
     xlab = "Time (years)",
     ylab="Tv Infectivity Curve")

lines(t.f,IC.f.a,lty=2,lwd=mylwd)
grid()


dev.off()