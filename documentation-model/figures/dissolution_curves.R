
### DISSOLUTION - AGE BOTH PARTNERS

Af.plus.Am = seq(20,130,by=0.1)  # sum of age female and male
age.m = 80
age.v = 0.12
proba.min = 0.7

g.age = 1/(1+exp(age.v*(Af.plus.Am-age.m)))*(1-proba.min)+proba.min

pdf("dissol_age.pdf")
plot(Af.plus.Am,g.age,
     lwd=2, main="Dissolution - Age Component",
     typ="l", ylim=c(0,1))
grid()
dev.off()

### DISSOLUTION - RISK GROUP


rmax = 2
rf.plus.rm =c(0:(2*rmax))
shap = 0.2
g.risk = exp(shap*(rf.plus.rm-2*rmax))

pdf("dissol_riskgroup.pdf")
plot(rf.plus.rm,g.risk,
     "o",ylim=c(0,1),pch=16,
     main=paste0("Dissolution - Risk Group Component (shape=",shap,")"))
grid()
dev.off()



### DISSOLUTION - PARTNERSHIP DEFICIT

def1 = c(0,0.3,0.5,0.7,1)
def2 = seq(0,1,by=0.01)
pp = 0.5
g.min = 0.8

g.def = matrix(nrow = length(def2), ncol=length(def1))

pdf("dissol_deficit.pdf")
for (i in 1:length(def1))
{
  g.def[,i] = g.min +(1-g.min)*((1-def1[i])^pp) * ((1-def2)^pp)

  if(i==1) plot(def2,g.def[,i],typ="l",
                main = "Dissolution - Partnership Deficit",
                ylab="",xlim=c(-0.2,1), xlab="deficit partner #2",
                ylim=c(g.min*0.8,1))
  if(i>1) lines(def2,g.def[,i])
  text(0,g.def[1,i],labels = paste("deficit partner #1 =",def1[i]),pos=2)
}
dev.off()

