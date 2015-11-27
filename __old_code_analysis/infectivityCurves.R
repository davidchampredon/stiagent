# input STIs here

saveToFile = 1

if (saveToFile) pdf(file="./Documentation/sti_infectivity_shapes.pdf")

sti.names = c("HIV","HSV2","HPV","Ct","Ng","Hd","Bv","Tp","Tv") 

n = length(sti.names)

par(mfrow=c(ceiling(n/3),3))

for (i in 1:n)
{
  filename = paste("infectivityCurve_",sti.names[i],".out", sep="")
  IC = read.csv(filename, header=FALSE)
  
  
  plot(x=(IC$V1+0.0001), y=IC$V2, ylim=c(0,1), 
       type="l", lwd=1,
       xlab = "Years",ylab="Infectivity curve", 
       main=sti.names[i])
  
  xx = c(0,IC$V1,IC$V1[length(IC$V1)])
  yy = c(0,IC$V2,0)
  polygon(x=xx, y=yy,col=rgb(1,0.7,0.7))
}

if (saveToFile) dev.off()