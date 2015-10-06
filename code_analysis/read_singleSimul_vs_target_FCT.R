require(plyr)


plot.2.pointvalues <- function(target,simul,thetitle,yrng=c(0,1))
{
  
  col.target = "black"
  col.simul = "red"
  
  pch.target = 22
  pch.simul = 16
  
  mycex = 2
  
  plot(x=0, y=target,cex.main = 2,
       ylim=yrng,
       xaxt = "n",
       col=col.target, bg="white",
       cex=mycex,
       typ="p", pch=pch.target,
       main=thetitle, xlab="", ylab="")
  
  points(x=0, y=target,
         cex=2*mycex,
         col=col.target, pch=pch.target)
  
  
  points(x=0, y=simul,
         cex=mycex,
         col=col.simul, pch=pch.simul)
  
  text(x=0, y=target,labels = round(target,4),col=col.target,pos = 2,offset=2, cex=2)
  text(x=0, y=simul,labels = round(simul,4),col=col.simul,pos = 4, offset=2,cex=2)
  
  
  grid()
  
  legend(x="topright",legend = c("Target","Model"),
         col=c(col.target,col.simul),
         pt.cex=mycex,
         cex=mycex,
         pch=c(pch.target,pch.simul),
         pt.bg=c("white")
  )
}


plot.2.pointvalues.mc <- function(target,simul,thetitle,yrng=c(0,1))
{
  
  col.target = "black"
  col.simul = "red"
  
  pch.target = 22
  pch.simul = 16
  
  mycex = 2
  
  simul.d = ddply(simul,c("V1"),summarize,
                  m = mean(V2),
                  min = min(V2),
                  max=max(V2))
  
  plot(x=as.factor(target$V1), 
       y=target$V2,
       cex.main = 1,
       ylim=yrng,
       col=col.target, bg="white",
       cex=mycex,
       typ="p", pch=pch.target,
       main=thetitle, xlab="", ylab="")
  
  points(x=as.factor(simul.d$V1), y=simul.d$m,
         cex=mycex,
         col=col.simul, pch=pch.simul)
  
  points(x=as.factor(simul.d$V1), y=simul.d$min,
         cex=1,
         col=col.simul, pch=pch.simul)
  
  points(x=as.factor(simul.d$V1), y=simul.d$max,
         cex=1,
         col=col.simul, pch=pch.simul)
  
  
  grid()
  
  legend(x="topright",legend = c("Target","Model"),
         col=c(col.target,col.simul),
         pt.cex=1,
         cex=1.1,
         pch=c(pch.target,pch.simul),
         pt.bg=c("white")
  )
}







plot.2.distributions <- function(target,simul,thetitle,xrng=NULL)
{
  # clean up (only for age gaps)
  # (TO DO: FIX THIS)
  simul<-simul[(simul$V1> -888),]
  
  if (is.null(xrng)) xrng = range(target$V1,simul$V1)
  
  yrng = range(target$V2,simul$V2)
  
  col.target = "lightgray"
  col.simul = "red"
  
  pch.target = 22
  pch.simul = 16
  
  lwd.target = 9
  lwd.simul=3
  
  plot(x=target$V1, y=target$V2,
       cex.main = 2,
       xlim = xrng, ylim=yrng,
       lwd = lwd.target,
       col=col.target, #bg="white",
       typ="o", pch=pch.target,
       main=thetitle, xlab="", ylab="")
  
  lines(x=simul$V1, y=simul$V2,
        lwd = lwd.simul,
        cex=2,
        col=col.simul, typ="o", pch=pch.simul)
  
  grid()
  
  legend(x="topright",legend = c("Target","Model"),
         col=c(col.target,col.simul),
         cex=2,
         lwd=c(lwd.target,lwd.simul),
         pch=c(pch.target,pch.simul),
         pt.bg=c("white")
  )
}



plot.2.distributions.mc <- function(target,simul,thetitle,xrng=NULL,yrng=NULL)
{
  # clean up (only for age gaps)
  # (TO DO: FIX THIS)
  simul<-simul[(simul$V1> -888),]
  
  if (is.null(xrng)) xrng = range(target$V1,simul$V1)
  
  if (is.null(yrng)) yrng = range(na.omit(target$V2),na.omit(simul$V2))
  
  col.target = "lightgray"
  col.simul = "red"
  
  pch.target = 22
  pch.simul = 16
  
  lwd.target = 9
  lwd.simul=3
  
  
  simul.d = ddply(simul,c("V1"),summarize,
                  m = mean(V2),
                  min = min(V2),
                  max=max(V2))
  
  plot(x=target$V1, y=target$V2,
       cex.main = 1,
       xlim = xrng, ylim=yrng,
       lwd = lwd.target,
       col=col.target, #bg="white",
       typ="o", pch=pch.target,
       main=thetitle, xlab="", ylab="")
  
  lines(x=simul.d$V1, y=simul.d$m,
        lwd = lwd.simul,
        cex=2,
        col=col.simul, typ="o", pch=pch.simul)
  
  polygon(x = c(simul.d$V1,rev(simul.d$V1)),
          y= c(simul.d$min,rev(simul.d$max)),
          col=rgb(1,0,0,0.2),border=FALSE)
  
  grid()
  
  legend(x="topright",legend = c("Target","Model"),
         col=c(col.target,col.simul),
         cex=1.1,
         lwd=c(lwd.target,lwd.simul),
         pch=c(pch.target,pch.simul),
         pt.bg=c("white")
  )
}