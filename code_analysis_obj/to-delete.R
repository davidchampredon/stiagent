# xx <- tsv[[3]]
# xx[1:10,1:10]

length(all.scen[[1]])

mc = 24

q1 = yy[length(yy)]

for(i in 1:5){
	z = all.scen[[i]][[mc]]
	D = as.data.frame(z$df_sim)
	yy = D$mtctHIV
	bb = cumsum(D$nNewBorn) # cumsum(D$nNewBorn)  D$nSexActRisk2

	if(i==1) plot(bb,typ="l")
	lines(bb,col=i,lwd=i)
	text(x=length(bb), y=bb[length(bb)],
		 labels = i,pos = 4,cex = 0.7,col=i)
	
	q3 = yy[length(yy)]
	
	r = q3/q1
}