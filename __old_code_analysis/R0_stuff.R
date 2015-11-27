#df  = transm.acq[as.character(transm.acq$stiname)=="HIV",]

df  = transm
df2 = ddply(df,c("stiname","from","successTransm"),summarize,R=length(from))

df3 = df2[order(df2$from),]


g = ggplot(df2,aes(x=R))+geom_histogram(binwidth=1)
g = g + facet_wrap(~stiname,scales = "free_y")
g
