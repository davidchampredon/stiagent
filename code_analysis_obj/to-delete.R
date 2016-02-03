
sim <- all.scen[[3]]

z <- list()
for( mc in 1:10){
z[[mc]] = as.data.frame(sim[[mc]]$df_sim)
z[[mc]]$mc <- mc

}

df <- do.call("rbind",z)
g <- ggplot(df)+geom_line(aes(x=time,y=HIVprevRisk9))+facet_wrap(~mc)
plot(g)

# 
# mc <- 4
# z = as.data.frame(sim[[mc]]$df_sim)
# 
# g <- ggplot(z)+geom_line(aes(x=time,y=HIVprevRisk0),colour=1)
# g <- g + geom_line(aes(x=time,y=HIVprevRisk1),colour=2)
# g <- g + geom_line(aes(x=time,y=HIVprevRisk2),colour=3)
# # g <- g + geom_line(aes(x=time,y=HIVprevRisk9),colour=4)
# g <- g + ggtitle(mc)
# plot(g)
