library(ggplot2)
theme_set(theme_bw())

dat <- read.csv("Coinfection_Susceptibility.csv")

g.hiv.suscept <- ggplot(dat) + geom_pointrange(aes(x=(STI.already.infecting),
                                              y=HIV.increased..susceptibility,
                                              ymin=lower,ymax=upper,colour=Source),
                                          size=1)

g.hiv.suscept <- g.hiv.suscept + geom_text(aes(x=(STI.already.infecting),
                                               y=HIV.increased..susceptibility,
                                               label=HIV.increased..susceptibility,
                                               hjust=-0.5))

g.hiv.suscept <- g.hiv.suscept + ggtitle("Increased HIV Susceptibility")+ylab("OR / RR / HR")+xlab("STI already infecting")

plot(g.hiv.suscept)
