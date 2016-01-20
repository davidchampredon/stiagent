age = seq(10,70,by=0.1)
a_peak = 25#19
sigma = 29#25
 q = 4#3

h.age = exp(-(abs(age-a_peak)/sigma)^q)

plot(age,h.age,typ="l",lwd=4)
grid(lty=1)