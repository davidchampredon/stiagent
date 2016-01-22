### Adherence to treatment

risk.decay = 0.1
amax = 0.99
asympt.factor = 0.7

rskgrp = c(0:2)


ADH.sympt = amax*exp(-risk.decay*rskgrp) 
ADH.asympt = ADH.sympt*asympt.factor

plot(rskgrp,ADH.sympt,typ="o",pch=16,ylim=c(0,1))
lines(rskgrp,ADH.asympt,typ="o")
