#-------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Plaatjes
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 

# Den Haag
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
# Thuis
setwd("~/Documenten/Stage PBL/modelTt")

source("TCRE.R")
source("costs.R")

#------------ grafieken ------------------

# plaatje van TCRE
par(mfrow=c(1,1))
plot(cumuvstempLL,xlim=c(0,9))
abline(fLL)
points(cumuvstempUL,xlim=c(0,9))
abline(fUL)
abline(intercept, slope)

# plaatje van costs
par(mfrow=c(1,1))
plot(costsLL,ylim=c(0,8))
abline(gLL)
points(costsUL)
abline(gUL)
abline(intercept_mean, slope_mean)


#----------- histogrammen --------------

N <- 10000
f.seed <- 21

# maak samples
cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
costs.sample <- f.costs.sample(N, f.seed)


# histogram van input
par(mfrow=c(2,2))
hist(cumuvstemp.sample$T2010, breaks = "Scott", main = "Histogram of T2010", xlab = "degrees Celsius, relative to pre-industrial")
hist(cumuvstemp.sample$TCRE, breaks = "Scott", main = "Histogram of TCRE", xlab = "TCRE (degrees Celsius/TtCO2)")
hist(cumuvstemp.sample$CO22010, breaks = "Scott", main = "Histogram of CO22010", xlab = "Tt cumuCO2, relative to pre-industrial")
par(mfrow=c(1,1))

# costs
par(mfrow=c(2,2))
hist(costs.sample$costs.slope, breaks = "Scott", main = "Histogram of costs.slope", xlab = "cost respons to emissions (%pt/tCO2)")
hist(costs.sample$baselineCO2, breaks = "Scott", main = "Histogram of baseline CO2", xlab = "Baseline CO2")
par(mfrow=c(1,1))

#maak resultaten
sample_en_result <- f.cumuCO2result(N,1.5,cumuvstemp.sample)
costs.sample_en_result <- data.frame(sample_en_result,f.costsresult(N,sample_en_result[,5],costs.sample))


hist.cumuCO2result <- function(sample_en_result) {
  hist(sample_en_result$cumuCO2result, breaks = "Scott", main = paste("CO2result, Ttarget = ", sample_en_result$f.Ttarget[1]) , xlab = "cumuCO2 (Tt)")
}


hist.costs.result <- function(costs.sample_en_result) {
  hist(costs.sample_en_result$costs, breaks = "Scott", main = paste("Costs, Ttarget = ", costs.sample_en_result$f.Ttarget[1]) , xlab = "Costs (aba + cons)")
}

# histogrammen van costs.result
par(mfrow=c(2,2))
hist(costs.result.14, breaks = "Scott", main = "costs.result, Ttarget = 1-4", xlab = "Costs")
hist(costs.result.1.5, breaks = "Scott", main = "costs.result, Ttarget = 1.5", xlab = "Costs")
hist(costs.result.2, breaks = "Scott", main = "costs.result, Ttarget = 2", xlab = "Costs")
hist(costs.result.3, breaks = "Scott", main = "costs.result, Ttarget = 3", xlab = "Costs")




# scatterplots van input vs cumuCOresult
# 1-4
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.14 <- plot(cumuCO2result.14~Ttarget.14, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.14 <- plot(cumuCO2result.14~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.14 <- plot(cumuCO2result.14~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.14 <- plot(cumuCO2result.14~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-CO2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=1-4", outer=TRUE)

# 1.5
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.1.5 <- plot(cumuCO2result.1.5~Ttarget.1.5, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.1.5 <- plot(cumuCO2result.1.5~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.1.5 <- plot(cumuCO2result.1.5~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.1.5 <- plot(cumuCO2result.1.5~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-CO2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=1.5", outer=TRUE)

# 2
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.2 <- plot(cumuCO2result.2~Ttarget.2, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.2 <- plot(cumuCO2result.2~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.2 <- plot(cumuCO2result.2~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.2 <- plot(cumuCO2result.2~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-CO2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=2", outer=TRUE)

# 3
par(mfrow=c(2,2), oma=c(2,0,2,0))
Ttarget.plot.3 <- plot(cumuCO2result.3~Ttarget.3, sub = "Ttarget", xlab = "temp", ylab = "cumulative CO2 emissions")
T2010.plot.3 <- plot(cumuCO2result.3~T2010, sub = "T2010", xlab = "temp", ylab = "cumulative CO2 emissions")
TCRE.plot.3 <- plot(cumuCO2result.3~TCRE, sub = "TCRE", xlab = "TCRE", ylab = "cumulative CO2 emissions")
CO22010.plot.3 <- plot(cumuCO2result.3~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "cumulative CO2 emissions")
title("Cumu-CO2-emissions", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=3", outer=TRUE)


# scatterplots van input vs costs.result
# 1-4
par(mfrow=c(3,2), oma=c(3,0,2,0))
costs.Ttarget.plot.14 <- plot(costs.result.14~Ttarget.14, sub = "Ttarget", xlab = "temp", ylab = "% GDP")
costs.T2010.plot.14 <- plot(costs.result.14~T2010, sub = "T2010", xlab = "temp", ylab = "% GDP")
costs.TCRE.plot.14 <- plot(costs.result.14~TCRE, sub = "TCRE", xlab = "degrees Celsius/tCO2", ylab = "% GDP")
costs.CO22010.plot.14 <- plot(costs.result.14~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "% GDP")
costs.slope.plot.14 <- plot(costs.result.14~costs.slope, sub = "costs.slope", xlab = "%pt/tCO2", ylab = "% GDP")
baselineCO2.plot.14 <- plot(costs.result.14~baselineCO2, sub = "Ttarget", xlab = "cumu CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=1-4", outer=TRUE)

# 1.5
par(mfrow=c(3,2), oma=c(3,0,2,0))
costs.Ttarget.plot.1.5 <- plot(costs.result.1.5~Ttarget.1.5, sub = "Ttarget", xlab = "temp", ylab = "% GDP")
costs.T2010.plot.1.5 <- plot(costs.result.1.5~T2010, sub = "T2010", xlab = "temp", ylab = "% GDP")
costs.TCRE.plot.1.5 <- plot(costs.result.1.5~TCRE, sub = "TCRE", xlab = "degrees Celsius/tCO2", ylab = "% GDP")
costs.CO22010.plot.1.5 <- plot(costs.result.1.5~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "% GDP")
costs.slope.plot.1.5 <- plot(costs.result.1.5~costs.slope, sub = "costs.slope", xlab = "%pt/tCO2", ylab = "% GDP")
baselineCO2.plot.1.5 <- plot(costs.result.1.5~baselineCO2, sub = "Ttarget", xlab = "cumu CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=1.5", outer=TRUE)

# 2
par(mfrow=c(3,2), oma=c(3,0,2,0))
costs.Ttarget.plot.2 <- plot(costs.result.2~Ttarget.2, sub = "Ttarget", xlab = "temp", ylab = "% GDP")
costs.T2010.plot.2 <- plot(costs.result.2~T2010, sub = "T2010", xlab = "temp", ylab = "% GDP")
costs.TCRE.plot.2 <- plot(costs.result.2~TCRE, sub = "TCRE", xlab = "degrees Celsius/tCO2", ylab = "% GDP")
costs.CO22010.plot.2 <- plot(costs.result.2~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "% GDP")
costs.slope.plot.2 <- plot(costs.result.2~costs.slope, sub = "costs.slope", xlab = "%pt/tCO2", ylab = "% GDP")
baselineCO2.plot.2 <- plot(costs.result.2~baselineCO2, sub = "Ttarget", xlab = "cumu CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=2", outer=TRUE)

# 3
par(mfrow=c(3,2), oma=c(3,0,2,0))
costs.Ttarget.plot.3 <- plot(costs.result.3~Ttarget.3, sub = "Ttarget", xlab = "temp", ylab = "% GDP")
costs.T2010.plot.3 <- plot(costs.result.3~T2010, sub = "T2010", xlab = "temp", ylab = "% GDP")
costs.TCRE.plot.3 <- plot(costs.result.3~TCRE, sub = "TCRE", xlab = "degrees Celsius/tCO2", ylab = "% GDP")
costs.CO22010.plot.3 <- plot(costs.result.3~CO22010, sub = "CO22010", xlab = "cumulative CO2 emissions", ylab = "% GDP")
costs.slope.plot.3 <- plot(costs.result.3~costs.slope, sub = "costs.slope", xlab = "%pt/tCO2", ylab = "% GDP")
baselineCO2.plot.3 <- plot(costs.result.3~baselineCO2, sub = "Ttarget", xlab = "cumu CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000, Ttarget=3", outer=TRUE)



# scatterplots van cumuCO2result vs costs
par(mfrow=c(2,2), oma=c(2,0,2,0))
cumuCO2vscosts.plot.14 <- plot(costs.result.14~cumuCO2result.14, sub = "Ttarget=1-4", xlab = "cumulative CO2 emissions", ylab = "% GDP")
cumuCO2vscosts.plot.1.5 <- plot(costs.result.1.5~cumuCO2result.1.5, sub = "Ttarget=1.5", xlab = "cumulative CO2 emissions", ylab = "% GDP")
cumuCO2vscosts.plot.2 <- plot(costs.result.2~cumuCO2result.2, sub = "Ttarget=2", xlab = "cumulative CO2 emissions", ylab = "% GDP")
cumuCO2vscosts.plot.3 <- plot(costs.result.3~cumuCO2result.3, sub = "Ttarget=3", xlab = "cumulative CO2 emissions", ylab = "% GDP")
title("Costs", outer=TRUE)
mtext(side=1, "LHS, N=10000", outer=TRUE)


# density function
d.14 <- density(cumuCO2result.14)
d.1.5 <- density(cumuCO2result.1.5)
d.2 <- density(cumuCO2result.2)
d.3 <- density(cumuCO2result.3)

