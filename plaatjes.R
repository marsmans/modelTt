#-------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Plaatjes
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 

if (dir.exists("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")) {     #Den Haag
  setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
} else if (dir.exists("~/Documenten/Stage PBL/modelTt")) {    #thuis
  setwd("~/Documenten/Stage PBL/modelTt")
}

# Den Haag
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
# Thuis
#setwd("~/Documenten/Stage PBL/modelTt")

source("TCRE.R")
#source("costs.R")

#------------ grafieken ------------------

# plaatje van TCRE en hoe de lijnen zijn gefit
par(mfrow=c(1,1))
plot(cumuvstempLL,xlim=c(0,9))
abline(fLL)
points(cumuvstempUL,xlim=c(0,9))
abline(fUL)
abline(intercept, slope)

# plaatje van costs en hoe de lijnen zijn gefit (lineaire geval)
par(mfrow=c(1,1))
plot(costsLL,xlim=c(0,9),ylim=c(0,8))
abline(gLL)
points(costsUL)
abline(gUL)
abline(intercept_mean, slope_mean)

# plaatje van CO2.results
CO2.results <- data.table(CO2.results)
CO2.results2 <-gather(CO2.results,temp,cumuCO2,as.character(seq(1, 4, by = 0.1)))
par(mfrow=c(1,1))
plot(CO2.results2$temp~CO2.results2$cumuCO2,xlim=c(0,9), ylim=c(0,4))


#----------- histogrammen --------------

N <- 10000
f.seed <- 21

# maak samples
cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
p.sample <- f.p.sample(N, f.seed)


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

# costs nonlineair
hist(p.sample, breaks = "Scott", main = "Histogram of p", xlab = "response parameter")



#maak resultaten
#sample_en_result <- f.cumuCO2result(N,1.5,cumuvstemp.sample)
#costs.sample_en_result <- data.frame(sample_en_result,f.costsresult(N,sample_en_result[,5],costs.sample))


# functies die een histogram van de cumuCO2 resultaten maken gegeven een dataframe
hist.cumuCO2result <- function(f.data) {
  hist(f.data$cumuCO2result, breaks = "Scott", main = paste("CO2result, Ttarget = ", f.data$f.Ttarget[1]) , xlab = "cumuCO2 (Tt)")
}

hist.costs <- function(f.data) {
  hist(f.data$costs, breaks = "Scott", main = paste("Costs, Ttarget = ", f.data$f.Ttarget[1]) , xlab = "Costs")
}


hist.TCRE <- function(f.data) {
  plot(f.data$TCRE, main=paste(f.data$f.Ttarget[1]), sub = "TCRE", xlab = "TCRE", ylab = "costs")
}

hist.slope <- function(f.data) {
  plot(f.data$costs.slope, main=paste(f.data$f.Ttarget[1]), sub = "costs.slope", xlab = "costs.slope", ylab = "costs")
}

hist.p <- function(f.data) {
  plot(f.data$p.sample, main=paste(f.data$f.Ttarget[1]), sub = "p", xlab = "p", ylab = "costs")
}




# scatterplots maken van een dataframe
scatter.TCRE <- function(f.data) {
  plot(f.data$costs~f.data$TCRE, main=paste(f.data$f.Ttarget[1]), sub = "TCRE", xlab = "TCRE", ylab = "costs", xlim=c(0.2,1), ylim=c(-6,4))
}

scatter.slope <- function(f.data) {
  plot(f.data$costs~f.data$costs.slope, main=paste(f.data$f.Ttarget[1]), sub = "costs.slope", xlab = "costs.slope", ylab = "costs", xlim=c(-1.2,-0.3),ylim=c(-6,4))
}

scatter.p <- function(f.data) {
  plot(f.data$costs~f.data$p.sample, main=paste(f.data$f.Ttarget[1]), sub = "p", xlab = "p", ylab = "costs", xlim=c(0.75,1),ylim=c(0,4))
}

scatter.cumuCO2result <- function(f.data) {
  plot(f.data$costs~f.data$cumuCO2result, main=paste(f.data$f.Ttarget[1]), sub = "cumuCO2result", xlab = "cumuCO2result", ylab = "costs", xlim=c(0,7),ylim=c(-8,8))
}


#----------- main plot -----------------
# krijgt een CC matrix
CCmat <- f.costs.CCmatrix(N,s.seed)
CCdata = data.table(CCmat)
# maak er een 'werkbaarder' format van
CC <-gather(CCdata,variable,value,c('T2010','TCRE','CO22010','cumuCO2result','costs.slope'))
CC=data.table(CC)
CC$temp <- as.character(seq(1, 4, by = 0.1))


#----------- plot van CC waarden (lin) met costs.slope --------------


# plotting (probeersel) staven naast elkaar
p = ggplot(CC[variable %in% c('T2010','CO22010','TCRE','costs.slope')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="dodge")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","costs.slope"="dark red","T2010"="black","TCRE"="green"))
p
ggsave(paste("CC_GE_lin.png"),p)

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CC$value <- CC$value*CC$value

p = ggplot(CC[variable %in% c('T2010','CO22010','TCRE','costs.slope')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","costs.slope"="dark red","T2010"="black","TCRE"="green"))
p = p + ggtitle("CC values of GE models with linear relation and fitted cost graph and bounded TCRE")
p
ggsave(paste("CC_GE_lin_stacked_squared_withcor.png"),p)

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door absolute waarde te nemen)
CC$value <- abs(CC$value)

p = ggplot(CC[variable %in% c('T2010','CO22010','TCRE','costs.slope')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","costs.slope"="dark red","T2010"="black","TCRE"="green"))
p
ggsave(paste("CC_GE_lin_stacked_absolute.png"),p)


#----------- plot van CC waarden (lin) zonder costs.slope --------------

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CC$value <- CC$value*CC$value

q = ggplot(CC[variable %in% c('T2010','CO22010','TCRE')])
q = q + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
q = q + theme_bw()# + theme(axis.text.x=element_text(size=12))
q = q + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","T2010"="black","TCRE"="green"))
q = q + ggtitle("CC values of GE models with linear relation and fitted cost graph and bounded TCRE")
q
ggsave(paste("CC_GE_lin_stacked_squared_withcor.png"),q)


#----------- plot van CC waarden (non-lin) --------------
# krijgt een CC matrix
CCmat <- f.costs.CCmatrix(N,s.seed)
CCdata = data.table(CCmat)
# maak er een 'werkbaarder' format van
CC <-gather(CCdata,variable,value,c('T2010','TCRE','CO22010','cumuCO2result','p.sample'))
CC=data.table(CC)
CC$temp <- as.character(seq(1, 4, by = 0.1))

# plotting (probeersel) staven naast elkaar
p = ggplot(CC[variable %in% c('T2010','CO22010','TCRE','p.sample')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="dodge")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","p.sample"="dark red","T2010"="black","TCRE"="green"))
p
ggsave(paste("CC_GE_nonlin.png"),p)

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CC$value <- CC$value*CC$value


p = ggplot(CC[variable %in% c('p.sample','TCRE','T2010','CO22010')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill", order = c('p.sample','TCRE','T2010','CO22010'))
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("p.sample"="dark red","TCRE"="green","CO22010"="grey","T2010"="black"))
p
ggsave(paste("CC_GE_lin_stacked_squared.png"),p)

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door absolute waarde te nemen)
CC$value <- abs(CC$value)

p = ggplot(arrange(CC[variable %in% c('T2010','CO22010','TCRE','p.sample')],variable,c('p.sample','TCRE','T2010','CO22010')))
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","p.sample"="dark red","T2010"="black","TCRE"="green"))
p
ggsave(paste("CC_GE_lin_stacked_absolute.png"),p)


# maak er een werkbaarder format van
CC <-gather(CC,variable,value,c('T2010','TCRE','CO22010','cumuCO2result','p.sample'))
CC=data.table(CC)

# plotting (probeersel)
p = ggplot(data=CC[variable %in% c()])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="dodge")
p = p + theme_bw() + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","p.sample"="dark red","T2010"="black","TCRE"="green"))
ggsave(paste("Fig1.png"),p,width=12,height=12,dpi=300)



#--------- boxplots van costs -----------------------

# probeersel
b = ggplot(results)
b = b + geom_boxplot(aes)
b
