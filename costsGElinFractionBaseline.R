#------------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Sensitivity in costs
# GE models, linear fit
#
#------------------------------------------------

if (dir.exists("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")) {     #Den Haag
  setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
} else if (dir.exists("~/Documenten/Stage PBL/modelTt")) {    #thuis
  setwd("~/Documenten/Stage PBL/modelTt")
}


source("TCRE.R")
source("TCRE-T2010HV.R")
#source("TCREcor.R")
#source("TCREcorT-tcre.R")
#source("TCREcorCO2-tcre.R")


#----------- Relatie cumulatieve CO2 <-> mitigatie kosten -----------------

# data inlezen
costsLL <- read.csv(file = "./../Databases/costs_LL_GE.txt", header = TRUE)
costsUL <- read.csv(file = "./../Databases/costs_UL_GE.txt", header = TRUE)

# schalen naar Tt al gedaan met inlezen!
#costsLL[1] <- costsLL[1]/1000
#costsUL[1] <- costsUL[1]/1000

# baseline = 6500 - 8443 GtCO2 vanaf 1870
# de kosten zijn als functie van 'residual' cumulatieve CO2
# van fractie van baseline emissies van periode 2011-2100
# maar wel als fractie van 'residual' cumulatieve CO2 
# dus om cumulatieve CO2 te krijgen: 
# met fractie van baseline residual cumu CO2 kunnen we dus niet werken.
#
# Anderemethode: 
# TCRE-figuur: de concentratie-ellipsen lopen van 2397 tot 8443 GtCO2
# kostenfiguur: de punten lopen van 0,113 tot 0,600
# als we dit herparametriseren moeten we de x-as van de kostenfiguur in deze functie stoppen: cumuCO2 = 12,415 * fr.baseline + 0,994

herparam.slope <- 12.415
herparam.intercept <- 0.994

costsLL$cumuCO2 <- herparam.slope*(costsLL$cumuCO2/10) + herparam.intercept
costsUL$cumuCO2 <- herparam.slope*(costsUL$cumuCO2/10) + herparam.intercept

# rechte lijn best fits
gLL <- lm(data = costsLL, mitigation_costs ~ cumuCO2)
gUL <- lm(data = costsUL, mitigation_costs ~ cumuCO2)

# "gemiddelde" lijn
intercept_mean = (coef(gLL)[1] + coef(gUL)[1])/2
slope_mean = (coef(gLL)[2] + coef(gUL)[2])/2

costs_mean <- slope_mean

# std van de hellingshoek
# Als onzekerheidsrange [10%,90%] is
costs.std90 <- (coef(gUL)[2] - slope_mean)/abs(qnorm(0.90))
costs.std10 <- (slope_mean - coef(gLL)[2])/abs(qnorm(0.10))
costs.std = abs((costs.std10 + costs.std90)/2)

# Als onzekerheidsrange [5%,95%] is
costs.std95 <- (coef(gUL)[2] - slope_mean)/abs(qnorm(0.95))
costs.std05 <- (slope_mean - coef(gLL)[2])/abs(qnorm(0.05))
costs.std2 = abs((costs.std05 + costs.std95)/2)


#--------------- Nulpunt berekenen -------------------------

nulpunt <- function(lijn) {
  b <- lijn$coefficients[1]
  a <- lijn$coefficients[2]
  return((-1*b)/a)
}

#--------------- Sample cost.slope en baselineCO2  ---------

f.costs.sample <- function(N, f.seed) {
  require(lhs)
  
  # maak "random" LHS
  set.seed(f.seed)
  costs.x <- randomLHS(N, 2)
  # geef namen
  colnames(costs.x) <- c("cost.slope","baselineCO2")
  
  # transformeer random LHS naar LHS met goede parameters
  
  #costs.slope <- qnorm(costs.x[,1], mean=costs_mean, sd=costs.std)
  costs.slope <- qpert(costs.x[,1], coef(gUL)[2], costs_mean, coef(gLL)[2], shape = 4)
  cumu.no_costs <- (nulpunt(gLL)+nulpunt(gUL))/2
  baselineCO2 <- rep(cumu.no_costs, N)
  # bundel in dataframe
  return(data.frame(costs.slope, baselineCO2))
}



#----------- Define model ---------------------

# cost = 0 => cumuCO2 = baselineCO2
# =>
# intercept = -baselineCO2 * slope


costs.oneRun <- function(cumuCO2,costs.slope,baselineCO2) {
  return(costs.slope * (cumuCO2 - baselineCO2))
}




#---------- run model -----------

# functie om costs uit te rekenen aan de hand van een gegeven Ttarget
# voorwaarden: gemiddelde en std van costs.slope, en model zijn gegeven

f.costsresult <- function(N,cumuCO2result, sample) {
  
  # run model
  costs <- mapply(costs.oneRun, cumuCO2result, sample[,1], sample[,2])
  
  # plak resultaat aan sample
  return(data.frame(sample, costs))
}


#--------- maak data per Ttarget ----------------

f.dataframe <- function(N,Ttarget,f.seed) {
  # maak samples
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  p.sample <- f.costs.sample(N, f.seed)
  # reken resultaten uit
  sample_en_result <- f.cumuCO2result(N,Ttarget,cumuvstemp.sample)
  costs.sample_en_result <- data.frame(sample_en_result,f.costsresult(N,sample_en_result[,5], p.sample))
  return(costs.sample_en_result)
}

N <- 10000
s.seed <- 21
data1 <- f.dataframe(N,1,s.seed)
data1.5 <- f.dataframe(N,1.5,s.seed)
data2 <- f.dataframe(N,2,s.seed)
data2.5 <- f.dataframe(N,2.5,s.seed)
data3 <- f.dataframe(N,3,s.seed)
data4 <- f.dataframe(N,4,s.seed)


#--------- bundel resultaten van het carbon budget voor verschillende Ttarget --------------

CO2.results <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframe(N,i,s.seed)
  CO2.results <- cbind(CO2.results, data$cumuCO2result)
}
colnames(CO2.results) <- as.character(seq(1, 4, by = 0.1))
CO2.results = data.table(CO2.results)


#--------- bundel resultaten van het carbon budget voor verschillende Ttarget --------------

costs.results <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframe(N,i,s.seed)
  costs.results <- cbind(costs.results, data$costs)
}
colnames(costs.results) <- as.character(seq(1, 4, by = 0.1))
costs.results = data.table(costs.results)


#-------- correlation coefficient matrix -----------

f.costs.CCmatrix <- function(N,f.seed) {
  # initialisatie
  costs.CCmatrix <- NULL
  teller <- 0
  
  # maak sample
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  costs.sample <- f.costs.sample(N, f.seed)
  
  for (i in seq(1, 4, by = 0.1)) {
    # print(i)
    # bereken resultaten
    sample_en_result <- f.cumuCO2result(N,i,cumuvstemp.sample)
    costs.sample_en_result <- data.frame(sample_en_result,f.costsresult(N,sample_en_result[,5],costs.sample))
    
    # bereken CC waarden
    costs.CCmatrix.hulp <- cor(costs.sample_en_result)[c(-1,-7,-8),]
    costs.CCmatrix <- rbind(costs.CCmatrix, costs.CCmatrix.hulp[,8])
    
    teller <- teller + 1
  }
  rownames(costs.CCmatrix) <- as.character(seq(1, 4, by = 0.1))
  
  return(costs.CCmatrix)
}

# maak een matrix van CCwaarden
CCmat <- f.costs.CCmatrix(N,s.seed)
# schrijf naar file
write.table(CCmat, file="CCmatGElin.txt", row.names=TRUE, col.names=TRUE, sep = ",")



#----------------- sensitivity package dingen ------------
library(boot)
n <- 100
X <- data.frame(X1 = runif(n, 0.5, 1.5),
                X2 = runif(n, 1.5, 4.5),
                X3 = runif(n, 4.5, 13.5))
# linear model : Y = X1 + X2 + X3
y <- with(X, X1 + X2 + X3)
# sensitivity analysis
pcc1.5 <- pcc(data1.5[,-8], data1.5[,8], nboot = 100)
print(x)
plot(x)


library(boot)
n <- 100
X <- data.frame(X1 = runif(n, 0.5, 1.5),
                X2 = runif(n, 1.5, 4.5),
                X3 = runif(n, 4.5, 13.5))
# linear model : Y = X1 + X2 + X3
y <- with(X, X1 + X2 + X3)
# sensitivity analysis
x <- src(X, y, nboot = 100)
print(x)
plot(x)
