#------------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Sensitivity in costs
# PE models, linear fit
#
#------------------------------------------------

# Den Haag
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
# Thuis
#setwd("~/Documenten/Stage PBL/modelTt")

source("TCRE.R")


#----------- Relatie cumulatieve CO2 <-> mitigatie kosten -----------------

# data inlezen
costsLL <- read.csv(file = "./../Databases/costs_LL_PE.txt", header = TRUE)
costsUL <- read.csv(file = "./../Databases/costs_UL_PE.txt", header = TRUE)

# schalen naar Tt al gedaan met inlezen!
#costsLL[1] <- costsLL[1]/1000
#costsUL[1] <- costsUL[1]/1000

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
  baselineCO2 <- rep(6, N)
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


#-------- correlation coefficient matrix -----------

N <- 10000
s.seed <- 21

f.costs.CCmatrix <- function(N,f.seed) {
  # initialisatie
  costs.CCmatrix <- NULL
  teller <- 0
  
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  costs.sample <- f.costs.sample(N, f.seed)
  
  for (i in seq(1, 4, by = 0.1)) {
    # print(i)
    sample_en_result <- f.cumuCO2result(N,i,cumuvstemp.sample)
    costs.sample_en_result <- data.frame(sample_en_result,f.costsresult(N,sample_en_result[,5],costs.sample))
    
    costs.CCmatrix.hulp <- cor(costs.sample_en_result)[c(-1,-7,-8),]
    costs.CCmatrix <- rbind(costs.CCmatrix, costs.CCmatrix.hulp[,8])
    
    teller <- teller + 1
  }
  rownames(costs.CCmatrix) <- as.character(seq(1, 4, by = 0.1))
  
  return(costs.CCmatrix)
}

CCmat <- f.costs.CCmatrix(N,s.seed)
# schrijf naar file
write.table(CCmat, file="CCmatPElin.txt", row.names=TRUE, col.names=TRUE, sep = ",")

