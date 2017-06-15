#------------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Sensitivity in costs
# nonlineaire fit in PE models
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

# schalen naar Tt is al gedaan bij het inlezen!
#costsLL[1] <- costsLL[1]/1000
#costsUL[1] <- costsUL[1]/1000


#---------- PE models ---------------
# onderkant redelijke fit:
# C = 1/(CO2-0.5)
# bovenkant redelijke fit:
# C = 3/(CO2-1)
# 
# => C = (4p-1)/(CO2-p) met 1/2 < p < 1
#
# we moeten p dus sampelen in waarden tussen 1/2 en 1

p.mean <- 0.75
p.min <- 0.5
p.max <- 1


# std van p
# Als onzekerheidsrange [10%,90%] is
p.std90 <- (p.max - p.mean)/abs(qnorm(0.90))

# Als onzekerheidsrange [5%,95%] is
p.std95 <- (p.max - p.mean)/abs(qnorm(0.95))


#--------------- Sample curveparameter ---------

f.p.sample <- function(N, f.seed) {
  require(lhs)
  
  # maak "random" LHS
  set.seed(f.seed)
  costs.x <- randomLHS(N, 1)
  # geef namen
  colnames(costs.x) <- c("curve.parameter")
  
  # transformeer random LHS naar LHS met goede parameters
  
  #costs.slope <- qnorm(costs.x[,1], mean=costs_mean, sd=costs.std)
  curve.parameter <- qpert(costs.x[,1], p.min, p.mean, p.max, shape = 4)
  
  # bundel in dataframe
  return(curve.parameter)
}



#----------- Define model ---------------------

# => C = (4p-1)/(CO2-p) met 1/2 < p < 1

costs.oneRun <- function(cumuCO2,curve.parameter) {
  return((4 * curve.parameter - 1)/(cumuCO2 - curve.parameter))
}


#---------- run model -----------

# functie om costs uit te rekenen aan de hand van een gegeven Ttarget
# voorwaarden: gemiddelde en std van costs.slope, en model zijn gegeven

f.costsresult <- function(N,cumuCO2result, p.sample) {
  
  # run model
  costs <- mapply(costs.oneRun, cumuCO2result, p.sample)
  
  # plak resultaat aan sample
  return(data.frame(p.sample, costs))
}


#--------- maak data per Ttarget ----------------

f.dataframe <- function(N,Ttarget,f.seed) {
  # maak samples
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  p.sample <- f.p.sample(N, f.seed)
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
  p.sample <- f.p.sample(N, f.seed)
  
  for (i in seq(1, 4, by = 0.1)) {
    # print(i)
    sample_en_result <- f.cumuCO2result(N,i,cumuvstemp.sample)
    cumuCO2result <- sample_en_result[,5]
    costs.result <- f.costsresult(N,sample_en_result[,5],p.sample)
    costs.sample_en_result <- data.frame(sample_en_result,f.costsresult(N,sample_en_result[,5], p.sample))
    
    # laat CC van costs met tTarget en costs met zichzelf eruit
    costs.CCmatrix.hulp <- cor(costs.sample_en_result)[c(-1,-7),]
    costs.CCmatrix <- rbind(costs.CCmatrix, costs.CCmatrix.hulp[,7])
    
    teller <- teller + 1
  }
  rownames(costs.CCmatrix) <- as.character(seq(1, 4, by = 0.1))
  
  return(costs.CCmatrix)
}

CCmat <- f.costs.CCmatrix(N,s.seed)
# schrijf naar file
write.table(CCmat, file="CCmatPEnonlin.txt", row.names=TRUE, col.names=TRUE, sep = ",")

