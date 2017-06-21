#-------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Sensitivity in TCRE
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 

# Den Haag
#setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
# Thuis
#setwd("~/Documenten/Stage PBL/modelTt")

source("packages.R")

#----------- Relatie cumulatieve CO2 <-> temperatuur -----------------

# data inlezen
cumuvstempLL <- read.csv(file = "./../Databases/cumuvstemp_lowerlimit.txt", header = TRUE)
cumuvstempUL <- read.csv(file = "./../Databases/cumuvstemp_upperlimit.txt", header = TRUE)


# schalen naar Tt
cumuvstempLL[1] <- cumuvstempLL[1]/1000
cumuvstempUL[1] <- cumuvstempUL[1]/1000

# rechte lijn best fits
fLL <- lm(data = cumuvstempLL, temp ~ cumuCO2)
fUL <- lm(data = cumuvstempUL, temp ~ cumuCO2)

# "gemiddelde" lijn
intercept = (coef(fLL)[1] + coef(fUL)[1])/2
slope = (coef(fLL)[2] + coef(fUL)[2])/2

TCREmean <- slope

# Als onzekerheidsrange [10%,90%] is
TCREstd90 <- (coef(fUL)[2] - slope)/abs(qnorm(0.90))
TCREstd10 <- (slope - coef(fLL)[2])/abs(qnorm(0.10))
TCREstd = (TCREstd10 + TCREstd90)/2

# Als onzekerheidsrange [5%,95%] is
TCREstd95 <- (coef(fUL)[2] - slope)/abs(qnorm(0.95))
TCREstd05 <- (slope - coef(fLL)[2])/abs(qnorm(0.05))
TCREstd2 = (TCREstd05 + TCREstd95)/2

#------------- Temp 2010 --------------------------
# data inlezen
temp2010 <- read.csv(file = "./../Databases/temp2010.txt", header = TRUE)

T2010mean <- with(temp2010,temp)[1]

# [10%,90%]
T2010std90 <- (with(temp2010,temp)[2] - with(temp2010,temp)[1])/abs(qnorm(0.90))
T2010std10 <- (with(temp2010,temp)[1] - with(temp2010,temp)[3])/abs(qnorm(0.10))
T2010std = (T2010std10 + T2010std90)/2

# [5%,95%]
T2010std95 <- (with(temp2010,temp)[2] - with(temp2010,temp)[1])/abs(qnorm(0.95))
T2010std05 <- (with(temp2010,temp)[1] - with(temp2010,temp)[3])/abs(qnorm(0.05))
T2010std2 = (T2010std05 + T2010std95)/2


#------------ CumuCO2 2010 ------------------------
# data inlezen
cumuCO22010 <- read.csv(file = "./../Databases/cumuCO22010.txt", header = TRUE)

# schalen naar Tt
cumuCO22010[1] <- cumuCO22010[1]/1000

CO22010mean <- 3.67 * with(cumuCO22010,cumuCO2)[1]

# [10%,90%]
CO22010std90 <- (with(cumuCO22010,cumuCO2)[2] - with(cumuCO22010,cumuCO2)[1])/abs(qnorm(0.90))
CO22010std10 <- (with(cumuCO22010,cumuCO2)[1] - with(cumuCO22010,cumuCO2)[3])/abs(qnorm(0.10))
CO22010std = 3.67 * (CO22010std10 + CO22010std90)/2

# [5%,95%]
CO22010std95 <- (with(cumuCO22010,cumuCO2)[2] - with(cumuCO22010,cumuCO2)[1])/abs(qnorm(0.95))
CO22010std05 <- (with(cumuCO22010,cumuCO2)[1] - with(cumuCO22010,cumuCO2)[3])/abs(qnorm(0.05))
CO22010std2 = 3.67 * (CO22010std05 + CO22010std95)/2


#----------- Maak sample ----------------------
# Zie ook http://r.789695.n4.nabble.com/Latin-Hypercube-Sampling-with-a-condition-td3563765.html
# en https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2570191/

# maakt een sample van T2010, TCRE en CO22010
f.cumuvstemp.sample <- function(N, f.seed) {
  require(lhs)
  
  # maak random LHS
  set.seed(f.seed)
  x <- randomLHS(N, 4)
  # geef namen
  colnames(x) <- c("Ttarget", "T2010", "TCRE", "CO22010")
  
  # transformeer random LHS naar LHS met goede parameters
  T2010 <- qnorm(x[,2], mean=T2010mean, sd=T2010std)
  TCRE <- qnorm(x[,3], mean=TCREmean,sd=TCREstd)
  CO22010 <- qnorm(x[,4], mean=CO22010mean, sd=CO22010std)
  
  # bundel in dataframe
  return(data.frame(T2010,TCRE,CO22010))
}



#----------- Define model ---------------------

oneRun <- function(Ttarget,T2010,TCRE,CO22010) {
  return(CO22010 + (Ttarget - T2010)/TCRE)
}


#--------------- Run model  ---------

# functie om cumuCO2 uit te rekenen aan de hand van een gegeven Ttarget 
# en gegeven sample voor T2010, TCRE en CO22010 

f.cumuCO2result <- function(N, Ttarget, sample) {
  f.Ttarget <- rep(Ttarget, N)
  # run model
  cumuCO2result <- mapply(oneRun, f.Ttarget, sample[,1], sample[,2], sample[,3])
  # plak resultaat aan sample
  return(data.frame(f.Ttarget, sample, cumuCO2result))
}

#-------------- CCmatrix -------------

N <- 10000
s.seed <- 21

# functie die voor waarden van Ttarget tussen 1 en 4 de Correlation Coefficent uitrekent tussen de inputparameters en cumuCO2, de model uitkomst
f.CCmatrix <- function(N,f.seed) {
  # initialisatie
  CCmatrix <- NULL
  teller <- 0
  
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  for (i in seq(1, 4, by = 0.1)) {
    # print(i)
    sample_en_result <- f.cumuCO2result(N,i,cumuvstemp.sample)
    CCmatrix.hulp <- cor(sample_en_result)[-1,]
    CCmatrix <- rbind(CCmatrix, CCmatrix.hulp[-4,5])
    
    teller <- teller + 1
  }
  rownames(CCmatrix) <- as.character(seq(1, 4, by = 0.1))

  return(CCmatrix)
}




