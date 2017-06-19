
# Den Haag
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
# Thuis
setwd("~/Documenten/Stage PBL/modelTt")


#----------- Relatie cumulatieve CO2 <-> kosten -------------------

# data inlezen
cumuvscostsGE <- read.csv(file = "./../Databases/cumuCO2vscostsGEmodels.txt", header = TRUE)
cumuvscostsPE <- read.csv(file = "./../Databases/cumuCO2vscostsPEmodels.txt", header = TRUE)
cumuvscostsAllE <- read.csv(file = "./../Databases/cumuCO2vscostsAllEmodels.txt", header = TRUE)

# schalen naar Tt
cumuvscostsGE$cumuCO2 <- cumuvscostsGE$cumuCO2*10
cumuvscostsPE$cumuCO2 <- cumuvscostsPE$cumuCO2*10


# plaatje van GE models
par(mfrow=c(1,1))
plot(cumuvscostsGE,xlim=c(0,7),ylim=c(-1,8),xlab = "cumu CO2 (Tt)",ylab = "Abatement costs (% GDP)")

# curve y = 1/(ax-b) erbij
#bovenkant redelijke fit
curve(7/(x-1), add = TRUE, col = "blue")
#bovenkant redelijke fit
curve(8/(x-0.5), add = TRUE, col = "green")
#bovenkant redelijke fit 2
curve(13.5/(x)-1, add = TRUE, col = "orange")


#onderkant redelijke fit
curve(.75/(x-0.75), add = TRUE, col = "blue")
#onderkant redelijke fit 2
curve(2/(x-0.5)-0.75, add = TRUE, col = "red")
#onderkant redelijke fit 3
curve(2.75/(x)-0.75, add = TRUE, col = "orange")


# plaatje van PE models
par(mfrow=c(1,1))
plot(cumuvscostsPE,xlim=c(0,7),ylim=c(0,8))

# curve y = 1/(ax-b) erbij
#bovenkant redelijke fit
curve(3/(x-1), add = T,xlim=c(0,7),ylim=c(0,8))

#onderkant redelijke fit
curve(1/(x-0.5), add = TRUE)

# gemiddelde
curve(2/(x-0.75), add = TRUE)
