files.dir <- "~/Desktop/TRI" 

filenames_TRI <- list.files(path=files.dir, full.names=T) 

TOLUENE <- NULL
BENZENE <- NULL
XYLENE <- NULL
EB <- NULL
for (i in 1:length(filenames_TRI)){
#for (i in 1:3){
  name1 <- (filenames_TRI[i])
  print(name1)
  TRI_a <- read.csv(name1)
  
  TOLUENE_a<- subset(TRI_a, TRI_a$X34..CHEMICAL == "TOLUENE" | TRI_a$X36..CAS...COMPOUND.ID == "108883")
  
  BENZENE_a <- subset(TRI_a, TRI_a$X34..CHEMICAL == "BENZENE"| TRI_a$X36..CAS...COMPOUND.ID == "71432")
  
  XYLENE_a<- subset(TRI_a, TRI_a$X34..CHEMICAL == "XYLENE (MIXED ISOMERS)"| TRI_a$X36..CAS...COMPOUND.ID == "108383" | TRI_a$X36..CAS...COMPOUND.ID == "1330207")
  
  EB_a <- subset(TRI_a, TRI_a$X34..CHEMICAL == "ETHYLBENZENE" | TRI_a$X36..CAS...COMPOUND.ID == "100414")
  
  TOLUENE <- as.data.frame(rbind(TOLUENE, TOLUENE_a))
  BENZENE <- as.data.frame(rbind(BENZENE, BENZENE_a))
  XYLENE <- as.data.frame(rbind(XYLENE, XYLENE_a))
  EB <- as.data.frame(rbind(EB, EB_a))
  
}




TOLUENE2 <- subset(TOLUENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR))
BENZENE2 <- subset(BENZENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR))
XYLENE2 <- subset(XYLENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR))
EB2 <- subset(EB, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR))


write.csv(TOLUENE2, file="~/Desktop/TRI/TOLUENE_allyears.csv", row.names=F) #all addresses
write.csv(BENZENE2, file="~/Desktop/TRI/BENZENE_allyears.csv", row.names=F) #all addresses
write.csv(XYLENE2, file="~/Desktop/TRI/XYLENE_allyears.csv", row.names=F) #all addresses
write.csv(EB2, file="~/Desktop/TRI/TOLUENE_allyears.csv", row.names=F) #all addresses


#Melissa will do research on finding mean values across single point over multiple years.


#Turn these into spatial points dataframes
