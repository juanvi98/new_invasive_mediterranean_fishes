#INITIAL FISHES

###---Set up libraries---###
library("rfishbase")
library("dplyr")
library("readxl")

#Read the excel with all the Mediterranean fishes names
all_fishes <- read.csv("./Datos/Initial_Fishes_Mediterranean_Sea.csv")

#Extract the names as a list
all_fishes <- all_fishes$Species

###---Compare the initial list with other databases of invasive species---###

###---WRiMS and JRC---###
wrims_JRC <- read.delim("./Datos/WRiMS_invasive_fishes.txt")

#Extract the names as a list
wrims_JRC <- unique(wrims_JRC$ScientificName)

#Change the Cyprinodon dispar name to compare with fishbase
wrims_JRC[4] <- "Aphaniops dispar"

#Add Plotosus lineatus from JRC list (Perccottus glenii is already in wrims)
wrims_JRC[27] <- "Plotosus lineatus"

#Compare with our list
invasive_wrims_JRC <- intersect(all_fishes, wrims_JRC)

###---GBIF---###
gbif <- read.delim("./Datos/Lista invasoras GBIF/invasives_2011-11-20/taxon.txt", header=FALSE)
gbif <- gbif$V2
invasive_gbif <- intersect(gbif,all_fishes)

###---InvaCost---###
InvaCost <- read.csv("./Datos/InvaCost.csv", sep=";")
InvaCost <- InvaCost$Species
invasive_InvaCost <- intersect(InvaCost,all_fishes)


###---GlobalNat---###
GlobalNat <- read.csv("./Datos/GlobalNat.csv", sep=";")
GlobalNat <- GlobalNat$Species

invasive_GlobalNat <- intersect(GlobalNat,all_fishes)

###---CIESM---###
lista_CIESM <- read.csv("./Datos/lista_CIESM.csv", sep=";")
lista_CIESM <- lista_CIESM$Taxon

invasive_CIESM <- intersect(lista_CIESM,all_fishes)

#Check the species who doesn't match
CIESM_review <- lista_CIESM[!(lista_CIESM %in% invasive_CIESM)]

###---ORMEF---###
ORMEF <- read.delim("./Datos/ORMEF.tsv")
ORMEF <- unique(ORMEF$Species)  
#Delete the space in Ablennes hians
ORMEF[33] <- "Ablennes hians"

invasive_ORMEF <- intersect(all_fishes, ORMEF)

#Check the species who doesn't match
ORMEF_review <- ORMEF[!(ORMEF %in% invasive_ORMEF)]

###---Final list---###
Lista_completa_mediterranean <- read.csv("./Datos/Lista_completa_mediterranean.csv", sep="")

Lista_completa_mediterranean <- Lista_completa_mediterranean[, -c(1,3,5)]

