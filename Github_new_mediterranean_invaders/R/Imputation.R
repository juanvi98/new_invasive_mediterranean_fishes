#IMPUTATION

###---Set up libraries---###
library(missForest)
library(ggplot2)
library(reshape2)
library(Amelia)
library(data.table)
library(dummy)
library(dplyr)

#Load the database 
traits_final <- read.csv("./Datos/traits_final.csv")
traits_final <- traits_final[,-1]

###--- Remove the traits that are not useful (Species, name, code, etc.) ---###

colnames(traits_final)

#Remove : Species 1, SpecCode.x 2, Genus.x 3, SpeciesRefNo 4, Author 5, FBname 6, picpref 7, famcode 8, gencode 9, source 11, TaxIssue 12,AirBreathing 17,Depth range ref 20, Ltypemax 23, Maxlengthref 24, importanceref 25,Pricereliability 28, gamefish 42,electrogenic 44, google image 45, comments 46, entered 49,date entered 50, 51, 52,53,54,55,56,57,58,62,63, FAO 65

#Save Species and Invasive variables for later
inv <- traits_final$Invasive
spc <- traits_final$Species

traits_final <- traits_final[,-c(1:8, 10, 11, 16, 19, 20 , 35, 37, 38, 39, 42:49, 52:56)]

###--- Change the necessary variables to factor ---###

traits_final[] <- lapply(traits_final, function(x) if(is.character(x)) as.factor(x) else x)

str(traits_final)
colnames(traits_final)

#Also the binary ones
traits_final[, c(2:4, 9:18, 24)] <- lapply(traits_final[, c(2:4, 9:18, 24)], as.factor)


#Check species with too many NA's
(num_NAs_por_observacion <- rowSums(is.na(traits_final))/ncol(traits_final))

which(num_NAs_por_observacion > 0.2) # Remove Hexanchus vitulus

traits_final <- traits_final[-354,]

#Check again the NA's percentage
(porcentaje_na_imp <- sapply(traits_final, function(x) mean(is.na(x)) * 100))

#Remove Invasive variable for the imputation
final_fishes_imp <- traits_final[, -27]

#Here we did the correlation analysis (Correlation script)

###--- Extract the error only in the variables with NA's ---###
dfntree_1 <- data.frame(matrix(NA, nrow = 100, ncol = 11))
colnames(dfntree_1) <- c("ntree","Length", "PriceCateg", "UsedforAquaculture", "UsedasBait", "Aquarium", "Dangerous", "PD50", "TempPrefMin", "TempPrefMean", "TempPrefMax")

for (n in (1:150)){
  dfntree_1[n, 1] <- n
  o <- missForest(final_fishes_imp, maxiter = 10, ntree = n, variablewise = TRUE) 
  dfntree_1[n,2] <- o$OOBerror[7]
  dfntree_1[n,3] <- o$OOBerror[8]    
  dfntree_1[n,4] <- o$OOBerror[19]
  dfntree_1[n,5] <- o$OOBerror[20]
  dfntree_1[n,6] <- o$OOBerror[21]    
  dfntree_1[n,7] <- o$OOBerror[22]
  dfntree_1[n,8] <- o$OOBerror[23]
  dfntree_1[n,9] <- o$OOBerror[29]
  dfntree_1[n,10] <- o$OOBerror[30]    
  dfntree_1[n,11] <- o$OOBerror[31]
  
  #save OOBerror for target traits only
  save(dfntree_1, file ="./Datos/error_dfntree_selection_1_dfall.RData")
}

o$ximp$BodyShapeI

#Error is too high for Lenght

###--- Create dummy variables to reduce the error ---###

colnames(final_fishes_imp)

fishes_dummy <- dummy(final_fishes_imp[, c(1, 5, 8, 19, 20, 21, 22, 25, 26)])

#Remove the dummy variables
final_fishes_imp <- final_fishes_imp[, -c(1, 5, 8, 19, 20, 21, 22, 25, 26)] 

#Merge the dummy variables with the numeric and also the binary ones
final_fishes_imp$ID <- 1:nrow(final_fishes_imp)
fishes_dummy$ID <- 1:nrow(fishes_dummy)

final_fishes_imp <- merge(final_fishes_imp, fishes_dummy, by = "ID")
final_fishes_imp <- final_fishes_imp[, -1]

final_fishes_imp[] <- lapply(final_fishes_imp, function(x) if(is.character(x)) as.factor(x) else x)

colnames(final_fishes_imp)

###--- Check the error in the factor variables correlated with the Invasive variable (CORRELATION SCRIPT) ---###
dfntree_2 <- data.frame(matrix(NA, nrow = 100, ncol = 30))
colnames(dfntree_2) <- c("ntree", "High.price", "Low.Price", "Medium.price", "Unknown.price", "Very.high.price", "Commercial", "Experimental", "Never_rarely.commercial", "Used.as.bait.", "never.rarely.used", "Ocassionally.used", "Usually.used", "Aq.commercial", "Aq.never.rarely", "Aq.potential", "Aq.public", "Aq_show", "Harmless", "Other", "Poisonous.to.eat", "Potential.pest", "cig.pois", "Traumatogenic", "Venomous", "Length", "PD50", "TempPrefMin", "TempPrefMean", "TempPrefMax")

for (n in (1:150)){
  dfntree_2[n, 1] <- n
  o <- missForest(final_fishes_imp, maxiter = 15, ntree = n, variablewise = TRUE) 
  dfntree_2[n,2] <- o$OOBerror[37]
  dfntree_2[n,3] <- o$OOBerror[38]
  dfntree_2[n,4] <- o$OOBerror[39]
  dfntree_2[n,5] <- o$OOBerror[40]
  dfntree_2[n,6] <- o$OOBerror[41]
  dfntree_2[n,7] <- o$OOBerror[42]
  dfntree_2[n,8] <- o$OOBerror[43]
  dfntree_2[n,9] <- o$OOBerror[44]
  dfntree_2[n,10] <- o$OOBerror[45] 
  dfntree_2[n,11] <- o$OOBerror[46]
  dfntree_2[n,12] <- o$OOBerror[47]
  dfntree_2[n,13] <- o$OOBerror[48]
  dfntree_2[n,14] <- o$OOBerror[49]
  dfntree_2[n,15] <- o$OOBerror[50]
  dfntree_2[n,16] <- o$OOBerror[51]
  dfntree_2[n,17] <- o$OOBerror[52]
  dfntree_2[n,18] <- o$OOBerror[53]
  dfntree_2[n,19] <- o$OOBerror[54]
  dfntree_2[n,20] <- o$OOBerror[55]
  dfntree_2[n,21] <- o$OOBerror[56]
  dfntree_2[n,22] <- o$OOBerror[57]
  dfntree_2[n,23] <- o$OOBerror[58]
  dfntree_2[n,24] <- o$OOBerror[59]
  dfntree_2[n,25] <- o$OOBerror[60]
  dfntree_2[n,26] <- o$OOBerror[5]
  dfntree_2[n,27] <- o$OOBerror[16]
  dfntree_2[n,28] <- o$OOBerror[20]
  dfntree_2[n,29] <- o$OOBerror[21]
  dfntree_2[n,30] <- o$OOBerror[22]
  save(dfntree_2, file ="./Datos/error_dfntree_selection_dfall.RData")
}

#Around n=70 the error becomes stable, but in some numeric variables is still too high


###--- Check the best mtry value (number of variables randomly sampled at each split) ---###

dfmtry <- data.frame(matrix(NA, nrow = 25, ncol = 30))
colnames(dfmtry) <- c("ntree", "High.price", "Low.Price", "Medium.price", "Unknown.price", "Very.high.price", "Commercial", "Experimental", "Never_rarely.commercial", "Used.as.bait.", "never.rarely.used", "Ocassionally.used", "Usually.used", "Aq.commercial", "Aq.never.rarely", "Aq.potential", "Aq.public", "Aq_show", "Harmless", "Other", "Poisonous.to.eat", "Potential.pest", "cig.pois", "Traumatogenic", "Venomous", "Length", "PD50", "TempPrefMin", "TempPrefMean", "TempPrefMax")

for (n in (1:35)){
  dfmtry[n, 1] <- n
  o <- missForest(final_fishes_imp, maxiter = 15, mtry = n, ntree = 70 , variablewise = TRUE) 
  dfmtry[n,2] <- o$OOBerror[37]
  dfmtry[n,3] <- o$OOBerror[38]
  dfmtry[n,4] <- o$OOBerror[39]
  dfmtry[n,5] <- o$OOBerror[40]
  dfmtry[n,6] <- o$OOBerror[41]
  dfmtry[n,7] <- o$OOBerror[42]
  dfmtry[n,8] <- o$OOBerror[43]
  dfmtry[n,9] <- o$OOBerror[44]
  dfmtry[n,10] <- o$OOBerror[45] 
  dfmtry[n,11] <- o$OOBerror[46]
  dfmtry[n,12] <- o$OOBerror[47]
  dfmtry[n,13] <- o$OOBerror[48]
  dfmtry[n,14] <- o$OOBerror[49]
  dfmtry[n,15] <- o$OOBerror[50]
  dfmtry[n,16] <- o$OOBerror[51]
  dfmtry[n,17] <- o$OOBerror[52]
  dfmtry[n,18] <- o$OOBerror[53]
  dfmtry[n,19] <- o$OOBerror[54]
  dfmtry[n,20] <- o$OOBerror[55]
  dfmtry[n,21] <- o$OOBerror[56]
  dfmtry[n,22] <- o$OOBerror[57]
  dfmtry[n,23] <- o$OOBerror[58]
  dfmtry[n,24] <- o$OOBerror[59]
  dfmtry[n,25] <- o$OOBerror[60]
  dfmtry[n,26] <- o$OOBerror[5]
  dfmtry[n,27] <- o$OOBerror[16]
  dfmtry[n,28] <- o$OOBerror[20]
  dfmtry[n,29] <- o$OOBerror[21]
  dfmtry[n,30] <- o$OOBerror[22]
  save(dfmtry, file ="./Datos/error_mtry_selection_dfall.RData")
}

#The error is lower with a higher mtry, but a high mtry could overfit the model, so around 20 is a good value (low error and prevents overfitting).

###--- Now we input the variables with the chosen values and the whole dataset ---###
set.seed(345)
#Set seed for reproducibility
imp_final <- missForest(final_fishes_imp, maxiter = 15, mtry = 19, ntree = 70 , variablewise = TRUE)

colnames(final_fishes_imp)

#Replace the original variables with the imputed ones

final_fishes_imp[,c(16, 21, 37:60)] <- imp_final$ximp[,c(16, 21, 37:60)]



#Add the Invasive and the Species variables again to model the database

final_fishes_imp$Invasive <- inv
final_fishes_imp$Species <- spc

final_fishes_imp <- final_fishes_imp %>%
  select(c(Species, Invasive), everything())

###--- Eliminate the species with NA's in 'Lenght' variable ---###
final_fishes_imp <- final_fishes_imp[-c(19, 54, 229, 230, 305, 482, 517, 527, 539, 541, 581, 588, 755, 777, 817),]

#Only Oxyurichthys petersii was Invasive between the discards

#There are still too many variables, eliminate the correlated and the less important ones

colnames(final_fishes_imp)

final_fishes_imp <- final_fishes_imp[, -c(3:5, 20, 22, 24, 39:71)]


#Put the dummy variables together to create new variables
trad_fishing <- factor(ifelse(final_fishes_imp$MGillnets|final_fishes_imp$MCastnets|final_fishes_imp$MTraps|final_fishes_imp$MSpears|final_fishes_imp$MLiftnets == 1, 1, 0))

trawl_fishing <- factor(ifelse(final_fishes_imp$MTrawls|final_fishes_imp$MDredges == 1, 1, 0))

pelag_fishing <- factor(ifelse(final_fishes_imp$MSeines|final_fishes_imp$MHooksLines == 1, 1, 0))

Body_shape <- factor(ifelse(final_fishes_imp$BodyShapeI_eel.like|final_fishes_imp$BodyShapeI_elongated|final_fishes_imp$BodyShapeI_Elongated|final_fishes_imp$BodyShapeI_other|final_fishes_imp$BodyShapeI_short.and...or.deep == 1, "Other", "Fusiform_normal"))

Pelagic <- factor(ifelse(final_fishes_imp$DemersPelag_bathypelagic|final_fishes_imp$DemersPelag_benthopelagic|final_fishes_imp$DemersPelag_pelagic|final_fishes_imp$DemersPelag_pelagic.neritic|final_fishes_imp$DemersPelag_pelagic.oceanic == 1, 1, 0))

Demersal <- factor(ifelse(final_fishes_imp$DemersPelag_demersal|final_fishes_imp$DemersPelag_bathydemersal == 1, 1, 0))

Reef_associated <- factor(ifelse(final_fishes_imp$DemersPelag_reef.associated == 1, 1, 0))

#Delete the changed variables
final_fishes_imp <- final_fishes_imp[, -c(5:14, 19:32)]

#Add the new ones
final_fishes_imp$trad_fishing <- trad_fishing
final_fishes_imp$trawl_fishing <- trawl_fishing
final_fishes_imp$pelag_fishing <- pelag_fishing
final_fishes_imp$Body_shape <- Body_shape
final_fishes_imp$Demersal <- Demersal
final_fishes_imp$Pelagic <- Pelagic
final_fishes_imp$Reef_associated <- Reef_associated
final_fishes_imp$Emblematic <- as.factor(final_fishes_imp$Emblematic)

#Change the response variable to numeric with 0's and 1's
final_fishes_imp$Invasive <- as.numeric(as.factor(final_fishes_imp$Invasive))

final_fishes_imp$Invasive <- final_fishes_imp$Invasive-1

#Length is Maxlength 
final_fishes_imp <- final_fishes_imp %>% rename(Max_length = Length )

###--- Save the created final database ---### 

#write.csv(final_fishes_imp, "Datos/fishes_final.csv")


