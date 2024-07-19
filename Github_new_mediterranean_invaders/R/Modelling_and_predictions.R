#MODELLING


###---Set up libraries---###
library(INLA)
library(dplyr)

fishes_final <- read.csv("./Datos/fishes_final.csv")

fishes_final <- fishes_final[, -1]

#Here we did the descriptive analysis in another script

#Delete the 'Emblematic' variable

fishes_final <- fishes_final[, -6]

#Convert all the binary variables to factor, except the response one (Invasive)
fishes_final[, -2] <- fishes_final[, -2] %>%
  mutate_if(is.integer, as.factor)

#Pasamos la variable respuesta a numerico
fishes_final$Invasive <- as.numeric(fishes_final$Invasive)


### --- Using a function to select the best models ---###

#Define the response variable 
resp <- fishes_final$Invasive

#Define the variables to include in the models
variables <- colnames(fishes_final)
#Delete the name and the response variable
variables <- variables[-c(1,2)]

### --- Call the function --- ###
models_bin <- Bdiclcpomodel_stack(resp=resp, variables=variables, datos=fishes_final, n=20,
                                family="binomial", 
                                control.family = list(link = "logit"),
                                control.predictor=list(compute=TRUE),
                                control.compute = list(config=TRUE, dic=TRUE, cpo=TRUE, waic=TRUE),
                                num.threads=2,
                                control.inla=list(strategy="gaussian"),
                                verbose=FALSE)


#saveRDS(models_bin, "Datos/models_bin.rds")

models_bin<- readRDS("Datos/models_bin.rds")

models_bin$`Modelos dic`[1:10,]
models_bin$`Modelos waic`[1:10,]
models_bin$`Modelos lcpo`[1:10,]

#The best model is:                                    resp ~ 1 + Max_length + TempPrefMean + trad_fishing + pelag_fishing + Reef_associated



###--- Final model ---###
mod_inla_final <- inla( Invasive ~ 1 + fishes_final$Max_length +
                             fishes_final$TempPrefMean +
                             fishes_final$trad_fishing +
                          fishes_final$pelag_fishing +
                          fishes_final$Reef_associated,
                           family = "binomial",
                           control.family = list(link = "logit"),
                           control.compute = list(dic = TRUE, waic = TRUE, return.marginals.predictor = TRUE),
  
                           control.predictor = list(compute = TRUE),
                          data = fishes_final)

#Model summary
summary(mod_inla_final)

###--- Posterior distributions plots ---###

#Intercept
par(mfrow= c(2,3))
plot(mod_inla_final$marginals.fixed$`(Intercept)`[, 1], mod_inla_final$marginals.fixed$`(Intercept)`[, 2], xlab = "Intercept", ylab = "densidad")

#Max_Lenght
plot(mod_inla_final$marginals.fixed$`fishes_final|S|Max_length`[, 1], mod_inla_final$marginals.fixed$`fishes_final|S|Max_length`[, 2], xlab = "Longitud Máxima", ylab = "densidad")

#Mean Temperature
plot(mod_inla_final$marginals.fixed$`fishes_final|S|TempPrefMean`[, 1], mod_inla_final$marginals.fixed$`fishes_final|S|TempPrefMean`[, 2], xlab = "Temperatura Media Preferida", ylab = "densidad")

#Traditional fishing
plot(mod_inla_final$marginals.fixed$`fishes_final|S|trad_fishing1`[, 1], mod_inla_final$marginals.fixed$`fishes_final|S|trad_fishing1`[, 2], xlab = "Pesca Tradicional", ylab = "densidad")

#Pelagic fishing
plot(mod_inla_final$marginals.fixed$`fishes_final|S|pelag_fishing1`[, 1], mod_inla_final$marginals.fixed$`fishes_final|S|pelag_fishing1`[, 2], xlab = "Pesca Pelágica", ylab = "densidad")

#Reef associated
plot(mod_inla_final$marginals.fixed$`fishes_final|S|Reef_associated1`[, 1], mod_inla_final$marginals.fixed$`fishes_final|S|Reef_associated1`[, 2], xlab = "Asociada a arrecifes", ylab = "densidad")

###--- Predictions ---###

#Create an object with all the observations to predict in each species. Include the intercept and all the variables included in the model

lin <- inla.make.lincombs(`(Intercept)` = rep(1, 817) ,
                          `Max_length` = fishes_final$Max_length,
                         `TempPrefMean` = fishes_final$TempPrefMean,
                         `trad_fishing1` = fishes_final$trad_fishing,
                         `pelag_fishing1` = fishes_final$pelag_fishing,
                         `Reef_associated1` = fishes_final$Reef_associated)

#Run again the model with the linear combinations in the "lincomb" option.

mod_inla_final_pred <- inla( Invasive ~ 1 + Max_length +
                          TempPrefMean +
                          trad_fishing +
                          pelag_fishing +
                          Reef_associated,
                        family = "binomial",
                        lincomb = lin,
                        control.family = list(link = "logit"),
                        control.compute = list(dic = TRUE, waic = TRUE, return.marginals.predictor = TRUE),
                        control.predictor = list(compute = TRUE),
                        data = fishes_final)

#Save the invasives positions
invasives <- which(fishes_final$Invasive==1)

#Save the no invasives positions
not_invasives <- which(fishes_final$Invasive==0)

#Invert the logit to obtain the probabilities of the prediction for each invasive species
distrib_invasive <- plogis(mod_inla_final_pred$summary.lincomb.derived$mean[invasives])
distrib_invasive <- as.data.frame(distrib_invasive)
distrib_invasive$ID <- invasives

#Invert the logit to obtain the probabilities of the prediction for each not invasive species
distrib_not_invasive <- plogis(mod_inla_final_pred$summary.lincomb.derived$mean[not_invasives])
distrib_not_invasive <- as.data.frame(distrib_not_invasive)
distrib_not_invasive$ID <- not_invasives

#Calculate the 75% percentile of the invasives distribution
quant0.75 <- quantile(plogis(mod_inla_final_pred$summary.lincomb.derived$mean[invasives]), probs = seq(0, 1, 0.75))[2]

#Calculate the 75% percentile of the not invasives distribution
quant0.75_not_invasive <- quantile(plogis(mod_inla_final_pred$summary.lincomb.derived$mean[not_invasives]), probs = seq(0, 1, 0.75))[2]

#Choose the potential invaders (Probability > 75% percentil of invasives distribution)
sum(potentialinvasives <- plogis(mod_inla_final_pred$summary.lincomb.derived$mean[not_invasives]) >= quant0.75)

nuevas_invasoras <- which(plogis(mod_inla_final_pred$summary.lincomb.derived$mean[not_invasives]) >= quant0.75)

nuevas_invasoras <- distrib_not_invasive$ID[nuevas_invasoras]

#Invasive species as invasives with that criteria
which(plogis(mod_inla_final_pred$summary.lincomb.derived$mean[distrib_invasive$ID]) >= quant0.75)

#Names of the potential invaders
fishes_final$Species[nuevas_invasoras]

