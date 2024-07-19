#TRAIT EXTRACTION

###---Set up libraries---###
library("rfishbase")
library("dplyr")

#Read the archive with all the species and the invasive status
Lista_completa_mediterranean <- read.csv("./Datos/Lista_completa_mediterranean.csv", sep="")

#Extract only the names and the invasive status
Lista_completa_mediterranean <- Lista_completa_mediterranean[, -c(1,3,5)]

#Check the number of invasive species
sum(Lista_completa_mediterranean$Invasive == "Yes")

#check that no species are repeated in the data
nrow(Lista_completa_mediterranean) == length(unique(Lista_completa_mediterranean$Species)) #MUST be TRUE

#Extract the column of the data frame which contains the species names
list_fish <- Lista_completa_mediterranean$Species

#Check that all the species in your lists are found in the repositories of fishbase

#Create an empty vector to store the species not found in FishBase
species_not_found_fish <- c()

###--- Loop through each species name to extract all the traits ---###
for (i  in 1:length(list_fish) ) {
  # Check if the species is in the FishBase dataset
  species_info_fish <- species(list_fish, server = "fishbase")
  if (nrow(species_info_fish) == 0) {
    # Species not found in FishBase, add it to the vector
    species_not_found_fish <- c(species_not_found_fish, list_fish)
  }
}

# Print the species not found in FishBase
print(species_not_found_fish)

which(!(unique(Lista_completa_mediterranean$Species) %in% unique(species_info_fish$Species)))
#Hexanchus vitulus data is missing, also known as Hexanchus nakamurai

###---load all the taxa data available ---###
fishbase_taxa <- load_taxa(server="fishbase")

#the function "load_taxa()" does not extract the "Phylum" information from Fishbase (for fish).
#as all fish are chordates add a column with this info manually so that we have no issues later when joining the data from fishbase and sealifebase
fishbase_taxa$Phylum <- "Chordata"

#with the function species() we are missing the information about: order, class and phylum
#To obtain this, merge the dataframe that contains all the data from fishbase with fish_info_Fg:
all_fish_info_1 <-merge(x = species_info_fish, y =fishbase_taxa, by = "Species", all.x = TRUE)

###--- Extract FAO information ---###
#this will extract the info for traits " Occurence status" (check from which area the info is coming)
fish_fao <- faoareas(list_fish, fields= c("AreaCode",  
                                          "StockCode",
                                          "Status",  #Occurrence status
                                          "FAO"))

#despite not specifying it in "fields" this function add a column names "SpecCode"
#this columns needs to be deleted so that it does not create duplicates later on the script.
fish_fao$SpecCode <- NULL
colnames(fish_fao)

#For this function, sometimes there is more than one data row per species, as the FAO info is given per area. 
#We need to filter the outputs ONLY for the area of interest
#first determine that the column is a factor
fish_fao$FAO <- as.factor(fish_fao$FAO)
str(fish_fao$FAO)
#explore the levels of the factor
levels(fish_fao$FAO)
#now filter
#in this case we are filtering for the Mediterranean sea
subset_fish_fao <- subset(fish_fao, FAO == "Mediterranean and Black Sea")

#merge it with species info and Fg from before
all_fish_info_2 <- merge(x = all_fish_info_1, y =subset_fish_fao, by = "Species", all = TRUE)

###--- Extract stock information ---###
fish_stock<-stocks(list_fish, field= c("Species",
                                       "StockDefs",
                                       "StockDefsGeneral",
                                       "IUCN_Code",
                                       "IUCN_ID",
                                       "IUCN_Assessment"))


#The information of IUCN status from fishbase/sealifebase for certain areas is really scarce. This is the case for the Mediterranean Sea
#I recommend saving  and exploring this dataframe (fish_stock) with ALL the information.
#Before doing the subset for your area, explore what information is available and if any info from another close area could be of use instead. 
#write.csv(inv_stock, "inv_IUCN.csv")

#this data has more than on data per species, as the Stock info is given per area. We need to filter the outputs only for the study area of interest.
#first determine that the column is a factor
fish_stock$StockDefsGeneral<-as.factor(fish_stock$StockDefsGeneral)
str(fish_stock$StockDefsGeneral)
#explore the levels of the factor
levels(fish_stock$StockDefsGeneral)
#now filter
#in this case i have filtered for the mediterrnanea sea
subset_fish_stock <- subset(fish_stock, StockDefsGeneral == "Mediterranean Sea")

#you can also filter by more than one level BUT in that case make sure that you obtain no repeated rows/species
#subset_fish_stock <- subset(fish_stock, StockDefsGeneral ==  "Northeast Atlantic and the Mediterranean Sea"|StockDefsGeneral == "Mediterranean Sea")
nrow(subset_fish_stock)==length(unique(subset_fish_stock$Species))
str(subset_fish_stock)

#merge the csv with species info, Fg and FAO with stock information
all_fish_info_3<-merge(x = all_fish_info_2, y =subset_fish_stock, by = "Species", all.x = TRUE)

#merge with original data to have your information of invasive status and to maintain all the rows.
traits_final <- merge(x = all_fish_info_3, y =Lista_completa_mediterranean, by = "Species", all = TRUE)

###--- Extract estimate traits and select the important ones ---###
fish_estimate <- estimate(list_fish)

fish_estimate <- fish_estimate[,c(1, 3, 5, 42, 43, 44)]

###--- merge with estimate traits to obtain the final trait database---### 
traits_final <- merge(x = traits_final, y =fish_estimate, by = "Species", all = TRUE)

#Check the duplicates and eliminate them
duplicados <- which(duplicated(traits_final$Species))

traits_final <- traits_final[-duplicados, ]

which(is.na(traits_final$SpecCode.x))
#Hexanchus vitulus traits are missing, also known as Hexanchus nakamurai



#Count the NA's in all the variables
(porcentaje_na <- sapply(traits_final, function(x) mean(is.na(x)) * 100)) 

#Extract variables with less than 10 % of NA's
(traits_use <- which(porcentaje_na < 10))

traits_final <- traits_final[,traits_use]

#Save de database
#write.csv(traits_final, "Datos/traits_final.csv")


#Count the NA's in all the variables of the invasive species (Only Informative)

invasoras_final <- subset(traits_final, traits_final$Invasive == "Yes" )

(porcentaje_na_inv <- sapply(invasoras_final, function(x) mean(is.na(x)) * 100))

(traits_use_inv <- which(porcentaje_na_inv < 10))

