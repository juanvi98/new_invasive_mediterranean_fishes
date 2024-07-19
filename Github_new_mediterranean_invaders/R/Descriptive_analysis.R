#DESCRIPTIVE ANALYSIS

###---Set up libraries---###
library(psych)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(tidyr)
library(knitr)
library(dplyr)


fishes_final <- read.csv("./Datos/fishes_final.csv")

fishes_final <- fishes_final[, -1]


#Transform all the binary variables to factors, except invasive one
fishes_final[, -2] <- fishes_final[, -2] %>%
  mutate_if(is.integer, as.factor)



###--- Univariate descriptive analysis ---###
tabla_num <- describe(fishes_final[, c(3,4,5,7,8)])

tabla_num <- tabla_num[, c(3,4,5,11,12,13)]

tabla_num <- round(tabla_num, 3)

kable(tabla_num, format= "latex")

#--Numerical variables--#

#Histogram PD50
ggplot(fishes_final, aes(x = PD50)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "white") +
  labs(title = "Histograma de numeric_var", x = "numeric_var", y = "Frecuencia") +
  theme_minimal()

#Pointchart PD50
ggplot(fishes_final, aes(x = PD50, y = 1)) +
  geom_point(position = position_jitter(width = 0, height = 0.1), color = "blue") +
  labs(title = "Diagrama de Puntos de numeric_var", x = "Valor", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#PD50 density
ggplot(fishes_final, aes(x = PD50)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Gráfico de Densidad", x = "PD50", y = "Densidad") +
  theme_minimal()

#Mean temperature density
ggplot(fishes_final, aes(x = TempPrefMean)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Gráfico de Densidad de la Temperatura", x = "Temperatura preferente media", y = "Densidad") +
  theme_minimal()

#Fises with a preferred mean temperature over 25
temp_25 <- which(fishes_final$TempPrefMean > 25)

#Invasive ones
sum(fishes_final$Invasive[temp_25])

#Max lenght density
ggplot(fishes_final, aes(x = Max_length)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Densidad Longitud Máxima", x = "Longitud Máxima", y = "Densidad") +
  theme_minimal()

#Trophic level density
ggplot(fishes_final, aes(x = Troph)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Densidad temperatura", x = "Temperatura media preferente", y = "Densidad") +
  theme_minimal()

#Vulnerability density
ggplot(fishes_final, aes(x = Vulnerability)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Densidad temperatura", x = "Temperatura media preferente", y = "Densidad") +
  theme_minimal()

#Boxplotss
boxplot(fishes_final$Vulnerability)
boxplot(fishes_final$Max_length)
boxplot(fishes_final$PD50)
boxplot(fishes_final$Troph)
boxplot(fishes_final$TempPrefMean)

#Transform to a long format
fishes_num_1 <- fishes_final[, c(2,7)]
fishes_num_2 <- fishes_final[, c(4,6)]


num_long_1 <- fishes_num_1 %>%
  pivot_longer(cols = everything(), names_to = "Grupo", values_to = "Valor")

num_long_2 <- fishes_num_2 %>%
  pivot_longer(cols = everything(), names_to = "Grupo", values_to = "Valor")



#Max Lenght
ggplot(fishes_final, aes(y = Max_length)) +
  geom_boxplot() +
  labs(title = "Boxplot Longitud Máxima",
       x = "Longitud Máxima",
       y = "Valores") +
  theme_minimal()

boxplot_length <- boxplot.stats(fishes_final$Max_length)

#outliers Max_lenght
out_length <- which(fishes_final$Max_length > 180)

#Invasives between outliers
sum(fishes_final$Invasive[out_length])


#Combined boxplots
#Temperature and vulnerability
ggplot(num_long_1, aes(x = Grupo, y = Valor, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Comparación de Diferentes Grupos",
       x = "Grupos",
       y = "Valores") +
  theme_minimal()

#Trophic level and PD50
ggplot(num_long_2, aes(x = Grupo, y = Valor, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Comparación de Diferentes Grupos",
       x = "Grupos",
       y = "Valores") +
  theme_minimal()

#--- Correlation between numerical variables ---#

matcor <- cor(na.omit(fishes_final[, c(3,4,5,7,8)]))
corrplot(matcor, method = "ellipse", type = "upper", diag = T, tl.pos = "n")
corrplot(matcor, method = "number", type = "lower", diag = F,
         add = T, tl.pos = "l")

ggcorrplot(matcor, hc.order = F, type = "upper", lab = "TRUE")

#--- Categorical variables ---#

#Barplots
barplot(table(fishes_final$Invasive))
barplot(table(fishes_final$Emblematic))
barplot(table(fishes_final$trad_fishing))
barplot(table(fishes_final$trawl_fishing))
barplot(table(fishes_final$pelag_fishing))
barplot(table(fishes_final$Body_shape))
barplot(table(fishes_final$Demersal))
barplot(table(fishes_final$Pelagic))
barplot(table(fishes_final$Reef_associated))

#Transform to a long format
cat_long <- fishes_final[, -c(1,3,4,5,7,8)]
cat_long$Invasive <- as.factor(cat_long$Invasive)

cat_long <- cat_long %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")


#Barplot of all categorical variables
ggplot(cat_long, aes(x = Valor, fill = Variable)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Barplot de Frecuencias de Varias Variables",
       x = "Valor",
       y = "Frecuencia") +
  theme_minimal()

#Transform to a long format
cat_long <- fishes_final[, -c(1,3,4,5,6,7,8)]
cat_long$Invasive <- as.factor(cat_long$Invasive)

colnames <- colnames(cat_long)

cat_long <- pivot_longer(cat_long, cols = all_of(colnames), names_to = "Nivel", values_to = "Valor")


#Barplots divided by invasive nature
ggplot(cat_long, aes(x = Valor, fill = )) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Nivel, scales = "free") +
  labs(title = "Barplots de Variables Categóricas Divididas por Grupo",
       x = "Categoría",
       y = "Frecuencia") +
  theme_minimal()

#Reef_associated proportions
Reef_table <- table(fishes_final$Reef_associated, fishes_final$Invasive)

Reef_proportions <- prop.table(Reef_table)

#Trad_fishing proportions
Trad_table <- table(fishes_final$trad_fishing, fishes_final$Invasive)

Trad_proportions <- prop.table(Trad_table)

#Demersal proportions
Demer_table <- table(fishes_final$Demersal, fishes_final$Invasive)

Demer_proportions <- prop.table(Demer_table)

#Pelagic proportions
Pelag_table <- table(fishes_final$Pelagic, fishes_final$Invasive)

Pelagic_proportions <- prop.table(Pelagic_table)

#Eliminar Emblematic, solo 2 especies 

ggplot(fishes_imp_final, aes(x = trad_fishing, fill = as.factor(Invasive))) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot de la Variable 'categoria'",
       x = "Categoría",
       y = "Frecuencia") +
  theme_minimal()

