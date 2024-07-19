#CORRELATION

###---Set up libraries---###
library(corrplot)
library(dplyr)
library(data.table)
require(data.table)

#Load the database 
traits_final <- read.csv("./Datos/traits_final.csv")
traits_final <- traits_final[,-1]

#Remove the traits that are not useful (Species, name, code, etc.)

colnames(traits_final)

#Remove : Species 1, SpecCode.x 2, Genus.x 3, SpeciesRefNo 4, Author 5, FBname 6, picpref 7, famcode 8, gencode 9, source 11, TaxIssue 12,AirBreathing 17,Depth range ref 20, Ltypemax 23, Maxlengthref 24, importanceref 25,Pricereliability 28, gamefish 42,electrogenic 44, google image 45, comments 46, entered 49,date entered 50, 51, 52,53,54,55,56,57,58,62,63, FAO 65



traits_final <- traits_final[,-c(1:8, 10, 11, 16, 19, 20 , 35, 37, 38, 39, 42:49, 52:56)]

#Change the necessary variables to factor

traits_final[] <- lapply(traits_final, function(x) if(is.character(x)) as.factor(x) else x)

colnames(traits_final)

#Also the binary ones
traits_final[, c(2:4, 9:18, 24)] <- lapply(traits_final[, c(2:4, 9:18, 24)], as.factor)

#Divide the dataset in factors and numeric
variables_factor <- select_if(traits_final, is.factor)
variables_num <- select_if(traits_final, is.numeric)

#Correlation analisys in categorical variables 

## Step1: check the correlations between cathegorical traits and invasive status
ind<-combn(colnames(variables_factor),2)
listres<-list()
for (i in 1:NCOL(ind)){
  tt<-chisq.test(variables_factor[,ind[1,i]],variables_factor[,ind[2,i]])
  dfres<-data.frame(tr1=ind[1,i],tr2=ind[2,i],chi=tt$statistic,
                    pval=tt$p.value)
  listres[[i]]<-dfres
}

#p value < 0.05 = correlation
listres<-rbindlist(listres)
listres<-listres[pval>0.05 | tr1=="Invasive" | tr2=="Invasive"]
listres[pval<0.06 & tr2=="Invasive"]
#Traits correlated with invasive variable (The variables useful for our analisys)

#Correlation analisys in nummerical variables

mat_cor <- cor(variables_num, use = "complete.obs")

#Graphic result of correlation between numerical variables
corrplot(mat_cor)


#T student test for correlation between numerical variables and invasive status
resultados_t_test <- lapply(traits_final[, sapply(traits_final, is.numeric)], function(var) {
  t_test <- t.test(var ~ traits_final$Invasive)
  return(c(Variable = names(var), p_valor = t_test$p.value))
})

#Results as a data frame
resultados_t_test_df <- do.call(rbind, resultados_t_test)
print(resultados_t_test_df)
resultados_t_test < 0.05

#Include all Traits because PD50 p-value is not that big and the remaining numerical traits are all highly correlated to the invasive status


