# Analyse empirique des marches
# Ce document regrouppe tout le code utilisé pour le traitement des données disponibles en libre acces
## Les données sont issus par des grands cabinéts statistiques

# Loading packages
require(tidyverse)

# Organising and cleaning data, prepared as described in "Code.jl"

## Loading data
bnvd = read.csv("Donnees_ref/bnvd-vp.csv")
## Caract. data
View(bnvd) # read suc.
## Nothing to change

## Loading data
vin = read.csv("Donnees_ref/vin-p.csv")
## Caract. data
dim(vin) 
## Removing NA's
vin = vin %>% drop_na(annee)
## Verification
View(vin)
## Saving results
write.csv(vin, file = "Donnees_ref/vin-p.csv")
## End first changes  
vin.p = read.csv("Donnees_ref/vin-p.csv")
View(vin.p)

##loading data
# usage=read.csv("C:/Users/jupiter/Documents/Blanc Arnaud/master MIASHS C2ES/M2/analyse empirique de march�/pesticides/info/usages_des_produits_autorises_v2_utf8.csv", sep = ";")
write.csv(usage, file = "Donnees_ref/usage_pa.csv")
# superficie=read.csv("C:/Users/jupiter/Documents/projet-aem-s/Donnees/superficie d�partementale par an de vignes.csv", sep = ";", skip = 7, header = T)
write.csv(superficie, file = "Donnees_ref/superficie_vignes.csv")
View(superficie)

## Loading packages
require(tidyverse)
## Loading data
usage = read.csv("Donnees_ref/usage_pa.csv")
View(usage)
dim(usage) # testing line
## identifiant.usage
usage.v = usage %>% 
    filter(str_detect(identifiant.usage, "Vigne"))
dim(usage.v) # succesfull filtering
## Preparation for concat
names(usage.v)[3]
class(usage.v$X.produit..numero.AMM)
usage.v = usage.v %>% 
    mutate(amm = as.character(X.produit..numero.AMM))
head(usage.v$amm)
## Loading second df to merge
bnvd = read.csv("Donnees_ref/bnvd-vp.csv")
bnvd = bnvd %>% 
    mutate(amm = as.character(amm))
View(bnvd) # verification
head(bnvd$amm)
## Merge
dim(bnvd) # ref
dim(usage.v) # ref
bnvdvin = left_join(bnvd, usage.v, by = "amm")
View(bnvdvin) # verification
## Clearing
bnvdvin.n = bnvdvin[is.na(bnvdvin$X.produit..numero.AMM) == F,]
View(bnvdvin.n) # verif
names(bnvdvin.n)
class(bnvdvin.n[,26])
# Group data and create final dataframe for estimation
bnvdvin.sh = bnvdvin.n[, c(1:5, 25:26)] %>% 
    group_by(annee, departement, amm, quantite_produit,
        conditionnement, dose.retenue.unite) %>%
    summarise(mean.dose = mean(dose.retenue))
View(bnvdvin.sh)
# Aggregation test
dim(bnvdvin.sh)
# Saving main file
write.csv(bnvdvin.sh, file = "Donnees_ref/pesticides.csv")

# Saving auxilary dataframe
bnvdvin.n = bnvdvin.n[, c(1:7, 16:18, 20:32)]
## Find a way to reduce data weight
## Saving
write.csv(bnvdvin.n, file = "Donnees_ref/bnvdvin.csv")

# Loading donnees pesticides
pesticides = read.csv("Donnees_ref/pesticides.csv")
View(pesticides)

# Loading data for vin
vin = read.csv("Donnees_ref/vin-p.csv")
View(vin)
names(vin)
# Creating reduced database
# Changing n_dep
vin.r = vin %>% 
    select(annee, n_dep, surface, qq_total) %>%
    separate(n_dep, 
        c("number", "departement"), 
        " ", extra = "merge")
# Writting database
write.csv(vin.r, "Donnees_ref/vin.csv")

# Read vin data
vin = read.csv("Donnees_ref/vin.csv")