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