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
names(pesticides)

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
# Clear workspace
list = ls()
rm(list)

# Read vin data
vin = read.csv("Donnees_ref/vin.csv")
vin = vin %>% 
    mutate(departement = as.character(departement),
        number = as.character(number)) %>%
    mutate_if(is.factor, function(x) as.numeric(str_replace_all(as.character(x), "[[:space:]]", "")))
summary(vin)
# Save results
write.csv(vin.r, "Donnees_ref/vin_final.csv")


# Read data
vin = read.csv("Donnees_ref/vin_final.csv", stringsAsFactors = F)
pesticides = read.csv("Donnees_ref/pesticides.csv", stringsAsFactors = F)
# Department verification
dep = data.frame(v = unique(vin$departement), 
    p = append(unique(as.character(pesticides$departement)), rep(NA, 29)))
dep2 = data.frame(v = append(unique(vin$number), rep(NA, 4)), 
    p = unique(as.numeric(pesticides$departement)))
View(dep)
View(dep2)
# Department correction
# Ndep ordered by depname
reg = as.character(c("01", "02", "03", "04", "06", "07", "08", "09", 10, 11, 12, 67, 13, 14, 15, 16, 17, 18, 19, "2A", 21, 22, 23, 79, 24, 25, 26, 91, 27, 28, 29, 30, 32, 33, 971, 973, "2B", 31, 43, 52, "05", 70, 74, 65, 87, 68, 92, 34, 35, 36, 37, 38, 39, 974, 40, 42, 44, 45, 41, 46, 47, 48, 49, 50, 51, 972, 53, 54, 55, 56, 57, 58, 59, 60, 61, 75, 62, 63, 64, 66, 69, 71, 72, 73, 77, 76, 93, 80, 81, 82, 90, 94, 95, 85, 83, 84, 86, 88, 89, 78))
# Assignement
ndep = data.frame(
    departement = sort(unique(as.character(pesticides$departement))),
    number = reg, stringsAsFactors = F)
# Concatenate
vin.x = left_join(vin[,-4], ndep, by = "number")
vin.x[is.na(vin.x$departement),] # verification of the NA presence
pesticides.x = left_join(pesticides, ndep, by = "departement")
pesticides.x[is.na(pesticides.x$number),] # verification of the NA presence
# Change to numeric vin surface and quantities
vin.x = vin.x %>% 
    mutate(
        qq_total = as.numeric(gsub("[[:space:]]", "", qq_total)),
        surface = as.numeric(gsub("[[:space:]]", "", surface))
    )
# Save
write.csv(vin.x, file = "Donnees_ref/vin_final.csv")
write.csv(pesticides.x, file = "Donnees_ref/pesticides.csv")

# Joining data
require(tidyverse)
# Read data
vin = read.csv("Donnees_ref/vin_final.csv", stringsAsFactors = F)
pesticides = read.csv("Donnees_ref/pesticides.csv", stringsAsFactors = F)
# Concatenate (what for?)
vxp = left_join(vin, pesticides, by = c("number", "annee"))
# dim(vxp) # There is no sense in this
# Removing NAs
vin = na.omit(vin)
# Correlation analysis
cor(vin[,6:7])
# Combining data
names(vxp)
vxp = vxp[, c(4:7, 12:14)]
summary(vxp)
# Removing missing data
vxp.c = na.omit(vxp)
summary(vxp)
unique(vxp$conditionnement)
class(vxp$quantite_produit)
# Problem !!!!!

# Plots
png(filename="Graphiques/SurfaceVinBox.png", width = 600, height = 600)
vin %>% 
    ggplot(aes(y = surface, color = as.factor(annee))) + 
    geom_boxplot() +
    xlab("Annee") +
    ylab("Surface des vignes")
dev.off()

vin %>%
    ggplot(aes(y = surface, x = annee)) +
    geom_smooth() + 
    geom_point()

png(filename="Graphiques/QVinBox.png", width = 600, height = 600)
vin %>% 
    ggplot(aes(y = qq_total, color = as.factor(annee))) +
    geom_boxplot() +
    xlab("Annee") +
    ylab("Quantité de vin produit")
dev.off()

png(filename="Graphiques/QSurfacePoint.png", width = 600, height = 600)
vin %>% 
    ggplot(aes(y = qq_total, x = surface)) +
    geom_point() +
    geom_smooth() +
    xlab("Surface des vignes") +
    ylab("Quantité de vin produit")
dev.off()