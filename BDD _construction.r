###############################
# Analyse empirique des marches
###############################
# Ce document regrouppe tout le code utilisé pour le traitement des données disponibles en libre acces
# Les données sont issues des grandes agences statistiques et des ministères

##################
# Loading packages
##################
require(tidyverse)
require(rlang)
require(plm)
require(Formula)

##################################################################
# Organising and cleaning data, prepared as described in "Code.jl"
##################################################################
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
vin = vin %>% 
  drop_na(annee)
## Verification
View(vin)
## Saving results
write.csv(vin, file = "Donnees_ref/vin-p.csv")
## End first changes  

# Loading data
# usage=read.csv("C:/Users/jupiter/Documents/Blanc Arnaud/master MIASHS C2ES/M2/analyse empirique de marchï¿½/pesticides/info/usages_des_produits_autorises_v2_utf8.csv", sep = ";")
write.csv(usage, file = "Donnees_ref/usage_pa.csv")
# superficie=read.csv("C:/Users/jupiter/Documents/projet-aem-s/Donnees/superficie départementale par an de vignes.csv", sep = ";", skip = 7, header = T)
write.csv(superficie, file = "Donnees_ref/superficie_vignes.csv")
View(superficie)


#Construction de la table pesticides
library(tidyverse)
usage %>% as.numeric(X.produit..numero.AMM)

pesticide=left_join(bnvd, usage, by=c("amm"="X.produit..numero.AMM"))

## Loading data
usage = read.csv("Donnees_ref/usage_pa.csv")
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
bnvdvin.sh = bnvdvin.n[, c(1:5, 26:27)] %>% 
  group_by(annee, departement, amm, quantite_produit,
           conditionnement, dose.retenue.unite) %>%
  summarise(mean.dose = mean(dose.retenue))
View(bnvdvin.sh)
# Aggregation test
dim(bnvdvin.sh)
# Saving main file
write.csv(bnvdvin.sh, file = "Donnees_ref/pesticides.csv")

# Saving auxilary dataframe
bnvdvin.n = bnvdvin.n[, c(1:7, 17:19, 21:33)]
## Find a way to reduce data weight
## Saving
write.csv(bnvdvin.n, file = "Donnees_ref/bnvdvin.csv")

# Loading donnees pesticides
pesticides = read.csv("Donnees_ref/pesticides.csv")
View(pesticides)
names(pesticides)
pesticides %>% select(-X)->pesticides

# Loading data for vin
vin = read.csv("Donnees_ref/vin-p.csv", stringsAsFactors = F)
vin = lapply(vin, function(x) gsub(" ", "", x))
# Write results
write.csv(vin.s, "Donnees_ref/vin-p.csv")
# Reload table to avoid factors
vin = read.csv("Donnees_ref/vin-p.csv", stringsAsFactors = F)
# Creating reduced database
vin.red = vin %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(s_total = surface, 
         s_vin_simple = s_autres + s_vsig,
         q_blanc = q_autres_b + q_b + q_vsig_b,
         q_rouge = q_autres_r + q_r + q_vsig_rg + q_vsig_rs,
         q_total = qq_total) %>%
  select(s_total, s_vin_simple,
         q_blanc, q_rouge, q_total,
         n_dep, annee)
# Changing n_dep
vin.r = vin.red %>% 
  separate(n_dep, 
           c("number", "departement"), 
           2, extra = "merge")
# Writting database
write.csv(vin.r, "Donnees_ref/vin.csv")
# Clear workspace
list = ls()
rm(list)

# Read vin data
vin = read.csv("Donnees_ref/vin.csv", stringsAsFactors = F)
vin = vin %>% 
  mutate(departement = as.character(departement),
         number = as.character(number)) %>%
  mutate_if(is.factor, function(x) as.numeric(str_replace_all(as.character(x), "[[:space:]]", "")))
summary(vin)
# Save results
write.csv(vin.r, "Donnees_ref/vin_final.csv")


# Read data

# Joining data
require(tidyverse)
# Read data
vin = read.csv("Donnees_ref/vin_final.csv", stringsAsFactors = F)

# Construction de la base de données sur les prix du vin
# Données de 2000 à 2016
vin_blanc<-read.csv2("Donnees/cotation-vin-blanc2.csv", sep=";", skip=3, header=T)

################
# Arnaud section
################
#Construction de la base de données sur les prix du vin
#Données de 2000 à 2016
vin_blanc<-read.csv2("Donnees/cotation-vin-blanc.csv", sep=",", skip=3, header=T)

names(vin_blanc)
library(tidyverse)
vin_blanc %>% 
  select(Mois,semaine.dans.la.campagne,en.milliers.d.hl,en.euros.hl,X)->vin_blanc_sans_IG
names(vin_blanc_sans_IG)[5]<-"campagne"

#création d'une colonne année
vin_blanc_sans_IG %>% 
  separate(col=Mois, into = c("Mois","Année"))->vin_blanc_sans_IG

vin_blanc_sans_IG %>% select(Année, en.milliers.d.hl, en.euros.hl,campagne)->vin_blanc_sans_IG

vin_blanc_sans_IG %>% group_by(Année) %>% mutate(moyenne_hectolitre=mean(as.numeric(as.character(en.milliers.d.hl))))->vin_blanc_sans_IG
vin_blanc_sans_IG %>% 
  group_by(Année) %>% 
  mutate(moyenne_euros.hl=mean(as.numeric(as.character(en.euros.hl))))->vin_blanc_sans_IG

#Vin rouge
vin_rouge<-read.csv2("Donnees/cotation-vin-rouge2.csv", sep=",", skip=3, header = T)
names(vin_rouge)
vin_rouge %>% 
  select(Mois,semaine.dans.la.campagne,en.milliers.d.hl,en.euros.hl,X)->vin_rouge_sans_IG

names(vin_rouge_sans_IG)[5]<-"campagne"

vin_rouge_sans_IG %>% 
  separate(col = Mois,into = c("Mois","Année"))->vin_rouge_sans_IG
vin_rouge_sans_IG %>% 
  select(Année, en.milliers.d.hl, en.euros.hl, campagne)->vin_rouge_sans_IG
vin_rouge_sans_IG %>% 
  group_by(Année) %>%
  mutate(moyenne_hectolitre=mean(as.numeric(as.character(en.milliers.d.hl))))->vin_rouge_sans_IG
vin_rouge_sans_IG %>% 
  group_by(Année) %>% 
  mutate(moyenne_euros.litre=mean(as.numeric(as.character(en.euros.hl))))->vin_rouge_sans_IG


#Suite de la base de données années 2009 à 2019
vin_rouge_blanc<-read.csv2("Donnees/cotation-vin-rouge-blanc2.csv", sep=",", skip=3, header=T)
names(vin_rouge_blanc)
vin_rouge_blanc %>% 
  select(X,X.1,Vin.AOP,Vin.IGP,Vin.sans.IG.avec.mention.de.cépages,Vin.sans.IG.sans.mention.de.cépages)->cotation_vin_rouge
names(cotation_vin_rouge)[c(1,2)]<-c("Année","mois")
vin_rouge_blanc %>% 
  select(X,X.1,Vin.AOP.1,Vin.IGP.1,Vin.sans.IG.avec.mention.de.cépages.1,Vin.sans.IG.sans.mention.de.cépages.1)->cotation_vin_blanc
names(cotation_vin_blanc)[c(1,2)]<-c("Année","mois")
#cotation prix vin blanc

cotation_vin_blanc %>% 
  select(Vin.sans.IG.avec.mention.de.cépages.1, Vin.sans.IG.sans.mention.de.cépages.1) %>% 
  mutate(Vin.sans.IG.avec.cepage=as.numeric(as.character(Vin.sans.IG.avec.mention.de.cépages.1))) %>% 
  mutate(vin.sans.IG.sans.cepage=as.numeric(as.character(Vin.sans.IG.sans.mention.de.cépages.1))) %>% 
  select(Vin.sans.IG.avec.cepage, vin.sans.IG.sans.cepage)->cotation_blanc_sans_IG
rowMeans(cotation_blanc_sans_IG)->cotation_blanc
cbind(cotation_vin_blanc,cotation_blanc)->cotation_sans_IG
cotation_sans_IG %>% 
  select(Année, cotation_blanc) %>% 
  mutate(Année=as.character(Année)) %>% 
  group_by(Année) %>% 
  mutate(moyenne_euros.hl=mean(cotation_blanc))->cotation_vin_blanc_sans_IG
#cotation prix vin rouge

cotation_vin_rouge %>% 
  select(Vin.sans.IG.avec.mention.de.cépages, Vin.sans.IG.sans.mention.de.cépages) %>% 
  mutate(Vin.sans.IG.avec.cepage=as.numeric(as.character(Vin.sans.IG.avec.mention.de.cépages))) %>% 
  mutate(vin.sans.IG.sans.cepage=as.numeric(as.character(Vin.sans.IG.sans.mention.de.cépages))) %>%
  select(Vin.sans.IG.avec.cepage, vin.sans.IG.sans.cepage) %>% 
  rowMeans()->cotation_rouge
cbind(cotation_vin_rouge,cotation_rouge)->cotation_sans_IG_rouge
cotation_sans_IG_rouge %>% 
  select(Année,cotation_rouge) %>% 
  mutate(Année=as.character(Année)) %>% 
  group_by(Année) %>% 
  mutate(moyenne_euros.hl=mean(cotation_rouge))->cotation_vin_rouge_sans_IG
#Création de la base de données de prix


names(vin_rouge_sans_IG)[6]<-"moyenne_euros.hl"
vin_rouge_sans_IG %>% 
  select(Année, moyenne_euros.hl)->vin_rouge_sans_IG2
cotation_vin_rouge_sans_IG %>% 
  select(Année, moyenne_euros.hl)->cotation_vin_rouge_sans_IG2
full_join(vin_rouge_sans_IG2, cotation_vin_rouge_sans_IG2, by=c("Année","moyenne_euros.hl"))->cotation_vin_rouge_sans_IG_final



vin_blanc_sans_IG %>% 
  select(Année, moyenne_euros.hl)->vin_blanc_sans_IG2
cotation_vin_blanc_sans_IG %>% 
  select(Année, moyenne_euros.hl)->cotation_vin_blanc_sans_IG2
full_join(vin_blanc_sans_IG2, cotation_vin_blanc_sans_IG2, by=c("Année","moyenne_euros.hl"))->cotation_vin_blanc_sans_IG_final

#sélectionnons les dates importantes dans notre étude

cotation_vin_rouge_sans_IG_final %>% 
  filter(Année>=2009) %>% 
  group_by(Année) %>% 
  summarise(moyenne_euros.hl=mean(moyenne_euros.hl))->cotation_vin_rouge_sans_IG_final

cotation_vin_blanc_sans_IG_final %>% 
  filter(Année>=2009) %>% 
  group_by(Année) %>% 
  summarise(moyenne_euros.hl=mean(moyenne_euros.hl))->cotation_vin_blanc_sans_IG_final

#écriture des fichiers de cotations en csv
write.csv2(cotation_vin_rouge_final, file = "Donnees_ref/cotation_vin_rouge_final.csv")
write.csv2(cotation_vin_rouge_sans_IG_final, file = "Donnees_ref/cotation_vin_rouge_sans_IG_final.csv")
write.csv2(cotation_vin_blanc_final, file = "Donnees_ref/cotation_vin_blanc_final.csv")
write.csv2(cotation_vin_blanc_sans_IG_final, file = "Donnees_ref/cotation_vin_blanc_sans_IG_final.csv")

#Construction des prix déflatés
indice_prix<-read.csv2(file = "Donnees/indice_prix.csv", skip = 2, header = T)
indice_prix %>% 
  select(Periode, indice.prix.a.la.consommation) %>% 
  mutate(Année=as.character(Periode)) %>% 
  mutate(indice.prix.a.la.consommation=as.numeric(as.character(indice.prix.a.la.consommation))) %>% 
  select(indice.prix.a.la.consommation,Année)->indice_prix


#Création d'une table unique pour les vins 
left_join(cotation_vin_rouge_final,cotation_vin_rouge_sans_IG_final,by="Année")->cotation_vin
left_join(cotation_vin,cotation_vin_blanc_sans_IG_final, by="Année")->cotation_vin
left_join(cotation_vin, indice_prix, by="Année")->cotation_vin
cotation_vin %>% 
  mutate(prix_vin_rouge_sans_IG=(moyenne_euros.hl.y/indice.prix.a.la.consommation)*100) %>% 
  mutate(prix_vin_blanc_sans_IG=(moyenne_euros.hl.y.y/indice.prix.a.la.consommation)*100)->cotation_vin

#Construction de la base de données
rev2012<-read.csv2(file = "Donnees/revenu médian 2012.csv", sep = ";", skip = 5, header = T)
rev2013<-read.csv2(file = "Donnees/revenu médian 2013.csv", sep = ";", skip = 5, header = T)
rev2014<-read.csv2(file = "Donnees/revenu médian 2014.csv", sep=";", skip = 5, header = T)
rev2015<-read.csv2(file = "Donnees/revenu médian 2015.csv", sep = ";", skip = 5, header = T)
rev2016<-read.csv2(file = "Donnees/revenu médian 2016.csv", sep = ";", skip = 5, header = T)
rev2012 %>% 
  mutate(médiane=as.numeric(as.character(Q212))) %>% 
  mutate(annee=2012) %>% 
  select(médiane, annee, LIBGEO, CODGEO)->rev2012
rev2013 %>% 
  mutate(médiane=as.numeric(as.character(Q213))) %>% 
  mutate(annee=2013) %>% 
  select(médiane, annee, LIBGEO, CODGEO)->rev2013
rev2014 %>% 
  mutate(médiane=as.numeric(as.character(Q214))) %>% 
  mutate(annee=2014) %>% 
  select(médiane, annee, LIBGEO, CODGEO)->rev2014
rev2015 %>% 
  mutate(médiane=as.numeric(as.character(Q215))) %>% 
  mutate(annee=2015) %>% 
  select(médiane, annee, LIBGEO, CODGEO)->rev2015
rev2016 %>% 
  mutate(médiane=as.numeric(as.character(Q216))) %>% 
  mutate(annee=2016) %>% 
  select(médiane, annee, LIBGEO, CODGEO)->rev2016
rbind(rev2012,rev2013)->revenu1
rbind(revenu1,rev2014)->revenu2
rbind(revenu2,rev2015)->revenu3
rbind(revenu3,rev2016)->revenu_median
revenu_median %>% 
  mutate(dep=LIBGEO) %>% 
  mutate(ndep=CODGEO) %>% 
  select(médiane, annee, dep, ndep)->revenu_median
left_join(revenu_median, BD, by=c("ndep"="ndep", "annee"="annee"))->BD3
BD3 %>% 
  select(médiane,annee,ndep,dep.y,s_nig,s_total,q_blanc,q_rouge,q_total,p_blanc,p_rouge,qk_prod,ql_prod)->BD3
BD3 %>% 
  filter(!is.na(s_nig))->BD3
BD3 %>% 
  filter(dep.y!="CORSE-DU-SUD" & dep.y!="HAUTE-CORSE")->BD3
write.csv2(BD3, file = "Donnees_ref/base de données.csv")

#Reconstruction de la base de vin
vin = read.csv("Donnees_ref/vin-pS.csv", stringsAsFactors = F)
names(vin)
summary(vin)
names(vin)
require(tidyverse)
# Creating reduced database
vin.red = vin %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(s_total = surface, 
         s_vin_simple = s_autres + s_vsig,
         q_blanc = q_autres_b + q_b + q_vsig_b,
         q_rouge = q_autres_r + q_r + q_vsig_rg + q_vsig_rs,
         q_total = qq_total) %>%
  select(s_total, s_vin_simple,
         q_blanc, q_rouge, q_total,
         n_dep, annee) %>% 
  filter(annee>=2012)->vin3
vin4<-vin3 %>% 
  separate(n_dep, 
           c("number", "departement"), 
           2, extra = "merge") %>% 
  mutate(departement = as.character(departement),
         number = as.character(number)) %>%
  mutate_if(is.factor, function(x) as.numeric(str_replace_all(as.character(x), "[[:space:]]", "")))
summary(vin4)
vin5<-filter(vin4,departement!="CORSESUD" & departement!="CORSE(HTE)")
write.csv2(vin5, file = "Donnees_ref/vinfinal.csv")
pesticides = read.csv("Donnees_ref/pesticides.csv", stringsAsFactors = F)
pesticides %>% 
  filter(departement!="CORSE-DU-SUD" & departement!="HAUTE-CORSE")->pesticides2
pesticides2 %>% 
  select(annee, departement, quantite_produit, conditionnement)->pesticides2
pesticides2 %>% 
  filter(annee>=2012)->pesticides2
pesticides2 %>% 
  filter(departement!="MARTINIQUE" & departement!="LA REUNION" & departement!="GUYANE" & departement!="GUADELOUPE")->pesticides2
write.csv2(pesticides2, file = "Donnees_ref/pest.csv")
#Création des numéros par département
reg = as.character(c("01", "02", "03", "04", "06", "07", "08", "09", 10, 11, 12, 67, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 79, 24, 25, 26, 91, 27, 28, 29, 30, 32, 33, 68, 31, 43, 52, 70, 74, 87, "05", 65, 92, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 57, 58, 59, 60, 61, 75, 62, 63, 64, 66, 69, 71, 72, 73, 77, 76, 93, 80, 81, 82, 90, 95, 94, 83, 84, 85, 86, 88, 89, 78))
# Assignement
ndep = data.frame(
  departement = sort(unique(as.character(pesticides2$departement))),
  number = reg, stringsAsFactors = F)
# Concatenate
vin.x = left_join(vin5, ndep, by = "number")
vin.x[is.na(vin.x$departement),] # verification of the NA presence
head(vin.x)
vin.x %>% 
  select(s_total, s_vin_simple, q_blanc, q_rouge, q_total, number, annee, departement.y)->vin.x
pesticides.x = left_join(pesticides2, ndep, by = "departement")
pesticides.x[is.na(pesticides.x$number),] # verification of the NA presence
head(pesticides2)
pesticides.x %>% 
  select(annee, departement, quantite_produit, conditionnement, number)->pesticides.x
# Concatenate (what for?)
vxp = left_join(vin.x, pesticides.x, by = c("number", "annee"))
# dim(vxp) # There is no sense in this
# Removing NAs
vin.x = na.omit(vin.x)
# Correlation analysis
cor(vin.x[,5:6])
# Combining data
names(vxp)
vxp = vxp[, c(3:7, 11:15)]
summary(vxp)
# Removing missing data
vxp.c = na.omit(vxp)
summary(vxp)
unique(vxp$conditionnement)
class(vxp$quantite_produit)
vxp.c %>% 
  select(-departement.y)->vinfinal
write.csv(vinfinal, file = "Donnees_ref/vin_final.csv")
pvpx = vinfinal %>% 
  mutate(K = as.numeric(conditionnement == "K"), 
         L = as.numeric(conditionnement == "L"),
         cond = as.numeric(conditionnement)) %>%
  mutate(qk_prod = quantite_produit*K,
         ql_prod = quantite_produit*L) %>%
  group_by(annee, number, departement) %>%
  summarise(s_vin_simple = mean(s_vin_simple), s_total = mean(s_total),
            q_blanc = mean(q_blanc), q_rouge = mean(q_rouge),
            q_total = mean(q_total), 
            qk_prod = sum(qk_prod), ql_prod = sum(ql_prod))
# Ajout des revenus médian à la base de données
revenu_median %>% 
  filter(dep!="Corse-du-Sud" & dep!="Haute-Corse")->revenu_median
revenu_median %>% 
  mutate(ndep=as.character(ndep)) %>% 
  mutate(dep=as.character(dep))->revenu_median
left_join(revenu_median, pvpx, by=c("ndep"="number", "annee"="annee"))->BD3
BD3 %>% 
  select(médiane,annee,ndep,departement,s_vin_simple,s_total,q_blanc,q_rouge,q_total,qk_prod,ql_prod)->BD3
BD3 %>% 
  filter(!is.na(s_vin_simple))->BD3
write.csv2(BD3, file = "Donnees_ref/BD_final.csv")
prix = read.csv("./Donnees_ref/prix_revenu.csv")
left_join(BD3, prix, by=c("annee"="Année"))->BD4
BD4 %>% 
  select(-X, -revenu.déflaté)->BD4
indice_prix %>% 
  mutate(Année=as.numeric(Année))->indice_prix
BD4 %>% 
  left_join(indice_prix, by=c("annee"="Année"))->BD5
BD5 %>% 
  mutate(revenu.déflaté=(médiane/indice.prix.a.la.consommation)*100) %>% 
  select(-indice.prix.a.la.consommation, -médiane)->BD5
BD5 %>% 
  filter(s_vin_simple!=0 & q_blanc+q_rouge!=0)->BD5
Bestimation = BD5 %>%
  # pvpi = pvpd %>% 
  arrange(ndep) %>%
  mutate(s = log(s_vin_simple), 
         qi = log(q_blanc + q_rouge), 
         p = log((prix_vin_blanc_sans_IG + prix_vin_rouge_sans_IG)/2),
         r = log(revenu.déflaté),
         qki = log(qk_prod + ql_prod),
         annee = annee)
Bestimation %>% 
  select(s, qi, p, qki, r, annee, departement, ndep)->BD_estimation

# Loading data
data = read.csv("./Donnees/Base-de-donnees-indice-prix.csv")
# names(data)
# Arrange
dn = data %>%
  filter(s_vin_simple != 0 & 
           (q_rouge + q_blanc) != 0 &
           (qk_prod + ql_prod) != 0 &
           IP != 0) %>%
  na.omit() %>%
  group_by(ndep) %>%
  count() %>% 
  filter(n == 5) # %>%
# select(ndep)
datax = data %>% 
  filter(ndep %in% dn$ndep) 
datay = datax %>% 
  filter(annee == 2012) %>%
  mutate(refqki = qk_prod + ql_prod) %>% 
  select(ndep, refqki) 
datax = left_join(datax, datay)
datax = datax %>%
  mutate(IQK = (qk_prod + ql_prod)/refqki)
datai = datax %>%
  arrange(ndep) %>%
  mutate(si = log(s_vin_simple + 0.001), 
         qi = log(q_blanc + q_rouge + 0.001), 
         ipi = log(IP),
         ri = log(revenu.deflate),
         iki = log(IQK),
         t = as.integer(as.factor(annee)),
         year = annee) %>%
  dplyr::select(year, ndep, qi, ipi, si, ri, iki, t)
data = pdata.frame(datai, index = c("ndep", "year"),
                   drop.index = F)
# Prewiev
head(data)