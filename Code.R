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


#Construction de la table pesticides
library(tidyverse)
usage %>% as.numeric(X.produit..numero.AMM)

pesticide=left_join(bnvd, usage, by=c("amm"="X.produit..numero.AMM"))




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
vin.s = lapply(vin, function(x) gsub(" ", "", x))
write.csv(vin.s, "Donnees_ref/vin-pS.csv")
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
write.csv(ndep, "Donnees_ref/ndep.csv")
ndep = read.csv("Donnees_ref/ndep.csv")
# Concatenate
vin.x = left_join(vin, ndep, by = "number")
vin.x[is.na(vin.x$departement),] # verification of the NA presence
head(vin.x)
write.csv(vin.x[,-c(1,10)], "Donnees_ref/vin_final.csv")
pesticides.x = left_join(pesticides, ndep, by = "departement")
pesticides.x[is.na(pesticides.x$number),] # verification of the NA presence
head(pesticides)
pesticides.x = pesticides.x[,-c(1:2, 11)]
write.csv(pesticides.x, "Donnees_ref/pesticides_final.csv")

# Joining data
require(tidyverse)
# Read data
vin = read.csv("Donnees_ref/vin_final.csv", stringsAsFactors = F)
pesticides = read.csv("Donnees_ref/pesticides_final.csv", stringsAsFactors = F)
# Concatenate (what for?)
vxp = left_join(vin, pesticides, by = c("number", "annee"))
# dim(vxp) # There is no sense in this
# Removing NAs
vin = na.omit(vin)
# Correlation analysis
cor(vin[,5:6])
# Combining data
names(vxp)
vxp = vxp[, c(3:7, 11:15)]
summary(vxp)
# Removing missing data
vxp.c = na.omit(vxp)
summary(vxp)
unique(vxp$conditionnement)
class(vxp$quantite_produit)
# Problem !!!!!
<<<<<<< HEAD
>>>>>>> b218823e9f4e9eb4d25e1e3b715561b5567e7f04
=======

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
<<<<<<< HEAD
>>>>>>> 980b4e792e80f4cb0faf3075959e497082a626ea
=======

png(filename="Graphiques/QvinQpest.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(qq_total = mean(qq_total), quantite_produit = mean(quantite_produit)) %>%
    ggplot(aes(y = qq_total, x = as.numeric(quantite_produit))) +
    geom_point() +
    xlab("Quantité des pésticides") +
    ylab("Quantité de vin produit")
dev.off()

png(filename="Graphiques/SvinQpest.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(surface = mean(surface), quantite_produit = mean(quantite_produit)) %>%
    ggplot(aes(y = surface, x = as.numeric(quantite_produit))) +
    geom_point() +
    xlab("Surface des vignes") +
    ylab("Quantité de vin produit")
dev.off()

png(filename="Graphiques/QvinQpestAnnee.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(qq_total = mean(qq_total), quantite_produit = mean(quantite_produit)) %>%
    ggplot(aes(y = qq_total, x = as.numeric(quantite_produit), col = as.factor(annee))) +
    geom_point() +
    geom_smooth(method = "lm", se = F) + 
    xlab("Quantité des pésticides") +
    ylab("Quantité de vin produit")
dev.off()

png(filename="Graphiques/QvinQpestRegion.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(qq_total = mean(qq_total), quantite_produit = mean(quantite_produit)) %>%
    ggplot(aes(y = qq_total, x = as.numeric(quantite_produit), col = as.factor(number))) +
    geom_point() +
    geom_smooth(method = "lm", se = F) + 
    xlab("Quantité des pésticides") +
    ylab("Quantité de vin produit")
dev.off()

png(filename="Graphiques/SvinQpestAnnee.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(surface = mean(surface), quantite_produit = mean(quantite_produit)) %>%
    ggplot(aes(y = surface, x = as.numeric(quantite_produit), col = as.factor(annee))) +
    geom_point() +
    geom_smooth(method = "lm", se = F) + 
    xlab("Quantité des pésticides") +
    ylab("Surface des vignes")
dev.off()

png(filename="Graphiques/SvinQpestRegion.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(surface = mean(surface), quantite_produit = mean(quantite_produit)) %>%
    ggplot(aes(y = surface, x = as.numeric(quantite_produit), col = as.factor(number))) +
    geom_point() +
    geom_smooth(method = "lm", se = F) + 
    xlab("Quantité des pésticides") +
    ylab("Surface des vignes")
dev.off()

png(filename="Graphiques/QvinQpest2.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(qq_total = mean(qq_total), quantite_produit = sum(quantite_produit)) %>%
    ggplot(aes(y = qq_total, x = as.numeric(quantite_produit))) +
    geom_point() +
    xlab("Quantité des pésticides") +
    ylab("Quantité de vin produit")
dev.off()

png(filename="Graphiques/SvinQpest2.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(surface = mean(surface), quantite_produit = sum(quantite_produit)) %>%
    ggplot(aes(y = surface, x = as.numeric(quantite_produit))) +
    geom_point() +
    xlab("Surface des vignes") +
    ylab("Quantité de vin produit")
dev.off()

png(filename="Graphiques/QvinQpestAnnee2.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(qq_total = mean(qq_total), quantite_produit = sum(quantite_produit)) %>%
    ggplot(aes(y = qq_total, x = as.numeric(quantite_produit), col = as.factor(annee))) +
    geom_point() +
    geom_smooth(method = "lm", se = F) + 
    xlab("Quantité des pésticides") +
    ylab("Quantité de vin produit")
dev.off()

png(filename="Graphiques/QvinQpestRegion2.png", width = 600, height = 600)
vxp %>% filter(quantite_produit < 1000000) %>% 
    group_by(annee, number) %>%
    summarise(qq_total = mean(qq_total), quantite_produit = sum(quantite_produit)) %>%
    ggplot(aes(y = qq_total, x = as.numeric(quantite_produit), col = as.factor(number))) +
    geom_point() +
    geom_smooth(method = "lm", se = F) + 
    xlab("Quantité des pésticides") +
    ylab("Quantité de vin produit")
dev.off()

dim(vin) # 869*8
dim(pesticides) # 134025*10
dim(vxp) # 114730*16
>>>>>>> c184c3b37128495fb06f51a1e862196ccc299a0e

#Construction de la base de donn�es sur les prix du vin
#Donn�es de 2000 � 2016
vin_blanc<-read.csv2("Donnees/cotation-vin-blanc.csv", sep=",", skip=3, header=T)
names(vin_blanc)
library(tidyverse)
vin_blanc %>% 
  select(Mois,semaine.dans.la.campagne,en.milliers.d.hl,en.euros.hl,X)->vin_blanc_sans_IG
vin_blanc %>% 
  select(Mois, semaine.dans.la.campagne, en.milliers.d.hl.1,en.euros.hl.1,X)->vin_blanc_IGP
names(vin_blanc_sans_IG)[5]<-"campagne"
vin_rouge<-read.csv2("Donnees/cotation-vin-rouge.csv", sep=",", skip=3, header = T)
names(vin_rouge)
vin_rouge %>% 
  select(Mois..,semaine.dans.la.campagne,en.milliers.d.hl,en.euros.hl,X)->vin_rouge_sans_IG
vin_rouge %>% 
  select(Mois..,semaine.dans.la.campagne,en.milliers.d.hl.1,en.euros.hl.1,X)->vin_rouge_IGP
#Suite de la base de donn�es ann�es 2009 � 2019
vin_rouge_blanc<-read.csv2("Donnees/cotation-vin-rouge-blanc .csv", sep=",", skip=3, header=T)
names(vin_rouge_blanc)
vin_rouge_blanc %>% 
  select(X,X.1,Vin.AOP,Vin.IGP,Vin.sans.IG.avec.mention.de.c�pages,Vin.sans.IG.sans.mention.de.c�pages)->cotation_vin_rouge
vin_rouge_blanc %>% 
  select(X,X.1,Vin.AOP.1,Vin.IGP.1,Vin.sans.IG.avec.mention.de.c�pages.1,Vin.sans.IG.sans.mention.de.c�pages.1)->cotation_vin_blanc


list = ls()
rm(list)
vin = read.csv("./Donnees_ref/vin_final.csv")
pesticides = read.csv("./Donnees_ref/pesticides_final.csv")

require(arsenal)
t1 = tableby( ~ s_total + s_vin_simple + q_blanc + q_rouge + q_total, vin)
summary(t1, text = "latex")
t2 = tableby(conditionnement ~ quantite_produit + mean.dose, pesticides)
summary(t2, text = "latex")

require(openxlsx)
prix = read.xlsx("./Donnees_ref/prix_vin_revenu.xlsx")
summary(prix)
prix2 = prix[,c(2,9,11,13)]
write.csv(prix2, file = "./Donnees_ref/prix_revenu.csv")

list = ls()
rm(list)
prix = read.csv("./Donnees_ref/prix_revenu.csv")
vin = read.csv("./Donnees_ref/vin_final.csv")
pesticides = read.csv("./Donnees_ref/pesticides_final.csv")

require(tidyverse)
names(vin)
names(prix)[2] = "annee"
prixvin = left_join(vin, prix, by = "annee")
summary(prixvin)
prixvin %>% 
    group_by(annee) %>%
    summarise(mean(s_vin_simple), ratio_s = mean(s_vin_simple)/mean(s_total),
        mean(s_total), 
        mean(q_blanc), ratio_b = mean(q_blanc)/mean(q_total),
        mean(q_rouge), ratio_r = mean(q_rouge)/mean(q_total),
        q_br = mean(q_blanc) + mean(q_rouge),
        ratio_br = (mean(q_blanc) + mean(q_rouge))/mean(q_total),
        mean(q_total),
        mean(prix_vin_blanc_sans_IG),
        mean(prix_vin_rouge_sans_IG)) %>%
    View()
prixvin %>% 
    group_by(departement.y) %>%
    summarise(mean(s_vin_simple), ratio_s = mean(s_vin_simple)/mean(s_total),
        mean(s_total), 
        mean(q_blanc), ratio_b = mean(q_blanc)/mean(q_total),
        mean(q_rouge), ratio_r = mean(q_rouge)/mean(q_total),
        q_br = mean(q_blanc) + mean(q_rouge),
        ratio_br = (mean(q_blanc) + mean(q_rouge))/mean(q_total),
        mean(q_total),
        mean(prix_vin_blanc_sans_IG),
        mean(prix_vin_rouge_sans_IG)) %>%
    View()

pv1 = prixvin %>% 
    select(annee, ndep = number, 
        dep = departement.y,
        s_nig = s_vin_simple, 
        s_total,
        q_blanc, q_rouge, q_total, 
        p_blanc = prix_vin_blanc_sans_IG,
        p_rouge = prix_vin_rouge_sans_IG,
        revenu = revenu.déflaté)
write.csv(pv1, file = "./Donnees_ref/prixETvin.csv")

list = ls()
rm(list)
pesticides = read.csv("./Donnees_ref/pesticides_final.csv")
pv = read.csv("./Donnees_ref/prixETvin.csv")
names(pv)
names(pesticides)
pest = pesticides %>% 
    select(annee, ndep = number.y,
        q_prod = quantite_produit,
        cond = conditionnement)
write.csv(pest, file = "./Donnees_ref/pest.csv")

list = ls()
rm(list)
pest = read.csv("./Donnees_ref/pest.csv")
pv = read.csv("./Donnees_ref/prixETvin.csv")

pvp = left_join(pv, pest, by = c("annee", "ndep"))
View(pvp)
names(pvp)
pvp1 = pvp[,-c(1,13)]
write.csv(pvp1, file = "./Donnees_ref/pvp1.csv")

pvp1 = pvp1 %>% na.omit()
write.csv(pvp1, file = "./Donnees_ref/pvp1.csv")

list = ls()
rm(list)
pvp = read.csv("./Donnees_ref/pvp1.csv")
pvpx = pvp %>% 
    mutate(K = as.numeric(cond == "K"), 
        L = as.numeric(cond == "L"),
        cond = as.numeric(cond)) %>%
    mutate(qk_prod = q_prod*K,
        ql_prod = q_prod*L) %>%
    group_by(annee, ndep, dep) %>%
    summarise(s_nig = mean(s_nig), s_total = mean(s_total),
        q_blanc = mean(q_blanc), q_rouge = mean(q_rouge),
        q_total = mean(q_total), 
        p_blanc = mean(p_blanc), p_rouge = mean(p_rouge),
        revenu = mean(revenu),
        qk_prod = sum(qk_prod), ql_prod = sum(ql_prod))
write.csv(pvpx, file = "./Donnees_ref/prefinal.csv")    

list = ls()
rm(list)
pvp = read.csv("./Donnees_ref/prefinal.csv")
depdel = pvp %>% 
    group_by(ndep) %>%
    count() %>% 
    filter(n == 9) %>%
    select(ndep)
pvpx = pvp %>% 
    filter(ndep %in% depdel$ndep)
pvpx %>% 
    group_by(ndep) %>%
    count()
pvpy = filter(pvpx, annee >= 2012)
names(pvpy)
write.csv(pvpy[,-1], file = "./Donnees_ref/final.csv", row.names = FALSE)

list = ls()
rm(list)
pvp = read.csv("./Donnees_ref/final.csv")
names(pvp)
pvp[, -c(2:3)] %>%  
    cor() %>% View()
pvp %>% 
    group_by(dep) %>%
    summarise_each(var) %>%
    xtable(type = "latex", include.rownames = FALSE)
pvps = pvp %>% 
    mutate(q = q_blanc + q_rouge) %>%
    group_by(ndep, dep) %>%
    summarise_each(mean) 
pvps %>% 
    arrange(q) %>% 
    View()
x = 
x[, -c(1:2)] %>%
    cor() %>% View()
    xtable(type = "latex")

require(arsenal) 
names(pvp)
mycontrols = 
    tableby.control(test = FALSE, total = FALSE,
        numeric.test = "kwt", cat.test = "chisq",
        numeric.stats = 
            c("mean"),
        cat.stats = c("countpct"),
        stats.labels = 
            list(mean = 'Mean'))
ta = tableby(annee ~ s_nig + s_total + q_blanc + q_rouge +
    q_total + p_blanc + p_rouge + revenu +
    qk_prod + ql_prod, data = pvp, 
    control = mycontrols, digits = 2) %>%
    summary(text = "latex")
tn = tableby( ~ s_nig + s_total + 
    q_blanc + q_rouge + q_total + 
    qk_prod + ql_prod, 
    strata = dep,
    data = pvp, 
    control = mycontrols, digits = 2) %>%
    summary(text = "latex")

list = ls()
rm(list)
require(tidyverse)
pvp = read.csv("./Donnees_ref/final.csv")
require(xtable)
ta = pvp %>% 
    group_by(annee) %>%
    summarise_each(mean) %>%
    xtable(type = "latex")
td = pvp %>% 
    group_by(dep) %>%
    select(s_nig, s_total, q_blanc, q_rouge,
        q_total, qk_prod, ql_prod) %>% 
    summarise_each(mean) %>%
    xtable(type = "latex")

pvpx = pvp %>% 
    filter(annee > 2012) %>% 
    group_by(annee) %>%
    summarise(s = log(sum(s_nig)), 
        q = log(sum(q_blanc) + sum(q_rouge)), 
        p = log(mean(p_blanc + p_rouge)/2),
        r = log(mean(revenu)),
        qk = log(sum(qk_prod)),
        ql = log(sum(ql_prod)))
require(xtable)
pvpx %>% 
    cor() %>%
    xtable(type = "latex") 
pvpx = pvp %>% 
    filter(s_nig != 0 & q_rouge != 0 & q_blanc != 0) %>%
    mutate(s = log(s_nig), 
        q = log(q_blanc + q_rouge), 
        p = log((p_blanc + p_rouge)/2),
        r = log(revenu),
        qk = log(qk_prod + ql_prod),
        t = annee)
model = lm(p ~ s + r + qk + ql, data = pvpx)
require(stargazer)
stargazer(model, type="latex")

pvp %>% group_by(dep) %>% 
    summarise_each()

pvpx = pvp %>% 
    #filter(annee > 2012) %>% 
    group_by(annee) %>%
    summarise(s = log(sum(s_nig)), 
        q = log(sum(q_blanc) + sum(q_rouge)), 
        p = log(mean(p_blanc + p_rouge)/2),
        r = log(mean(revenu)),
        qk = log(sum(qk_prod)),
        ql = log(sum(ql_prod)))
require(xtable)
pvpx %>% 
    summarise_each(var) %>%
    xtable(type = "latex") 