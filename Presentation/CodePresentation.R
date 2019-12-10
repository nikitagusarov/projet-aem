###################
# Setting r options
###################
# knitr::opts_chunk$set(echo = FALSE)
# knitr::opts_chunk$set(size = "tiny")
# knitr::opts_chunk$set(fig_caption = " ")
# knitr::opts_chunk$set(dev = "pdf")
# knitr::opts_chunk$set(dpi = 600)
# Extract R code from Rmd
# require(knitr)
# purl("Presentation.Rmd", 
#     output = "CodePresentation.R", 
#     documentation = 0)

##########
# Packages
##########
require(tidyverse)
require(plm)
require(Formula)
require(gridExtra)
require(stargazer)
require(texreg)
require(rlang)
require(dummies)
require(stargazer)
require(systemfit)
require(olsrr)

##############
# Loading data
##############
# Data
data = read.csv("../Donnees/Base-de-donnees-indice-prix.csv")
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

##################
# Transformed data
##################
datai = datax %>%
    arrange(ndep) %>%
    mutate(si = log(s_vin_simple + 0.001), 
        qi = log(q_blanc + q_rouge + 0.001), 
        ipi = log(IP),
        ri = log(revenu.déflaté),
        iki = log(IQK),
        t = as.integer(as.factor(annee)),
        year = annee) %>%
    dplyr::select(year, ndep, qi, ipi, si, ri, iki, t)

############
# Panel data
############
datap = pdata.frame(datai, index = c("ndep", "year"),
    drop.index = T)

#################
# Clear workplace
#################
list = ls()
keep = c("datai", "datap")
omit = setdiff(list, keep)
rm(omit)

###################
# Support functions
###################
# xtsum (overall, within and between variance) for panel data
# STATA version
xtsum = function(data, varname, unit) {
    # the variable to xtsum over
    varname = enquo(varname)
    # the identifier dimention
    loc.unit = enquo(unit)
    # overall
    ores = data %>% 
        summarise(ovr.mean = mean(!! varname, na.rm = TRUE), 
        ovr.sd = sd(!! varname, na.rm = TRUE), 
        ovr.min = min(!! varname, na.rm = TRUE), 
        ovr.max = max(!! varname, na.rm = TRUE), 
        ovr.N = sum(as.numeric((!is.na(!! varname)))))
    # between
    bmeans = data %>% 
        group_by(!! loc.unit) %>% 
        summarise(meanx = mean(!! varname, na.rm = TRUE), 
        t.count = sum(as.numeric(!is.na(!! varname))))
    bres = bmeans %>% 
        ungroup() %>% 
        summarise(between.sd = sd(meanx, na.rm = TRUE), 
        between.min = min(meanx, na.rm = TRUE), 
        between.max = max(meanx, na.rm = TRUE), 
        units = sum(as.numeric(!is.na(t.count))), 
        t.bar = mean(t.count, na.rm = TRUE))
    # within
    wdat = data %>% 
        group_by(!! loc.unit) %>% 
        mutate(W.x = scale(!! varname, scale = FALSE))
    wres = wdat %>% 
        ungroup() %>%  
        summarise(within.sd = sd(W.x, na.rm = TRUE), 
        within.min = min(W.x, na.rm = TRUE), 
        within.max = max(W.x, na.rm = TRUE))
    # results
    return(list(var = varname, ores = ores, bres = bres, wres = wres))
}
# Print results for a list of xtsums
print.xtsum = function(xtsums.list) {
    # takes multiple xtsums as list
    df = data.frame(Variable = NA, Mean = NA,
        Overall = NA, Between = NA, Within = NA)
    # Filling loop
    for (i in 1:length(xtsums.list)) {
        df[i,1] = as_name(xtsums.list[[i]]$var)
        df[i,2] = xtsums.list[[i]]$ores$ovr.mean 
        df[i,3] = xtsums.list[[i]]$ores$ovr.sd
        df[i,4] = xtsums.list[[i]]$bres$between.sd
        df[i,5] = xtsums.list[[i]]$wres$within.sd
    }
    # Rownames
    rownames(df) = df[,1]
    # Results
    return(df = df[,-1])
}

##################
# Set ggplot style
##################
pres_theme = theme(text = element_text(size = rel(3)),
    legend.position = "none")

#################
# Effects testing 
#################
Effect.testing = function(Formulas, data) {
    Dtest = data.frame(var = 0,
        Random = 0, Fixed = 0, 
        Individual = 0, Time = 0, Twoways = 0)
        for (i in 1:length(Formulas)) {
            Dtest[i,1] = names(Formulas)[i]
            ## Chow test
            # Random coefs for random effects          
            Dtest[i,2] = pooltest(Formulas[[i]],
                data = data,
                model = "random")$p.val 
            # Different coefs for fixed effects
            Dtest[i,3] = pooltest(Formulas[[i]],
                data = data,
                model = "within")$p.val
            ## Lagrange multiplier tests
            # Individual effects
            Dtest[i,4] = plmtest(Formulas[[i]],
                data = data,
                effect = "individual",
                type = "bp")$p.val
            # Time effects
            Dtest[i,5] = plmtest(Formulas[[i]],
                data = data,
                effect = "time",
                type = "bp")$p.val
            # Two-ways effects (individual and time)
            Dtest[i,6] = plmtest(Formulas[[i]],
                data = data,
                effect = "twoways",
                type = "ghm")$p.val   
        }
    rownames(Dtest) = Dtest[,1]
    return(Dtest = Dtest[,-1])
}

## ####################################################
## ################### Introduction ###################
## ####################################################

## ####################################################
## ###################  Pesticides  ###################
## ####################################################

## #####################################################
## ###################  Viticulture  ###################
## #####################################################

## #####################################################
## ###################  Théorie mod  ###################
## #####################################################

## #####################################################
## ###################  Données mob  ###################
## #####################################################

## #####################################################
## ###################  Statistique  ###################
## #####################################################

p1 = datai %>% 
    ggplot(aes(qi, ipi, col = as.factor(ndep))) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", se = F, size = 0.25) +
    # ggtitle("Q ~ IP") +
    xlab("Quantité du vin") + ylab("Index du prix") +
    pres_theme
p2 = datai %>% 
    ggplot(aes(qi, iki, col = as.factor(ndep))) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", se = F, size = 0.25) +
    # ggtitle("Q ~ IK") +
    xlab("Quantité du vin") + ylab("Index des pésticides") +
    pres_theme
grid.arrange(p1, p2, nrow = 1)
# Probably it is better to do it by variable

p1 = datai %>% 
    ggplot(aes(qi, si, col = as.factor(ndep))) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", se = F, size = 0.25) +
    # ggtitle("Q ~ S") +
    xlab("Quantité du vin") + ylab("Surface qultivé") +
    pres_theme
p2 = datai %>% 
    ggplot(aes(qi, ri, col = as.factor(ndep))) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", se = F, size = 0.25) +
    # ggtitle("Q ~ R") +
    xlab("Quantité du vin") + ylab("Revenus") +
    pres_theme
grid.arrange(p1, p2, nrow = 1)

lxtsums = list()
# list
lxtsums[[1]] = xtsum(datai, ipi, ndep)
lxtsums[[2]] = xtsum(datai, iki, ndep)
lxtsums[[3]] = xtsum(datai, si, ndep)
lxtsums[[4]] = xtsum(datai, ri, ndep)
lxtsums[[5]] = xtsum(datai, t, ndep)
# results
results = print.xtsum(lxtsums)
rownames(results) = c("Index prix", "Index pesticides",
    "Surface", "Revenus", "Temps")
## stargazer(results,
##     title = "Variance study",
##     summary = FALSE)

Formulas = list(
    ipi = qi ~ ipi,
    iki = qi ~ iki,
    si = qi ~ si,
    ri = qi ~ ri)
Dtest = Effect.testing(Formulas, data = datap)
rownames(Dtest) = c("Index prix", "Index pesticides",
    "Surface", "Revenus")
stargazer(Dtest[,c(1:2)], 
    title = "Chow pooling test",
    summary = FALSE)

stargazer(Dtest[,c(3:ncol(Dtest))], 
    title = "Lagrange multiplier test, p-values",
    summary = FALSE)

correlation = cor(datap)
colnames(correlation) = c("Quantité du vin", "IP", 
        "Surface", "Revenus", 
        "Index pésticides", "Temps")
rownames(correlation) = c("Quantité du vin", "IP", 
        "Surface", "Revenus", 
        "Index pésticides", "Temps")
stargazer(correlation, 
    title = "Overall correlation",
    summary = F)

###################
# Rework the matrix WITHIN_TRANSFORM
###################
rm(datax) ; rm(datay) ; rm(data) ; rm(dn)
dataW = datap 
dataW$qi = Within(datap$qi)
dataW$ipi = Within(datap$ipi)
dataW$iki = Within(datap$iki)
dataW$si = Within(datap$si)
dataW$ri = Within(datap$ri)
correlationW = cor(dataW)
dataW$ndep = index(datap)$ndep
colnames(correlationW) = c("Quantité du vin", "IP", 
        "Surface", "Revenus", 
        "Index pésticides", "Temps")
rownames(correlationW) = c("Quantité du vin", "IP", 
        "Surface", "Revenus", 
        "Index pésticides", "Temps")
stargazer(correlationW, 
    title = "Within transformation correlation",
    summary = F)

p1 = dataW %>% 
    ggplot(aes(qi, ipi, col = as.factor(ndep))) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", se = F, size = 0.25) +
    geom_smooth(aes(qi, ipi, col = "black"), method = "loess", size = 0.4) +
    # ggtitle("Q ~ IP") +
    xlab("Quantité du vin") + ylab("Index du prix") +
    pres_theme
p2 = dataW %>% 
    ggplot(aes(qi, iki, col = as.factor(ndep))) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", se = F, size = 0.25) +
    geom_smooth(aes(qi, iki, col = "black"), method = "loess", size = 0.4) +
    # ggtitle("Q ~ IK") +
    xlab("Quantité du vin") + ylab("Index des pésticides") +
    pres_theme
grid.arrange(p1, p2, nrow = 1)
# Probably it is better to do it by variable

p1 = dataW %>% 
    ggplot(aes(qi, si, col = as.factor(ndep))) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", se = F, size = 0.25) +
    geom_smooth(aes(qi, si, col = "black"), method = "loess", size = 0.4) +
    # ggtitle("Q ~ S") +
    xlab("Quantité du vin") + ylab("Surface qultivé") +
    pres_theme
p2 = dataW %>% 
    ggplot(aes(qi, ri, col = as.factor(ndep))) +
    geom_point(size = 0.5) + 
    geom_smooth(method = "lm", se = F, size = 0.25) +
    geom_smooth(aes(qi, ri, col = "black"), method = "loess", size = 0.4) +
    # ggtitle("Q ~ R") +
    xlab("Quantité du vin") + ylab("Revenus") +
    pres_theme
grid.arrange(p1, p2, nrow = 1)

## #####################################################
## ###################  Modèlisation  ##################
## #####################################################

## ###################
## # Not evaluated !!!
## ###################
## ###################
## # Rework the matrix DUMMIES
## ###################
## Dum = dummy(datai$ndep, sep = "_")
## # Index pésticides
## IKI = as.matrix(datai$iki)[, rep(1, each = length(unique(datai$ndep)))]
## ikiDum = as.data.frame(Dum*IKI) %>%
##     setNames(paste0('iki_', names(.)))
## rm(IKI)
## # Revenus
## RI = as.matrix(datai$ri)[, rep(1, each = length(unique(datai$ndep)))]
## riDum = as.data.frame(Dum*RI) %>%
##     setNames(paste0('ri_', names(.)))
## rm(RI)
## # Concatenate
## dataD = datai %>%
##     select(-c(t, ri, iki)) %>%
##     cbind(Dum) %>%
##     cbind(ikiDum) %>%
##     cbind(riDum)

## ###################
## # Not evaluated !!!
## ###################
## ###################
## # Rework the matrix WITHINxDUMMIES
## ###################
## Dum = dummy(datai$ndep, sep = "_")
## # Index pésticides
## IKI = as.data.frame(dataW$iki, ncol = 1)[, rep(1, each = length(unique(datai$ndep)))]
## ikiDum = as.data.frame(Dum*IKI)
## ikiDum = ikiDum %>%
##     set_names(~str_replace_all(., "dataW\\$", ""))
## rm(IKI)
## # Revenus
## RI = as.data.frame(dataW$ri)[, rep(1, each = length(unique(datai$ndep)))]
## riDum = as.data.frame(Dum*RI) %>%
##     rename_all(list(~str_replace_all(., "dataW\\$", "")))
## rm(RI)
## # Concatenate
## dataWD = dataW %>%
##     select(-c(t, ri, iki)) %>%
##     cbind(ikiDum) %>%
##     cbind(riDum)
## rm(ikiDum) ; rm(riDum)

## # - Vérification des hypothèses (5 hypothèses) :
## #     - La moyenne nulle des erreurs
## #     - La normalité des residus
## #     - Homoscedacité
## #     - Autocorrélation
## #     - Spécification du modèle
## # 3SLS and FIML are asymptotically equivalent.
## # Hence 3SLS is efficient and FIML is consistent even if residuals are not normal.

# Data transformation for systemfit
dataWX = as.data.frame(dataW)

# equations
eqdemand = qi ~ 0 + ipi + ri
eqoffer = qi ~ 0 + ipi + si + iki 
system = list(Demande = eqdemand, Offre = eqoffer)
# OLS
ols = systemfit(system, 
    data = dataWX, 
    method = "OLS")
# WLS
wls = systemfit(system, 
    data = dataWX, 
    method = "WLS")
# SUR
sur = systemfit(system, 
    data = dataWX, 
    method = "SUR")

texreg(list(ols, wls, sur),
    custom.model.names = c("OLS", "WLS", "SUR"),
    label = "table : ols, wls and sur")

cordata = dataWX %>% 
    mutate(u1 = ols$eq[[1]]$res,
        u2 = ols$eq[[2]]$res,
        u3 = wls$eq[[1]]$res,
        u4 = wls$eq[[2]]$res,
        u5 = sur$eq[[1]]$res,
        u6 = sur$eq[[2]]$res)
cormat = cor(cordata[,-c(6,7)])[1:5, 6:11]
colnames(cormat) = c("OLS D", "OLS O", 
        "WLS D", "WLS O", 
        "SUR D", "SUR O")
rownames(cormat) = c("Vin", "IP", 
        "Surface", "Revenus", 
        "Pesticides")
stargazer(cormat, 
    title = "Correlation des résidus",
    summary = F)
# cordata %>% ggplot(aes(x = u6, y = f6)) + geom_point() + geom_smooth()

# Panel Durbin-Watson test
cordataDW = cordata %>% 
    group_by(ndep) %>%
    mutate(u1_diff = u1 - dplyr::lag(u1),
        u2_diff = u2 - dplyr::lag(u2),
        u3_diff = u3 - dplyr::lag(u3),
        u4_diff = u4 - dplyr::lag(u4),
        u5_diff = u5 - dplyr::lag(u5),
        u6_diff = u6 - dplyr::lag(u6)) %>%
    ungroup() %>%
    select(contains("u")) %>%
    mutate_all(function(x) replace_na(x, 0)) %>%
    summarise_all(function(x) sum(x^2))
pDW = data.frame(ncol = 3, nrow = 2)
pDW[1,1] = cordataDW[1,1]/cordataDW[1,7]
pDW[2,1] = cordataDW[1,2]/cordataDW[1,8]
pDW[1,2] = cordataDW[1,3]/cordataDW[1,9]
pDW[2,2] = cordataDW[1,4]/cordataDW[1,10]
pDW[1,3] = cordataDW[1,5]/cordataDW[1,11]
pDW[2,3] = cordataDW[1,6]/cordataDW[1,12]
colnames(pDW) = c("OLS", "WLS", "SUR")
rownames(pDW) = c("Equation de demande", "Equation d'offre")
stargazer(pDW, 
    title = "Durbin-Watson test statistics",
    summary = F)

BartT = data.frame(ncol = 3, nrow = 2)
BartT[1,1] = ols_test_bartlett(cordata, u1, 
    group_var = ndep)$pval
BartT[2,1] = ols_test_bartlett(cordata, u2, 
    group_var = ndep)$pval
BartT[1,2] = ols_test_bartlett(cordata, u3, 
    group_var = ndep)$pval
BartT[2,2] = ols_test_bartlett(cordata, u4, 
    group_var = ndep)$pval
BartT[1,3] = ols_test_bartlett(cordata, u5, 
    group_var = ndep)$pval
BartT[2,3] = ols_test_bartlett(cordata, u6, 
    group_var = ndep)$pval
colnames(BartT) = c("OLS", "WLS", "SUR")
rownames(BartT) = c("Equation de demande", "Equation d'offre")
stargazer(BartT,
    summary = FALSE,
    title = "Bartlett heteroscedasticity test")

ShapT = data.frame(nrow = 2, ncol = 6)
ShapT[1,1] = shapiro.test(ols$eq[[1]]$res)$p.val
ShapT[2,1] = shapiro.test(ols$eq[[2]]$res)$p.val
ShapT[1,2] = shapiro.test(wls$eq[[1]]$res)$p.val
ShapT[2,2] = shapiro.test(wls$eq[[2]]$res)$p.val
ShapT[1,3] = shapiro.test(sur$eq[[1]]$res)$p.val
ShapT[2,3] = shapiro.test(sur$eq[[2]]$res)$p.val
colnames(ShapT) = c("OLS", "WLS", "SUR")
rownames(ShapT) = c("Equation de demande", "Equation d'offre")
stargazer(ShapT,
    summary = FALSE,
    title = "Shapiro-Wilk normality test")

par(mfrow = c(1,2), cex = 0.5)
plot(y = ols$eq[[1]]$res, x = ols$eq[[1]]$fit,
    col = "blue", 
    main = "Demande", 
    ylab = "Residuals", xlab = "Fitted")
points(y = wls$eq[[1]]$res, x = wls$eq[[1]]$fit,
    col = "green", pch = 17)
points(y = sur$eq[[1]]$res, x = sur$eq[[1]]$fit,
    col = "red", pch = 14)
plot(x = ols$eq[[2]]$res, y = ols$eq[[2]]$fit,
    col = "blue", 
    main = "Offre", 
    ylab = "Residuals", xlab = "Fitted")
points(y = wls$eq[[2]]$res, x = wls$eq[[2]]$fit,
    col = "green", pch = 17)
points(y = sur$eq[[2]]$res, x = sur$eq[[2]]$fit,
    col = "red", pch = 14)

# equations
eqdemand = qi ~ 0 + ipi + ri
eqoffer = qi ~ 0 + ipi + si + iki 
inst = ~ ri + si + iki
system = list(Demande = eqdemand, Offre = eqoffer)
# 2SLS
# 2SLS is an equivalent of ILS (indirect least squares)
sls2 = systemfit(system, 
    inst = inst,
    data = dataWX, 
    method = "2SLS")
# 2WSLS
wsls2 = systemfit(system, 
    inst = inst,
    data = dataWX, 
    method = "W2SLS")
# 3SLS (errors correction)
sls3 = systemfit(system, 
    inst = inst,
    data = dataWX, 
    method = "3SLS")
# FIML (iterated 3SLS)
fiml = systemfit(system, 
    inst = inst,
    data = dataWX, 
    method = "3SLS", maxit = 1000)

texreg(list(sls2, wsls2, sls3, fiml),
    custom.model.names = c("2SLS", "W2SLS", "3SLS", "i3SLS"),
    label = "table : 2sls, w2sls, 3sls and fiml")

h1 = hausman.systemfit(sls2, sls3) # p = 1, 3SLS inconsistent
# 2SLS estimator is consistent
h2 = hausman.systemfit(sls2, fiml)
res = data.frame(Test = c("2SLS contre 3SLS", "2SLS contre i3SLS"),
    Resultats = c(h1$p.val, h2$p.val))

stargazer(res, 
    title = "Hausman 3SLS consistency test",
    summary = F, 
    header = F)

# - Linear test :
# What should be tested ???
# linearHypothesis(sls2, 
lr = lrtest(sls2, sls3, fiml)
stargazer(lr, title = "Likelihood test",
    summary = FALSE)

ShapT = data.frame(nrow = 2, ncol = 6)
ShapT[1,1] = shapiro.test(sls2$eq[[1]]$res)$p.val
ShapT[2,1] = shapiro.test(sls2$eq[[2]]$res)$p.val
ShapT[1,2] = shapiro.test(sls3$eq[[1]]$res)$p.val
ShapT[2,2] = shapiro.test(sls3$eq[[2]]$res)$p.val
ShapT[1,3] = shapiro.test(fiml$eq[[1]]$res)$p.val
ShapT[2,3] = shapiro.test(fiml$eq[[2]]$res)$p.val
colnames(ShapT) = c("2SLS", "3SLS", "i3SLS")
rownames(ShapT) = c("Equation de demande", "Equation d'offre")
stargazer(ShapT,
    summary = FALSE,
    title = "Shapiro-Wilk normality test")

resdata3 = dataWX %>% 
    mutate(u1 = sls2$eq[[1]]$res,
        u2 = sls2$eq[[2]]$res,
        u3 = sls3$eq[[1]]$res,
        u4 = sls3$eq[[2]]$res,
        u5 = fiml$eq[[1]]$res,
        u6 = fiml$eq[[2]]$res)
BartT = data.frame(ncol = 3, nrow = 2)
BartT[1,1] = ols_test_bartlett(resdata3, u1, group_var = ndep)$pval
BartT[2,1] = ols_test_bartlett(resdata3, u2, group_var = ndep)$pval
BartT[1,2] = ols_test_bartlett(resdata3, u3, group_var = ndep)$pval
BartT[2,2] = ols_test_bartlett(resdata3, u4, group_var = ndep)$pval
BartT[1,3] = ols_test_bartlett(resdata3, u5, group_var = ndep)$pval
BartT[2,3] = ols_test_bartlett(resdata3, u6, group_var = ndep)$pval
colnames(BartT) = c("2SLS", "3SLS", "i3SLS")
rownames(BartT) = c("Equation de demande", "Equation d'offre")
stargazer(BartT,
    summary = FALSE,
    title = "Bartlett heteroscedasticity test")

par(mfrow = c(1,2), cex = 0.5)
plot(density(sls2$eq[[1]]$res), col = "blue", 
    main = "Demande", xlab = "Residuals")
lines(density(fiml$eq[[1]]$res), col = "green")
lines(density(sls3$eq[[1]]$res), col = "red")
plot(density(sls3$eq[[2]]$res), col = "red", 
    main = "Offre", xlab = "Residuals")
lines(density(fiml$eq[[2]]$res), col = "green")
lines(density(sls2$eq[[2]]$res), col = "blue")

par(mfrow = c(1,2), cex = 0.5)
plot(y = resdata3$qi, x = sls2$eq[[1]]$fit,
    col = "blue", 
    main = "Demande", 
    ylab = "Real", xlab = "Fitted")
points(y = resdata3$qi, x = fiml$eq[[1]]$fit,
    col = "green", pch = 17)
points(y = resdata3$qi, x = sls3$eq[[1]]$fit,
    col = "red", pch = 14)
plot(y = resdata3$qi, x = sls2$eq[[2]]$fit,
    col = "blue", 
    main = "Offre", 
    ylab = "Real", xlab = "Fitted")
points(y = resdata3$qi, x = fiml$eq[[2]]$fit,
    col = "green", pch = 17)
points(y = resdata3$qi, x = sls3$eq[[2]]$fit,
    col = "red", pch = 14)

par(mfrow = c(1,2), cex = 0.5)
plot(y = resdata3$qi, x = fiml$eq[[1]]$fit,
    col = "blue", pch = 17, 
    main = "Demande", 
    ylab = "Real - résidus", xlab = "Fitted")
points(y = fiml$eq[[1]]$res, x = fiml$eq[[1]]$fit,
    col = "red", pch = 14)
lines(y = resdata3$qi, x = fiml$eq[[1]]$fit+fiml$eq[[1]]$res,
    col = "black")
plot(y = resdata3$qi, x = fiml$eq[[2]]$fit,
    col = "blue", pch = 17, 
    main = "Demande", 
    ylab = "Real - résidus", xlab = "Fitted")
points(y = fiml$eq[[2]]$res, x = fiml$eq[[2]]$fit,
    col = "red", pch = 14)
lines(y = resdata3$qi, x = fiml$eq[[2]]$fit+fiml$eq[[2]]$res,
    col = "black")

par(mfrow = c(1,2), cex = 0.5)
plot(y = sls2$eq[[1]]$res, x = sls2$eq[[1]]$fit,
    col = "blue", 
    main = "Demande", 
    ylab = "Residuals", xlab = "Fitted")
points(y = fiml$eq[[1]]$res, x = fiml$eq[[1]]$fit,
    col = "green", pch = 17)
points(y = sls3$eq[[1]]$res, x = sls3$eq[[1]]$fit,
    col = "red", pch = 14)
plot(x = sls2$eq[[2]]$res, y = sls2$eq[[2]]$fit,
    col = "blue", 
    main = "Offre", 
    ylab = "Residuals", xlab = "Fitted")
points(y = fiml$eq[[2]]$res, x = fiml$eq[[2]]$fit,
    col = "green", pch = 17)
points(y = sls3$eq[[2]]$res, x = sls3$eq[[2]]$fit,
    col = "red", pch = 14)

# Panel Durbin-Watson test
# Create dataframe
resdataDW = resdata3 %>% 
    group_by(ndep) %>%
    mutate(u1_diff = u1 - dplyr::lag(u1),
        u2_diff = u2 - dplyr::lag(u2),
        u3_diff = u3 - dplyr::lag(u3),
        u4_diff = u4 - dplyr::lag(u4),
        u5_diff = u5 - dplyr::lag(u5),
        u6_diff = u6 - dplyr::lag(u6)) %>%
    ungroup() %>%
    select(contains("u")) %>%
    mutate_all(function(x) replace_na(x, 0)) %>%
    summarise_all(function(x) sum(x^2))
# Calculate test statistics
pDW = data.frame(ncol = 3, nrow = 2)
pDW[1,1] = resdataDW[1,1]/resdataDW[1,7]
pDW[2,1] = resdataDW[1,2]/resdataDW[1,8]
pDW[1,2] = resdataDW[1,3]/resdataDW[1,9]
pDW[2,2] = resdataDW[1,4]/resdataDW[1,10]
pDW[1,3] = resdataDW[1,5]/resdataDW[1,11]
pDW[2,3] = resdataDW[1,6]/resdataDW[1,12]
# Rename
colnames(pDW) = c("2SLS", "3SLS", "i3SLS")
rownames(pDW) = c("Equation de demande", "Equation d'offre")
stargazer(pDW, 
    title = "Durbin-Watson test statistics",
    summary = F)

par(mfrow = c(1,2), cex = 0.5)
acf(fiml$eq[[1]]$res, main = "Demande")
acf(fiml$eq[[2]]$res, main = "Offre")

cormat = cor(resdata3[,-c(6,7)])
cormat = cormat[1:5, 6:11]
colnames(cormat) = c("2SLS D", "2SLS O", 
        "3SLS D", "3SLS O",
        "i3SLS D", "i3SLS O")
rownames(cormat) = c("Vin", "IP", 
        "Surface", "Revenus", 
        "Pesticides")
stargazer(cormat, 
    title = "Correlation des résidus",
    summary = F)

## # Clustering
## # Between dataframe
## dataB = datap
## dataB$qi = Between(datap$qi)
## dataB$ipi = Between(datap$ipi)
## dataB$iki = Between(datap$iki)
## dataB$si = Between(datap$si)
## dataB$ri = Between(datap$ri)
## dataB$ndep = index(datap)$ndep
## # Between correction
## dataB = dataB %>%
##     select(-t)
## dataB = dataB %>%
##     group_by(ndep) %>%
##     summarise_all(mean)
## # Analysis for clustering
## wss = (nrow(dataB)-1)*sum(apply(dataB,2,var))
## for (i in 2:15) {
##     wss[i] = sum(kmeans(dataB, centers = i)$withinss)
## }

## plot(1:15, wss, type = "l",
##     xlab = "Number of Clusters",
##     ylab = "Within groups sum of squares",
##     main = "Le choix des clusters") # 3, 4, 5
## points(1:15, wss)

## # Grouping
## fit = kmeans(dataB[,-1], 3)
## # require(FactoMineR)
## nclef = data.frame(ndep = dataB$ndep, clust = fit$cluster)
## dataWY = left_join(dataW, nclef, by = "ndep")
## # Equations
## eqdemandx = qi ~ 0  +
##     I(ipi*as.numeric(clust == 1)) +
##     I(ipi*as.numeric(clust == 2)) +
##     I(ipi*as.numeric(clust == 3)) +
##     I(ri*as.numeric(clust == 1)) +
##     I(ri*as.numeric(clust == 2)) +
##     I(ri*as.numeric(clust == 3))
## eqofferx = qi ~ 0 +
##     I(ipi*as.numeric(clust == 1)) +
##     I(ipi*as.numeric(clust == 2)) +
##     I(ipi*as.numeric(clust == 3)) +
##     I(si*as.numeric(clust == 1)) +
##     I(si*as.numeric(clust == 2)) +
##     I(si*as.numeric(clust == 3)) +
##     I(iki*as.numeric(clust == 1)) +
##     I(iki*as.numeric(clust == 2)) +
##     I(iki*as.numeric(clust == 3))
## instx = ~ I(ri*as.numeric(clust == 1)) +
##     I(ri*as.numeric(clust == 2)) +
##     I(ri*as.numeric(clust == 3)) +
##     I(si*as.numeric(clust == 1)) +
##     I(si*as.numeric(clust == 2)) +
##     I(si*as.numeric(clust == 3)) +
##     I(iki*as.numeric(clust == 1)) +
##     I(iki*as.numeric(clust == 2)) +
##     I(iki*as.numeric(clust == 3))
## systemx = list(Demande = eqdemandx, Offre = eqofferx)

## # 2SLS
## sls2x = systemfit(systemx,
##     inst = inst,
##     data = dataWY,
##     method = "2SLS")

## #####################################################
## ###################  Modèlisation  ##################
## #####################################################
