# article 7 : Forecasting Bordeaux wine prices using state-space methods

Les prix sont des déterminants très important dans le marché du vin. En effet, il conditionnent les décisions d'achat et de vente Pour estimer ces prix, on utilise soit les déterminants de l'offre et de la demande, soit les mouvements de prix sur la base des prix futurs, soit en se basant sur le taux d'échange, utiliser un réseau neuronal ou utiliser des modèle de série teporelles. De nombreux auteurs utilisent des relations hédoniques pour déterminer le prix du vin. Le vin de bordeaux génériques représentent 40% de la production totale. Ce type de vin voit de large volume fait l'objet d'échange et la qualité devrait être constante. Il n'y a pas de prix futur pour le vin car les participants à ce marché n'ont pas suffisemment d'information pour faire des prédictions. 
Les prix du vin sont considérés comme très volatiles. Ils créent donc une incertitude importante pour les acteurs.Les auteurs de cet article cherche à construire un modèle de prévision du prix du vin rouge générique de Bordeaux.
Cela n'a pas été fait avant à cause du manque de données disponibles Pour ce papier, ils ont eu accès à des données mensuelles sur les prix, le volume et le nombre de contrat, avec le la quantité de récolte annuelle provenant de l'organisation professionnel d'échange des vins de bordeaux, entre 1999 et 2016.
Pour obtenir cette prévisions ils utilisent un modèle univarié pour le prix moyen du vin.

## Les données et le modèle
The specific series that we use is the monthly average
price per barrel (containing 900 l) for the period late
1999 to June 2016. The average is taken of the prices
in contracts signed in the period. Information is also
available on the volume sold and the number of
contracts signed. The price series (Figure 1) shows
a trend decline between 2000 and 2005, then rises
slightly before falling back to its 2005 level in 2010.
The role of
the grape harvest will be factor since there are at
least two disappointing harvests in the period examined (in 2008 and 2013; see Figure 3).

The price series is analysed using a state-space
model which is estimated for the purposes of predicting wine prices. This kind of model is based on
the notion that a series can be decomposed into
trend, cycle, seasonal and irregular components.
Sometimes certain variables can be
used as proxies such as a GDP measure for the
cyclical component, but this often proves unsatisfactory. The different components can be replaced by
artificial scales (linear trends and dummy variables),
resulting in a deterministic representation of these
components.
yt= ??t + ??t + ??t, ??t~NID (0; ??2)

 (1)
where ??t is the irregular term and ??t is the stochastic
trend determined as
??t = ??t-1 + ??t-1 + ??t (2)
where ??t is the slope of the trend and is also
stochastic:
??t= ??t-1 + ??t (3)
The stochastic terms ??t and ??t are assumed to have zero
means and their variances are ??2
?? and ??2
??, respectively.
In fact, it is assumed that ??t
~NID (0; ??2
??)
and
??t~NID (0; ??2
??).
 If the estimated variance of either of
these components is close to zero, then that component can be treated as being deterministic. If ??2
?? = 0
then the slope ?? is constant across the sample period
since ??t = ??t-1. If ??2
?? = 0 then the trend is deterministic. The seasonal component, ??t
, can be modelled using
dummies plus a stochastic term for each
month j = 1; 2; ; 12:
??jt = ??jt-1 + ??jt ??jt~NID (0; ??2
??)
Only one of these components affects the dependent
variable at a given time and so:
??t = ??jt for t = 1; 2; ; T
If ??jt has a zero variance (??2
?? = 0) then the seasonality is deterministic.
The different parameters of the model are obtained
using Gaussian state-space methods, involving the
Kalman filter and maximum-likelihood estimation.

## Les résultats
 It
would be difficult to model the path of wine prices
with a deterministic trend specification. The stochastic nature of seasonal component is apparent from
the fact that the seasonal variation in prices is less
pronounced after 2006. This feature of price variation links in with the nature of the behaviour of
Bordeaux wine producers mentioned in the introduction, where uncertainty about the size of the
grape harvest is attenuated or exacerbated at certain
points in the year. However, the impact on price is
not that substantial relative to the trend component.

## Conclusion
Due to their flexibility and ability to adapt to new
information, state-space methods have a number of
strengths in forecasting the path of time series variables. In the context of generic Bordeaux red wine
prices, these methods highlight the stochastic trend
and the seasonality present in the evolution of the
price over the period 2000 to 2016. They also satisfactorily predict the path of wine prices out of sample,
suggesting that the models could be useful for making
accurate forecasts of future price trends. In the absence
of futures markets for wines at the lower end of the
market, such forecasts could in principle provide additional information to market participants and improve
the functioning of the bulk market for generic wines.
This paper therefore contributes to both the wine
Figure 8. Unconditional forecasts for various sub-periods (with one standard error confidence intervals) - red line is the forecast
and blue dots are the observed prices.
APPLIED ECONOMICS 5119
economics and commodity price forecasting literatures, as well as providing a tool for wine professionals


# French Wines on the decline ? Econometric evidence from Britain

Despite the large array of geographical labels and a stringent approach to labelling,
French wines with geographical appellation were, for many decades, successfully
marketed in Britain and other markets.
In
response to the decline, French producers have adopted new labelling approaches
for a limited number of regional wines and a number of questions arise in this
context.  First, has the French expansion of, and emphasis on, varietal wines during
the early 1990s been able to mitigate the overall decline of French wine in the
British wine market?2 Second, how did consumers value these new varietal wines
relative to the traditionally labelled AOC wines (as epitomised, for example, by the
wines from the Bordeaux region)?

 First, Great Britain has become renowned for its expertise in selecting, importing, bottling and cellaring wine. UK imports of still light wine were responsible for
34% of the total wine imports of the EU in 1993 (by value). Second, since Britain
has never produced much wine of its own, consumers have traditionally been
exposed to a large variety of wines from many origins.5 Third, the emergence of
supermarkets as the most crucial retailing channel for wine has resulted in
intensified competition among distributors and the restructuring of many specialist
shops (Spahni, 2000, p.187).

By estimating the relative contribution of grape characteristics to wine
quality, and using the monetary values from the Californian market, the authors are
able to provide a producer pricing schedule for Israel. This quality-based pricing
schedule is derived to reduce the production of poor-quality wines, by giving Israeli
farmers an appropriate incentive to supply higher quality grapes. 
 
Based on recommended retail prices for Australian premium table wine, Oczkowski
(1994) identifies the implicit valuation of wine attributes for consumers and
retailers. On the producer side, the author suggests that the hedonic functions
estimated provide important information upon which longer-term investment
decisions may be made. Oczkowski (1994) includes dummy variables for producer
size in the hedonic regression and argues that this allows for two effects.

In contrast to previous hedonic analyses, Nerlove (1995) assumes that variety prices
are exogeneously determined and consumer preferences are expressed by the
quantities of each variety they buy. His assumption that the state-owned Swedish
retail monopoly does not exercise any monopsony power leads him to take variety
supplies as perfectly elastic for the group of consumers being considered. 

The authors suggest that
for their data set, quality, as measured by a jury grade assigned by professional
wine tasters is mainly explained by the 'subjective' sensory characteristics of the
wine, which are unobservable when consumers choose the wine. Implicit price
estimates are derived from data of a wine tasting panel that is unable to observe any
of the 'objective characteristics' (grape variety, vintage year etc.), including price,
of the wines they judge.

Our analysis contributes to the existing hedonic price literature on wine markets in
the following ways. First, we expand the dummy variable approach that was
pioneered by Kennedy (1986) and Oczkowski (1994) to obtain a distinct and
comparable contribution of each attribute to the variation of prices. The
econometric approach addresses heteroscedasticity explicitly by using a General
Least Squares (GLS) estimator. Second, in contrast to Combris et al. (1997) we do
not rely on sensory characteristics, but use attributes that can be observed by
consumers through the label (grape variety, vintage year etc.). Third, in contrast to 
previous hedonic studies related to wine, we do not rely on recommended retailer
prices, but rather on actual retail prices. This has significant implications since
many wines sold in the off-trade are sold by way of promotional discounts. The
empirical study uses a survey comprising actual retail prices of 7062 bottles of still
wine, identified by 'objective' labelling attributes (region of origin, vintage etc.). 

##Le marché du vin britannique
The structure of the off-licence trade is such
that two main commercial categories can be distinguished: those firms who only
have wines and other alcoholic drinks on offer (the 'wine specialists') and others,
for which wine is only part of their product range (the grocery-multiples). 
 The grocery-multiples include independent
food retailers, large general retailers (e.g. Marks and Spencer) and Co-ops. With
more than 45,000 points of sale and 70% (by value) of total wine sales in 1993, the
off-licence sector dominates the wine market in the UK. Within the off-licence
sector, France, Germany, and Italy held traditionally the largest market shares

For French wines, the European classification scheme for 'quality wines that are produced in certain regions' (VQPRD) contains both AOC wines and 'high-quality wine from an approved regional vineyard' (VDQS).8 The varietal wines are to be found in the non-VQPRD wine category, which consists of table wine ('Vins de
table') and country wines ('Vins de pays').

Whereas French varietal wines have sustained their sales and have recorded a slow
but steady growth since 1995 (Figure 1), only a slight upward trend is apparent for
French wines with geographic appellation (Figure 2). Nevertheless, French wines
with geographic appellation appear to have performed better than geographic
appellations from other origins (Figure 2). However, comparing non-VQPRD wines
from France with non-VQPRD from other origins (Figure 1), it is apparent that
varietal wines from other countries have been far more successful than varietal
wines from France. Whereas sales of non-VDPRD wines from other origins more
than tripled between 1991 and 1998, the growth of French non-VDPRD wines was
modest over the same period. 

Since the country wine category (VdP) is that category in which most varietal wines
are found, and since most varietal wines which are sold off-licence in the UK
originate from the region of Languedoc-Roussillon (86% by volume in 2000,
CFCE, 2001), consider the evolution of production of varietal wines versus AOC
wines in this region. Figure 3 displays the production of AOC wines, VdP from
Languedoc-Roussillon, 'VdP de petite zone' and 'VdP de région'. 

## L'application du modèle hédonique à la labellisation de vin

What is being
estimated in Rosen's (1974) description of a competitive equilibrium is the locus of
intersections of the demand curves of different consumers with varying tastes and
the supply functions of different producers with possibly varying technologies of production. The implicit estimated prices give us, therefore, the implicit marginal
valuation that consumers and producers place on labelling attributes. 

 Thus, equilibrium prices may not be linearly
decomposed in the Lancaster (1966) sense and a mutually advantageous trade in
goods may not be possible, with the result that marginal utilities may not be
proportional in equilibrium. Given that attribute packages may not be untied, yet
perfect divisibility in both product and attribute markets is satisfied, we expect that
the hedonic price function is convex in equilibrium (Jones, 1988).

 In the case of the British
wine market, we assume that perfect competition prevails amongst retailers and that
no discontinuities apply. However, we acknowledge that this will be an
approximation since production sets of wine attribute bundles may be non-convex
due to the inability of producers (and retailers) to supply a continuous range of
attribute combinations to consumers.

As a result of this modification, and after adjusting the coefficient estimates with
the estimated variances, the correct interpretation is that the coefficients measure
the relative impact on the dependent variable (the unit price evaluated at the sample
means) in the presence of the attribute ceteris paribus. 

From theory we expect that non-linear functional forms provide an appropriate
specification, although the choice of the functional form for the hedonic price
function should remain an empirical matter. Considering a semilogarithmic (log-lin)
model, we follow Kennedy (1981) and Goldberger (1968) to obtain a dummy
variable coefficient estimate g , such that
g*=exp(c-1/2 V(c))-1 (1)
where ^V c( )^ is an estimate of the variance of ( ) c^ , the estimated coefficient of the
dummy variable. Further, we rely on a procedure for adjusting dummy variable
coefficient estimates that does not result in the discarding of variables from the
equation (Suits, 1984). 

Le modèle hédonique est donné par :
P=a2*Xa2+a3*Xa3+B1*Xb1+B3*Xb3+B4*Xb4+u
where p is a N ×1 vector of transformed observations on the price per bottle P ,
there are five N ×1 vectors of X observations, ?? and ?? define the unknown
parameters, and  is a N ×1 vector of unknown stochastic disturbances. A
symmetrical substitution generates estimates for the remaining coefficients ??1 and
??2.  So, for example, the
estimated implicit price differences between Cabernet Sauvignon and Merlot would
be assumed to be the same across all regions. To avoid this equivalence effect we
specify an empirical model that includes interaction terms.

### Les données et variables

Our analysis relies on bottle prices and labelling attributes of French still wines
from a survey that was undertaken in August 1994 in 94 retail outlets of different
commercial forms in England and Scotland.

All French wines sold in those outlets were sampled, according to their market
share in those regions. Since the data set reveals the number of outlets in which a
particular bottle was found, we employ this information as a quantity proxy. In
total, the data set comprises the prices of 7062 bottles. Since each bottle of wine
appears on average in 3.2 outlets of each retailer, there are 2200 uniquely identified
bottles.
Each price for a bottle of wine is, where appropriate, described by colour, grape
variety, region of origin, category (e.g. AOC), importer, volume, producer, place of
bottling and vintage. Thus all information that appears on the label of the bottles
was recorded, except for the degree of alcohol. Summary statistics of the variables
used in the following analysis are presented in Table 3. It is striking that the mean
price of Appellations Contrôlées is more than twice as high as that for vin de pays,
and that there is a much larger variance in Appellations Contrôlées prices (36.82)
compared to that for the vin de pays prices (0.31). 

### 3.4 The functional form
Since all our explanatory variables are dummy variables, the choice of the
functional form is limited to the linear and the log-lin specifications. Nevertheless,
the use of interaction terms allows us to gain additional flexibility.14 When we
employ a log-lin hedonic price function, we assume non-constant marginal Engel
prices (the prices paid for incremental units of characteristics when purchased as
part of the same bundle) and constancy of relative prices with respect to changes in
proportions of characteristics (Triplett, 1975). This log-lin specification therefore
assumes homotheticity of the utility function, hence homogeneity of degree zero of
the demand equations for attributes. Since only relative prices matter, the imputed
price is independent of the level of the characteristic, which appears to be a realistic
and convenient assumption, since only dummy variables are used as explanatory
variables in the present model. Also, since the log-lin form allows each marginal
implicit price to be a non-linear function of the entire set of characteristics, it
appears as an attractive alternative hypothesis, since it accommodates the idea that
bundling constraints are present for wine attributes in a bottle of wine. 

Hence in the initial regression, we
include region of origin, category, brand, importer, grape variety, colour and
vintage, jointly with a subset of interaction terms: colour/region of origin,
category/region of origin, grape variety/region of origin, and grape variety/category. 

(a) Tests for Heteroscedasticity
We apply the Breusch-Pagan test (Breusch and Pagan, 1979) and its extension by
Koenker (1981) to correct for heteroscedasticity.17 Using weighted regressions also
satisfies hedonic theory, as each attribute should be accounted for in terms of its
market significance.

three diagnostics are examined: single-row diagnostics, an
examination of the hat matrix, and the Lagrange-Multiplier test for normality
(Jarque-Bera).20 For detecting those observations that are most strongly influential
in relation to the others, we follow Belsley et al. (1980) and apply external scaling
with corresponding size-adjusted cut-off values. If observations have a high
leverage and a significant influence on the estimated parameters, we consider them
as presenting potentially serious problems. We find that 2.1% of the observations
fall into this category. However, after running trimmed least squares estimation, the
parameter estimates were judged to be sufficiently robust to continue with weighted
least squares regression. 

First, employing GLS rather than OLS as an estimation rule is pursued on the basis
that each attribute (and its price) in the context of hedonic market studies is
important only to the extent that it captures some relevant fraction of the market
(Griliches, 1961). Here, the weights applied in the GLS regressions reflect in how
many retail outlets of each retailer type (e.g. Marks and Spencer) a uniquely
identified bottle was found. It is therefore implicitly assumed that the sample
fractions are directly proportionate to the number of bottles sold. Second, the
implementation of GLS allows us to account for heteroscedasticity due to omitted
variables and/or due to misspecification. 

However, lacking
further heterogeneity in the data, we were only able to obtain an aggregate valuation
of retailer traits which may well include the valuation of product diversity,
reputation, as well as service and locational cost advantages of a given retailer.

In these instances, outstanding grape varieties are
shown to have a strongly positive or negative regional impact on price, just as
outstanding regions have a similar grape varietal impact. 

The estimates are therefore
interpreted as follows. The relative impact of red and white wines on price (1.97%
and ???0.99%, respectively) suggests that market participants attribute a higher
valuation to red wines, although the impact is close to average. This is expected
insofar as France is better known in Britain for its classic reds (clarets) than for its
white wines. The fact that rosé wines are relatively less appreciated seems to be
reflected in the negative impact on price (???20.26%). 

As for the wine categories, consumers appear to value regional appellations (AOC
wines) positively (6.9%), whereas the Vins de Pays (VdP), which contain most
varietal wines, and the Vins de Table (VdT) show a negative impact on price
(???33.3% and ???21.06% respectively). The more negative valuation of the VdP
compared to the VdT appears initially surprising, because the VdP were originally
conceived as a superior sub-category of the VdT. 

 Therefore, the
conclusions that we can draw from the production data is in line with our estimates,
which suggest that French varietal wines, as they appeared in retail outlets during
1994, had not achieved a sufficiently positive valuation by British market
participants in order to reverse the overall diminishing role of French wines in
Britain. 

 Although Alsace is not a very important contributor to the
overall sample, the fact that Riesling, Muscat, Pinot Blanc and Pinot Noir are
classic grapes that appear on the label of wines from Alsace, puts their overall price
impacts in a different light (no significant impact of Riesling; +41.5%, -31.2% and
+12.3% for the others respectively). The negative overall impact of Muscat from
Alsace (-27.2%) is especially surprising and suggests that consumers' valuation for
Muscat from other regions has a far greater influence on Muscat's overall impact on
price (+41.5%) than the impact of Muscat from Alsace. 

 While Syrah is the noble grape of many Rhône
wines (it also goes into red AOC Chateauneuf-du-Pape wines), our finding of a
positive impact on price is also supported by its high appreciation when LanguedocRoussillon is its regional origin (Robinson, 1994, p.942). Gamay, which is the
basis for Beaujolais, is generally not known for superior quality.25 The estimate for
Merlot is more difficult to explain. Merlot is an increasingly important grape variety
that appears explicitly on the label of many wines from the South West and
Languedoc-Roussillon. In Languedoc-Roussillon itself, the area of Merlot plantings
has increased by 166% between 1979 and 2000 (Harpers, 2003). But Bordeaux
wines, which are dominated by AOC wines for which the label does not disclose the
grape variety, is responsible for two thirds of the 40,000 ha merlot plantings in
France. In sum, despite insignificance of corresponding region-variety interaction
terms, the positive impact of Syrah and Merlot is likely to be explained by its
valuation when originating from Languedoc-Roussillon.

In sum, two findings are particularly striking. First, the low market valuation for
Bordeaux wines, which represent not only the second largest regional origin in the
off-licence sales under investigation, but also a reputational cornerstone of French
wines. Second, the modest valuation by market participants of a relatively small
market segment, the varietal wines. Given these findings, it is not surprising that
French wines underwent a competitive decline in Britain since the early 1990s

# Chapitre 16 : Simultaneous equations models

Another important form of endogeneity of explanatory variables is simultaneity. This
arises when one or more of the explanatory variables is jointly determined with the dependent variable, typically through an equilibrium mechanism.
The classic example of an SEM is a supply and demand equation for some commodity
or input to production (such as labor). A simple labor supply function is
hs =a1w+B1z1+ ??? u1, [16.1]
where z1 is some observed variable affecting labor supply-say, the average manufacturing
wage in the county. The error term, u1, contains other factors that affect labor supply.
 This name comes from the fact that the labor supply function is derivable from
economic theory and has a causal interpretation. The coefficient a1 measures how labor supply changes when the wage changes; if hs and w are in logarithmic form, a1 is the labor
supply elasticity. Typically, we expect a1 to be positive.
 In deciding how
to analyze these data, we must understand that they are best described by the interaction
of labor supply and demand. Under the assumption that labor markets clear, we actually
observe equilibrium values of wages and hours worked.
To describe how equilibrium wages and hours are determined, we need to bring in the
demand for labor, which we suppose is given by
hd =??? a2w+ ??? b2z2+ ??? u2, [16.2]
where hd is hours demanded. As with the supply function, we graph hours demanded as
a function of wage, w, keeping z2 and u2 fixed. The variable z2-say, agricultural land
area-is an observable demand shifter, while u2 is an unobservable demand shifter.
at a2 ??? 0. Because labor and land are complements in production, we expect b2 ??? 0.
Notice how equations (16.1) and (16.2) describe entirely different relationships. Labor
supply is a behavioral equation for workers, and labor demand is a behavioral relationship
for farmers. Each equation has a ceteris paribus interpretation and stands on its own. They
become linked in an econometric analysis only because observed wage and hours are
determined by the intersection of supply and demand. 
 These two equations constitute a simultaneous equations
model (SEM), which has several important features. First, given zi1, zi2, ui1, and ui2, these
two equations determine hi and wi. (Actually, we must assume that a1 ??? a2, which means
that the slopes of the supply and demand functions differ; see Problem 1.) For this reason, hi and wi are the endogenous variables in this SEM. What about zi1 and zi2? Because
they are determined outside of the model, we view them as exogenous variables. From
a statistical standpoint, the key assumption concerning zi1 and zi2 is that they are both
uncorrelated with the supply and demand errors, ui1 and ui2, respectively. These are
examples of structural errors because they appear in the structural equations.
 Each equation should have a behavioral, ceteris paribus interpretation on its
own. Because we only observe equilibrium outcomes, specifying an SEM requires us to
ask such counterfactual questions as: How much labor would workers provide if the wage
were different from its equilibrium value? Example 16.1 provides another illustration of
an SEM where each equation has a ceteris paribus interpretation.
The important lesson is this: just because two variables are determined simultaneously does not mean that a simultaneous equations model is suitable. For an SEM to make
sense, each equation in the SEM should
have a ceteris paribus interpretation in
isolation from the other equation. As we
discussed earlier, supply and demand
examples, and Example 16.1, have this
feature. Usually, basic economic reasoning, supported in some cases by simple
economic models, can help us use SEMs
intelligently (including knowing when
not to use an SEM).
##Simultaneity BIAS in OLS
Equation (16.14), which expresses y2 in terms of the exogenous variables and the error
terms, is the reduced form equation for y2, a concept we introduced in Chapter 15 in
the context of instrumental variables estimation. The parameters p21 and p22 are called
reduced form parameters; notice how they are nonlinear functions of the structural
parameters, which appear in the structural equations, (16.10) and (16.11).
The reduced form error, v2, is a linear function of the structural error terms, u1 and u2.
Because u1 and u2 are each uncorrelated with z1 and z2, v2 is also uncorrelated with z1 and z2.
Therefore, we can consistently estimate p21 and p22 by OLS, something that is used for
two stage least squares estimation (which we return to in the next section). In addition, the
reduced form parameters are sometimes of direct interest, although we are focusing here
on estimating equation (16.10).
When y2 is correlated with u1 because of simultaneity, we say that OLS suffers from
simultaneity bias. Obtaining the direction of the bias in the coefficients is generally complicated, as we saw with omitted variables bias in Chapters 3 and 5. But in simple models, we
can determine the direction of the bias. For example, suppose that we simplify equation (16.10)
by dropping z1 from the equation, and we assume that u1 and u2 are uncorrelated. Then, the
covariance between y2 and u1 is

where s1
2 ??? Var(u1)> ??? 0. Therefore, the asymptotic bias (or inconsistency) in the OLS
estimator of a1 has the same sign as a2/(1 ??? a2a1). If a2 >??? 0 and a2a1<= ??? 1, the asymptotic
bias is positive. (Unfortunately, just as in our calculation of omitted variables bias from
Section 3.3, the conclusions do not carry over to more general models. But they do serve
as a useful guide.) For example, in Example 16.1, we think a2 >??? 0 and a2a1<= ??? 0, which
means that the OLS estimator of a1 would have a positive bias. If a1 -??? 0, OLS would, on
average, estimate a positive impact of more police on the murder rate; generally, the estimator of a1 is biased upward. Because we expect an increase in the size of the police force to
reduce murder rates (ceteris paribus), the upward bias means that OLS will underestimate
##Identifying and Estimating a Structural Equation
the effectiveness of a larger police force.
The mechanics of 2SLS are similar to those in Chapter 15. The difference is that, because we specify a structural equation for each endogenous variable, we can immediately
see whether sufficient IVs are available to estimate either equation. We begin by discussing the identification problem.
We mentioned the notion of identification in Chapter 15. When we estimate a model by
OLS, the key identification condition is that each explanatory variable is uncorrelated with
the error term. As we demonstrated in Section 16.2, this fundamental condition no longer
holds, in general, for SEMs. However, if we have some instrumental variables, we can
still identify (or consistently estimate) the parameters in an SEM equation, just as with
omitted variables or measurement error.

Before we consider a general two-equation SEM, it is useful to gain intuition by considering a simple supply and demand example. Write the system in equilibrium form (that
is, with qs ??? qd ??? q imposed) as
q =??? a1 p+ ??? b1z1+ ??? u1 [16.15]
and
q =??? a2 p +??? u2. [16.16]
For concreteness, let q be per capita milk consumption at the county level, let p be the
average price per gallon of milk in the county, and let z1 be the price of cattle feed, which
we assume is exogenous to the supply and demand equations for milk. This means that
(16.15) must be the supply function, as the price of cattle feed would shift supply (b1 ???< 0)
but not demand. The demand function contains no observed demand shifters.
 If, as in the labor demand
function (16.2), we have an observed exogenous demand shifter-such as income in the
milk demand function-then the supply function would also be identified.
To summarize: In the system of (16.15) and (16.16), it is the presence of an exogenous variable in the supply equation that allows us to estimate the demand equation.
The fact that z1 and z2 generally contain different exogenous variables means that we
have imposed exclusion restrictions on the model. In other words, we assume that certain
exogenous variables do not appear in the first equation and others are absent from the second equation.
The key question is: Under what assumptions can we estimate the parameters in, say,
(16.17)? This is the identification issue. The rank condition for identification of equation
(16.17) is easy to state.
Rank Condition for Identification of a Structural Equation. The first equation in a
two-equation simultaneous equations model is identified if, and only if, the second equation contains at least one exogenous variable (with a nonzero coefficient) that is excluded
from the first equation.
This ensures that at least one of the exogenous variables omitted from the first equation actually appears in the reduced form of y2, so that we
can use these variables as instruments for y2. We can test this using a t or an F test, as in
Chapter 15; some examples follow.
Identification of the second equation is, naturally, just the mirror image of the statement for the first equation. Also, if we write the equations as in the labor supply and
demand example in Section 16.1-so that y1 appears on the left-hand side in both equations, with y2 on the right-hand side-the identification condition is identical.

Estimation by 2SLS
Once we have determined that an equation is identified, we can estimate it by two stage
least squares. The instrumental variables consist of the exogenous variables appearing in
either equation.

## Simultaneous Equations Models with Panel Data
In addition to allowing for simultaneous
determination of variables within each time period, we can allow for unobserved effects in
each equation. In a labor supply function, it would be useful to allow an unobserved taste
for leisure that does not change over time.
The basic approach to estimating SEMs with panel data involves two steps: (1) eliminate the unobserved effects from the equations of interest using the fixed effects transformation
or first differencing and (2) find instrumental variables for the endogenous variables in the
transformed equation. 
yit1 =??? a1yit2 +??? zit1b1+ ??? ai1+ ??? uit1 [16.37]
yit2 =??? a2yit1 +??? zit2b2+ ??? ai2+ ??? uit2, [16.38]
where i denotes cross section, t denotes time period, and zit1b1 or zit2b2 denotes linear
functions of a set of exogenous explanatory variables in each equation. The most general
analysis allows the unobserved effects, ai1 and ai2, to be correlated with all explanatory
variables, even the elements in z. However, we assume that the idiosyncratic structural
errors, uit1 and uit2, are uncorrelated with the z in both equations and across all time periods;
this is the sense in which the z are exogenous. Except under special circumstances, yit2 is
correlated with uit1, and yit1 is correlated with uit2.
 This is because we need an instrument for
???yit2, and a change in a variable from one period to the next is unlikely to be highly correlated with the level of exogenous variables.
After differencing, suppose we have the equation
???hoursit= ??? b0+ ??? a1???log(wageit)+ ??? ???(other factorsit),
and we wish to use ???experit as an instrument for ???log(wageit). The problem is that, because we are looking at people who work in every time period, ???experit ??? 1 for all i and t.
(Each person gets another year of experience after a year passes.) We cannot use an IV
that is the same value for all i and t, and so we must look elsewhere.
Often, participation in an experimental program can be used to obtain IVs in panel data
contexts. In Example 15.10, we used receipt of job training grants as an IV for the change in
hours of training in determining the effects of job training on worker productivity. In fact, we
could view that in an SEM context: job training and worker productivity are jointly determined, but receiving a job training grant is exogenous in equation (15.57).
 The
first year is lost because of the lagging. Then, the usual 2SLS t statistic on the lagged residual is a valid test for serial correlation. In Example 16.8, the coefficient on rit1 is only
about .076 with t ??? 1.67. With such a small coefficient and modest t statistic, we can
safely assume serial independence.

An alternative approach to estimating SEMs with panel data is to use the fixed effects
transformation and then to apply an IV technique such as pooled 2SLS. A simple procedure
is to estimate the time-demeaned equation by pooled 2SLS, which would look like
ÿit1 ???= a1ÿt2 +??? ¨zit1b1+ ??? üit1, t= ??? 1, 2, ., T, [16.42]
where ¨zit1 and ¨zit2 are IVs. This is equivalent to using 2SLS in the dummy variable formulation, where the unit-specific dummy variables act as their own instruments. Ayres and
Levitt (1998) applied 2SLS to a time-demeaned equation to estimate the effect of LoJack
electronic theft prevention devices on car theft rates in cities. If (16.42) is estimated directly, then the df needs to be corrected to N(T ???- 1) -??? k1, where k1 is the total number of
elements in a1 and b1. Including unit-specific dummy variables and applying pooled 2SLS
to the original data produces the correct df. A detailed treatment of 2SLS with panel data
is given in Wooldridge (2010, Chapter 11).

## En résumer
Simultaneous equations models are appropriate when each equation in the system has a ceteris
paribus interpretation. Good examples are when separate equations describe different sides
of a market or the behavioral relationships of different economic agents. Supply and demand
examples are leading cases, but there are many other applications of SEMs in economics and
the social sciences.
An important feature of SEMs is that, by fully specifying the system, it is clear which
variables are assumed to be exogenous and which ones appear in each equation. Given a full
system, we are able to determine which equations can be identified (that is, can be estimated). In
the important case of a two-equation system, identification of (say) the first equation is easy to
state: at least one exogenous variable must be excluded from the first equation that appears with
a nonzero coefficient in the second equation.
As we know from previous chapters, OLS estimation of an equation that contains an
endogenous explanatory variable generally produces biased and inconsistent estimators.
Instead, 2SLS can be used to estimate any identified equation in a system. More advanced
system methods are available, but they are beyond the scope of our treatment.
The distinction between omitted variables and simultaneity in applications is not always
sharp. Both problems, not to mention measurement error, can appear in the same equation.
A good example is the labor supply of married women. Years of education (educ) appears in
both the labor supply and the wage offer functions [see equations (16.19) and (16.20)]. If omitted ability is in the error term of the labor supply function, then wage and education are both
endogenous. The important thing is that an equation estimated by 2SLS can stand on its own.
SEMs can be applied to time series data as well. As with OLS estimation, we must
be aware of trending, integrated processes in applying 2SLS. Problems such as serial correlation can be handled as in Section 15.7. We also gave an example of how to estimate an
SEM using panel data, where the equation is first differenced to remove the unobserved
effect. Then, we can estimate the differenced equation by pooled 2SLS, just as in Chapter 15.
Alternatively, in some cases, we can use time-demeaning of all variables, including the
IVs, and then apply pooled 2SLS; this is identical to putting in dummies for each crosssectional observation and using 2SLS, where the dummies act as their own instruments.
SEM applications with panel data are very powerful, as they allow us to control for unobserved heterogeneity while dealing with simultaneity. They are becoming more and more
common and are not especially difficult to estimate.

# La vigne et le vin en Suisse

En 1960, le vignoble occupait en Suisse une surface de 12 310 ha
et a produit 1 104 000 hl de vin. 
Le prix des vignes ne cesse de monter. On en a vendu cette année
jusqu'à 25 F le m2 (1), on parle même de 37 F. Couramment des vignes
atteignent 10 F le m2, 100 000 F l'ha, 12 millions d'anciens francs.
 En moyenne, la
production équilibre la consommation, mais la surproduction est une
menace permanente, d'autant plus que la désaffection pour le vin
blanc a tendance à s'affirmer, et en particulier pour les vins
blancs secs que produisent les vignobles nationaux 
Les vignerons ne peuvent produire un vin de qualité qu'au prix
d'une culture intensive qui revient très cher. Les traitements se
multiplient, le machinisme pénétre de plus en plus dans le vignoble,
les salaires des indispensables ouvriers augmentent chaque année.
A cause de la poussée des prix industriels et des salaires les frais de
culture vont croissant.
L'enquête s'étend
actuellement à 300 exploitations des différentes régions viticoles de la
Suisse. Le questionnaire ne comprend pas moins de 60 pages et entre
dans tous les détails de la technique et de l'économie des
exploitations.
Les calculs sur le prix de revient révèlent que 45 % du coût de
production sont dus aux travaux manuels. Or, c'est la main-d'ouvre
qui a proportionnellement le plus renchéri. Les ouvriers agricoles
se font de plus en plus rares, leurs salaires et leurs exigences vont
croissant. Leur salaire annuel qui se montait à 1 200 F pendant la
guerre atteint plus de 2 500 F actuellement, et comme leur entretien est entièrement à la charge de leur employeur, le salaire réel se monte
à plus de 5 000 F par an.  
Les vignerons reçoivent des conseils sur le
choix des plants, la façon de les tailler, la vinification du vin rouge. 
 Les nouveaux plants sont plus résistants
aux gelées, à la sécheresse, au mildiou, mais les raisins sont plus
sensibles à la pourriture et sont attaqués par une dégénérescence
infectieuse contre laquelle on sait mal lutter. Leur rendement est en général
plus faible à l'hectare et il faudrait les vendre plus cher que les blancs. 
Cépages. Certains vignerons, dès avant la guerre, avaient pensé
trouver une solution à ce problème par l'abandon des plants européens
peu productifs et fragiles, remplacés par des hybrides producteurs
directs beaucoup plus résistants aux maladies et aux parasites, dont
la culture est très simplifiée et qui donnent des rendements à l'hectare
bien plus élevés.
La taille. L'enquête révèle que les soins donnés aux ceps.
La mécanisation. Le machinisme pénètre de plus en plus dans la
culture de la vigne et sans cesse de nouveaux engins adaptés aux
circonstances apparaissent sur le marché. Des treuils, des tracteurs
enjambeurs, tirant des charrues vigneronnes ou des sarcleuses
permettent d'économiser 60 % du temps nécessaire aux travaux de la
terre. Les traitements chimiques, grâce aux compresseurs et aux
atomiseurs se font aussi beaucoup plus rapidement. 
Le vigneron actuel est mieux armé que ses parents contre les
aléas de la récolte. Il sait doser scientifiquement ses engrais,
sélectionner au mieux ses plants,
L'examen
de ces prix est intéressant : acceptés par les producteurs, ils donnent
une idée de la valeur que doivent atteindre les vins pour que la culture
soit rentable en année moyenne; consentis par les commerçants ils
permettent de classer les crus d'après leur réputation et leur valeur
marchande (1). 

# Le fonctionnement des marchés entre vignoble et négoce dans le secteur des vins d'AOC : Trois études régionales
mieux cerner les conditions à réunir pour assurer l'ajustement offre-demande
de raisin et de vins ;
- mettre en évidence l'effet possible sur les équilibres offre-demande de divers
leviers d'action (vendangeoirs, regroupement de l'offre, création d'outils de
stockage, contrats d'approvisionnement...).

La première étude concernant les vins rosés de Loire a été réalisée en 1998. Elle a pour
objectif d'identifier les mécanismes qui, au sein de cette filière, induisent de fortes variations
des cours et des volumes des transactions entre vignoble te négoce. Le diagnostic débouche
sur des propositions d'action en vue de d'améliorer l'organisation de la filière.

La seconde étude portant sur les appellations du Beaujolais a été réalisée en 2000. Elle se
focalise sur l'analyse de variations de cours et tente d'identifier les déterminants de ces
variations.

La troisième étude sur les Appellations Régionales de Bourgogne a été menée1999. Elle a été
conduite dans le cadre du contrat INRA-BIVB initié par l'équipe INRA-ESR de Dijon. La
partie présentée ici concerne la possibilité d'instaurer des contrats pour améliorer le niveau
qualitatif des vins achetés par le négoce. L'étude propose une démarche de simulation pour
évaluer les impacts possibles du développement de tels types de contrats.

# Deuxième partie :
Etude sur les variations des cours
dans les Appellations de Beaujolais

Les opérateurs du Vignoble et du Négoce de la filière Beaujolais sont confrontés depuis de
nombreuses années à de fortes variations interannuelles des volumes et des prix des produits
qui circulent de la production jusqu'au marché. Ces variations portent par exemple sur :
. les niveau des récoltes et des quantités de vin disponibles chaque année ;
. les stocks dont le niveau peut varier d'une année sur l'autre de façon importante
.les sorties 
. les cours à la production dont les variations d'une année sur l'autre peuvent atteindre +36%
(entre 88/89 et 89/90) et - 27% (entre 90/91 et 91/92) dans le cas des Beaujolais rouges ;
En particulier, les
fluctuations des quantités disponibles et leurs conséquences en termes de prix finaux peuvent
se traduire par des pertes de parts de marchés auprès d'importateurs qui attendent une certaine
régularité dans les volumes et les prix des produits offerts.
Les raisons peuvent en être multiples et mêler des facteurs « exogènes » (aléas
climatiques, par exemple) et d'autres qui tiennent au jeu stratégique des opérateurs les uns par
rapport aux autres (stratégies de « rationnement », par exemple, en vue de maintenir les prix à
la production ; stratégies de « sur-achat » certaines années en raison des difficultés à anticiper
marchés futurs ; constitution de stocks de sécurité...)
- une analyse qualitative des stratégies d'achats et de vente à chaque niveau de la filière
(interfaces Vignoble/Négoce, Vignoble/Coopératives, Coopératives/Négoce, Opérateurs
/Marché) en relation avec les fluctuations observées des volumes et des prix des transactions.
- une analyse quantitative des données disponibles sur les transactions et les flux (volumes et
valeur) de la production jusqu'au marché sur les grandes catégories d'appellations en
Beaujolais. Cette étude économétrique a été réalisée avec la collaboration de S. Lecocq et de
P. Leroy (INRA-ESR).

## Analyse des fluctuations de cours et identification des variables déterminantes
 une divergence entre l'estimation des ventes au moment où la
campagne d'achat se réalise et les ventes effectives.
Tout d'abord, le vignoble n'a
pas de visibilité sur les ventes du négoce et constate en début de campagne un stock initial
propriété faible, ce qui ne l'incite pas à réajuster les cours ; ensuite s'intercale la campagne
des nouveaux qui fonctionne sur une logique de demande finale différente. Si le cours des
primeurs baisse, alors le réajustement a lieu sur les non nouveaux (91/92), sinon à nouveau
une année supplémentaire peut être nécessaire (98/99) pour retrouver un cours d'équilibre.
i un choc positif se produit
sur la demande finale, l'intermédiaire achète non seulement pour recompléter son stock
mais pour tenir compte d'un réajustement à la hausse de son estimation de la demande
future (et réciproquement à l'inverse). A ceci peut s'ajouter une anticipation de
rationnement futur (ou de hausse du cours) qui amplifie à nouveau la commande
d'approvisionnement. Enfin, le décalage temporel entre les achats et la vente ainsi que le
non fractionnement possible des commandes d'approvisionnement amplifient le
phénomène.
 C'est donc l'offre initiale du vignoble et la politique de
gestion de stock du négoce qui semblent les facteurs déterminants du cours des non
nouveaux, plus que les ventes finales du négoce ou les ventes directes du vignoble.

## Analyse quantitative des relations entre variables déterminantes à des fins de gestion prévisionnelle
SIp : Stock Initial Propriété évalué fin Août
SFp : Stock Final Propriété évalué fin Août
E : Enregistrements totaux (achats en vrac du négoce au vignoble)
ENN : Enregistrements Non Nouveaux (=Hors Primeurs)
EN : Enregistrements Nouveaux (=Primeurs)
CMC : Cours moyen de campagne (moyenne sur l'année des cours mensuels des transactions
en vrac entre vignoble et négoce)
CMP : Cours moyen pondéré (coût des approvisionnements du négoce calculé en établissant
la moyenne pondérée des stocks initiaux du négoce achetés au cours de l'année antérieure et
des enregistrements de l'année réalisés au cours moyen de l'année)
CNN : Cours des Non Nouveaux
CN : Cours des Nouveaux
VD : Vente directe (vente du vignoble sur le marché final)
Ce sont donc des données internes au Beaujolais qui ont été utilisées pour reconstituer les flux
entre vignoble et négoce ou en direction du marché final. Dans cette analyse, ne sont pas
intégrées des données externes parce qu'elles ne sont pas disponibles sur des séries d'années
suffisamment longues être significatives sur le plan statistique.
Dans la suite, nous essayons de déterminer les relations entre les variables de sorties et les
cours. Nous commençons par étudier les sorties totales en fonction des cours des
enregistrements ; puis les ventes finales du négoce en fonction des cours ; puis les
enregistrements en fonction des cours.
Quand on examine les relations entre variables deux à deux, on constate que les variations du
CMC expliquent une très faible partie des variations des sorties totales. La relation est
négative (les sorties varient en sens inverse du CMC) mais le R² est très faible : le CMC
n'explique que 4% des variations des sorties totales (tableau 1).
Il existe, par contre, une relation significative entre les sorties totales et le CMP. On constate
que le CMP explique mieux les sorties totales que le CMC : le R² est de 0,41 (le CMP
explique donc 41% de la variation inter-annuelle des sorties totales) (tableau 2).
 La meilleure estimation passe par la prise en compte des stocks, de la récolte, de
la part de nouveaux sur la récolte et des cours des nouveaux. On obtient alors des R² de 0.49
avec le CMC (tableau 3) et de 0.58 avec le CMP (tableau 4).
 Ceci montre
que les cours qui influencent les sorties totales sont par ordre décroissant le CMP, le cours des
nouveaux puis, de façon beaucoup plus faible, le CMC.
- d'une part, les sorties totales sont forcément influencées par des conditions de marché
externes qui ne sont pas prises en compte par le jeu de variables disponibles ;
- d'autre part, les sorties totales comprennent les ventes du négoce et les ventes directes du
vignoble, ces dernières n'étant probablement influencées qu'indirectement par le niveau
des cours. Cette remarque est attestée par le tableau 5 dans lequel on voit que les sorties en
vente directe ne sont expliquées par le CMC qu'avec un R² de 0.2.
Pour ces deux raisons, on se concentre maintenant sur les seules ventes finales du négoce.
On trouve des relations significatives entre les ventes finales du négoce et les cours des
transactions entre vignoble et négoce. De façon générale, ces relations sont négatives (les
ventes finales varient en sens inverse des cours). Si on s'en tient à ces deux seuls éléments, on
obtient des R² d'environ 0,4. Cela signifie qu'on explique environ 40% de la variabilité des
ventes du négoce à partir des seules informations sur les cours.
Si on ajoute des variables de stocks, la récolte, la part des beaujolais nouveaux dans la récolte
et le cours des nouveaux, on améliore sensiblement l'explication de la variabilité des ventes
finales puisqu'on atteint des R² d'environ 0,6. Avec cet ensemble de variables, on explique
donc environ 60% de la variabilité des sorties du négoce.
Le CMP, associé à des variables de stocks, à la récolte, la part des nouveaux sur la récolte, est
donc clairement un facteur explicatif des sorties du négoce.
- Les enregistrements totaux ne sont pas reliés de façon significative aux cours moyens de
campagne. Ceux-ci sont fondamentalement déterminés par le niveau des stocks du vignoble,
la récolte et le volumes des achats en nouveau. Ces variables permettent d'expliquer plus de
60% des variations du CMC (tableau 8). On peut comprendre cela dans la mesure où la
tension offre-demande sur ce marché peut probablement être approchée par, d'un côté, les
stocks vignoble et la récolte (offre), de l'autre côté, le rapport du volume d'achats en
nouveaux sur la récolte (demande anticipée).
Les cours des non nouveaux (BJL et Villages non nouveaux et ensemble des Crus) sont
déterminés de façon significative par les stocks initiaux du vignoble (STINIV), la récolte en
non nouveaux (RECOHN), les enregistrements en non nouveaux (ENN), les enregistrements
en nouveaux (EN) et le cours des nouveaux (CN). Les cours des non nouveaux varient dans le
même sens que les enregistrements en nouveaux et non nouveaux ainsi que dans l emême sens
que les cours des nouveaux ; ils varient en sens inverse des stocks du vignoble et de la récolte
hors nouveaux.
On explique aussi de façon très significative les enregistrements en non nouveaux. Les cours
des non nouveaux sont dans une relation positive avec les enregistrements. Pour des raisons
techniques (absence de variable non corrélée aux ENN permettant « d'instrumenter » les cours
des non nouveaux), on ne peut cependant pas prendre en compte les CNN dans la prévision
des enregistrements. La meilleure relation obtenue est la suivante :
![](images/modèles.jpg)
Elle montre que la meilleure base
pour faire des prévisions reste la détermination simultanée des cours et des enregistrements en
non nouveaux, une fois connue la campagne des nouveaux.
En l'absence de phénomène nouveau (ne s'étant pas produit dans les dix dernières années) et
si les comportements des acteurs obéissent à des logiques de même nature que lors des années
passées, les cours et volumes des enregistrements en non nouveaux devraient se situer aux
niveaux indiqués par la prévision simultanée.
Pour faciliter la construction de ces scénarios, le comité interprofessionnel doit s'entendre sur
les hypothèses clés à formuler (dont la réalisation sera suivie) pour expliquer les variations de
vente par circuit de distribution d'une année à l'autre : taux de change et conjoncture par
grande zone géographique pour l'export, concurrence des grands vignobles français pour la
GMS, efforts de promotion, état des stocks dans les circuits. Pour être en mesure de bâtir ces
scénarios le plus rapidement possible et d'en suivre la réalisation, un effort doit être accompli
en matière de remontée et de communication de données concernant l'aval de la filière au
comité de pilotage : évolution des stocks du négoce par appellation, indicateurs sur les stocks
et ventes finales par circuit de distribution.

# Les facteurs déterminants les prix du vin
prend en compte la pluralité des situations d'échange ainsi que le rôle spécifique de
l'information. L'analyse peut aussi prendre en compte la relation entre le prix d'un vin,
déterminé par ses caractéristiques individuelles, et la tendance du marché. Finalement, dans
une situation d'offre et de demande, l'effet de rareté peut aussi expliquer certains prix. 

Pourtant, la
médiatisation du marché du vin a déplacé l'attention sur une minorité de domaines prestigieux
pour lesquels l'image, la rareté, la spéculation, le rêve, sont les déterminants du prix sur des
critères proches du monde du produit de luxe ou de l'ouvre d'art. Pour l'immense majorité des
domaines viticoles, cette logique ne s'applique pas. 
En revanche, si les acheteurs sont des profanes, ce qui est la situation la plus fréquente, le prix
devient une variable contrôlée par les offreurs pour suggérer un niveau de qualité. 
##Revue de la littérature sur la formation du prix du vin
Facteurs géo-viticoles Climat Sol Région Cépage
Facteurs temporels Age Millésime
Coûts de production Coûts fixes et variables Type de vin Rendement
Information Etiquette Appellation Réputation
Offre et demande Rareté Culture

Le rendement dépend lui-même du type et de l'âge des vignes, du climat et des
conditions météorologiques, et de la qualité du travail du vigneron. 

 Le prix est une variable mais les producteurs
utilisent généralement plusieurs critères pour signaler au consommateur la qualité de leur vin :
le style de la bouteille, le dessin de l'étiquette, des campagnes de publicité 

Elle permet d'intégrer dans l'évaluation du prix les facteurs géo-viticoles
(Jones & Storchmann (2001), les facteurs temporels (Horowitz et al. (2002), Ashenfelter
(2008), et surtout des facteurs liés à l'information révélée sur le marché (Lecocq & Visser
(2006), Hadj et al (2008), Miler et al. (2007), Horowitz et al. (2002).

Le principal inconvénient de la méthode dite des prix hédonistiques est qu'elle est uniquement
conçue pour s'appliquer aux cas d'information parfaite. Si les acheteurs sont des profanes, ce
qui est la situation la plus fréquente, le prix devient une variable contrôlée par les offreurs
pour suggérer un niveau de qualité. On est dans une situation typique d'asymétrie
d'information. La formation des prix dépend essentiellement de la réputation perçue par le
consommateur et cette réputation est évolutive dans le temps

## Le marché peut-il évaluer les différences entre les prix des vins ?

On suppose que tous les individus ont les mêmes anticipations sur le prix et la qualité des
vins, qu'ils sont rationnels et cherchent à maximiser leurs achats. Il existe un nombre fixe de
produits pour une quantité connue et tous les individus ont accès à une même cave potentielle
(un catalogue de vins) qui représente le marché. Selon les hypothèses habituelles de
concurrence, les produits sont tous parfaitement disponibles et il n'y a pas de coûts de
transaction ni d'imperfections sur le marché.
 L'espérance et la variance du
rendement sur le marché dépendent à la fois des caractéristiques du vin et des caractéristiques
du marché.
 Le portefeuille de marché capte les
effets qui affectent en moyenne l'ensemble des vins (inflation générale à la production,
millésime par exemple).7
 Par contre la mesure de la volatilité (l'élasticité) qui existe entre le
prix du vin et le prix du marché capte la qualité associée à un vin en particulier et permet de
différencier les AOC réputées des autres.
 L'existence d'un marché garantit la convergence des
prix en fonction de la qualité des vins. Le prix d'un vin est donc conditionné par un ensemble
de variables qui sont captées par le marché et la sensibilité des prix du vin par rapport au
marché est révélatrice des caractéristiques propres au vin.
L'hétérogénéité des prix est fortement liée au système d'appellation d'origine mis en place en
France et qui lève en partie l'asymétrie d'information sur la qualité des vins.8
 Mais cette
hétérogénéité n'est pas aléatoire et dépend largement de la relation avec le marché. On peut
considérer que les vins ayant la même sensibilité par rapport au marché appartiennent à la
même catégorie de vins et on peut ainsi classer les vins par appellation, de la plus prestigieuse
à l'appellation la plus ordinaire.

## Le cas de la réputation individuelle et l'effet de rareté
 la réputation individuelle du vin ou du producteur à une incidence majeure sur le prix. Cette réputation individuelle est elle-même souvent liée a l'image crée par
des experts. 
L'approche walrassienne du marché concurrentiel où les prix résultent de la rencontre d'une
offre et d'une demande, n'est-elle pas toujours valable? Walras considère que la valeur d'un
bien est fonction de sa rareté. Il a été démontré (Shannon, 1948) que la rareté d'un bien peut
être appréhendée par la probabilité de disposer de ce bien. Cette mesure entropique peut être
représentée graphiquement (Graphique 1) et montre que la valeur (le prix) d'un bien est une
fonction croissante de la rareté (ou de la probabilité décroissante de disposer de ce bien). Une
telle analyse est-elle applicable à la formation du prix du vin ? 

La rareté sur le marché peut être due à une limitation de l'offre réelle (production limitée) ou
provoquée (le marketing de la rareté), à un coût de production très élevé, ou à une demande
accrue due à une cause de popularité ou un effet de mode ou de collection. Les exemples sont
nombreux dans le domaine du vin et pourtant l'analyse de la relation prix-rareté n'a pour
l'instant pas eu d'application sur le prix des vins rares. Les voies de recherches envisageables
peuvent concerner aussi bien la distribution que le comportement du consommateur.
Outreville(2009) étudie l'effet de rareté du à de la taille des bouteilles de champagne sur le
prix. Si l'on calcule le prix relatif du vin par rapport à la taille de la bouteille, il varie d'un
facteur de 1.0 pour une bouteille standard (0,75cl) à plus de 2.0 pour les grandes bouteilles
(Balthazar, Nabuchodonosor). Le prix augmente quand la taille de la bouteille augmente
quelle que soit le nom du producteur ou la qualité du vin mis en bouteille. L'hypothèse de
rareté de Walras est confirmée puisque la valeur du vin augmente en fonction de la taille des
bouteilles et donc de la rareté de l'offre. 
##Conclusion
Produit de consommation mondialement répandu, le vin obéit aujourd'hui à des normes de
production bien établies et fait l'objet, sur un marché concurrentiel, d'une offre et d'une
demande qui s'équilibrent en fonction de son prix.
Son prix
porte donc la trace d'un grand nombre facteurs qui déterminent son niveau et son évolution.
Pour reprendre les termes de Chiffoleau et Laporte (2004), l'analyse de la formation des prix
est aussi liée aux situations d'incertitude sur la qualité du produit. 
Mais le vin ne saurait être réduit à une simple fonction d'usage. Il y a l'objet marchand,
produit de consommation, l'objet culturel, et pour quelques cas d'exception, l'objet de
collection et de toutes les convoitises.

## Article n°13 : Les réponses de la recherche aux nouveaux enjeux de l’économie vivi-viticole.

Secteur vini-viticole : emblématique des recherché en agro-alimentaire. Différenciation des produits au niveau consommateur + signes officiels de qualité plus développés en amont (protection de l’environnement, innovations des modes d’encéagement…) et à aval(techniques de vinification, système de commercialization…). D’où une complexification de l’arbitrage “rapport qualité/prix” au niveau consommateur.
Viticulture : decisive dans l’activité économique et sociale dans certaines regions (peut dépasser 30% de la production agricole finale) mais réalités différentes d’une region viticole à l’autre (dimension du vignoble, degré de specialisation des exploitations et des types de vins produits)
Diversité offre de vin : illustre richesse culturelle du secteur et garantit une meilleure segmentation des marches + brouillage de l’information donnée au consommateur + multiplication des comportements opportunistes des intervenants sur le marché.
Questions de recherché : l’offre de vin est-elle adaptée à la demande des consommateurs (actuels et futurs)? Faut-il maintenir et renforcer les marques collectives symbolisées par les signes de qualité (appellation d’origine : AOP, AOC…etc.)? Faut-il harmoniser à un niveau international  l’ensemble des règles de production et de consummation agissant sur les échanges internationaux?
Valorisation de la production via les marques-ombrelles : gamme restreinte de produits standardizes facilement identifiables par le consommateur ( + bonnes relation avec la grande distribution.
Mode de production issu des cooperatives, grands groupes industriels dans le secteur du négoce : entraine relations complexes d’échanges de matières premières ou de produits finis d’où difficulties de coordination puis difficultés de commercialisation des produits sur les marchés d’exportation via la grande distribution alimentaire.
Schéma opposé de production viticole : artisanat de la production de vin mais moins present au niveau national
Bilan mitigé de l’intérêt des AOC : crédibilité du système français car element de marketing percutant ou bien promotion privée de marques industrielles ?
Critères de la demande des consommateurs sur le vin : plutôt des elements hors-prix (caractéristiques des produits) : critères organoleptiques (gout, odeur), critères marketing, reputation des divers signes de qualité  (classement, mention “vieilli en fût de chêne), respect de l’environnement, mode de distribution du vin (vente directe, caviste, grande distribution…etc.)
Travaux antérieurs : analyses économétriques sur la substitution vin/ autres boissons alcoolisées/ soft drinks/ eaux minerals : recours aux elasticités-prix croisées : utilization souvent du modèle AIDS (Almost ideal demand system). En France, forte substitution entre vin de qualité et eaux minerals (et degree moindre pour les soft drinks) mais bière produit complémentaire du vin.
Aujourd’hui : relation qualité prix des produits étudiée via method des prix hédoniques : regression prix sur caractéristiques et mesure de la disponibilité à payer des consommateurs. Constat : caractéristiques objectives (classement, millésime…) expliquent plus les differences de prix que les caractéristiques sensorielles (car ces dernières sont plus difficiles à apprécier).
Réussite économique d’un système d’organisation et production viticole : ne depend pas essentiellement de la manière de se signaler auprès des consommateurs mais surtout de l’efficacité productive de la filière et de son mode d’organisation (du producteur au consommateur : prise en compte des intermediaries comme les négociants et les distributeurs).
Problème de dissociation de la production du raisin de celle du vin : approvisionnement en raisin qui n’arrive pas à suivre les investissements sur la promotion privée de la marquee de vin

## Texte 14 : La regulation des pesticides : le cas du vin français.

Question de recherche : Etude des effets potentiels de l'introduction d'une taxe sur les pesticides sur le marché français du vin.
Cadre d’analyse du marché étudié : oligopole non réglementé.
Constat : Le régime d’appelation favorise l’utilisiation des pesticides tout en limitant la quantité de raisins  utilisée dans la production de vin par rapport à la situation non réglementée.
 Raisons: les rendements des plantation viticoles sont limitées par certaines réglementation professionnelles si bien que l’utilisation des pesticides est peu sensible à un faible niveau d’imposition car incitation à administrer une plus forte dose de pesticides sur les cultures pour permettre aux producteurs de garantir une bonne qualité du vin produit et donc la perception de revenus suffisants pour que leur exploitation perpétue dans le temps.
Une réglementation économique à elle seule n’est pas suffisante pour limiter les dommages environnementaux commis par les pesticides. 

## Texte n°15 : Segmentation du marché du vin en fonction du prix : régression hédonique lorsque des prix différents signifient des produits different

Estimation de fonctions hédoniques spécifiques aux gammes de prix des vins car on cherche à montrer que le marché du vin est segmenté en plusieurs classes de produits ou segments de marché. 
Déterminants du prix du vin : essentiellement caractéristiques objectives (classement établit par experts, le millésime…) mais peu d’influence des caractéristiques sensorielles (meme raison qu’avant, caractéristiques difficielement identifiables, peu de consommateurs sont de vrais connaisseurs du produit donc l’avis des experts agit comme un signal de qualité sur les consommateurs ce qui impacte les prix du vin). En particulier, intéressant d’intégrer les côtes de vin octroyées par des revues spécialisées pour modéliser les prix du vin. 
De plus, la région de production, les différences de coûts de production et les effets de la collectivisation - la reputation du territoire et le millésime sont souvent présentés comme des variables significatives.
Conclusion : Nous constatons qu'un modèle qui tient compte de l'existence des classes de vin est plus en mesure d'expliquer la variabilité des données et produit des résultats plus précis et interprétables en ce qui concerne les prix implicites des attributs. L'analyse identifie les classes de vins en fonction de fourchettes de prix ainsi que d’informations hors échantillon relatives à l'existence de différents segments de vin. En spécifiant des fonctions hédoniques pour différentes catégories de produits, nous trouvons la preuve que les consommateurs apprécient les mêmes attributs du vin différemment d'une catégorie à l'autre. Toutefois, il serait possible d’améliorer cette segmentation (meilleur decoupage des classes de vin) en recourant à des informations complémentaires au prix  afin de determiner de manière endogène le nombre de segments de prix.

## Texte n°16 : Le goût ou la réputation : ce qui motive  les prix du marché dans l'industrie vitivinicole ? Estimation d'un modèle hédonique pour Vins italiens haut de gamme

Le but de cet article est de fournir de nouvelles preuves sur les facteurs affectant les prix du vin pour des raisons à la fois méthodologiques et factuelles. Sur le plan méthodologique, cette étude est la première à appliquer une méthode générale de Box-Cox via une transformation dans le cadre de modèles hédoniques qui exploitent toutes les variables (caractéristiques objectives et sensorielles, réputation) mises en avant par la littérature antérieure comme étant pertinentes dans la détermination des prix de marché du vin. Sur le plan factuel, l'article comble le manque de preuves empiriques sur la question pour l'Italie, l'un des principaux producteurs de vin, en utilisant un large ensemble de données sur deux vins de première qualité (Barolo et Barbaresco) couvrant les millésimes 1995-1998. Les données ont été collectées via une inspection des publications sur le vin ainsi que des entrevues avec les producteurs. Les résultats corroborent les preuves obtenues à partir de données antérieurs provenant d'autres pays, montrant que les traits sensoriels, la réputation des vins et des producteurs, ainsi que des variables objectives sont tous des facteurs importants qui influencent la volonté de payer des consommateurs. Ces resultats ont été obtenues via une estimation générale Box-Cox des différents modèles sensoriels (CLV) et de réputation (LS), qui n'imposent pas de restrictions préalables sur la forme de la fonction de prix hédonique. Plus important encore, en recourant à un test statistique non intégré (Vuong, 1989) entre les deux modèles d’estimation présentant deux spécifications alternatives (goût et reputation ) du prix du vin,  on constate que le modèle de réputation surpasse largement celui du goût, ce qui suggère qu'une plus grande quantité d'informations sur la façon dont le prix du vin est formé est contenue dans le modèle avec spécification de la réputation.
Implication pertinente pour strategies des enterprises : cela suggère avant tout que les producteurs visent à se forger une réputation bien établie - tant au niveau du vin qu'au niveau de l'entreprise - par des activités promotionnelles (par exemple, la participation à des expositions de vins) qui facilitent les citations dans des guides bien connus tout ceci pour accroître le prix de vente de leur vin.

## Texte n°17 : L'IMPACT DE LA DEMANDE ET DU FOURNISSEUR SUR LA PERFORMANCE DE LA CHAÎNE D'APPROVISIONNEMENT DU VIN

ce rapport vise à identifier et à mesurer l'effet des variables latentes  telles que la demande, les fournisseurs, la qualité et les délais de livraison sur les bénéfices financiers des caves. Les variables latentes ont été intégrées par les variables observées et un questionnaire a été distribué à 64 établissements vinicoles de La Rioja, en Espagne. Une analyse descriptive apparaît pour les indicateurs et les variables latentes qui ont été reliés à l'aide de la modélisation par équation structurelle. Les résultats indiquent qu'il n'y a pas d'effet direct entre une prévision efficace de la demande et des fournisseurs sur les bénéfices financiers et sociaux des établissements vinicoles, mais qu'il y a des effets indirects sur la qualité et le délai de livraison, la variable la plus importante analysée. Les responsables de chais doivent porter leur attention sur la qualité et les délais de livraison aux clients, tout en ayant une relation directe avec les fournisseurs. Les directeurs de cave doivent s'efforcer de disposer d’ une prédiction rapide des variantes dans la prévision et ajuster la commande aux fournisseurs et au processus de production afin que la chaîne d'approvisionnement puisse être plus agile et avec des livraisons de qualité et dans les délais, le résultat final étant le bénéfice financier de l'entreprise. Une chaîne d'approvisionnement agile avec des produits de qualité semble être insignifiante pour les gestionnaires des vignobles.

## Texte n°18 : QUE CONTIENT LA BASE DE DONNÉES ANNUELLE DES MARCHÉS MONDIAUX DU VIN, DE 1835 À 2016 ?

Cet article documente une nouvelle base de données annuelle unique en son genre sur les marchés mondiaux du vin, couvrant la période de 1835 à 2016. La base de données élargit considérablement les possibilités de mener des études sur la production, la consommation et le commerce du vin dans une perspective historique pour le monde dans son ensemble et pour la plupart des pays concernés. La combinaison de ces informations de base avec d'autres variables économiques telles que le PIB réel, la population, le commerce total de marchandises, et la consommation d'autres boissons alcoolisées  a permis de générer une myriade de variables dérivées qui sont utiles pour des analyses comparatives ainsi que pour étudier les deux vagues de la mondialisation.
Structure de la base de données :  La base de données fournit des informations quantitatives historiques pour tous les pays clés impliqués dans la production, la consommation et le commerce du vin. Elle dispose également de données auxiliaires sur des variables macroéconomiques telles que la superficie totale des cultures agricoles, la population totale et la population adulte, le PIB réel aux prix de 1990, les taux de change courants de la monnaie locale par rapport au dollar américain, les exportations et importations totales de marchandises, les volumes de production, la consommation et le commerce de la bière, et la consommation des autres boissons alcoolisées et donc de tout alcool (exprimée en litres d'alcool). Ces indicateurs sont précieux pour le calcul d'indicateurs intensifs, par exemple par habitant et par dollar du PIB réel, afin de compenser les différences de taille des pays. Les années couvertes diffèrent d'une variable à l'autre en fonction de la disponibilité des données. Bien que certains tableaux commencent avant le XIXe siècle, la base de données principale commence en 1835 mais n'est pas complète pour tous les pays à partir de cette année. Il existe des données complètes pour le monde entier en termes de superficie plantée en vigne à partir de 1900.
Deux organisations internationales se sont spécialisées dans la compilation de données agricoles mondiales : l'Institut international de l'agriculture (IIA) et l'Organisation des Nations Unies pour l'alimentation et l'agriculture (FAO). Ils constituent la principale source de données sur les superficies viticoles, la superficie totale des cultures agricoles, la production de vin et de bière et les volumes commerciaux. Les données publiées par l'IIA et la FAO sont basées sur les statistiques nationales officielles de chaque pays membre. Leur qualité dépend donc de la qualité des données rassemblées par les offices statistiques nationaux.
