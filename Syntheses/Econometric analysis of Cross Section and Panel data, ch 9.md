# Simultaneous Equations Models


## The scope
When an equation in an SEM has economic meaning in isolation from the other equations in the system, we say that the equation is *autonomous*.  
Causality is closely tied to *the autonomy requirement*. 
An equation in an SEM should represent a causal relationship; therefore, we should be interested in varying each of the explanatory variables—including any that are endogenous — while holding all the others fixed.  
Examples that fail *the autonomy requirement* often have the same feature: the endogenous variables in the system are all choice variables of the same economic unit.

## Identification in a Linear System
A linear system is constructed of a set of are *structural equations* ($G_{(g)}$) for the *endogenous variables* ($y_{(g)}$), explained by themselves and a set of *exogenous variables* ($z_{(g)}$).  
Generally, in the system, the error ug in equation g will be correlated with $y_{(g)}$ (we show this correlation explicitly later), and so OLS and GLS will be inconsistent.
Nevertheless, under certain identification assumptions, we can estimate this system using the instrumental variables procedures.  
To identify a curve we need at least one element in $z_{(g)}$, that is not also in $z_{(i \neq g)}$.
#### Theorem (Order Condition with Exclusion Restrictions):
In a linear system of equations with exclusion restrictions, a necessary condition for identifying any particular equation is that the number of excluded exogenous variables from the equation must be at least as large as the number of included right-hand-side endogenous variables in the equation.  
*It is important to remember that the order condition is only necessary, not su‰cient, for identification. If the order condition fails for a particular equation, there is no hope of estimating the parameters in that equation. If the order condition is met, the equation might be identified.*

## General Linear Restrictions and Structural Equations
#### Theorem (Rank Condition for Identification): 
Let $\beta_1$ be the $(G+M)x1 vector of structural parameters in the first equation, with the normalization restriction that one of the coe‰cients on an endogenous variable is $-1$. 
Let the additional information on $\beta_1$ be given by restriction $R_1 \beta_1 = 0$. Then $\beta_1$ is identified if and only if the rank condition holds.  
Here $R_1$ is a $J_1 (G+M)$ matrix, $J_1$ is the number of
restrictions on $\beta_1$ (in addition to the normalization restriction).
#### Theorem (Order Condition for Identification): 
In system under assumption, a necessary condition for the first equation to be identified is
$$J_1 > = G - 1$$
where $J_1$ is the row dimension of $G_1$. 
This equation is the general form of the order condition.
We can summarize the steps for checking whether the first equation in the system is identified.
1. Set one element of $\gamma_1$ to $-1$ as a normalization.
2. Define the $J_1 (G+M)$ matrix $R_1$ such that equation  captures all restrictions on $\beta_1$.
3. If $$J_1 < G - 1$$, the first equation is not identified.
4. If $$J_1 > = G - 1$$, the equation might be identified.  

## Unidentified, Just Identified, and Overidentified Equations