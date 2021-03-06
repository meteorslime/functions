## Basic stuff
#install.packages("installr")
#library(installr)
#updateR()

#data()
#setRepositories()

## Function definitions

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## Standard error
#sd(x)/sqrt(length(x))
se <- function(x) {
  return(sd(x, na.rm = T)/sqrt(length(na.omit(x))))
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## AIC(c)
aicc = function(modelname) {
    LL <- logLik(modelname)
    K <- 2
    n <- 909
    AIC <- -2*LL + 2*K
    m.aicc <- AIC + 2*K*(K+1)/(n-K-1)
    return(m.aicc)
    rm(LL, K, n, AIC, m.aicc)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## R-squared
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## VIF
vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## AIC(c) table generation function
aicc.table <- function(models = models, dataset = dataset) {

## Calculate AICc values
models.aicc <- as.numeric()

for (i in models) {
  models.aicc[i] <- aicc(get(i))
}
rm(i)

## List variables
vars <- names(dataset[7:length(names(dataset))])

## Create data frame
aicc.df <- data.frame(
  Model = c("NULL", "GLOBAL", vars), 
  AICc = round(models.aicc, digits = 2), stringsAsFactors = F
)

## Delta-AICc
aicc.df$Delta <- round((aicc.df$AICc - min(aicc.df$AICc)), digits = 2)

## Akaike weight
library(MuMIn)
aicc.df$w <- round(Weights(models.aicc), digits = 2)

## Calculate r2 values
models.r2 <- as.numeric()

for (i in models) {
  models.r2[i] <- r2.corr.mer(get(i))
}
rm(i)

## Add r2 column to table
aicc.df$r2 <- round(models.r2, digits = 2)

## Sort by AICc
aicc.df <- aicc.df[order(aicc.df$AICc), ]

return(aicc.df)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## Univariate AIC(c) table generation function
univariate.table <- function(models = models, dataset = dataset) {

## Calculate AICc values
models.aicc <- as.numeric()

for (i in models) {
  models.aicc[i] <- aicc(get(i))
}
rm(i)

## List variables
vars <- models

## Create data frame
aicc.df <- data.frame(
  Model = c(vars), 
  AICc = round(models.aicc, digits = 2), stringsAsFactors = F
)

## Delta-AICc
aicc.df$Delta <- round((aicc.df$AICc - min(aicc.df$AICc)), digits = 2)

## Akaike weight
#install.packages("MuMIn")
library(MuMIn)
aicc.df$w <- round(Weights(models.aicc), digits = 2)

## Calculate r2 values
models.r2 <- as.numeric()

for (i in models) {
  models.r2[i] <- r2.corr.mer(get(i))
}
rm(i)

## Add r2 column to table
aicc.df$r2 <- round(models.r2, digits = 2)

## Sort by AICc
aicc.df <- aicc.df[order(aicc.df$AICc), ]

return(aicc.df)
}