## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(magrittr)
library(pROC)

set.seed(5)


## ----setup--------------------------------------------------------------------
library(riskscores)

## ---- eval = FALSE------------------------------------------------------------
#  data("breastcancer")
#  

## -----------------------------------------------------------------------------
y <- breastcancer[,1]
X <- as.matrix(breastcancer[,-1])


## -----------------------------------------------------------------------------
foldids <- stratify_folds(y, nfolds = 5, seed = 5)


## -----------------------------------------------------------------------------
cv_results <- cv_risk_mod(X, y, foldids = foldids, nlambda = 25)

## ---- fig.width = 5, fig.height = 3, dpi = 125--------------------------------
plot(cv_results)

## -----------------------------------------------------------------------------
cv_results$lambda_min

## -----------------------------------------------------------------------------
cv_results$lambda_1se

## -----------------------------------------------------------------------------
tail(cv_results$results)

## -----------------------------------------------------------------------------
mod <- risk_mod(X, y, lambda0 = cv_results$lambda_1se)


## ---- echo = FALSE------------------------------------------------------------
mod$model_card %>%
  kable(caption = "`mod$model_card`")

## ---- echo = FALSE------------------------------------------------------------
mod$score_map %>%
  filter(Score %in% seq(30, 300, 30)) %>%
  kable(caption = "`mod$score_map`")

## -----------------------------------------------------------------------------
get_risk(mod, score = 150)

get_score(mod, risk = 0.8270133)

## -----------------------------------------------------------------------------
get_metrics(mod, threshold = seq(0.1, 0.9, 0.1))

## -----------------------------------------------------------------------------
summary(mod)


## -----------------------------------------------------------------------------
coef(mod) # equivalently: mod$beta


## -----------------------------------------------------------------------------
coef(mod) * mod$gamma


## -----------------------------------------------------------------------------
coef(mod$glm_mod)


## ---- echo = FALSE------------------------------------------------------------


link <- predict(mod, type = "link")[1:5] %>%
  round(2)
response <- predict(mod, type = "response")[1:5] %>%
  round(3)
score <- predict(mod, type = "score")[1:5]

data.frame(X[1:5,which(dimnames(X)[[2]] %in% c("ClumpThickness",
                                               "UniformityOfCellShape",
                                               "BareNuclei",
                                               "BlandChromatin"))],
                       score, link, response) %>%
  kable("html",
        booktabs = T,
        col.names = c("CT", "UCS", "BN", "BC", 
                     "'score'", "'link'", "'response'"),
        caption = "Comparison of `predict()` outputs") %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c("Covariates" = 4, "Prediction" = 3))


## -----------------------------------------------------------------------------
plot(mod, score_min = 30, score_max = 300)

