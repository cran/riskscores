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


## ----setup--------------------------------------------------------------------
library(riskscores)

## ---- eval = FALSE------------------------------------------------------------
#  data("breastcancer")
#  

## -----------------------------------------------------------------------------
y <- breastcancer[[1]]
X <- as.matrix(breastcancer[,2:ncol(breastcancer)])


## -----------------------------------------------------------------------------
foldids <- stratify_folds(y, nfolds = 5, seed = 1)


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
  filter(Score %in% seq(25, 200, 25)) %>%
  kable(caption = "`mod$score_map`")

## -----------------------------------------------------------------------------
get_risk(mod, score = 125)

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
                                             "BareNuclei",
                                             "BlandChromatin"))],
                       score, link, response) %>%
  kable("html",
        booktabs = T,
        col.names = c("CT","BN", "BC", 
                     "'score'", "'link'", "'response'"),
        caption = "Comparison of `predict()` outputs") %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c("Covariates" = 3, "Prediction" = 3))


## -----------------------------------------------------------------------------
plot(mod, score_min = 25, score_max = 200)

