source("Code/process_features.R")

library(MASS)
library(pscl)

# inter_features is a list of feature name sets for interaction
# names in features should not be in inter_features
# ignore is a set of columns in the dataframe to ignore
get.features <- function(features, df, ignore=NULL, inter_features=NULL, except=FALSE) {
  if (except) {
    if (!is.null(ignore)) {
      df <- df[, ! names(df) %in% ignore]
    }
    
    possible.features <- names(df)
    features <- possible.features[! possible.features %in% features]
    features <- paste(features, collapse="+")
  } else {
    features <- paste(features, collapse="+")
  }
  
  if (!is.null(inter_features)) {
    inters <- ""
    for (inter in inter_features) {
      inters <- paste(inters, paste(inter, collapse=":"), sep="+")
    }
    features <- paste(features, inters, sep="")
  }
  features
}

get.model <- function(df, 
                      response, 
                      features, 
                      bin.features="1", 
                      zero=TRUE, 
                      family="negbin") {
  if (zero) {
    formula <- as.formula(sprintf("%s ~ %s | %s", response, features, bin.features))
    zeroinfl(formula, data=df, dist=family)
  } else {
    formula <- as.formula(sprintf("%s ~ %s", response, features))
    if (family == "negbin") {
      glm.nb(formula, data=df)
    } else {
      glm(formula, data=df, family=family)
    }
  }
}

get.rate.ratio <- function(beta, std.beta) {
  rate.ratio <- exp(beta)
  std.rate.ratio <- sqrt(std.beta^2 * exp(2 * beta))
  
  list(rate.ratio, rate.ratio - 1.96 * std.rate.ratio, rate.ratio + 1.96 * std.rate.ratio)
}

# normalized sum of squared pearson residuals for glm model
get.pearson.chi.square <- function(model) {
  resids <- resid(model, type="pearson")
  df <- model$df.residual
  sum(resids^2) / df
}

# aic for zero inflated model object
get.aic <- function(model) {
  loglik <- model$loglik
  df <- length(model$coefficients$count) + length(model$coefficients$zero)
  2 * (df - loglik)
}

# Necessary sets of features for replication
responses <- c("hosp.stays", "er.visits", "doctor.visits",
               "w2.hosp.stays", "w2.er.visits", "w2.doctor.visits")
to.ignore <- c(responses, c("id", "died", "smoking", "child.econ.sit", "home.rems",
                            "mon.income", "alcohol", "ponce", "mayaguez", "arecibo",
                            "zona este", "ill.score"))
frailty.covs <- get.features(c("pre.frail", "frail"), data)
with.dem.covs <- get.features(c("pre.frail", "frail", "age", "is.male", "negro",
                                                      "mulato", "mestizo", "otra",
                                                      "education"), data)
with.all.covs <- get.features(c(), data, ignore=to.ignore, except=TRUE)
with.dia.inter <- get.features(c(), data, ignore=to.ignore, except=TRUE,
                               inter_features = list(c("pre.frail", "diabetes"), c("frail", "diabetes")))
with.cog.inter <- get.features(c(), data, ignore=to.ignore, except=TRUE,
                               inter_features = list(c("pre.frail", "cog.func"), c("frail", "cog.func")))
bin.common <- get.features(c("vd.score", "kl.score", "depress"))

# Features for my analysis
# Different Binomial Covariates
hosp.bin.covs <- get.features(c("vd.score", "kl.score", "depress", "age", "none"), data)
er.bin.covs <- get.features(c("vd.score", "kl.score", "depress", "barriers", "none", "medicare", "private"), data)

# With new covariates
my.to.ignore <- c(responses, c("id", "died"))
my.with.all.covs <- get.features(c(), data, ignore=my.to.ignore, except=TRUE)
my.with.dia.inter <- get.features(c(), data, ignore=my.to.ignore, except=TRUE,
                               inter_features = list(c("pre.frail", "diabetes"), c("frail", "diabetes")))
my.with.cog.inter <- get.features(c(), data, ignore=my.to.ignore, except=TRUE,
                               inter_features = list(c("pre.frail", "cog.func"), c("frail", "cog.func")))

my.hosp.bin.covs <- get.features(c("vd.score", "kl.score", "depress", "education", "alcohol", "ill.score", "frail"), data)
my.er.bin.covs <- get.features(c("vd.score", "kl.score", "depress", "age", "private", "medicare", "ill.score"), data)

### Models from which results were obtained
## Baseline
# frailty and response association
# zi nb hosp
summary(get.model(data, "hosp.stays", with.all.covs, bin.common))
# zi nb er
summary(get.model(data, "er.visits", with.all.covs))
# nb doctor
summary(get.model(data, "doctor.visits", with.all.covs, zero=FALSE))

# interactions
# hosp
summary(get.model(data, "hosp.stays", with.dia.inter, bin.common))
summary(get.model(data, "hosp.stays", with.cog.inter, bin.common))
# er
summary(get.model(data, "er.visits", with.dia.inter))
summary(get.model(data, "er.visits", with.cog.inter))
# doctor
summary(get.model(data, "doctor.visits", with.dia.inter, zero=FALSE))
summary(get.model(data, "doctor.visits", with.dia.inter, zero=FALSE))

## Longitudinal
# frailty and response association
# zi nb hosp
summary(get.model(data, "w2.hosp.stays", with.all.covs, "vd.score"))
# zi nb er
summary(get.model(data, "w2.er.visits", with.all.covs, "vd.score"))
# nb doctor
summary(get.model(data, "w2.doctor.visits", with.all.covs, zero=FALSE))

# interactions
# hosp
summary(get.model(data, "w2.hosp.stays", with.dia.inter, "vd.score"))
summary(get.model(data, "w2.hosp.stays", with.cog.inter, "vd.score"))
# er
summary(get.model(data, "w2.er.visits", with.dia.inter, "vd.score"))
summary(get.model(data, "w2.er.visits", with.cog.inter, "vd.score"))
# doctor
summary(get.model(data, "w2.doctor.visits", with.dia.inter, zero=FALSE))
summary(get.model(data, "w2.doctor.visits", with.dia.inter, zero=FALSE))

# logistic regression for association between frailty and death
summary(get.model(data, "died", with.all.covs, zero=FALSE, family="binomial"))

### Models used for analysis
# (they didnt do this) poissons of baseline
model <- get.model(data, "hosp.stays", frailty.covs, zero=FALSE, family="poisson")
model <- get.model(data, "er.visits", frailty.covs, zero=FALSE, family="poisson")
model <- get.model(data, "doctor.visits", frailty.covs, zero=FALSE, family="poisson")

# poissons of longitudinal (unclear what their features were for this analysis)
model <- get.model(data, "w2.hosp.stays", frailty.covs, zero=FALSE, family="poisson")
model <- get.model(data, "w2.er.visits", frailty.covs, zero=FALSE, family="poisson")
model <- get.model(data, "w2.doctor.visits", frailty.covs, zero=FALSE, family="poisson")

# zi poissons for hosp and er (unclear what their features were for this comparison)
summary(get.model(data, "hosp.stays", with.all.covs, bin.common, family="poisson"))
summary(get.model(data, "er.visits", with.all.covs, family="poisson"))

### My Analysis ###
# negative binomials at baseline
model <- get.model(data, "hosp.stays", frailty.covs, zero=FALSE)
model <- get.model(data, "er.visits", frailty.covs, zero=FALSE)
model <- get.model(data, "doctor.visits", frailty.covs, zero=FALSE)

# results with new bin covs
summary(get.model(data, "hosp.stays", with.all.covs, hosp.bin.covs))
summary(get.model(data, "er.visits", with.all.covs, er.bin.covs))
summary(get.model(data, "hosp.stays", with.dia.inter, hosp.bin.covs))
summary(get.model(data, "er.visits", with.dia.inter, er.bin.covs))

# logistic models to predict perfect state
temp <- data
temp$hosp.stays[temp$hosp.stays != 0] <- 1
temp$er.visits[temp$er.visits != 0] <- 1
# theirs
summary(get.model(temp, "hosp.stays", with.all.covs, zero=FALSE, family="binomial"))
summary(get.model(temp, "er.visits", with.all.covs, zero=FALSE, family="binomial"))

# mine
summary(get.model(temp, "hosp.stays", my.with.all.covs, zero=FALSE, family="binomial"))
summary(get.model(temp, "er.visits", my.with.all.covs, zero=FALSE, family="binomial"))

# results with new covs
summary(get.model(data, "hosp.stays", my.with.all.covs, my.hosp.bin.covs))
summary(get.model(data, "er.visits", my.with.all.covs, my.er.bin.covs))
summary(get.model(data, "hosp.stays", my.with.dia.inter, my.hosp.bin.covs))
summary(get.model(data, "er.visits", my.with.dia.inter, my.er.bin.covs))
summary(get.model(data, "doctor.visits", my.with.all.covs, zero=FALSE))



