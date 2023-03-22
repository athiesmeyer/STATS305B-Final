source("Code/make_features.R")

library(MASS)
library(pscl)

### Build preset selections of features ###

## For replication
responses <- c("hosp.stays", "er.visits", "doctor.visits")

# Ignore variables which are not original covariates
to.ignore <- c(responses, c("id", "smoking", "child.econ.sit", "home.rems",
                            "mon.income", "alcohol", "ponce", "mayaguez", "arecibo",
                            "zona.este", "ill.score"))

# The authors gradually included covariates
# frailty.covs <- get.features(c("pre.frail", "frail"), data)
# with.dem.covs <- get.features(c("pre.frail", "frail", "age", "sex", "negro",
#                                                       "mulato", "mestizo", "otra",
#                                                       "education"), data)

with.all.covs <- get.features(c(), data, ignore=to.ignore, except=TRUE)
with.dia.inter <- get.features(c(), data, ignore=to.ignore, except=TRUE,
                               inter_features = list(c("pre.frail", "diabetes"), c("frail", "diabetes")))
with.cog.inter <- get.features(c(), data, ignore=to.ignore, except=TRUE,
                               inter_features = list(c("pre.frail", "cog.func"), c("frail", "cog.func")))

# These are binomial covariates used by the authors for their hospital stays
# model
their.bin.covs <- get.features(c("vd.score", "kl.score", "depress"))

## For my analysis
# Different binomial covariates with respect to their original covariates,
# found using log reg models below
hosp.bin.covs <- get.features(c("vd.score", "kl.score", "depress", "age", "none"), data)
er.bin.covs <- get.features(c("vd.score", "kl.score", "depress", "barriers", "none", "medicare", "private"), data)

# Additional covariates
my.to.ignore <- c(responses, c("id"))
my.with.all.covs <- get.features(c(), data, ignore=my.to.ignore, except=TRUE)
my.with.dia.inter <- get.features(c(), data, ignore=my.to.ignore, except=TRUE,
                               inter_features = list(c("pre.frail", "diabetes"), c("frail", "diabetes")))
my.with.cog.inter <- get.features(c(), data, ignore=my.to.ignore, except=TRUE,
                               inter_features = list(c("pre.frail", "cog.func"), c("frail", "cog.func")))

# Binomial covariates with respect to my set of covariates, found using log reg
# models below
my.hosp.bin.covs <- get.features(c("vd.score", "kl.score", "depress", "education", "alcohol", "ill.score", "frail"), data)
my.er.bin.covs <- get.features(c("vd.score", "kl.score", "depress", "age", "private", "medicare", "ill.score"), data)

#### Models ####

### Log reg models to select binomial covariates ###

# Alter responses to be a flag for either zero or non-zero
temp <- data
temp$hosp.stays[temp$hosp.stays != 0] <- 1
temp$er.visits[temp$er.visits != 0] <- 1

# With their covariates
model <- get.model(temp, "hosp.stays", with.all.covs, zero=FALSE, family="binomial")
summary(model)
model <- get.model(temp, "er.visits", with.all.covs, zero=FALSE, family="binomial")
summary(model)

# With my covariates
model <- get.model(temp, "hosp.stays", my.with.all.covs, zero=FALSE, family="binomial")
summary(model)
model <- get.model(temp, "er.visits", my.with.all.covs, zero=FALSE, family="binomial")
summary(model)

rm(temp)


### Models with their covariates and their binomial covariates ###

# zi nb hosp
model <- get.model(data, "hosp.stays", with.all.covs, their.bin.covs)
summary(model)
# zi nb er
model <- get.model(data, "er.visits", with.all.covs)
summary(model)
# nb doctor
model <- get.model(data, "doctor.visits", with.all.covs, zero=FALSE)
summary(model)

# zi pois hosp
model <- get.model(data, "hosp.stays", with.all.covs, their.bin.covs, family="poisson")
summary(model)
# zi pois er
model <- get.model(data, "er.visits", with.all.covs, family="poisson")
summary(model)
# pois doctor
model <- get.model(data, "doctor.visits", with.all.covs, zero=FALSE, family="poisson")
summary(model)

# interactions
# hosp
model <- get.model(data, "hosp.stays", with.dia.inter, their.bin.covs)
summary(model)
model <- get.model(data, "hosp.stays", with.cog.inter, their.bin.covs)
summary(model)
# er
model <- get.model(data, "er.visits", with.dia.inter)
summary(model)
model <- get.model(data, "er.visits", with.cog.inter)
summary(model)
# doctor
model <- get.model(data, "doctor.visits", with.dia.inter, zero=FALSE)
summary(model)
model <- get.model(data, "doctor.visits", with.cog.inter, zero=FALSE)
summary(model)


### Models with their covariates and modified binomial covariates ###

# zi nb hosp
model <- get.model(data, "hosp.stays", with.all.covs, hosp.bin.covs)
summary(model)
# zi nb er
model <- get.model(data, "er.visits", with.all.covs, er.bin.covs)
summary(model)

# zi pois hosp
model <- get.model(data, "hosp.stays", with.all.covs, hosp.bin.covs, family="poisson")
summary(model)
# zi pois er
model <- get.model(data, "er.visits", with.all.covs, er.bin.covs, family="poisson")
summary(model)

# hosp diabetes interaction
model <- get.model(data, "hosp.stays", with.dia.inter, hosp.bin.covs)
summary(model)
# er diabetes interaction
model <- get.model(data, "er.visits", with.dia.inter, er.bin.covs)
summary(model)


### Models with my covariates and respective binomial covariates ###

# zi nb hosp
model <- get.model(data, "hosp.stays", my.with.all.covs, my.hosp.bin.covs)
summary(model)
# zi nb er
model <- get.model(data, "er.visits", my.with.all.covs, my.er.bin.covs)
summary(model)
# nb doctor
model <- get.model(data, "doctor.visits", my.with.all.covs, zero=FALSE)
summary(model)

# zi pois hosp
model <- get.model(data, "hosp.stays", my.with.all.covs, my.hosp.bin.covs, family="poisson")
summary(model)
# zi pois er
model <- get.model(data, "er.visits", my.with.all.covs, my.er.bin.covs, family="poisson")
summary(model)
# pois doctor
model <- get.model(data, "doctor.visits", my.with.all.covs, zero=FALSE, family="poisson")
summary(model)

# hosp diabetes interaction
model <- get.model(data, "hosp.stays", my.with.dia.inter, my.hosp.bin.covs)
summary(model)
# er diabetes interaction
model <- get.model(data, "er.visits", my.with.dia.inter, my.er.bin.covs)
summary(model)










