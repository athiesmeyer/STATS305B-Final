## This file calculates aggregate features, combines them with non-aggregate
## features, and applies exclusion criteria to form a final dataframe containing
## all relevant variables for later analysis

## The raw data can be downloaded from https://www.icpsr.umich.edu/web/DSDR/studies/34596/publications

library(data.table)
library(fastDummies)
source("Code/utils.R")

wave.one <- fread(file="DS0001/Wave I Data.tsv", sep="\t", data.table=FALSE)
n <- dim(wave.one)[1]


### Construction of features used by the original researchers ###
## Feature (Index in dataset) ##

## Frailty Status (2168) ##

# Calculate leg stand indicator from non NA value
get.frailty.status <- function(row, df) {
  vals <- as.vector(c(row[[526]], row[[1465]], row[[639]],
                      row[[1470]], row[[614]]))
  vals[check.na(vals)] <- -1

  if (sum(vals == -1) > 1) {
    NA
  } else {
    vals[c(1, 2, 4)] <- binary.switch(vals[c(1, 2, 4)])
    vals[c(3, 5)] <- binary.switch(vals[c(3, 5)], TRUE)

    vals[2] <- get.ls.indicator(vals[2], row[[1467]])
    vals[4] <- get.tug.indicator(vals[4], row, df)
    score <- sum(vals[vals != -1])

    if (score == 0) {
      0
    } else if ((score == 1) || (score == 2)) {
      1
    } else {
      2
    }
  }
}


## Summed Vascular Disease Score (2169) ##
get.vd.score <- function(row) {
  vals <- c(row[[352]], row[[414]], row[[419]], row[[426]])
  
  if (any(check.na(vals))) {
    NA
  } else {
    sum(binary.switch(vals))
  }
}

## Sum of Katz and Lawton Indices (2170) ##
get.kl.score <- function(row) {
  vals <- c(row[[1071]], row[[1074]], row[[1077]], row[[1080]],
            row[[1083]], row[[1086]], row[[1049]], row[[1052]],
            row[[1055]], row[[1058]], row[[1061]], row[[1064]],
            row[[1067]])
  
  if (any(check.na(vals))) {
    NA
  } else {
    sum(convert.to.binary(vals))
  }
}

## Depression (642) ##
# NA values are -5 and 99
wave.one[wave.one[, 642] == 99, 642] <- -1
replace.na(wave.one, 642)

## Health Insurance (696) ##
# NA values are NA and -1
wave.one[check.na(wave.one[, 696]), 696] <- -1
wave.one[wave.one[, 696] == 1, 696] <- 0
wave.one[(wave.one[, 696] == 2) | (wave.one[, 696] == 3), 696] <- 1
wave.one[(wave.one[, 696] > 3) & (wave.one[, 696] < 13), 696] <- 2
wave.one[wave.one[, 696] == -1, 696] <- 3

## Household Size (71) ##
# No NA values 
wave.one[wave.one[, 71] != 1, 71] <- 0

## Cognitive Functioning (64) ##
# NA values are -10
replace.na(wave.one, 64)

## Diabetes (360) ##
# NA values are -1
wave.one[, 360] <- binary.switch(wave.one[, 360])
replace.na(wave.one, 360)

## Age (13) ##
# No NA values
# Check

## Sex (12) ##
# No NA values
wave.one[, 12] <- binary.switch(wave.one[, 12])

## Race (1428) ##
# NA values are -1, -2, -5
replace.na(wave.one, 1428)

## Education (83) ##
# NA values are -1
replace.na(wave.one, 83)

## Hospital Stays (701) ##
# NA values are -1 and -2
replace.na(wave.one, 701)

## ER visits (705) ##
# NA values are -1 and -2
replace.na(wave.one, 705)

## Doctor visits (709) ##
# NA values are -1 and -2
replace.na(wave.one, 709)

## Obese (1461) ##
# NA values are NA
# Check

## Barriers to Care (742) ##
# NA values are -1 and -2
wave.one[, 742] <- binary.switch(wave.one[, 742])
replace.na(wave.one, 724)

### Construction of added features ###

## Illness Score (2171) ##
get.health.score <- function(row) {
  vals <- c(row[[378]], row[[383]], row[[401]],
            row[[407]], row[[438]], row[[450]],
            row[[457]])
  
  if (any(check.na(vals))) {
    NA
  } else {
    sum(binary.switch(vals))
  }
}

## Locality (3) ##
# No NA values

## Alcohol (589) ##
replace.na(wave.one, 589)

## Smoking (598) ##
wave.one[, 598] <- binary.switch(wave.one[, 598])
replace.na(wave.one, 598)

## Childhood Economic Situation (643) ##
wave.one[wave.one[, 643] == 3, 643] <- 2
wave.one[, 643] <- binary.switch(wave.one[, 643])
replace.na(wave.one, 643)

## Home Remedies (693) ##
wave.one[, 693] <- binary.switch(wave.one[, 693])
replace.na(wave.one, 693)

## Income (1269) ##
wave.one[, 1269] <- binary.switch(wave.one[, 1269])
replace.na(wave.one, 1269)

## Iterate through dataframe once to calculate: Frailty Status, Summed
## Vascular Disease Score, Sum of KL Score, and Illness Score

frailty.status <- vector(mode="integer", length=n)
vd.score <- vector(mode="integer", length=n)
kl.score <- vector(mode="integer", length=n)
ill.score <- vector(mode="integer", length=n)
for (i in 1:n) {
  row <- wave.one[i, ]
  
  # Frailty status
  frailty.status[i] <- get.frailty.status(row, wave.one)
  
  # Summed Vascular Disease Score
  vd.score[i] <- get.vd.score(row)
  
  # Sum of Katz and Lawton Indices
  kl.score[i] <- get.kl.score(row)
  
  ill.score[i] <- get.health.score(row)
}

## Add constructed features to dataset
wave.one <- cbind(wave.one, frailty.status, vd.score, kl.score, ill.score)

### Apply exclusion criteria ###

## Remove rows if proxy was needed (Exclusion 1)
wave.one <- wave.one[wave.one[, 6] != 26, ]

## Get indices of all the original features
indices <- c(701, 705, 709, 2168, 2169, 2170, 71, 696,
             64, 360, 13, 12, 1428, 83, 1461, 642, 742)

## Remove rows if missing variables of interest (besides frailty status) (Exclusion 2)
mask <- rep(TRUE, dim(wave.one)[1])
for (index in indices) {
  if (index != 2168) {
    mask <- mask & !check.na(wave.one[, index])
  } 
}
wave.one <- wave.one[mask, ]

## Remove rows if missing frailty status (Exclusion 3)
wave.one <- wave.one[!check.na(wave.one[, 2168]), ]

## Ignore all features except the original features and my added features
indices <- c(indices, c(1, 3, 598, 589, 643, 693, 1269, 2171))
wave.one <- wave.one[, indices]

## Add new feature names
names(wave.one) <- c("hosp.stays", "er.visits",
                     "doctor.visits", "frailty.status",
                     "vd.score", "kl.score", "alone", "insurance",
                     "cog.func", "diabetes", "age", "sex", "race", 
                     "education", "obese", "depress", "barriers",
                     "id", "locality", "smoking", "alcohol",
                     "child.econ.sit", "home.rems", "mon.income", "ill.score")

## Rename data set
data <- wave.one
rm(wave.one)

### Add dummy variables ###

## Frailty Status
data <- dummy_cols(data, select_columns="frailty.status")
# Not frail is the ref group
data <- data[, names(data) != "frailty.status_0"]
names(data)[names(data) %in% c("frailty.status_1",
                               "frailty.status_2")] <- c("pre.frail",
                                                         "frail")
data <- data[, names(data) != "frailty.status"]

## Insurance
data <- dummy_cols(data, select_columns="insurance")
# Medicaid is the ref group
data <- data[, names(data) != "insurance_0"]
names(data)[names(data) %in% c("insurance_1",
                               "insurance_2",
                               "insurance_3")] <- c("medicare",
                                                    "private",
                                                    "none")
data <- data[, names(data) != "insurance"]

## Race
data <- dummy_cols(data, select_columns="race")
# White is the reference group
data <- data[, names(data) != "race_3"]
names(data)[names(data) %in%
              c("race_1", "race_2",
                "race_4", "race_5")] <- c("negro", "mulato",
                                          "mestizo", "otra")
data <- data[, names(data) != "race"]

## Locality
data <- dummy_cols(data, select_columns="locality")
# San Juan is the ref group
data <- data[, names(data) != "locality_1"]
names(data)[names(data) %in% c("locality_2",
                               "locality_3",
                               "locality_4",
                               "locality_5")] <- c("ponce",
                                                   "mayaguez",
                                                   "arecibo",
                                                   "zona.este")
data <- data[, names(data) != "locality"]


