source("Code/make_features.R")

library(fastDummies)

## Process features into usable form ##

# Hospital Stays
# Check (Integer)

# ER Visits
# Check (Integer)

# Doctor Visits
# Check (Integer)

# Frailty Status
data <- dummy_cols(data, select_columns="frailty.status")
# Not frail is the ref group
data <- data[, names(data) != "frailty.status_0"]
names(data)[names(data) %in% c("frailty.status_1",
                                     "frailty.status_2")] <- c("pre.frail",
                                                              "frail")
data <- data[, names(data) != "frailty.status"]
# Check (Categorical: 3 classes, not frail is ref)

# VD Score
# Check (Integer from 0-4)

# KL Score
# Check (Integer from 0-12)

# Household Size
# Check (Binary)

# Insurance
data <- dummy_cols(data, select_columns="insurance")
# Medicaid is the ref group
data <- data[, names(data) != "insurance_0"]
names(data)[names(data) %in% c("insurance_1",
                                     "insurance_2",
                                     "insurance_3")] <- c("medicare",
                                                          "private",
                                                          "none")
data <- data[, names(data) != "insurance"]
# Check (Categorical: 4 classes, medicaid is ref)

# Cognitive Functioning
# Check (Integer)

# Diabetes
data$diabetes <- binary.switch(data$diabetes)
# Check (Binary)

# Age
# Check (Integer)

# Sex
data$sex <- binary.switch(data$sex)
names(data)[names(data) == 'sex'] <- 'is.male'
# Check (Binary)

# Race
data <- dummy_cols(data, select_columns="race")
# White is the reference group
data <- data[, names(data) != "race_3"]
names(data)[names(data) %in%
                c("race_1", "race_2",
                  "race_4", "race_5")] <- c("negro", "mulato",
                                            "mestizo", "otra")
data <- data[, names(data) != "race"]
# Check (Categorical: 5 classes, white is ref)

# Education
# Check (Integer)

# Obese
data$obese <- binary.switch(data$obese, TRUE)
# Check (Binary)

# Depression
# Check (Integer)

# Barriers to Care
data$barriers <- binary.switch(data$barriers)
# Check (Binary)

# Wave Two Responses
data[check.na(data[, "w2.hosp.stays"]), "w2.hosp.stays"] <- NA
data[check.na(data[, "w2.er.visits"]), "w2.er.visits"] <- NA
data[check.na(data[, "w2.doctor.visits"]), "w2.doctor.visits"] <- NA

# Death
data[data[, "died"] != 4 & data[, "died"] != -9, "died"] <- 0
data[data[, "died"] == 4, "died"] <- 1
data[data[, "died"] == -9, "died"] <- NA

# Smoking
data$smoking[check.na(data$smoking)] <- NA
data$smoking <- binary.switch(data$smoking)

# Childhood Economic Situation (1 if bad, 0 if good or ok)
data$child.econ.sit[check.na(data$child.econ.sit)] <- NA
data$child.econ.sit[data$child.econ.sit == 3] <- 2
data$child.econ.sit <- binary.switch(data$child.econ.sit)

# Uses Home Remedies
data$home.rems[check.na(data$home.rems)] <- NA
data$home.rems <- binary.switch(data$home.rems)

# Monthly Income
data$mon.income[check.na(data$mon.income)] <- NA
data$mon.income <- binary.switch(data$mon.income)

# Alcohol 
data$alcohol[check.na(data$alcohol)] <- NA

# Locality
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

# Illness score

