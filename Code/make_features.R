library(data.table)

wave.one <- fread(file="DS0001/Wave I Data.tsv", sep="\t", data.table=FALSE)
wave.two <- fread(file="DS0004/Wave II Data.tsv", sep="\t", data.table=FALSE)
n <- dim(wave.one)[1]

# Takes a vector and returns a bool vector with true where NA
check.na <- function(val) {
  (val < 0) | (is.na(val))
}

# Takes a vector of values either 1 or 2. If switch, maps 1 to 0, 2 to 1
# Else maps 1 to 1 and 2 to 0.
binary.switch <- function(val, switch=FALSE) {
  if (switch) {
    val[val == 1] <- 0
    val[val == 2] <- 1
    val
  } else {
    val[val == 2] <- 0
    val
  }
}

# Hospital Stays (701)
# NA values are -1 and -2. Discrete
# Check

# ER visits (705)
# NA values are -1 and -2. Discrete
# Check

# Doctor visits (709)
# NA values are -1 and -2. Discrete
# Check


# Frailty Status (2168)
# Calculate leg stand indicator from non NA value
get.ls.indicator <- function(achieved, value) {
  if (achieved == -1) {
    -1
  } else if (achieved == 1) {
    if (value < 10) {
      1
    } else {
      0
    }
  } else {
    1
  }
}

get.quantile <- function(vals, alpha) {
  vals <- sort(vals)
  vals[ceiling(alpha * length(vals))]
}

get.tug.indicator <- function(achieved, row, df) {
  if (achieved == -1) {
    -1
  } else if (achieved == 1) {
    tug.time <- row[[1471]]
    sex <- row[[12]]
    
    quantile <- get.quantile(df[df[, 12] == sex, 1471], 0.2)
    if (tug.time <= quantile){
      1
    } else {
      0
    }
  } else {
    1
  }
}

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


# Cognitive Functioning (64)
# NA values are -10. Discrete
# Check

# Diabetes (360)
# NA values are -1. Binary
# Check

# Age (13)
# No NA values. Discrete
# Check

# Sex (12)
# No NA values. Binary
# Check

# Race (1428)
# NA values are -1, -2, -5. Discrete

# Education (83)
# NA values are -1. Discrete
# Check

# Summed Vascular Disease Score (2169)
get.vd.score <- function(row) {
  vals <- c(row[[352]], row[[414]], row[[419]], row[[426]])
  
  if (any(check.na(vals))) {
    NA
  } else {
    sum(binary.switch(vals))
  }
}
# Check

# Obese (1461)
# NA values are NA. Binary
# Check

# Depression (642)
# NA values are -5 and 99. Discrete
wave.one[wave.one[, 642] == 99, 642] <- -1
# Check

# Sum of Katz and Lawton Indices (2170)
convert.to.binary <- function(vals) {
  vals[(vals == 3) | (vals == 4)] <- 2
  binary.switch(vals)
}

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
# Check

# Health Insurance (696)
# NA values are NA and -1. Discrete
wave.one[check.na(wave.one[, 696]), 696] <- -1
wave.one[wave.one[, 696] == 1, 696] <- 0
wave.one[(wave.one[, 696] == 2) | (wave.one[, 696] == 3), 696] <- 1
wave.one[(wave.one[, 696] > 3) & (wave.one[, 696] < 13), 696] <- 2
wave.one[wave.one[, 696] == -1, 696] <- 3
# Check

# Barriers to Care 1 (742)
# NA values are -1 and -2. Binary
# Check

# Household Size (71)
# No NA values. Discrete
wave.one[wave.one[, 71] != 1, 71] <- 0
# Check

# Iterate through dataframe once to calculate: Frailty Status, Summed
# Vascular Disease Score, Sum of Katz and Lawton Indices

frailty.status <- vector(mode="integer", length=n)
vd.score <- vector(mode="integer", length=n)
kl.score <- vector(mode="integer", length=n)
for (i in 1:n) {
  row <- wave.one[i, ]
  
  # Frailty status
  frailty.status[i] <- get.frailty.status(row, wave.one)
  
  # Summed Vascular Disease Score
  vd.score[i] <- get.vd.score(row)
  
  # Sum of Katz and Lawton Indices
  kl.score[i] <- get.kl.score(row)
}

# Add constructed features to dataset
wave.one <- cbind(wave.one, frailty.status, vd.score, kl.score)

# Remove if proxy was needed
wave.one <- wave.one[wave.one[, 6] != 26, ]

# Get indices of all relevant features and caseid
indices <- c(1, 701, 705, 709, 2168, 2169, 2170, 71, 696,
             64, 360, 13, 12, 1428, 83, 1461, 642, 742)

# Remove if missing variables of interest (besides frailty status)
mask <- rep(TRUE, dim(wave.one)[1])
for (index in indices) {
  if (index != 2168 && index != 1) {
    mask <- mask & !check.na(wave.one[, index])
  } 
}
wave.one <- wave.one[mask, ]

# Remove if missing frailty status
wave.one <- wave.one[!check.na(wave.one[, 2168]), ]

# Remove unwanted features
wave.one <- wave.one[, indices]

# Add new feature names
names(wave.one) <- c("id", "hosp.stays", "er.visits", "doctor.visits", "frailty.status",
                     "vd.score", "kl.score", "alone", "insurance",
                     "cog.func", "diabetes", "age", "sex", "race", 
                     "education", "obese", "depress", "barriers")

# Get wave two reponse counts for all individuals in restricted wave one table
mask <- vector(mode="logical")
for (i in 1:dim(wave.two)[1]) {
  mask <- append(mask, (wave.two[i, 1] %in% wave.one$id))
}
wave.two <- wave.two[mask,]

wave.one$w2.hosp.stays <- wave.two[, 896]
wave.one$w2.er.visits <- wave.two[, 901]
wave.one$w2.doctor.visits <- wave.two[, 905]
wave.one$died <- wave.two[, 3]

data <- wave.one

rm(wave.one)
rm(wave.two)

