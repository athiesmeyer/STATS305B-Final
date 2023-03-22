### For feature processing ###

# Takes a vector and returns a bool vector with true where NA. Negative values 
# represent missing data in the PREHCO dataset
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

replace.na <- function(df, index) {
  df[check.na(df[, index]), index] <- NA
}

get.quantile <- function(vals, alpha) {
  vals <- sort(vals)
  vals[ceiling(alpha * length(vals))]
}

# specific to frailty status calculation
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

# specific to frailty status calculation
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


# specific to kl.score calculation
convert.to.binary <- function(vals) {
  vals[(vals == 3) | (vals == 4)] <- 2
  binary.switch(vals)
}


### For modeling ###

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
  sum(resids^2) /df
}

# aic for zero inflated model object
get.aic <- function(model) {
  loglik <- model$loglik
  df <- length(model$coefficients$count) + length(model$coefficients$zero)
  2 * (df - loglik)
}

