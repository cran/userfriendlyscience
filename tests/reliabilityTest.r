
### Generate an object and store the parameters
reliabilityExample <- list();
reliabilityExample$nrOfItems <- 10;
reliabilityExample$trueScoreMean <- 5;
reliabilityExample$trueScoreVariance <- 5;
reliabilityExample$nrOfParticipants <- 250;
reliabilityExample$nrOfMoments <- 2;
reliabilityExample$itemErrorVariance <- .5;
reliabilityExample$transientErrorVariance <- 1;
reliabilityExample$seed <- 19811026;

### Set random seed
set.seed(reliabilityExample$seed);

### Generate the dataframe and each participants' true score
reliabilityExample$dat <- data.frame(trueScore = 
  rnorm(reliabilityExample$nrOfParticipants,
  mean=reliabilityExample$trueScoreMean,
  sd=sqrt(reliabilityExample$trueScoreVariance)));

### Generate items nested within test administrations
for (currentMoment in 0:(reliabilityExample$nrOfMoments-1)) {
  ### Determine transient error for this moment for each participant
  currentTransientErrors <- rnorm(reliabilityExample$nrOfParticipants,
                                  sd=sqrt(reliabilityExample$transientErrorVariance));
  for (currentItem in 1:reliabilityExample$nrOfItems) {
    ### Determine item scores for this item for this moment for each participant;
    ### conform page 93 of Green (2003, "Test-retest alpha"), this is the participants'
    ###   true score plus
    ###   a random measurement error with a given variance plus
    ###   a transient error for this moment for this participant
    reliabilityExample$dat[[paste0("t", currentMoment, "_item", currentItem)]] <-
      reliabilityExample$dat$trueScore +
      rnorm(reliabilityExample$nrOfParticipants, sd = sqrt(reliabilityExample$itemErrorVariance)) +
      currentTransientErrors;
  }
}

### Show correlation matrices within each measurement moment
for (currentMoment in 0:(reliabilityExample$nrOfMoments-1)) {
  print(cor(reliabilityExample$dat[, paste0("exampleData$t", currentMoment, "_item", 1:reliabilityExample$nrOfItems)]));
}

### Show complete correlation matrix
cor(reliabilityExample$dat[, 2:ncol(reliabilityExample$dat)]);

### And covariance matrix
cov(reliabilityExample$dat[, 2:ncol(reliabilityExample$dat)]);

### Compute reliability estimates for each measurement moment
for (currentMoment in 0:(reliabilityExample$nrOfMoments-1)) {
  print(scaleReliability(reliabilityExample$dat,
                         paste0("exampleData$t", currentMoment, "_item", 1:reliabilityExample$nrOfItems),
                         ci=FALSE));
}

### Split dataframe into separate dataframes for each measurement moment
reliabilityExample$dat.split <- list();
for (currentMoment in 0:(reliabilityExample$nrOfMoments-1)) {
  reliabilityExample$dat.split[[paste0("t", currentMoment)]] <-
    reliabilityExample$dat[, paste0("exampleData$t", currentMoment, "_item", 1:reliabilityExample$nrOfItems)];
}

### Show item-time covariances for the first two measurements
cov(x=reliabilityExample$dat.split$t0,
    y=reliabilityExample$dat.split$t1);

### Show test-retest alpha
print(testRetestAlpha(reliabilityExample$dat[, 2:ncol(reliabilityExample$dat)]));
