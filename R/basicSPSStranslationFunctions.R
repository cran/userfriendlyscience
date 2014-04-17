getData <- function(filename=NULL,
                    errorMessage = "Specified file does not exist or does not have the extension .sav!",
                    use.value.labels=TRUE, to.data.frame=TRUE, ...) {
  if (is.null(filename)) {
    ### If no filename is specified, request one from the user
    filename <- file.choose();
  }
  if (!file.exists(filename) |
        tolower(substring(filename, nchar(filename) - 3))!= ".sav") {
    ### Show error if the file doesn't exist or has the wrong extension
    stop(errorMessage);
  }
  else {
    ### Import the datafile, using the default settings
    dat <- read.spss(filename, use.value.labels=use.value.labels,
                     to.data.frame=to.data.frame, ...);
    ### Store the file where we got this dataframe
    attr(dat, "fileName") <- filename;
    ### Return the resuls
    return(dat);
  }  
}

mediaan <- function(vector) {
  if (is.data.frame(vector) | is.matrix(vector)) {
    stop("The first argument is not a vector! If you need to specify ",
         "a variable from a dataframe, separate the name of the ",
         "dataframe and the variable name with a dollar sign, for ",
         "example using 'dat$gender' to extract variable 'gender' from ",
         "dataframe 'dat'.");
  }
  if (is.character(vector)) {
    stop('The first argument is a character vector; please convert it ',
         'to a factor or a numeric vector first.');
  }
  ### Store original class
  originalClass <- class(vector);
  ### Store original vector
  originalVector <- vector;
  ### Convert to numeric vector
  vector <- as.numeric(vector);
  ### If need be, convert to relevant category
  if ("factor" %in% originalClass) {
    levelIndex <- median(vector, na.rm=TRUE);
    if (round(levelIndex) == levelIndex) {
      res <- levels(originalVector)[median(vector, na.rm=TRUE)];
    }
    else {
      res <- c(levels(originalVector)[round(median(vector, na.rm=TRUE)-.5)],
               levels(originalVector)[round(median(vector, na.rm=TRUE)+.5)]);
    }
  }
  else {
    res <- median(vector, na.rm=TRUE);
  }
  return(res);
}

modus <- function(vector) {
  if (is.data.frame(vector) | is.matrix(vector)) {
    stop("The first argument is not a vector! If you need to specify ",
         "a variable from a dataframe, separate the name of the ",
         "dataframe and the variable name with a dollar sign, for ",
         "example using 'dat$gender' to extract variable 'gender' from ",
         "dataframe 'dat'.");
  }
  ### Store original class
  originalClass <- class(vector);
  ### Convert to factor
  vector <- as.factor(vector);
  ### Store frequencies
  freqs <- summary(vector);
  ### Determine highest frequency
  highestFreq <- max(freqs);
  ### Store the names of the most common category (or categories)
  categoryVector <- names(freqs[freqs==highestFreq]);
  ### Now, we need to supply this back in the same class as the original.
  if (originalClass=="factor") {
    categoryVector <- as.factor(categoryVector);
  }
  else {
    class(categoryVector) <- originalClass;
  }
   return(categoryVector);
}