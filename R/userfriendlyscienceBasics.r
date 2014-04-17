###########################################################
###########################################################
###
### Collection of very basic functions
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### This function checks whether a package is installed;
### if not, it installs it. It then loads the package.
safeRequire <- function(packageName) {
  if (!is.element(packageName, installed.packages()[,1])) {
    install.packages(packageName);
  }
  require(package = packageName, character.only=TRUE);
}

### trim simply trims spaces from the start and end of a string
trim <- function(str) {
  ### Based on 'trim' in package Gdata by
  ### Gregory R. Warnes <greg at warnes.net> and others
  str <- sub(pattern="^ +", replacement="", x=str)
  str <- sub(pattern=" +$", replacement="", x=str)
  str <- sub(pattern="^\t+", replacement="", x=str)
  str <- sub(pattern="\t+$", replacement="", x=str)
  return(str);
}

### Function to remove zero at start of number
noZero <- function (str) {
  return(gsub("0\\.", ".", str));  
}

### Function to format Pearson r
formatR <- function (r, digits) {
  return(noZero(round(r, digits)));
}

### repeat a string a given number of times
repeatStr <- function (str = " ", n = 1) {
  if (n < 1) {
    return("");
  }
  else if (n == 1) {
    return(str);
  }
  else {
    res <- str;
    for(i in c(1:(n-1))) {
      res <- paste0(res, str);
    }
    return(res);
  }
}

### The regular ifelse cannot return objects
ifelseObj <- function(condition, ifTrue, ifFalse) {
  if (condition) {
    return(ifTrue);
  }
  else {
    return(ifFalse);
  }
}

### To invert mirrored items
invertItem <- function(item, range=NULL, ignorePreviousInversion = FALSE) {
  ### Check whether this was already inverted
  if (!is.null(attr(item, "inverted"))) {
    if ((attr(item, "inverted") == TRUE) & !(ignorePreviousInversion)) {
      stop("This vector has already been inverted! Set ignorePreviousInversion to TRUE to override this check and invert the factor anyway.");
    }
  }
  
  ### Not inverted yet (or ignorePreviousInversion set to TRUE)
  if (is.numeric(item)) {
    if (is.null(range) | (length(range) != 2)) {
      res <- sum(range(item)) - item;
    }
    else {
      res <- sum(range) - item;
    }
  }
  else {
    stop("Provide a numeric vector!");
  }
  attr(res, "inverted") <- TRUE;
  return(res);
}

