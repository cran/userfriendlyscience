getData <- function(filename=NULL,
                    errorMessage = "[defaultErrorMessage]",
                    use.value.labels=TRUE,
                    to.data.frame=TRUE,
                    stringsAsFactors=TRUE, ...) {
  
  ### File formats that have been implemented
  supportedFormats <- c(".sav", ".csv", ".tsv", ".ods", ".xls", "xlsx");
  
  ### Set error message
  errorMessage <- sub("\\[defaultErrorMessage\\]",
                      paste0("Specified file does not exist or does not have an extension identifying ",
                             "it as a readable filetype (valid extensions are: '",
                             paste(supportedFormats, collapse="', '"), "')."),
                      errorMessage);
  
  if (is.null(filename)) {
    ### If no filename is specified, request one from the user
    filename <- file.choose();
  }

  extension <- tolower(substring(filename, nchar(filename) - 3));

  if (!file.exists(filename) |
        !(extension %in% supportedFormats)) {
    ### Show error if the file doesn't exist or has the wrong extension
    stop(errorMessage);
  }
  else {
    if (extension == ".sav") {
      dat <- suppressWarnings(read.spss(filename, use.value.labels=use.value.labels,
                              to.data.frame=to.data.frame, ...));
#       dat <- read.spss(filename, use.value.labels=use.value.labels,
#                        to.data.frame=to.data.frame, ...);
#       cat("Note that a warning like 'Unrecognized record type 7, subtype ## encountered in system file'",
#           "is no cause for concern; the file is read normally.\n");
      
#       dat <- tryCatch({
#         read.spss(filename, use.value.labels=use.value.labels,
#                   to.data.frame=to.data.frame, ...);
#       }, warning=function(w) {
#         if (grepl("Unrecognized record type 7, subtype [0123456789]+ encountered in system file", w)) {
#           return(suppressWarnings(read.spss(filename, use.value.labels=use.value.labels,
#                                   to.data.frame=to.data.frame, ...)));
#          }
#          else {
#            return(read.spss(filename, use.value.labels=use.value.labels,
#                             to.data.frame=to.data.frame, ...));
#          }
#       });
      
    }
    else if (extension == ".csv") {
      dat <- read.csv(filename, stringsAsFactors=stringsAsFactors, ...);
    }
    else if (extension == ".tsv") {
      dat <- read.delim(filename, stringsAsFactors=stringsAsFactors, ...);
    }
    else if (extension == ".ods") {
      
      stop("Sorry, I currently do not know how to import OpenOffice files. If you do, ",
           "please contact me and I'll add this as well!\nOf course, you can always export from ",
           "LibreOffice or OpenOffice to .csv (comma separated values) and load that file.");
      
#       if (!is.element('ROpenOffice', installed.packages()[, 1])) {
#          stop("To load OpenOffice or LibreOffice files, I need package 'ROpenOffice', ",
#               "which is not on CRAN. Please visit http://omegahat.org for instructions, ",
#               "or you can try to downloads and install it yourself directly using:\n\n",
#               "install.packages('ROpenOffice', repos = 'http://www.omegahat.org/R', type = 'source');\n\n",
#               "Note that you might need specific tools to compile this source package ",
#               "(see Details in the install.packages() help, displayed with:\n\n?install.packages;");
#       }
#       require('ROpenOffice');
#       dat <- read.ods(filename, ...);
    }
    else if ((extension == ".xls") || (extension == "xlsx")) {
      if (!is.element('XLConnect', installed.packages()[, 1])) {
         stop("To load Excel (.xls or .xlsx) files, I need package 'XLConnect', ",
              "which in turn requires Java. Please install it yourself if you wish to ",
              "use this. You can insstall it using:\n\n",
              "install.packages('XLConnect')\n\nOf course, you can always export from ",
              "Excel to .csv (comma separated values) and load that file.");
      }
      else {
        if (requireNamespace('XLConnect')) {
          wb <- XLConnect::loadWorkbook(filename, ...);
          dat <- XLConnect::readWorksheet(wb, sheet=1);
		} else {
         stop("To load Excel (.xls or .xlsx) files, I need package 'XLConnect', ",
              "which in turn requires Java. Please install it yourself if you wish to ",
              "use this. You can insstall it using:\n\n",
              "install.packages('XLConnect')\n\nOf course, you can always export from ",
              "Excel to .csv (comma separated values) and load that file.");
		}
      }
    }
    
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

filterBy <- function(dat, expression,
                     replaceOriginalDataframe=TRUE,
                     envir = parent.frame()) {
  ### Store original dataframe and current time
  originalDataframeName <- as.character(substitute(dat));
  currentTime <- Sys.time();
  timeStamp <- round(as.numeric(currentTime) * 100);
  newDataframeName <- paste0('.', originalDataframeName, "_at_", timeStamp);
  
  ### Store original dataframe with new name in parent environment
  assign(newDataframeName, value=dat, envir=envir);
  
  ### Store number of rows for reporting to user
  nrOfRows <- nrow(dat);
  
  if (!is.logical(expression)) {
    if (is.character(expression)) {
      ### Replace single 'equals' characters with the 'equals' operator 
      expression <- gsub("([^=])=([^=])", "\\1==\\2", expression);
      ### Generate logical vector
      expression <- with(dat, eval(parse(text=expression)));
    }
    else {
      stop("The argument 'expression' must be either a logical vector or a character string with a logical expression!");
    }
  }
  
  ### Create filtered dataframe
  dat <- dat[expression, ];
  
  attr(dat, "originalDataframeName") <- originalDataframeName;
  attr(dat, "lastUnfilteredDataframeName") <- newDataframeName;
  attr(dat, "lastUnfilteredDataframeEnvir") <- envir;
  attr(dat, "lastFiltering") <- currentTime;
  
  cat("Filtered ", nrOfRows - nrow(dat) ,
      " rows (records, cases, participants, or datapoints) from dataframe '",
      originalDataframeName, "'; result has ", nrow(dat), " rows.\n", sep="");
  
  if (replaceOriginalDataframe) {
    assign(originalDataframeName, value=dat, envir=sys.frame(-1));
    invisible(dat);
  }
  else {
    return(dat);
  }

}

useAll <- function(dat, replaceFilteredDataframe = TRUE) {
  ### Store name of filtered dataframe
  filteredDataframeName <- as.character(substitute(dat));
  ### Store number of rows in filtered dataframe
  nrOfRows <- nrow(dat);
  
  ### Get information required to find original dataframe
  originalDataframeName <- attr(dat, "originalDataframeName");
  lastUnfilteredDataframeName <- attr(dat, "lastUnfilteredDataframeName");
  lastUnfilteredDataframeEnvir <- attr(dat, "lastUnfilteredDataframeEnvir");
  lastFiltering <- attr(dat, "lastFiltering");
  
  ### Check whether original exists
  if (exists(lastUnfilteredDataframeName, envir=lastUnfilteredDataframeEnvir)) {
    dat <- get(lastUnfilteredDataframeName, envir=lastUnfilteredDataframeEnvir);
    rm(list=lastUnfilteredDataframeName, envir=lastUnfilteredDataframeEnvir);
  }
  else {
    stop("Could not find the original, prefiltered version of the dataframe (which was stored as '",
         lastUnfilteredDataframeName, " in environment '", lastUnfilteredDataframeEnvir,"').");
  }
  
  cat("Removed last applied filter to dataframe '", filteredDataframeName, "', which was ",
      "applied at ", format(lastFiltering), " and removed (filtered) ",
      nrow(dat) - nrOfRows, " rows (records, cases, participants, or datapoints) ",
      "from the dataframe that was originally called '", originalDataframeName,
      "'. Restored dataframe has ", nrow(dat), " rows.\n", sep="");
  
  if (replaceFilteredDataframe) {
    assign(filteredDataframeName, value=dat, envir=sys.frame(-1));
    cat("Replaced filtered dataframe '", filteredDataframeName, "'.\n", sep="");
    invisible(dat);
  }
  else {
    return(dat);
  }
  
}
