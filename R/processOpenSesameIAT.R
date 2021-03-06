###########################################################
###########################################################
###
### Function to read OpenSesame IAT data files (i.e. a
### collection of .csv files), process these, compute the
### D600, and produce a wide dataframe with the output
###
### For a description of the procedure, see e.g.
### http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3769683/
###
### File created by Frederik van Acker and Gjalt-Jorn
### Peters. Questions? You can contact us through
### http://behaviorchange.eu
###
### This file is licensed under Creative Commons BY-SA 3.0
### (Attribution-ShareAlike, which means that you can
### freely use and distribute this file, and you're
### allowed to alter it as long as you release the edited
### version using the same license (i.e. again freely
### available). This license is used to promote Open
### Science and Full Disclosure. For the complete
### license, see http://creativecommons.org/licenses/by-sa/3.0/deed.en_US
### For more information about Full Disclosure, see
### http://sciencerep.org/fulldisclosure
###
###########################################################
###########################################################

###########################################################
### Secondary function definitions
###########################################################

### This function adds a message to the log
### and displays it depending on a boolean
addToLog <- function(fullLog, ..., showLog = FALSE) {
  if (showLog) {
    cat(paste0(..., collapse="\n"))
  }
  return(paste0(fullLog, "\n", paste0(..., collapse="\n")));
}

###########################################################
### Main function
###########################################################



#' processOpenSesameIAT
#' 
#' This function reads IAT files as generated by the OpenSesame script
#' available at [INSERT URL].
#' 
#' 
#' Note that this function was developed to read the OpenSesame IAT datafiles
#' created by the OpenSesame script developed by Kenny Wolfs, Jacques van
#' Lankveld, and Frederik van Acker at the Open University of the Netherlands.
#' If you use a different version (for example, the one contributed to the
#' OpenSesame paradigm repository by Hansika Kapoor, see
#' \url{http://osdoc.cogsci.nl/3.0/standard-tasks/#implicit-association-test-iat}),
#' you will have to specify the variable names you specified to OpenSesame for
#' the response time and for whether the response was correct in
#' \code{openSesameVarNames}. For example, if you use Hansika's IAT task,
#' you'll have to specify \code{openSesameVarNames = list(correct = "correct",
#' response_time = "avg_rt")}
#' 
#' Similarly, of course you will probably have to specify the number of trials
#' per block etc. Also, you may want to set showLog to FALSE, as the logging is
#' quite detailed.
#' 
#' @param dataPath A directory containing the .csv files that OpenSesame
#' provides.
#' @param blocks.sizes A vector containing the number of trials of each block.
#' @param blocks.congruent A vector containing the numbers of the congruent
#' blocks.
#' @param blocks.incongruent A vector containing the numbers of the incongruent
#' blocks.
#' @param blocks.realTrials A vector containing the numbers of the real trials.
#' @param blocks.practiceTrials A vector containing the numbers of the practice
#' trials.
#' @param congruentLarger Whether the response latencies for the congruent
#' trials (TRUE) or the incongruent trials (FALSE) are expected to be larger.
#' This simply multiplies the final D600 measures with -1.
#' @param responseTime.min Minimum number of milliseconds of response time (all
#' shorter times will be removed).
#' @param responseTime.max Maximum number of milliseconds of response time (all
#' longer times will be replaced with this number).
#' @param responseTime.penalty Penalty in milliseconds to add to the response
#' times for incorrect responses.
#' @param outputFile If specified, the aggregated datafile is stored in this
#' file.
#' @param wideOutputFile If specified, the wide version of the datafile will be
#' stored in this file.
#' @param showLog Boolean; if TRUE, shows the log (is stored in the resulting
#' object anyway).
#' @param filenameRegEx Regular expression describing the filenames. This has
#' two purposes.  First, only files matching this regular expression will be
#' processed (note that you can set it to \code{NULL} to process all files).
#' Second, by using \code{"\1"}, \code{"\2"}, etc, matched patterns can be
#' extracted from the filenames and stored as variables in the final datafile
#' (see \code{\link{sub}} for more information on regular expression matching).
#' The default pattern, \code{"subject-(\d+)(\w+)\.csv"}, which is read by R as
#' \code{"subject-(\d+)(\w+)\.csv"} (because the backslash is the escape
#' symbol, double backspaces are needed to specify one backspace, see
#' \code{\link{Quotes}}), assumes that all filenames start with
#' \code{'subject-'}, followed by the subject number (\code{"\d+"} matches one
#' or more digits), immediately followed by one or more letters and digits
#' (\code{"\w+"} matches one or more letters or digits) indicating the session
#' that the datafile pertains to. If you only have subject numbers, you'd use
#' \code{"subject-(\d+)\.csv"} or perhaps \code{"subject-(\w+)\.csv"} if the
#' subjects could also have letters in their identifiers. Note that you have to
#' include the variable names of each of these extractable patterns in
#' \code{regExValues}!
#' @param regExValues Here, the names of the variables extracted using the
#' regular expression specified in \code{filenameRegEx} are provided. Must of
#' course have the same length as the number of patterns specified in
#' \code{filenameRegEx}, and in the same order.
#' @param participantVarName,taskVarName Variable name of the variable
#' identifying participants and tasks (usually extracted from the filename, so
#' should be a value in regExValues). Tasks are usually different
#' within-subject conditions.
#' @param openSesameVarNames A list with the two elements 'correct' and
#' 'response_time', which should be the variable names that OpenSesame used to
#' write, for each trial, whether the response was correct or not ('correct')
#' and what the response time was ('response_time');
#' @param stimulusSelectionVarName,stimulusSelectionValues These arguments can
#' be used to specify a subset of stimuli to process. Specify which column
#' contains the values to select in \code{stimulusSelectionVarName}, and
#' specify the value(s) to select in \code{stimulusSelectionValues}.
#' @param roundOutput Number of digits to round the output to. This is useful
#' for importing into a program that doesn't quite get how storing numbers
#' works, such as SPSS or Excel; they sometimes don't manage to import numbers
#' with many decimals.
#' @param decimalSeparator When working with e.g. Excel, it can be easier to
#' just specify the decimal separator rather than switch Excel's (and therefore
#' Windows') locale.
#' @param inputDecimalSeparator The decimal separator to specify to
#' \code{\link{read.csv}} when reading the data files.
#' @param inputfileSelectionColumns,inputfileSelectionValues This functionality
#' still has to be implemented. Once implemented, these arguments can be used
#' to specify a column, and a (set of) value(s) in that column to use to select
#' which rows to process (also see \code{stimulusSelectionVarName} and
#' \code{stimulusSelectionValues}).
#' @return An object with the raw files, the processed files, and the file
#' converted to wide format. But most users will probably specify
#' \code{outputFile} and/or \code{wideOutputFile} to just export the output
#' files directly.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @references Mathot, S., Schreij, D., & Theeuwes, J. (2012). OpenSesame: An
#' open-source, graphical experiment builder for the social sciences.
#' \emph{Behavior Research Methods}, 44(2), 314-324.
#' doi:10.3758/s13428-011-0168-7
#' 
#' Kapoor, H. (2015). The creative side of the Dark Triad. \emph{Creativity
#' Research Journal}, 27(1), 58-67. doi:10.1080/10400419.2014.961775.
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#' ### This will process all files in the specified directory, but not
#' ### export anything
#' processed <- processOpenSesameIAT("C:/directory/with/datafiles");
#' 
#' ### This will export both the aggregated datafile and the wide datafile
#' processed <- processOpenSesameIAT("C:/directory/with/datafiles",
#'                                   outputFile="C:/directory/aggregated.csv",
#'                                   wideOutputFile="C:/directory/wide.csv");
#' 
#' }
#' 
#' 
#' @export processOpenSesameIAT
processOpenSesameIAT <- function(dataPath,
                                 blocks.sizes = c(18, 36, 48, 36, 48),
                                 blocks.congruent = c(2, 3),
                                 blocks.incongruent = c(4, 5),
                                 blocks.realTrials = c(3, 5),
                                 blocks.practiceTrials = c(2, 4),
                                 congruentLarger = TRUE,
                                 responseTime.min = 400,
                                 responseTime.max = 2500,
                                 responseTime.penalty = 600,
                                 outputFile = NULL,
                                 wideOutputFile = NULL,
                                 showLog = TRUE,
                                 filenameRegEx = "subject-(\\d+)(\\w+)\\.csv",
                                 regExValues = c("subject","session"),
                                 participantVarName = "subject",
                                 taskVarName = "session",
                                 openSesameVarNames = list(correct = "correct",
                                                           response_time = "response_time"),
                                 stimulusSelectionVarName = NULL,
                                 stimulusSelectionValues = NULL,
                                 roundOutput = 6,
                                 decimalSeparator = ".",
                                 inputDecimalSeparator = ".",
                                 inputfileSelectionColumns = NULL,
                                 inputfileSelectionValues = NULL) {
  
  ### Use inputfileSelectionColumns and the allowed values in those columns in 
  ### inputfileSelectionValues to select only those rows.
  
  if (!is.null(participantVarName) && !is.null(regExValues) &&
        !(participantVarName %in% regExValues)) {
    stop(paste0("The participantVarName ('", participantVarName,
                "') is not included in the regExValues vector!"));
  }
  if (!is.null(taskVarName) && !is.null(regExValues) &&
        !(taskVarName %in% regExValues)) {
    stop(paste0("The taskVarName ('", taskVarName,
                "') is not included in the regExValues vector! ",
                "Note that you have to set it to NULL if you only ",
                "want to specify a participantVarName (by default, ",
                "when not specified, taskVarName = 'session')."));
  }
  
  ### Generate object to return
  res <- list(input = as.list(environment()));
  
  ### Store input variables
  res$dataPath <- dataPath;
  res$blocks.sizes <- blocks.sizes;
  res$blocks.congruent <- blocks.congruent;
  res$blocks.incongruent <- blocks.incongruent;
  res$blocks.realTrials <- blocks.realTrials;
  res$blocks.practiceTrials <- blocks.practiceTrials;
  res$responseTime.min <- responseTime.min;
  res$responseTime.max <- responseTime.max;
  res$responseTime.penalty <- responseTime.penalty;
  res$outputFile <- outputFile;

  ### Store object with each participants' datafile
  res$file.raw <- list();
  res$file.clean <- list();
  
  ### Store object with each participants' dataframe
  res$dat.raw <- list();
  ### And cleanly imported version
  res$dat.asImported <- list();
  
  ### Create a vector with the number of the block
  ### for each trial
  res$blocks.vector <- rep(1:length(blocks.sizes), blocks.sizes);

  ### Create a vector indicating whether each trial
  ### is congruent or not.
  res$congruency.vector <- ifelse(res$blocks.vector %in% res$blocks.congruent,
                                  TRUE,
                                  ifelse(res$blocks.vector %in% res$blocks.incongruent,
                                         FALSE,
                                         NA));
  
  res$blocks.sizes <- blocks.sizes;
  res$blocks.congruent <- blocks.congruent;
  res$blocks.incongruent <- blocks.incongruent;
  res$blocks.realTrials <- blocks.realTrials;
  res$blocks.practiceTrials <- blocks.practiceTrials;
  res$responseTime.min <- responseTime.min;
  res$responseTime.max <- responseTime.max;
  res$responseTime.penalty <- responseTime.penalty;
  
  logText <- addToLog("--- processOpenSesameIAT log ---\n",
                      paste0("Specified input:\n",
                             length(blocks.sizes), " blocks were specified,",
                             " of respectively ", vecTxt(blocks.sizes),
                             " trials each.\n", 
                             "Blocks ", vecTxt(blocks.congruent),
                             " are congruent and blocks ", vecTxt(blocks.incongruent),
                             " are incongruent.\n",
                             "Blocks ", vecTxt(blocks.realTrials),
                             " are the real trials and blocks ", vecTxt(blocks.practiceTrials),
                             " are the practice trials.\n",
                             "Response times below ", responseTime.min,
                             " milliseconds will be deleted, response times over ",
                             responseTime.max, " will be replaced with ",
                             responseTime.max, " and a penalty of ",
                             responseTime.penalty, " milliseconds will be ",
                             "added to incorrect responses.\n\n",
                             "In total, this design consists of ", sum(blocks.sizes),
                             " trials. That means that every datafile will need to ",
                             "have exactly ", sum(blocks.sizes), " valid rows (i.e. ",
                             "excluding the header and empty rows); datafiles ",
                             "with a different number of rows will be excluded.",
                             "\n\n"),
                             showLog=showLog);
                      
  if (!is.null(wideOutputFile) && is.null(taskVarName)) {
    logText <- addToLog(logText,
                        paste0("Note: you are requesting a wide output file, ",
                               "but you did not specify a taskVarName, so I ",
                               "don't know how to identify different datafiles for ",
                               "each participant. No wide datafile will be produced.\n\n"),
                        showLog=showLog);
  }
  
  ### Store filenames in a character vector.
  res$inputFiles.all <- list.files(dataPath);

  ### Check whether a regular expression was specified to select
  ### filenames and information about each participant/sessions
  if (!is.null(filenameRegEx)) {
    ### Select filenames matching regular expression
    res$inputFiles <- grep(filenameRegEx, res$inputFiles.all, value=TRUE);

    ### Start log by indicating number of valid files
    logText <- addToLog(logText,
                        paste0("Read directory '", dataPath, "'. Out of ",
                               length(res$inputFiles.all), " files, ",
                               length(res$inputFiles), " matched regular expression '",
                               filenameRegEx, "' and will be processed.\n"),
                        showLog=showLog);
    
    if (!length(res$inputFiles)) {
      stop("No valid files found!");
    }
  }
  else {
    ### Process all files
    res$inputFiles <- res$inputFiles.all;
    ### Start log by indicating number of valid files
    logText <- addToLog("--- processOpenSesameIAT log ---\n",
                        paste0("Read folder '", dataPath, "'. Processing all ",
                               length(res$inputFiles.all), " files.\n"),
                        showLog=showLog);
  }
  
  ### Create dataframe for aggregated results
  res$dat <- data.frame();
  
  ### Start loop to read each participants' file
  for (currentParticipant in 1:length(res$inputFiles)) {
    
    logText <- addToLog(logText,
             paste0("Starting to process file ", res$inputFiles[currentParticipant], "\n"),
             showLog=showLog);

    ### Store filename that was used
    res$dat[currentParticipant, 'filename'] <-
      res$inputFiles[currentParticipant];
    
    ### If a regular expression was set, extract information
    ### from the filename and store this as well
    if (!is.null(filenameRegEx)) {
      for (currentVariableIndex in 1:length(regExValues)) {
        res$dat[currentParticipant, regExValues[currentVariableIndex]] <-
          sub(filenameRegEx,
              paste0("\\", currentVariableIndex),
              res$inputFiles[currentParticipant]);
      }
    }
    
    ### For some reason, some datafiles have the header
    ### repeated in between the data. Therefore, we read
    ### the file manually, and then scan for lines that
    ### are duplicates and remove them.
    res$file.raw[[currentParticipant]] <-
      readLines(file.path(dataPath, res$inputFiles[currentParticipant]));
    ### Remove duplicates
    res$file.clean[[currentParticipant]] <-
      res$file.raw[[currentParticipant]][
        !duplicated(res$file.raw[[currentParticipant]])
      ];
    
    ### Also, for some reason, some datafiles use a single
    ### double quote character to quote all fields; but
    ### sometimes, this pattern gets screwed up, causing
    ### double double quotes to appear. This leads to
    ### problems when reading the file, and since there
    ### are no text fields in the file, just remove the
    ### quotes from every line.
    res$file.clean[[currentParticipant]] <-
      gsub('"', '', res$file.clean[[currentParticipant]]);
    
    ### Store number of rows for this participant
    res$dat[currentParticipant, 'nrOfRowsInDatafile']  <-
      length(res$file.clean[[currentParticipant]]);
    
    ### Check the number of rows in the datafile. This
    ### should be equal to the number of blocks + 1;
    ### otherwise, something unknown went wrong, rendering
    ### the datafile unreliable (i.e. exclude the
    ### participant)
    if (!(length(res$file.clean[[currentParticipant]]) ==
            (sum(res$blocks.sizes) + 1))) {
      logText <- addToLog(logText,
               paste0("    File has ",
                      length(res$file.clean[[currentParticipant]]) - 1,
                      " valid rows (trials); ", sum(res$blocks.sizes),
                      " required; excluding participant.\n"),
               showLog=showLog);
      ### Store 'invalidness' in dataframe
      res$dat[currentParticipant, 'validDatafile']  <- 0;
    }
    else {
      ### Store 'validness' in dataframe
      res$dat[currentParticipant, 'validDatafile']  <- 1;
      
      nrOfElements <- unlist(lapply(res$file.clean[[currentParticipant]],
                             function(x) { return (length(strsplit(x, ",")[[1]])); }));

      if (nrOfElements[1] < max(tail(nrOfElements, -1))) {
        logText <- addToLog(logText,
                            paste0("    Error: the first line (the header, ",
                                   "containing the variable names) only contains ",
                                   nrOfElements[1],
                                   " elements, whereas the other lines contain ",
                                   "between ",
                                   paste0(range(tail(nrOfElements, -1)), collapse=" and "),
                                   " elements; excluding participant.\n"),
                            showLog=showLog);
      } else {
      
        ### Parse participants' datafile
        res$dat.raw[[currentParticipant]] <-
          res$dat.asImported[[currentParticipant]] <-
          read.csv(text=res$file.clean[[currentParticipant]],
                   header = TRUE, dec=inputDecimalSeparator);
   
        ### Add vector with block numbers for each trial
        res$dat.raw[[currentParticipant]]$blockNumber <- res$blocks.vector;
    
        ### Add vector with congruency for each trial
        res$dat.raw[[currentParticipant]]$congruent <- res$congruency.vector;
  
        ### Correct the 'correct' answers, such that anything
        ### other than 1 is replaced by 0
        res$dat.raw[[currentParticipant]]$correct.corrected <-
          ifelse(res$dat.raw[[currentParticipant]][[openSesameVarNames$correct]] == 0,
                 0, 1);
        res$dat.raw[[currentParticipant]]$correct.corrected <-
          as.numeric(res$dat.raw[[currentParticipant]]$correct.corrected);
        
        ### For each trial, store whether the response
        ### time is sufficiently high
        res$dat.raw[[currentParticipant]]$rt_highEnough <-
          res$dat.raw[[currentParticipant]][[openSesameVarNames$response_time]] >= res$responseTime.min;
        
        logText <- addToLog(logText,
                 paste0("    ",
                        sum(res$dat.raw[[currentParticipant]]$rt_highEnough),
                        " trials with response time > ",
                        res$responseTime.min, "\n"),
                 showLog=showLog);
        
        ### For each trial, store whether the response
        ### time is sufficiently low
        res$dat.raw[[currentParticipant]]$rt_lowEnough <-
          res$dat.raw[[currentParticipant]][[openSesameVarNames$response_time]] <= res$responseTime.max;
    
        logText <- addToLog(logText,
                 paste0("    ",
                        sum(res$dat.raw[[currentParticipant]]$rt_lowEnough),
                        " trials with response time < ",
                        res$responseTime.max, "\n"),
                 showLog=showLog);
    
        ### For each trial, store whether the response
        ### time is valid
        res$dat.raw[[currentParticipant]]$rt_valid <-
          res$dat.raw[[currentParticipant]]$rt_lowEnough &
          res$dat.raw[[currentParticipant]]$rt_highEnough;
  
        logText <- addToLog(logText,
                 paste0("    ",
                        sum(res$dat.raw[[currentParticipant]]$rt_valid),
                        " trials with valid response time\n"),
                 showLog=showLog);
        
        ### Generate a new variable containing the response times,
        ### but where response times exceeding the highest acceptable
        ### value are replaced with max response time
        res$dat.raw[[currentParticipant]]$response_time_clipped <- ifelse(
          res$dat.raw[[currentParticipant]]$rt_lowEnough,
          res$dat.raw[[currentParticipant]][[openSesameVarNames$response_time]],
          res$responseTime.max);

        
        ### If we need to select a subset of stimuli, eliminate those not
        ### satisfying the criteria
        if (!is.null(stimulusSelectionVarName) && !is.null(stimulusSelectionValues)) {
          res$dat.raw[[currentParticipant]] <-
            res$dat.raw[[currentParticipant]][
              trim(res$dat.raw[[currentParticipant]][, stimulusSelectionVarName]) %IN%
                trim(stimulusSelectionValues), ];
          
          currentBlocks.sizes <- table(res$dat.raw[[currentParticipant]]$blockNumber);
          
          logText <- addToLog(logText,
                              paste0("    Removed ",
                                     nrow(res$dat.asImported[[currentParticipant]]) -
                                       nrow(res$dat.raw[[currentParticipant]]),
                                     " trials for this participant because '",
                                     stimulusSelectionVarName, "' was not one of ",
                                     vecTxtQ(stimulusSelectionValues), ".\n    Remaining ",
                                     "block sizes: ", vecTxt(currentBlocks.sizes), ".\n"),
                              showLog=showLog);

        } else {
          currentBlocks.sizes <- blocks.sizes;
        }
        
        ### Compute the mean response times per block,
        ### but only for correct responses, and based
        ### on the clipped response times (i.e. where
        ### unacceptably high response times are replaced
        ### with the max response time).
        ### Then repeat each mean response time with
        ### the number of sessions in that block using rep
        ### and store the result in a new variable in the
        ### dataframe, so that for every trial, we also
        ### have the mean for the relevant block available
        
        ### However, some people have no (zero) correct
        ### responses within a block, which will cause an
        ### error. Therefore, check that first.
        if (length(aggregate(response_time_clipped ~ blockNumber, data =
                           res$dat.raw[[currentParticipant]][
                             res$dat.raw[[currentParticipant]]$rt_valid &
                             (res$dat.raw[[currentParticipant]]$correct.corrected == 1)
                             , ],
                         FUN = "mean", na.rm=TRUE)$response_time_clipped) < length(currentBlocks.sizes)) {
          ### In this case, one or more blocks has zero correct responses.
          ### That means the participant has to be excluded.
          logText <- addToLog(logText,
                   paste0("    This participant has zero (0) correct ",
                          "responses in one or more blocks; excluding participant.\n"),
                   showLog=showLog);
        }
        else {
          
          ### Compute and store means per block
          res$dat.raw[[currentParticipant]]$response_time_blockMean <- 
            rep(aggregate(response_time_clipped ~ blockNumber, data =
                          res$dat.raw[[currentParticipant]][
                            res$dat.raw[[currentParticipant]]$rt_valid &
                            (res$dat.raw[[currentParticipant]]$correct.corrected == 1)
                          , ],
                          FUN = "mean", na.rm=TRUE)$response_time_clipped, currentBlocks.sizes);
  
          ### Replace the response times for incorrect
          ### answers with the mean response time for that
          ### block plus the penalty
          res$dat.raw[[currentParticipant]]$response_time_penalized <-
            ifelse(res$dat.raw[[currentParticipant]]$correct.corrected == 1,
                   res$dat.raw[[currentParticipant]]$response_time_clipped,
                   res$dat.raw[[currentParticipant]]$response_time_blockMean +
                     res$responseTime.penalty);

          logText <- addToLog(logText,
                              paste0("    ",
                                     sum(res$dat.raw[[currentParticipant]]$correct.corrected != 1),
                                     " trials were incorrect: setting block mean plus penalty as response time.\n"),
                              showLog=showLog);          
          
          ### Compute standard deviation, first for all blocks;
          ### then only real trials, then only practice trials.
          res$dat[currentParticipant, 'sd_all'] <-
            sd(res$dat.raw[[currentParticipant]]
               [res$dat.raw[[currentParticipant]]$rt_highEnough &
                res$dat.raw[[currentParticipant]]$rt_lowEnough,
                'response_time_penalized']);
           res$dat[currentParticipant, 'sd_real'] <-
             sd(res$dat.raw[[currentParticipant]]
                [res$dat.raw[[currentParticipant]]$rt_highEnough &
                 res$dat.raw[[currentParticipant]]$rt_lowEnough &
                 res$dat.raw[[currentParticipant]]$blockNumber %in% res$blocks.realTrials,
                 'response_time_penalized']);
           res$dat[currentParticipant, 'sd_practice'] <-
             sd(res$dat.raw[[currentParticipant]]
                [res$dat.raw[[currentParticipant]]$rt_highEnough &
                 res$dat.raw[[currentParticipant]]$rt_lowEnough &
                 res$dat.raw[[currentParticipant]]$blockNumber %in% res$blocks.practiceTrials,
                 'response_time_penalized']);
          
          ### Compute means for congruent blocks
          res$dat[currentParticipant, 'mean_congruent_all'] <-
            mean(res$dat.raw[[currentParticipant]]
                 [res$dat.raw[[currentParticipant]]$rt_highEnough &
                  res$dat.raw[[currentParticipant]]$rt_lowEnough &
                  res$dat.raw[[currentParticipant]]$congruent,
                  'response_time_penalized'], na.rm=TRUE);
           res$dat[currentParticipant, 'mean_congruent_real'] <-
             mean(res$dat.raw[[currentParticipant]]
                  [res$dat.raw[[currentParticipant]]$rt_highEnough &
                   res$dat.raw[[currentParticipant]]$rt_lowEnough &
                   res$dat.raw[[currentParticipant]]$congruent &
                     res$dat.raw[[currentParticipant]]$blockNumber %in% res$blocks.realTrials,
                   'response_time_penalized']);
           res$dat[currentParticipant, 'mean_congruent_practice'] <-
             mean(res$dat.raw[[currentParticipant]]
                  [res$dat.raw[[currentParticipant]]$rt_highEnough &
                   res$dat.raw[[currentParticipant]]$rt_lowEnough &
                   res$dat.raw[[currentParticipant]]$congruent &
                     res$dat.raw[[currentParticipant]]$blockNumber %in% res$blocks.practiceTrials,
                   'response_time_penalized']);
          
          ### And for incongruent blocks
          res$dat[currentParticipant, 'mean_incongruent_all'] <-
            mean(res$dat.raw[[currentParticipant]]
                 [res$dat.raw[[currentParticipant]]$rt_highEnough &
                  res$dat.raw[[currentParticipant]]$rt_lowEnough &
                  !res$dat.raw[[currentParticipant]]$congruent,
                  'response_time_penalized'], na.rm=TRUE);
           res$dat[currentParticipant, 'mean_incongruent_real'] <-
             mean(res$dat.raw[[currentParticipant]]
                  [res$dat.raw[[currentParticipant]]$rt_highEnough &
                   res$dat.raw[[currentParticipant]]$rt_lowEnough &
                   !res$dat.raw[[currentParticipant]]$congruent &
                     res$dat.raw[[currentParticipant]]$blockNumber %in% res$blocks.realTrials,
                   'response_time_penalized']);
           res$dat[currentParticipant, 'mean_incongruent_practice'] <-
             mean(res$dat.raw[[currentParticipant]]
                  [res$dat.raw[[currentParticipant]]$rt_highEnough &
                   res$dat.raw[[currentParticipant]]$rt_lowEnough &
                   !res$dat.raw[[currentParticipant]]$congruent &
                     res$dat.raw[[currentParticipant]]$blockNumber %in% res$blocks.practiceTrials,
                   'response_time_penalized']);
          
          ### Compute D600, which is simply the difference between
          ### the means divided by the standard deviation; again,
          ### repeat this for all three groups (all, real, and
          ### practice).
          res$dat[currentParticipant, 'd600_all'] <-
            (res$dat[currentParticipant, 'mean_congruent_all'] -
             res$dat[currentParticipant, 'mean_incongruent_all']) /
            res$dat[currentParticipant, 'sd_all'];
          res$dat[currentParticipant, 'd600_real'] <-
            (res$dat[currentParticipant, 'mean_congruent_real'] -
             res$dat[currentParticipant, 'mean_incongruent_real']) /
             res$dat[currentParticipant, 'sd_real'];
          res$dat[currentParticipant, 'd600_practice'] <-
            (res$dat[currentParticipant, 'mean_congruent_practice'] -
             res$dat[currentParticipant, 'mean_incongruent_practice']) /
             res$dat[currentParticipant, 'sd_practice'];
          
          if (!congruentLarger) {
            res$dat[currentParticipant, 'd600_all'] <- -1 * res$dat[currentParticipant, 'd600_all'];
            res$dat[currentParticipant, 'd600_real'] <- -1 * res$dat[currentParticipant, 'd600_real'];
            res$dat[currentParticipant, 'd600_practice'] <- -1 * res$dat[currentParticipant, 'd600_practice'];
          }
          
          ### Store number of trials that had response times
          ### below or above the acceptable bandwidth, as well
          ### as in between, for each block
          res$dat[currentParticipant, 'rt_tooLow'] <-
            sum(!res$dat.raw[[currentParticipant]]$rt_lowEnough);
          res$dat[currentParticipant, 'rt_tooHigh'] <-
            sum(!res$dat.raw[[currentParticipant]]$rt_highEnough);
          res$dat[currentParticipant, 'rt_valid'] <-
            sum(res$dat.raw[[currentParticipant]]$rt_valid);
      
        }
      }        
      logText <- addToLog(logText,
               paste0("Done processing file ",
                      res$inputFiles[currentParticipant], "\n"),
               showLog=showLog);
    }
  }
  
  if (sum(res$dat$validDatafile) == 0) {
    
    logText <- addToLog(logText,
                        paste0("None of the datafiles is valid! Are you ",
                               "sure you specified the correct number ",
                               "of blocks and trials (see top of this log)?\n",
                               "Note that the 'nrOfRowsInDatafile' column ",
                               "contains the number of rows in each datafile.\n"),
                        showLog=showLog);
    
  } else {

    if (!is.null(participantVarName) && !is.null(taskVarName)) {
      ### Convert data to wide format
      res$dat.wide <- reshape(res$dat,
                              timevar=taskVarName,
                              idvar=participantVarName,
                              direction="wide", sep="_");
      logText <- addToLog(logText,
                          paste0("Generated wide version of datafile\n"),
                          showLog=showLog);
      if (!is.null(wideOutputFile)) {
        ### Round numbers
        if(!is.null(roundOutput)) {
          roundedWideDat <- res$dat.wide;
          numericVars <- names(roundedWideDat)[as.vector(lapply(roundedWideDat, class)=='numeric')];
          roundedWideDat[, numericVars] <-
            round(roundedWideDat[, numericVars], roundOutput);
        }
        ### Store wide datafile
        write.table(roundedWideDat, wideOutputFile, sep="\t", dec=decimalSeparator, row.names=FALSE);
        logText <- addToLog(logText,
                            paste0("Saved wide datafile to ", outputFile, "\n"),
                            showLog=showLog);
      }
    }
    
    ### This is an attempt to do the long->wide conversion
    ### manually, but I didn't manage to get it working.
    ### However, I'm keeping it around just in case there's
    ### a need for some of this code later on.
  #   res$dat.wide <- ddply(a, "participant", sessionVar = 'session',
  #                         function(dat, sessionVar) {
  #     ### Create dataframe to return.
  #     res <- data.frame();
  #     ### Create object to store every line of the dataframe
  #     ### for this participant
  #     lines <- list();
  #     for (currentSession in levels(as.factor(dat[[sessionVar]]))) {
  #       ### Add current line from dataframe
  #       lines[[currentSession]] <- dat[dat[[sessionVar]] == currentSession, ];
  #       ### Prepend session to variable names
  #       names(lines[[currentSession]]) <-
  #         paste0(currentSession, "_", names(lines[[currentSession]]));
  #     }
  #     ### Combine dataframes and return using rbind.fill,
  #     ### from the plyr package
  #     return(rbind.fill(lines));
  #   });
    
  }

  if (!is.null(outputFile)) {
    ### Round numbers
    if(!is.null(roundOutput)) {
      roundedDat <- res$dat;
      numericVars <- names(roundedDat)[as.vector(lapply(roundedDat, class)=='numeric')];
      roundedDat[, numericVars] <-
        round(roundedDat[, numericVars], roundOutput);
    }
    
    ### Store aggregated datafile
    write.table(roundedDat, outputFile, sep="\t", dec=decimalSeparator, row.names=FALSE);
    logText <- addToLog(logText,
                        paste0("Saved aggregated datafile to ", outputFile, "\n"),
                        showLog=showLog);
  }
  
  ### Store log
  res$log <- logText;
  
  ### Set classes for returned object and the log
  class(res) <- c('processOpenSesameIAT');
  class(res$log) <- c("processOpenSesameIAT.log");
  
  return(res);

}

print.processOpenSesameIAT <- function (x, ...) {
  cat("Ran succesfully - parsed", nrow(x$dat), "files.\n");
  if (!is.null(x$outputFile)) {
    cat("Stored aggregated datafile in", x$outputFile, "\n");
  }
}

print.processOpenSesameIAT.log <- function(x, ...) {
  cat(x);
}
