###########################################################
###########################################################
###
### Function to generate an object with several useful
### statistics and a plot to assess how the elements
### (usually items) in a scale relate to each other.
###
### File created by Gjalt-Jorn Peters. Questions? You can
### contact me through http://behaviorchange.eu.
###
###########################################################
###########################################################

### Scale Diagnosis
scaleDiagnosis <- function(dat=NULL, items=NULL, plotSize=180, sizeMultiplier = 1,
                          axisLabels = "none", scaleReliability.ci=FALSE, conf.level=.95) {
  
  ### dat            = dataframe
  ### items  = a list of the variables that together
  ###                  make up the scale
  ### plotSize       = size of the final plot in millimeters
  ### sizeMultiplier = allows more flexible control over the size
  ###                  of the plot elements

  ### If no dataframe was specified, load it from an SPSS file
  if (is.null(dat)) {
    dat <- getData(errorMessage=paste0("No dataframe specified, and no valid SPSS file selected in ",
                                       "the dialog I then showed to allow selection of a dataset."),
                   use.value.labels=FALSE);
  }
  else {
    if (!is.data.frame(dat)) {
      stop("Argument 'dataframe' must be a dataframe or NULL! Class of ",
           "provided argument: ", class(dat));
    }
  }
  
  if (is.null(items)) {
    items <- names(dat);
  }
  
  ### Create object to store results
  res <- list();
  res$items <- items;
  res$plotSize <- plotSize;
  res$sizeMultiplier <- sizeMultiplier;
  
  ### Extract dataframe and select only complete cases
  res$dat <- dat[complete.cases(dat[, items]), items];
  res$n <- nrow(res$dat);
  
  ### Convert all variables to numeric vectors, if they weren't already
  res$dat <- data.frame(lapply(res$dat, 'as.numeric'));
  
  ### Basic univariate descriptives
  res$describe <- describe(res$dat);
  
  ### Bivariate correlations
  res$cor <- cor(res$dat, use="complete.obs");
  
  ### The size of each panel in the scattermatrix depends
  ### on the number of items - therefore, we need to adjust
  ### the plot sizes to the number of items.
  baseSize <- (sizeMultiplier * (plotSize / ncol(res$cor))) / 100;
  
  plotSettings <- theme(axis.line = element_line(size = baseSize),
                        panel.grid.major = element_line(size = baseSize/2),
                        line = element_line(size = baseSize/2),
                        axis.ticks = element_line (size=baseSize/2)
                       );  
  
  ### Visual representation of bivariate correlations
  ### First generate a normal scattermatrix with histograms
  ### on the diagonal
  res$ggpairs.normal <- ggpairs(res$dat, diag=list(continuous="bar", discrete="bar"),
                                axisLabels=axisLabels);
  ### Then generate one with jittered points
  res$ggpairs.jittered <- ggpairs(res$dat, params=c(position="jitter"), axisLabels=axisLabels);
  ### Then place the histograms on the diagonal of
  ### the jittered scattermatrix
  res$ggpairs.combined <- res$ggpairs.jittered;
  for (currentVar in 1:length(items)) {
    res$ggpairs.combined <-
      putPlot(res$ggpairs.combined,
              getPlot(res$ggpairs.normal, currentVar, currentVar),
              currentVar, currentVar);
  }

  for (currentRowFromTop in 1:length(items)) {
    for (currentColumnFromLeft in 1:length(items)) {
      res$ggpairs.combined <-
        putPlot(res$ggpairs.combined,
                getPlot(res$ggpairs.combined, currentRowFromTop, currentColumnFromLeft) + plotSettings,
                currentRowFromTop, currentColumnFromLeft);
    }
  }
  
  ### Exploratory factor analysis
  #pa.out <- factor.pa(r = bfi, nfactors = 5, residuals = FALSE,
  #                    + rotate = "varimax", n.obs = NA, scores = FALSE, SMC = TRUE,
  #                    + missing = FALSE, impute = "median", min.err = 0.001, digits = 2,
  #                    + max.iter = 100, symmetric = TRUE, warnings = TRUE, fm = "pa")
  
  ### Extract eigen values
  res$eigen <- eigen(res$cor);
  ### Determine how many factors have eigenvalues
  ### over 1 - note that we're not doing a real
  ### exploratory factor analysis, we're just interested
  ### in whether this scale works out (it's not
  ### unidimensional if more than one factor has an
  ### eigenvalue a lot over 1)
  res$factors <- sum(res$eigen$values > 1);
  
  ### If there are more than two items, do a principal
  ### component analysis and a factor analysis
  if (ncol(res$cor) > 2) {
    ### Principal components analysis
    res$pca <- principal(r = res$cor, n.obs = res$n, rotate="oblimin",
                         nfactors=res$factors);
    ### Exploratory factor analysis
    res$fa <- fa(r = res$cor, n.obs = res$n, rotate="oblimin",
                 fm="pa", nfactors=res$factors);
  }
  
  ### Internal consistency measures
  res$scaleReliability <- scaleReliability(dat=res$dat, items=items,
                                           ci=scaleReliability.ci,
                                           conf.level=conf.level);
  
  ### Return results
  class(res) <- c('scaleDiagnosis');
  return(res);
}

print.scaleDiagnosis <- function(x, ...) {
  print(x$scaleReliability, ...);
  cat(paste0("\nEigen values: ", paste(round(x$eigen$values, 3), collapse=", ")));
  print(x$pca$loadings, ...);
  cat("\n");
  print(x$describe, ...);
  print(x$ggpairs.combined, ...);
  invisible();
}
