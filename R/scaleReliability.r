scaleReliability <- function (dat=NULL, items = 'all', digits = 2,
                              ci = TRUE, conf.level=.95, silent=FALSE,
                              samples=1000, bootstrapSeed = NULL,
                              omega.psych = FALSE) {

  ### Make object to store results
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  ### If no dataframe was specified, load it from an SPSS file
  if (is.null(dat)) {
    dat <- getData(errorMessage=paste0("No dataframe specified, and no valid datafile selected in ",
                                       "the dialog I then showed to allow selection of a dataset.",
                                       "Original error:\n\n[defaultErrorMessage]"),
                   use.value.labels=FALSE);
    res$input$dat.name <- paste0("SPSS file imported from ", attr(dat, "filename"));
  }
  else {
    if (!is.data.frame(dat)) {
      stop("Argument 'dat' must be a dat or NULL! Class of ",
           "provided argument: ", class(dat));
    }
    res$input$dat.name <- deparse(substitute(dat));
  }
  
  ### if items contains only 1 element (or less), we
  ### include all items.
  if (length(items) <= 1) {
    ### Remove all cases with missing data (listwise deletion)
    res$input$dat <- na.omit(dat);
  }
  else {
    ### Select relevant items and remove all cases with
    ### missing data (listwise deletion)
    res$input$dat <- na.omit(subset(dat, select=items));
  }
  
  ### If any of the variables in the dataframe are factors, convert
  ### them to numeric vectors:
  if ("factor" %in% unlist(lapply(res$input$dat, 'class'))) {
    res$input$dat <- data.frame(lapply(res$input$dat, 'as.numeric'));
  }

  ### Set number of items and number of observations
  res$input$n.items <- ncol(res$input$dat);
  res$input$n.observations <- nrow(res$input$dat);
  
  if (samples < res$input$n.observations) {
    res$intermediate$samples <- samples <- 1.2*res$input$n.observations;
  }

  ### Also generate a dataframe (useful when
  ### requesting measures for many scales)
  res$output$dat <- data.frame(n.items        = res$input$n.items,
                               n.observations = res$input$n.observations);
                                     
  ### Get correlation matrix
  res$intermediate$cor <- cor(res$input$dat, use="complete.obs");
  ### Cronbach's alpha
  res$intermediate$alpha <- cronbach.alpha(res$input$dat, na.rm=TRUE);
  res$output$cronbach.alpha <- res$intermediate$alpha$alpha;
  res$output$dat$cronbach.alpha <- res$intermediate$alpha$alpha;
  
  ### GLB and Omega can only be computed if the number
  ### of items exceeds two
  
  if (res$input$n.items == 2) {
    ### Otherwise, compute Spearman Brown coefficient
    ### (see Eisinga, te Grotenhuis & Pelzer (2013). The reliability
    ### of a two-item scale: Pearson, Cronbach, or Spearman-Brown?
    ### International journal of public health, 58(4), 637-42.
    ### doi:10.1007/s00038-012-0416-3)
    ### Get r in numeric variable for convenience
    r <- res$intermediate$cor[1,2];
    res$intermediate$spearman.brown <- 1 / (1 + (1 / ((r/(1-r)) + (r/(1-r)))));
    res$output$spearman.brown <- res$intermediate$spearman.brown;
    res$output$dat$spearman.brown <- res$intermediate$spearman.brown;
  }
  else if (res$input$n.items > 2) {
    ### GLB
    suppressWarnings(res$intermediate$glb <- glb(res$input$dat));
    res$output$glb  <- res$intermediate$glb$glb.max;
    res$output$dat$glb  <- res$intermediate$glb$glb.max;
    ### Omega
    res$intermediate$omega <- ci.reliability(res$input$dat, type="omega");
    res$output$omega <- res$intermediate$omega$est;
    res$output$dat$omega <- res$intermediate$omega$est;
    if (omega.psych) {
      suppressWarnings(res$intermediate$omega.psych <- omega(res$input$dat, plot=FALSE));
      res$output$omega.psych <- res$intermediate$omega.psych$omega.tot;
      res$output$dat$omega.psych <- res$intermediate$omega.psych$omega.tot;
    }
    if (ci) {
      ### Also compute confidence intervals
      
      ### Check whether seed value is specified
      if (is.null(bootstrapSeed)) {
        bootstrapSeed <- as.numeric(format(Sys.time(), "%Y%m%d"));
        warning("No 'bootstrapSeed' specified. This means that the ",
                "bootstrapping results will not be exactly replicable. ",
                "Setting current date as seed (", bootstrapSeed, ").");
      }
      
      ### Set seed
      res$input$bootstrapSeed <- bootstrapSeed;
      set.seed(res$input$bootstrapSeed);
      
      if (!silent) {
        cat("-- STARTING BOOTSTRAPPING TO COMPUTE CONFIDENCE INTERVALS! --\n");
        cat("--    (this might take a while, computing", samples,"samples)    --\n");
      }
      
      ### Compute probabilities for Cronbach's alpha CI and compute it
      alpha.probs <- c((1-conf.level)/2, 1-(1-conf.level)/2);
      res$intermediate$alpha <- cronbach.alpha(res$input$dat, CI=TRUE, probs=alpha.probs,
                                               B=samples, na.rm=TRUE);
      ### Compute CI for omega
      res$intermediate$omega.ci <- ci.reliability(res$input$dat, type="omega",
                                                  conf.level = conf.level,
                                                  interval.type="bca", B=samples);
      ### Extract and store ci bounds
      res$output$alpha.ci <- res$intermediate$alpha$ci;
      res$output$omega.ci <- c(res$intermediate$omega.ci$ci.lower,
                               res$intermediate$omega.ci$ci.upper);
      ### Add to dat
      res$output$dat$alpha.ci.lo <- res$output$alpha.ci[1];
      res$output$dat$alpha.ci.hi <- res$output$alpha.ci[2];
      res$output$dat$omega.ci.lo <- res$output$omega.ci[1];
      res$output$dat$omega.ci.hi <- res$output$omega.ci[2];
      if (!silent) {
        cat("-- FINISHED BOOTSTRAPPING TO COMPUTE CONFIDENCE INTERVALS! --\n");
      }
    }  
  }
  
  class(res) <- "scaleReliability";
  ### Return result
  return(res);
}

print.scaleReliability <- function (x, digits=x$input$digits, ...) {
  
  cat(paste0("                       dat: ", x$input$dat.name,
             "\n                     Items: ", paste(x$input$items, collapse=", "),
             "\n              Observations: ", x$input$n.observations));
  if (x$input$n.items > 2) {
    
    cat("\n                     Omega:", round(x$output$omega, digits=digits));
    if (x$input$omega.psych) {
      cat(paste0("\nOmega (from psych package): ", round(x$output$omega.psych, digits=digits)));
    }
    cat(paste0("\nGreatest Lower Bound (GLB): ", round(x$output$glb, digits=digits),
               "\n          Cronbach's alpha: ", round(x$output$cronbach.alpha, digits=digits), "\n"));
  } else if (x$input$n.items == 2) {
    cat(paste0("\nSpearman Brown coefficient: ", round(x$output$spearman.brown, digits=digits),
               "\n          Cronbach's alpha: ", round(x$output$cronbach.alpha, digits=digits),
               "\n       Pearson Correlation: ", round(x$intermediate$cor[1, 2], digits=digits), "\n"));
  }
  if (x$input$ci & !is.null(x$output$alpha.ci)) {
    ### If confidence intervals were computed AND obtained, print them
    cat(paste0("\nConfidence intervals:\n                     Omega: [",
               round(x$output$omega.ci[1], digits=digits), ", ",
               round(x$output$omega.ci[2], digits=digits), "]\n",
               "          Cronbach's alpha: [", round(x$output$alpha[1], digits=digits),
               ", ", round(x$output$alpha[2], digits=digits), "]\n"));
    if (x$input$omega.psych) {
      cat(paste0("\nNote: the normal point estimate and confidence interval for omega are based on the procedure suggested by ",
                 "Dunn, Baguley & Brunsden (2013) using the MBESS function ci.reliability, whereas the psych package point estimate was ",
                 "suggested in Revelle & Zinbarg (2008). See the help ('?scale.ic') for more information.\n"));
    }
  }
  invisible();
}
