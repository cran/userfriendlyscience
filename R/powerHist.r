### Note: this is necessary to prevent Rcmd CHECK from throwing a note;
### otherwise it think these variable weren't defined yet.
utils::globalVariables(c('distribution', '..density..', 'normalX', 'normalY'));


### Make a 'better' histogram using ggplot
powerHist <- function(vector, distributionColor = "#2222CC",
                      normalColor = "#00CC00",
                      distributionLineSize = 2,
                      normalLineSize = 1,
                      xLabel = NULL,
                      yLabel = NULL, density=TRUE) {
  res <- list(input = list(), intermediate = list(), output = list());
  res$input <- list(vector = vector, distributionColor = distributionColor,
                    normalColor = normalColor,
                    distributionLineSize = distributionLineSize,
                    normalLineSize = normalLineSize, xLabel = xLabel,
                    yLabel = yLabel, sampleSize = length(vector));
  res$intermediate$normalX <- c(seq(min(res$input$vector), max(res$input$vector),
                     by=(max(res$input$vector) -
                         min(res$input$vector)) /
                         (res$input$sampleSize-1)));
  res$intermediate$normalY <- dnorm(res$intermediate$normalX,
                                    mean=mean(res$input$vector),
                                    sd=sd(res$input$vector));
  res$intermediate$distribution <- res$input$vector;
  res$dat <- data.frame(normalX = res$intermediate$normalX,
                        normalY = res$intermediate$normalY,
                        distribution = res$intermediate$distribution);
  res$intermediate$tempBinWidth <- (max(res$input$vector) -
                                    min(res$input$vector)) / 30;
  ### Generate labels if these weren't specified
  if (is.null(xLabel)) {
    xLabel <- paste0('Value of ', deparse(substitute(vector)));
  }
  if (is.null(yLabel)) {
    yLabel <- ifelse(density, "Density", "Frequency");
  }
  ### Plot distribution
  res$plot <- ggplot(data=res$dat, aes(x=distribution)) + 
    xlab(xLabel) +
    ylab(yLabel);
  if (density) {
    res$plot <- res$plot +
      geom_histogram(aes(y=..density..), color=NA, fill=distributionColor,
                     alpha=.25, binwidth=res$intermediate$tempBinWidth) +
      geom_density(color=distributionColor, size=distributionLineSize, alpha=.5) +
      geom_line(aes(x=normalX, y=normalY), color=normalColor, size=normalLineSize);
  }
  else {
    warning("This does not work properly yet!!! Do not use this option!");
    res$dat$normalY <- length(vector) * res$dat$normalY;
    res$plot <- res$plot +
      geom_histogram(color=NA, fill=distributionColor,
                     alpha=.25, binwidth=res$intermediate$tempBinWidth) +
      geom_density(color=distributionColor, size=distributionLineSize, alpha=.5) +
      geom_line(aes(x=normalX, y=normalY), color=normalColor, size=normalLineSize);
  }
  ### Set class and return result
  class(res) <- "powerHist";
  return(res);
}

print.powerHist <- function(x, ...) {
  print(x$plot, ...);
}
