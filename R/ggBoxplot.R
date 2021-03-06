#' Box plot using ggplot
#' 
#' This function provides a simple interface to create a \code{\link{ggplot}}
#' box plot, organising different boxplots by levels of a factor is desired,
#' and showing row numbers of outliers.
#' 
#' This function is based on JasonAizkalns' answer to a question on Stack
#' Exchange (Cross Validated; see
#' \url{http://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r}).
#' 
#' @param dat Either a vector of values (to display in the box plot) or a
#' dataframe containing variables to display in the box plot.
#' @param y If \code{dat} is a dataframe, this is the name of the variable to
#' make the box plot of.
#' @param x If \code{dat} is a dataframe, this is the name of the variable
#' (normally a factor) to place on the X axis. Separate box plots will be
#' generate for each level of this variable.
#' @param labelOutliers Whether or not to label outliers.
#' @param outlierColor If labeling outliers, this is the color to use.
#' @param theme The theme to use for the box plot.
#' @param \dots Any additional arguments will be passed to
#' \code{\link{geom_boxplot}}.
#' @return A \code{\link{ggplot}} plot is returned.
#' @author Jason Aizkalns; implemented in this package (and tweaked a bit) by
#' Gjalt-Jorn Peters.
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso \code{\link{geom_boxplot}}
#' @keywords hplot
#' @examples
#' 
#' ### A box plot for miles per gallon in the mtcars dataset:
#' ggBoxplot(mtcars$mpg);
#' 
#' ### And separate for each level of 'cyl' (number of cylinder):
#' ggBoxplot(mtcars, y='mpg', x='cyl');
#' 
#' @export ggBoxplot
ggBoxplot <- function(dat, y = NULL, x = NULL,
                      labelOutliers = TRUE,
                      outlierColor = 'red',
                      theme = theme_bw(), ...) {
  if (is.null(x) && is.null(y)) {
    if (is.numeric(dat)) {
      if (length(dat) < 5) {
        stop("If both arguments 'x' and 'y' are NULL, the first argument, 'dat, ",
             "should be a vector of values, but it's only ", length(dat),
             " elements long, which isn't enough to generate a boxplot.");      }
      varname <- deparse(substitute(dat));
      ### Take variable only in case a variable in a dataframe was specified
      varname <- extractVarName(varname);
      tmpDf <- data.frame(dat);
      names(tmpDf) <- varname;
      tmpDf$outlier <- ifelse(iqrOutlier(tmpDf[, varname]),
                              1:nrow(tmpDf),
                              as.numeric(NA));
      resPlot <- ggplot(tmpDf, aes_string(y=varname)) +
        geom_boxplot(aes(x=factor(varname)), ...) +
        xlab("") + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank());
      if (labelOutliers) {
        resPlot <- resPlot +
        geom_text_repel(aes_string(x='1', label = 'outlier'), na.rm = TRUE,
                        color = outlierColor);
      }
      return(resPlot);
    } else {
      stop("If both arguments 'x' and 'y' are NULL, the first argument, 'dat, ",
           "should be a vector, but instead, it has class '", class(dat),
           "'.");
    }
  } else {
    
    if (is.null(y)) {
      stop("Argument 'y' should be a text string specifying a variable in the ",
           "dataframe specified by 'dat'; instead, no 'y' is specified.");
    }
    
    if (length(y) > 1) {
      warning("If argument 'y' is specified, it should be the name of a ",
              "variable in the dataframe specified by argument 'dat'. However, ",
              "'y' had ", length(y), "elements. Discarding all but the last one.");
    }
    
    if (!(y %in% names(dat))) {
      stop("Argument 'y' should be a text string specifying a variable in the ",
           "dataframe specified by 'dat', but '", y, "' isn't among ",
           "names(dat). Please check your spelling, and remember that R is ",
           "case sensitive: it matters whether you use capitals or not!");
    }
    
    if (is.null(x)) {
      
      dat$outlier <- ifelse(iqrOutlier(dat[, y]),
                            1:nrow(dat),
                            as.numeric(NA));
      
      resPlot <- ggplot(dat, aes_string(y=y)) + geom_boxplot(aes(x=factor(y)), ...) +
               xlab("") + theme +
               theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank());
      if (labelOutliers) {
        resPlot <- resPlot +
          geom_text_repel(aes_string(x='1', label = 'outlier'), na.rm = TRUE,
                          color = outlierColor);
      }
      return(resPlot);
      
    } else {
      
      if (length(x) > 1) {
        warning("If argument 'x' is specified, it should be the name of a ",
                "variable (a factor, normally) in the dataframe",
                " specified by argument 'dat'. However, ",
                "'x' had ", length(x), "elements. Discarding all but the last one.");
      }
      
      
      if (!(x %in% names(dat))) {
        stop("Argument 'x' should be a text string specifying a variable in the ",
             "dataframe specified by 'dat', but '", x, "' isn't among ",
             "names(dat). Please check your spelling, and remember that R is ",
             "case sensitive: it matters whether you use capitals or not!");
      } else if (!is.factor(dat[, x])) {
        dat[, x] <- factor(dat[, x]);
      }
      
      ### Based on JasonAizkalns' answer at
      ### http://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r

      dat <-
        ddply(dat, x,
              function(datF) {
                datF$outlier <- iqrOutlier(datF[, y]);
                return(datF);
              });
      dat$outlier <- ifelse(dat$outlier,
                            1:nrow(dat),
                            as.numeric(NA));

      resPlot <- ggplot(dat, aes_string(y=y, x=x)) +
        geom_boxplot(...) +
        theme;
      if (labelOutliers) {
        resPlot <- resPlot +
          geom_text_repel(aes_string(label = 'outlier'), na.rm = TRUE,
                          color=outlierColor);
      }
      return(resPlot);

    }
  }
}
