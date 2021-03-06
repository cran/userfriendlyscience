### Function for omega squared (etasq)


#' Confidence intervals for Omega Squared
#' 
#' This function used the \link{MBESS} function \code{\link{conf.limits.ncf}}
#' and \code{\link{convert.ncf.to.omegasq}} to compute the point estimate and
#' confidence interval for Omega Squared.
#' 
#' 
#' @param var1,var2 The two variables: one should be a factor (or will be made
#' a factor), the other should have at least interval level of measurement. If
#' none of the variables is a factor, the function will look for the variable
#' with the least unique values and change it into a factor.
#' @param conf.level Level of confidence for the confidence interval.
#' @return
#' 
#' A \code{confIntOmegaSq} object is returned, with as elements:
#' 
#' \item{input}{The input arguments} \item{intermediate}{Objects generated
#' while computing the output} \item{output}{The output of the function,
#' consisting of:} \item{output$es}{The point estimate} \item{output$ci}{The
#' confidence interval}
#' @note Formula 16 in Steiger (2004) is used for the conversion in
#' \code{\link{convert.ncf.to.omegasq}}.
#' @author Gjalt-Jorn Peters
#' 
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @references Steiger, J. H. (2004). Beyond the F test: Effect size confidence
#' intervals and tests of close fit in the analysis of variance and contrast
#' analysis. Psychological Methods, 9(2), 164-82.
#' https://doi.org/10.1037/1082-989X.9.2.164
#' @keywords bivar
#' @examples
#' 
#' 
#' confIntOmegaSq(mtcars$mpg, mtcars$cyl);
#' 
#' 
#' @export confIntOmegaSq
confIntOmegaSq <- function(var1, var2, conf.level=.95) {
  
  if (is.factor(var1) & is.numeric(var2)) {
    factor <- var1;
    dependent <- var2;
  }
  else if (is.factor(var2) & is.numeric(var1)) {
    factor <- var2;
    dependent <- var1;
  } else if (nlevels(as.factor(var1)) < nlevels(as.factor(var2))) {
    ## We will treat var1 as factor
    factor <- factor(var1);
    dependent <- as.numeric(var2);
  }
  else {
    factor <- factor(var2);
    dependent <- as.numeric(var1);
  }
  
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  ### Confidence level should be doubled (i.e. unconfidence level
  ### should be doubled to be more precise), so .95 becomes .90 ->
  ### see http://daniellakens.blogspot.nl/2014/06/calculating-confidence-intervals-for.html
  ### for a brief explanation and links to more extensive explanations.
  
  res$intermediate$realConfidence <- 1 - ((1-conf.level) * 2);
  
  res$intermediate$object.aov <- aov(dependent ~ factor);

  df_num <- summary(res$intermediate$object.aov)[[1]][1,1];
  df_den <- summary(res$intermediate$object.aov)[[1]][2,1];
  f_val <- summary(res$intermediate$object.aov)[[1]][1,4];

  ### Noncentrality parameter for F distribution
  
  res$intermediate$object.ncf <- conf.limits.ncf(F.value = f_val,
                                                 conf.level = conf.level,
                                                 df.1 = df_num,
                                                 df.2 = df_den);

  res$output$es <- convert.f.to.omegasq(f_val, df_num, df_den);
  
  ### Converting to omega^2 using formula 16 in Steiger (2004)
  
  res$output$ci <- c(convert.ncf.to.omegasq(res$intermediate$object.ncf$Lower.Limit,
                                            df_num + df_den + 1),
                     convert.ncf.to.omegasq(res$intermediate$object.ncf$Upper.Limit,
                                            df_num + df_den + 1));
  
  class(res) <- 'confIntOmegaSq';
  return(res);
  
}

print.confIntOmegaSq <- function(x, ..., digits=2) {
  
  cat0("Omega squared: ",
       100*x$input$conf.level, "% CI = [",
      paste0(formatR(x$output$ci, digits=digits), collapse="; "),
      "], point estimate = ", formatR(x$output$es, digits=digits));
  
}
