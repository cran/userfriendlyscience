convert.r.to.t <- function(r, n) {
  return(r * sqrt((n - 2) / (1-r^2)));
}

convert.t.to.r <- function(t, n) {
  return(t / (sqrt(n-2+t^2)));
}

convert.t.to.p <- function(t, df) {
  return(2*pt(-abs(t),df));
}

convert.chisq.to.V <- function(chisq, n, minDim) {
  return(as.numeric(sqrt(chisq/(n*(minDim - 1)))));
}

convert.f.to.p <- function(f, df1, df2, lower.tail=FALSE) {
  return(2*pf(f, df1, df2, lower.tail=lower.tail));
}

convert.f.to.d <- function(f, df1, df2 = NULL, n1=NULL, n2=NULL, proportion=.5) {
  if (df1 != 1) {
    warning("You can only convert an F value for the comparison of two groups to Cohen's d, ",
            "and you specified a df1 of ", df1, ", which means this F value concerns the comparison ",
            "of ", df1 + 1, " groups. Returning NA.");
    return(NA);
  }
  else if (is.null(df2) && !is.null(n1) && !is.null(n2)) {
    groupSize1 <- n1;
    groupSize2 <- n2;
  }
  else if (!is.null(df2) && is.null(n1) && is.null(n2)) {
    groupSize1 <- proportion * (df1 + df2 + 1);
    groupSize2 <- (1 - proportion) * (df1 + df2 + 1);
  }
  else {
    warning("Specify either df2 (and ideally proportion) or n1 and n2! Returning NA.");
    return(NA);
  }
  
  d <- sqrt(f * ((groupSize1 + groupSize2) / (groupSize1 * groupSize2)) *
  ((groupSize1 + groupSize2) / (groupSize1 + groupSize2 - 2)));
  
  return(d);
}

convert.chisq.to.p <- function(chisq, df, lower.tail=FALSE) {
  return(2*pchisq(chisq, df, lower.tail=lower.tail));
}

convert.b.to.t <- function(b, se) {
  return(b/se);
}






