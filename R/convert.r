convert.r.to.t <- function(r, n) {
  return(r * sqrt((n - 2) / (1-r^2)));
}

convert.t.to.r <- function(t, n) {
  return(t / (sqrt(n-2+t^2)));
}

convert.t.to.p <- function(t, df) {
  return(2*pt(-abs(t),df));
}

convert.f.to.p <- function(f, df1, df2, lower.tail=FALSE) {
  return(2*pf(f, df1, df2, lower.tail=lower.tail));
}

convert.chisq.to.p <- function(chisq, df, lower.tail=FALSE) {
  return(2*pchisq(chisq, df, lower.tail=lower.tail));
}