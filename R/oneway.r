oneway <- function(y, x, posthoc=NULL, means=FALSE, fullDescribe=FALSE,
                   levene=FALSE, plot=FALSE, digits=2,
                   pvalueDigits=3, t=FALSE, conf.level=.95) {
  
  res <- list(input = as.list(environment()));
  
  res$input$x.name=unlist(strsplit(deparse(substitute(x)), "\\$"))[
    length(unlist(strsplit(deparse(substitute(x)), "\\$")))];
  res$input$y.name=unlist(strsplit(deparse(substitute(y)), "\\$"))[
    length(unlist(strsplit(deparse(substitute(y)), "\\$")))];
  
  if (!is.numeric(y)) {
    stop("The y variable (", res$input$y.name, ") is not a numeric ",
        "vector! Note that in analysis of variance, the 'y' variable ",
        "must have at least the interval level of measurement!");
  }
  
  if (!is.factor(x)) {
    warning("### Warning: the x variable (", res$input$x.name, ") is not a",
           "factor! Converting it myself - but note that variables in R have ",
           "data types, and it's advisable to set these adequately!");
    x <- as.factor(x);
  }
  
  assign(res$input$x.name, x);
  assign(res$input$y.name, y);
  
  res$intermediate <- list();
  
  res$intermediate$aov <- aov(formula(paste0(res$input$y.name, " ~ ",
                                             res$input$x.name)));
  
  res$intermediate$Anova <- Anova(res$intermediate$aov, type=3);
  
  if (!is.null(posthoc)) {
    if (tolower(posthoc)=="tukey") {
      res$intermediate$posthoc <- TukeyHSD(res$intermediate$aov);
    }
    else if (tolower(posthoc)=="games-howell") {
      res$intermediate$posthocTGH <- posthocTGH(y=y, x=x, method="Games-Howell");
      res$intermediate$posthoc <- res$intermediate$posthocTGH$output$games.howell;
    }
    else {
      res$intermediate$posthoc <-
        pairwise.t.test(x=y, g=x, p.adjust.method=posthoc);
    }
  }
  if (means) {
    res$intermediate$means <- by(y, x, describe);
    tmpAttributes <- attributes(res$intermediate$means);
    res$intermediate$means <- lapply(res$intermediate$means, function(x) {
      rownames(x)[1] <- ' ';
      return(x[, colnames(x) != 'vars']);
    });
    if (!fullDescribe) {
      res$intermediate$means <- lapply(res$intermediate$means, function(x) {
        return(x[, colnames(x) %in% c('n', 'mean', 'sd', 'se', 'median')]);
      });
    }
    if (t) {
      res$intermediate$means <- lapply(res$intermediate$means, t);
    }
    attributes(res$intermediate$means) <- tmpAttributes;
  }
  
  if (levene) {
    res$intermediate$leveneTest <- leveneTest(y, group=x, center=mean);
  }
  
  res$intermediate$etasq <- computeEffectSize_etasq(var1=x, var2=y,
                                                    conf.level=conf.level);
  
  res$output <- list(etasq = res$intermediate$Anova$`Sum Sq`[2] /
                       res$intermediate$Anova$`Sum Sq`[3],
                     etasq.ci = res$intermediate$etasq$ci);
  
  res$output$dat <- data.frame(SS = res$intermediate$Anova$`Sum Sq`[2:3],
                               Df = res$intermediate$Anova$Df[2:3]);
  res$output$dat$MS <- res$output$dat$SS / res$output$dat$Df;
  res$output$dat[1, 'F'] <- res$intermediate$Anova$F[2];
  res$output$dat[1, 'p'] <- res$intermediate$Anova$`Pr(>F)`[2];
  row.names(res$output$dat) <- c('Between groups (error + effect)',
                                 'Within groups (error only)');
  
  if (plot) {
    res$intermediate$dat <- data.frame(x, y);
    names(res$intermediate$dat) <- c(res$input$x.name, res$input$y.name);
    res$output$plot <- dlvPlot(res$intermediate$dat,
                               x=res$input$x.name,
                               y=res$input$y.name);
  }
  
  class(res) <- 'oneway';
  return(res);
}

print.oneway <- function(x, digits=x$input$digits,
                         pvalueDigits=x$input$pvalueDigits,
                         na.print="", ...) {  
  if (x$input$means) {
    cat(paste0("### Means for y (", x$input$y.name, ") separate for each level of x (", x$input$x.name, "):\n"));
    print(x$intermediate$means, digits=digits);
    cat("\n");
  }
  
  if (x$input$plot) {
    print(x$output$plot);
  }
  
  cat(paste0("### Oneway Anova for y=", x$input$y.name,
             " and x=", x$input$x.name, "\n"));
  cat(paste0("Eta Squared: 95% CI = [", round(x$output$etasq.ci[1], digits),
             ";", round(x$output$etasq.ci[2], digits),
             "], point estimate = ", round(x$output$etasq, digits), "\n\n"));
  
  x$output$dat[, 1:4] <- round(x$output$dat[, 1:4], digits);

  ### Format p-values nicely
  x$output$dat$p <- formatPvalue(x$output$dat$p,
                                 digits=pvalueDigits,
                                 includeP=FALSE);
  
  ### Temporarily store row names and transform everything to character
  tmpRowNames <- row.names(x$output$dat);
  x$output$dat <- data.frame(lapply(x$output$dat, as.character));
  row.names(x$output$dat) <- tmpRowNames;

  if(x$input$t) {
    print(t(x$output$dat), na.print=na.print, quote=FALSE);
  } else {
    print(x$output$dat, na.print=na.print, quote=FALSE);
  }
  
  if (x$input$levene) {
    cat("\n### Levene's test:\n");
    print(x$intermediate$leveneTest);
  }
  
  if (!is.null(x$input$posthoc)) {
    cat(paste0("\n### Post hoc test: ", x$input$posthoc,"\n"));
    print(x$intermediate$posthoc, digits=digits);
  }
  
}