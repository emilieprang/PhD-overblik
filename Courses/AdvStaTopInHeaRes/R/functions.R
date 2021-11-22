library(ggplot2)

plotEstimates <- function(...) {
  innames <- sapply(substitute(list(...))[-1], deparse)
  pF <- NULL
  ms <- list(...)
  mnames <- names(ms)
  
  if (is.null(mnames)) {
    mnames <- innames
  } else {
    emptyplaces <- mnames == ""
    mnames[emptyplaces] <- innames[emptyplaces]
  }
  
  n <- length(ms)
  
  for (i in 1:n) {
    m <- ms[[i]]  
    name <- mnames[[i]]
      if ("mipo" %in% class(m)) { 
        res <- summary(m) 
        if ("term" %in% names(res)) {
          sumci <- summary(m, conf.int = TRUE)
          mF <- data.frame(est = res$estimate,
                           lwr = sumci$`2.5 %`,
                           upr = sumci$`97.5 %`,
                           par = res$term,
                           model = name,
                           stringsAsFactors = FALSE)
        } else {
          #case: old version of mice, unclear when they made
          #this change
          mF <- data.frame(est = res[,1],
                           lwr = summary(m, conf.int = TRUE)[,6],
                           upr = summary(m, conf.int = TRUE)[,7],
                           par = rownames(res),
                           model = name,
                           stringsAsFactors = FALSE)
        }
      } else { #case: lm/glm/default
        mF <- data.frame(est = m$coefficients,
                         lwr = confint(m)[,1],
                         upr = confint(m)[,2],
                         par = names(coef(m)),
                         model = name,
                         stringsAsFactors = FALSE)
      }
    pF <- rbind(pF, mF)
  }
  
  pF$model <- factor(pF$model, levels = rev(mnames))
  pF$par <- factor(pF$par)
  
  q <- ggplot(pF, aes(x = par, y = est, ymin = lwr, ymax = upr, col = model)) +
    geom_point(position = position_dodge(0.8)) + 
    geom_errorbar(position = position_dodge(0.8)) +
    coord_flip() + 
    scale_x_discrete("", limits = rev(levels(pF$par))) +
    ylab("Estimate") +
    scale_color_discrete("Model", breaks = rev(levels(pF$model)))
  q
}
