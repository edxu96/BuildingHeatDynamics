
#' Perform log likehood test
#'
#' Perform a likelihood ratio test: lambda = lik(smallerModel) / lik(largerModel) ,
#' where the smallerModel is submodel of the largerModel and lambda is chi2(f)
#' distributed with f = dim(smallerModel) - dim(largerModel). Page 20 in Madsen2006.
#' @param mod_s Nested smaller model.
#' @param mod Original model.
test_logLik_ratio <- function(mod, mod_s){
  logLikSmallModel <- mod_s$loglik
  logLikLargeModel <- mod$loglik
  ## Calculate lambda
  chisqStat <- -2 * (logLikSmallModel - logLikLargeModel)
  prmDiff <- mod$model$NPARAM - mod_s$model$NPARAM
  p_value <- 1 - pchisq(chisqStat, prmDiff)
  if (p_value < 0.05) {
    message("The mod is preferred over the mod_s.")
  } else {
    message("The mod is not preferred over the mod_s.")
  }
}
