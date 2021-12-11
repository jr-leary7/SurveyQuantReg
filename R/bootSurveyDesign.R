#' Generate replicate weights for complex survey designs.
#'
#' @name bootSurveyDesign
#' @author Jack Leary
#' @description This function creates a \code{survey.design} object using the \code{survey} package, then uses resampling to generate sample weights.
#' @importFrom survey svydesign as.svrepdesign
#' @param survey.dat A data.frame containing survey data. Defaults to NULL.
#' @param id.var A string containing the name of the variable that denotes clusters of primary sampling units. Defaults to NULL.
#' @param strata.var A string containing the name of the variable that stratifies the PSUs. Defaults to NULL.
#' @param weight.var A string containing the name of the variable containing sampling weights. Defaults to NULL.
#' @param nested.clusters Are the clusters nested within strata? Defaults to TRUE
#' @param boot.type A string specifying the resampling method. Passed to the \code{type} argument of \code{\link[survey]{as.svrepdesign}}. Defaults to "bootstrap".
#' @param n.boot The number of replications to perform. Defaults to 1000.
#' @return An object of class \code{svyrep.design} containing replicate weights.
#' @seealso \code{\link[survey]{svydesign}} \code{\link[survey]{as.svrepdesign}}
#' @export
#' @examples
#' \dontrun{bootSurveyDesign(survey.dat = nhanes_data, id.var = "CLUST_ID", strata.var = "STRATUM", weight.var = "SAMPLE_WEIGHT")}

bootSurveyDesign <- function(survey.dat = NULL,
                             id.var = NULL,
                             strata.var = NULL,
                             weight.var = NULL,
                             nested.clusters = TRUE,
                             boot.type = "bootstrap",
                             n.boot = 1000) {
  # check inputs
  if (any(unlist(lapply(list(survey.dat, id.var, strata.var, weight.var, nested.clusters, survey.dat), is.null)))) {
    stop("Arguments to bootSurveyDesign() must be non-null.")
  }
  # survey design
  survey_design <- survey::svydesign(id = ~as.symbol(id.var),
                                     strata = ~as.symbol(strata.var),
                                     weights = ~as.symbol(weight.var),
                                     nest = nested.clusters,
                                     data = survey.dat)
  # bootstrap sample weights
  boot_weights <- survey::as.svrepdesign(survey_design,
                                         type = "bootstrap",
                                         replicates = n.boot)
  return(boot_weights)
}
