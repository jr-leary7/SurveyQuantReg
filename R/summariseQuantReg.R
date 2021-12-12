#' Get summary stats for quantile regression models of survey data.
#'
#' @name summariseQuantReg
#' @author Jack Leary
#' @description This function summarises the coefficients of quantile regression models.
#' @importFrom stats qnorm coef pt
#' @importFrom dplyr mutate
#' @param quant.reg.fit The bootstrapped coefficients from a quantile regression model. Defaults to NULL.
#' @param boot.weights Bootstrapped replicate weights as generated using \code{\link{bootSurveyDesign}}. Defaults to NULL.
#' @param alpha The desired Type I Error Rate. Defaults to \eqn{\alpha = 0.05}.
#' @return A data.frame containing variable names, coefficients, standard errors, test statistics, and \emph{p}-values.
#' @seealso \code{\link{bootSurveyDesign}} \code{\link[quantreg]{rq}}
#' @export
#' @examples
#' \dontrun{summariseQuantReg(quant.reg.fit = my_model, boot.weights = boot_sample_weights, alpha = 0.01)}

summariseQuantReg <- function(quant.reg.fit = NULL, boot.weights = NULL, alpha = 0.05) {
  # check inputs
  if (is.null(quant.reg.fit) | is.null(boot.weights)) { stop("Arguments to summariseQuantReg() must be non-null.") }
  Z <- abs(stats::qnorm(0.05 / 2))
  # compute degrees freedom
  n_covariates <- length(stats::coef(quant.reg.fit)) - 1
  weighted_n <- round(sum(boot.weights$pweights))
  residual_df <- weighted_n - n_covariates
  # extract coefficients, test statistics, and p-values
  var_covar_mat <- attr(quant.reg.fit, "var")
  se <- ifelse(length(var_covar_mat) == 1,
               list(sqrt(var_covar_mat)),
               list(sqrt(diag(var_covar_mat))))
  se <- unlist(se)
  res_table <- data.frame(VARIABLE = names(quant.reg.fit),
                          COEF = unname(as.matrix(quant.reg.fit))[, 1],
                          SE = unname(se)) %>%
               dplyr::mutate(STAT = COEF / SE,
                             PVAL = Z * (1 - stats::pt(abs(STAT), residual_df)))
  return(res_table)
}
