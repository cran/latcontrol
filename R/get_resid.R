#' Residual matrices of the model-implied variance-covariance matrices of
#' strcutural equation models with and without control variable(s)
#'
#' @param object_with Fit object from the `lavaan` package with the control
#' variable(s)
#' @param object_without Fit object from the `lavaan` package without the
#' control variable(s)
#' @param type Optional. Specifies whether a single-level structural equation
#' model or a multilevel structural equation model is entered
#' (DEFAULT = "simple").
#'
#' @return Unstandardized  and correlation residuals
#' @export

get_resid <- function(object_with,
                      object_without,
                      type = c("simple", "complex")){

  choice <- match.arg(type)
  input.kind <- switch(choice, simple = 1, complex = 2)
  if (input.kind == 1) {
    mat1 <- fitted(object_with)$cov
    mat2 <- fitted(object_without)$cov
  }

  if (input.kind == 2) {
    mat1 <- fitted(object_with)$within$cov
    mat2 <- fitted(object_without)$within$cov
  }

  n <- fitMeasures(object_with, "ntotal")

  # Extraction of rows and columns both matrices have in common
  ## Align orders of variables in both datasets
  mat1 <- mat1[sort(colnames(mat1)), sort(rownames(mat1))]
  mat2 <- mat2[sort(colnames(mat2)), sort(rownames(mat2))]

  ## Reduction of dataframes to rows and columns both have in common
  mat1 <- mat1[is.element(colnames(mat1), colnames(mat2)),
               is.element(colnames(mat1), colnames(mat2))]
  mat2 <- mat2[is.element(colnames(mat2), colnames(mat1)),
               is.element(colnames(mat2), colnames(mat1))]

  # Residual matrices
  ## Unstandardized

  resid_raw <- round(mat1 - mat2, 3)

  ## Correlation

  resid_cor <- round(cov2cor(mat1) - cov2cor(mat2), 3)

  return(list(raw = resid_raw,
              cor = resid_cor))
}
