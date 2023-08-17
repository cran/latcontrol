#' Evaluation of the equivalence of the model-implied matrices of structural
#' equation models with and without control variables
#'
#' @param object_with Fit object from the `lavaan` package with the control
#' variable(s)
#' @param object_without Fit object from the `lavaan` package without the
#' control variable(s)
#' @param type Optional. Specifies whether a single-level structural equation
#' model or a multilevel structural equation model is entered
#' (DEFAULT = "simple").
#'
#' @return Chi-square statistic, degrees of freedom, and descriptive fit indices
#' @export

latcontrol <- function(object_with,
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

  # Extraction of rows and columns both matrices have in common
  ## Align orders of variables in both datasets
  mat1 <- mat1[sort(colnames(mat1)), sort(rownames(mat1))]
  mat2 <- mat2[sort(colnames(mat2)), sort(rownames(mat2))]

  ## Reduction of dataframes to rows and columns both have in common
  mat1 <- mat1[is.element(colnames(mat1), colnames(mat2)),
               is.element(colnames(mat1), colnames(mat2))]
  mat2 <- mat2[is.element(colnames(mat2), colnames(mat1)),
               is.element(colnames(mat2), colnames(mat1))]

  # Defining the number of manifest variables

  k <- nrow(mat1)

  # Adaptation of the discrepancy function of the maximum likelihood estimator

  f_ML <- log(det(mat1)) - log(det(mat2)) + sum(diag(((mat2 %*% solve(mat1))))) - k

  X2 <- f_ML * (fitMeasures(object_with, "ntotal") - 1)

  df <- (k * (k + 1))

  p_value <- pchisq(X2, df, lower.tail = FALSE)

  # RMSEA

  if(X2 - df < 0){rmsea <- 0}
  else{rmsea <- sqrt((X2 - df) / (df * fitMeasures(object_with, "ntotal")))}

  # SRMR

  srmr <- mean(((cov2cor(mat1) - cov2cor(mat2))^2), na.rm = TRUE)

  # Specify output data frame

  out <- data.frame(X2 = round(X2, 3),
                    df = df,
                    p_value = round(p_value, 3),
                    rmsea = round(rmsea, 3),
                    srmr = round(srmr, 3))

  rownames(out) <- ""

  return(out)
}
