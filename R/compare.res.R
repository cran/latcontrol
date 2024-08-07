#' Parameter estimates of structural equation models with and without control
#' variable(s)
#'
#' @param object_with Fit object from the `lavaan` package with the control
#' variable(s)
#' @param object_without Fit object from the `lavaan` package without the
#' control variable(s)
#'
#' @return Parameter estimates across models and correlation between the
#' parameters
#' @export

compare.res <- function(object_with, object_without){

  # Extraction of loadings and p-values of models

  out.with <- standardizedsolution(object_with)
  out.wo <- standardizedsolution(object_without)

  if(any(colnames(out.with) == "label") | any(colnames(out.wo) == "label")){
    # Extraction of the lines of interest

    rows.with <- paste(out.with$lhs,
                       out.with$op,
                       out.with$rhs,
                       out.with$label)
    rows.wo <- paste(out.wo$lhs,
                     out.wo$op,
                     out.wo$rhs,
                     out.wo$label)}
  if(!any(colnames(out.with) == "label") | !any(colnames(out.wo) == "label")){
    rows.with <- paste(out.with$lhs,
                       out.with$op,
                       out.with$rhs)
    rows.wo <- paste(out.wo$lhs,
                     out.wo$op,
                     out.wo$rhs)}

  # Reduction of both tables to parameters both have in common

  out.w.red <- out.with[is.element(rows.with,
                                   rows.wo), ]
  out.wo.red <- out.wo[is.element(rows.wo,
                                  rows.with), ]

  # Produce the heads-on comparison

  if(any(colnames(out.with) == "label") | any(colnames(out.wo) == "label")){

    compare.res <- data.frame(out.w.red[1:nrow(out.w.red),
                                        c("lhs", "op", "rhs", "label",
                                          "est.std", "pvalue")],
                              out.wo.red[1:nrow(out.wo.red),
                                         c("est.std", "pvalue")])
    colnames(compare.res) <- c("lhs", "op", "rhs", "label",
                               "est.std.with", "p.with",
                               "est.std.wo", "p.wo")
  }

  if(!any(colnames(out.with) == "label") | !any(colnames(out.wo) == "label")){
    compare.res <- data.frame(out.w.red[1:nrow(out.w.red),
                                        c("lhs", "op", "rhs",
                                          "est.std", "pvalue")],
                              out.wo.red[1:nrow(out.wo.red),
                                         c("est.std", "pvalue")])
    colnames(compare.res) <- c("lhs", "op", "rhs",
                               "est.std.with", "p.with",
                               "est.std.wo", "p.wo")

  }

  # Quantify the agreement of the parameter profiles between the two models

  r <- round(cor(compare.res$est.std.with,
                 compare.res$est.std.wo),
             3)

  # Combine heads-on comparison and agreement in the output

  return(list(Results = compare.res,
              r = r))

}
