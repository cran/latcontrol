\name{compare.res}
\alias{compare.res}
\title{
Parameter estimates of structural equation models with and without control
variable(s)}

\description{
Comprehensive heads-on comparison of pertinent parameter estimates of two
structural equation models that only differ in terms of the inclusion or
exclusion of one or more control variable(s). Thereby, standardized loadings,
path coefficients, and covariances as well as p-values are displayed. The models
must have been fitted with the R package lavaan (Rosseel, 2012)
<doi:10.18637/jss.v048.i02>. The derivation of the methodology employed in this
package can be obtained from Blötner (2023) <doi:10.31234/osf.io/dy79z>.}

\usage{
compare.res(object_with, object_without)
}
\arguments{
  \item{object_with}{Fitted lavaan object involving the control variable(s).}
  \item{object_without}{Fitted lavaan object without the control variable(s).}
}

\value{
\item{Results}{A list containing the parameter estimates of the structural
equation models with and without the control variable(s) outlined below.}
\item{lhs}{Left-hand side of the parameter estimate both models contain.}
\item{op}{Operator (`=~` indicates a loading, `~` a prediction of the left-hand
side object by the right-hand side object, and `~~` an undirected covariance.
See the documentation of the lavaan package (Rosseel, 2012) for details.)}
\item{rhs}{Right-hand side of the parameter estimate both models contain.}
\item{est.std.with}{Standardized parameter estimate in the model with the
control variable(s).}
\item{p.with}{p-value of the standardized parameter estimate in the model with
the control variable(s).}
\item{est.std.wo}{Standardized parameter estimate in the model without the
control variable(s).}
\item{p.wo}{p-value of the standardized parameter estimate in the model without
the control variable(s).}
\item{r}{Bivariate correlation between the parameter estimates detected in both
models as an index of profile similarity.}
}

\references{
Blötner, C. (2023). latcontrol: Evaluation of the role of control
variables in structural equation models. PsyArXiv.
https://doi.org/10.31234/osf.io/dy79z

Rosseel, Y. (2012). lavaan: An R package for structural equation modeling.
Journal of Statistical Software, 48(2), 1–36.
https://doi.org/10.18637/jss.v048.i02
}

\author{Christian Blötner
\email{c.bloetner@gmail.com}}
\keyword{&htest}
\keyword{print}
\keyword{multivariate}

\examples{

data <- data.frame(i1 = rnorm(100),
                   i2 = rnorm(100),
                   i3 = rnorm(100),
                   i4 = rnorm(100),
                   i5 = rnorm(100),
                   i6 = rnorm(100),
                   i7 = rnorm(100),
                   i8 = rnorm(100),
                   i9 = rnorm(100),
                   i10 = rnorm(100),
                   i11 = rnorm(100),
                   i12 = rnorm(100))

m_with <- 'IV =~ i1 + i2 + i3 + i4
           DV =~ i5 + i6 + i7 + i8
           CV =~ i9 + i10 + i11 + i12

           DV ~ IV + CV
           IV ~ CV'
m_without <- 'IV =~ i1 + i2 + i3 + i4
              DV =~ i5 + i6 + i7 + i8

              DV ~ IV'

fit_with <- sem(m_with, data = data)
fit_without <- sem(m_without, data = data)

compare.res(fit_with, fit_without)
}
