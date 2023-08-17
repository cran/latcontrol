\name{latcontrol}
\alias{latcontrol}
\title{Evaluation of the equivalence of the model-implied matrices of structural
equation models with and without control variables}
\usage{
latcontrol(object_with, object_without, type = c("simple", "complex"))
}
\arguments{
\item{object_with}{Fit object from the `lavaan` package (Rosseel, 2012
<doi:10.18637/jss.v048.i02>) with the control variable(s).}
\item{object_without}{Fit object from the `lavaan` package (Rosseel, 2012
<doi:10.18637/jss.v048.i02>) without the control variable(s).}
\item{type}{Optional. Specifies whether a single-level structural equation model
or a multilevel structural equation model is entered (DEFAULT = "simple").}
}

\description{
Evaluation of the model-implied variance-covariance matrices of two structural
equation models that only differ by the inclusion versus exclusion of one or
more control variable(s). Both models need to be fitted with the R package
lavaan (Rosseel, 2012) <doi:10.18637/jss.v048.i02>. The derivation of the
methodology employed in this package can be obtained from Blötner (2023)
<doi:10.31234/osf.io/dy79z>.}

\details{
The latcontrol function itself was derived from the discrepancy function from
confirmatory factor analysis and structural equation models. In analogy to the
latter latent model classes, the function provides a chi-square-based index of
discrepancy, model degrees of freedom, a p-value, and derivatives of common
descriptive model fit indices (i.e., Root Mean Square Error of Approximation and
Square Root Mean Residual).}

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

latcontrol(fit_with, fit_without)
}

\value{
\item{X2}{Chi-square value, reflecting the difference between the two matrices.}
\item{df}{Degrees of freedom of the Chi-square statistic.}
\item{p_value}{Corresponding p-value of the Chi-square statistic with the stated
degrees of freedom.}
\item{rmsea}{Adapted version of the Root Mean Square Error of Approximation to
evaluate whether the two matrices differ.}
\item{srmr}{Adapted version of the Square Root Mean Residual to evaluate whether
the two matrices differ.}
}

\author{Christian Blötner
\email{c.bloetner@gmail.com}}
\keyword{&htest}
\keyword{multivariate}
\references{
Blötner, C. (2023). latcontrol: Evaluation of the role of control
variables in structural equation models. PsyArXiv.
https://doi.org/10.31234/osf.io/dy79z

Rosseel, Y. (2012). lavaan: An R package for structural equation modeling.
Journal of Statistical Software, 48(2), 1–36.
https://doi.org/10.18637/jss.v048.i02}
