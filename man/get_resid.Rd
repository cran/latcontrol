\name{get_resid}
\alias{get_resid}
\title{Raw and correlation residuals in the evaluation of structural equation
models with and without control variables}

\description{Computation of the differences between the variances and
covariances of items implied by two structural equation models that only differ
in terms of inclusion or exclusion of one or more control variable(s). Both
models need to be fitted with the R package lavaan (Rosseel, 2012)
<doi:10.18637/jss.v048.i02>. The derivation of the methodology employed in this
package can be obtained from Blötner (2023) <doi:10.31234/osf.io/dy79z>.}

\arguments{
\item{object_with}{Fit object from the lavaan package with the control
variable(s).}
\item{object_without}{Fit object from the lavaan package without the control
variable(s).}
\item{type}{Optional. Specifies whether a single-level structural equation model
or a multilevel structural equation model is entered (DEFAULT = "simple").}
}

\value{
\item{raw}{A matrix of unstandardized residuals.}
\item{cor}{A matrix of correlation residuals.}
}

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

get_resid(fit_with, fit_without)
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
