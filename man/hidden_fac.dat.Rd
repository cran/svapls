\name{hidden_fac.dat}
\alias{hidden_fac.dat}
\docType{data}
\title{
A gene expression data affected by a hidden variable.
}
\description{
The dataset contains the log transformed expression levels of 500
genes over 20 subjects distributed equally between two varieties 1 and 2. The
data is affected by the unknown effects from a hidden confounder whose effect 
changes over the two sample varieties. 
}
\usage{data(hidden_fac.dat)}
\format{
  A data frame with 500 observations on the following 20 variables.
  \describe{
    \item{\code{S1}}{a numeric vector}
    \item{\code{S2}}{a numeric vector}
    \item{\code{S3}}{a numeric vector}
    \item{\code{S4}}{a numeric vector}
    \item{\code{S5}}{a numeric vector}
    \item{\code{S6}}{a numeric vector}
    \item{\code{S7}}{a numeric vector}
    \item{\code{S8}}{a numeric vector}
    \item{\code{S9}}{a numeric vector}
    \item{\code{S10}}{a numeric vector}
    \item{\code{S11}}{a numeric vector}
    \item{\code{S12}}{a numeric vector}
    \item{\code{S13}}{a numeric vector}
    \item{\code{S14}}{a numeric vector}
    \item{\code{S15}}{a numeric vector}
    \item{\code{S16}}{a numeric vector}
    \item{\code{S17}}{a numeric vector}
    \item{\code{S18}}{a numeric vector}
    \item{\code{S19}}{a numeric vector}
    \item{\code{S20}}{a numeric vector}
  }
}

\examples{
data(hidden_fac.dat)
## maybe str(hidden_fac.dat) ; plot(hidden_fac.dat) ...
}
\keyword{datasets}
