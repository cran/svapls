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
  The format is:
 num [1:500, 1:21] 1 2 3 4 5 6 7 8 9 10 ...
}

\examples{
data(hidden_fac.dat)
dim(hidden_fac.dat)
}
\keyword{datasets}
