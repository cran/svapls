\name{batch.dat}
\alias{batch.dat}
\docType{data}
\title{
A gene expression data containing batch effects.
}
\description{
The dataset contains the log transformed expression levels of 500
genes over 20 subjects distributed equally between two varieties 1 and 2. The
data is affected by the batch effects from 4 subjects, with two from the same 
variety. 
}
\usage{data(batch.dat)}
\format{
  The format is:
 num [1:500, 1:21] 1 2 3 4 5 6 7 8 9 10 ...
}


\examples{
data(batch.dat)
dim(batch.dat)
}
\keyword{datasets}
