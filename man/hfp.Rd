\name{hfp}
\alias{hfp}

\title{
Function to construct a heatmap of the hidden variation in the gene
expression data. 
}
\description{
The function \code{hfp} produces a plot of the PLS imputed estimate of the
hidden variability in the data, derived from the optimal model, corresponding 
to an user-specified set of genes and subjects/samples. 
}
\usage{
hfp(obj, gen, ind, Y)
}

\arguments{
  \item{obj}{
       An \code{svpls} object.
}
  \item{gen}{
       An user-specified set of genes.
}
  \item{ind}{
      An user-specified set of subjects.
}
  \item{Y}{
      A log transformed gene expression matrix with genes along the rows
and subjects/samples along the columns.
}
}

\value{
A heatmap of the hidden variability corresponding to the specified set of
genes and subjects, attributable to the unknown subject-specific 
factors in the gene expression data.
}
\references{
Sutirtha Chakraborty, Somnath Datta and Susmita Datta. (2012) 
Surrogate Variable Analysis Using Partial Least Squares in Gene 
Expression Studies. Bioinformatics.
}
\author{
Sutirtha Chakraborty, Somnath Datta and Susmita Datta.
}

\seealso{
\code{\link{heatmap}}, \code{\link{fitModel}}, \code{\link{svpls}}
}
\examples{
## Fitting the optimal ANCOVA model to the data gives:
data(hidden_fac.dat)
fit <- svpls(10,10,hidden_fac.dat,pmax = 5)

## Specifying the sets of genes and subjects
gen <- paste("g",c(1:15,50:65),sep="")
sub <- paste("S",c(1:5,11:17),sep="")

hfp(fit,gen,sub,hidden_fac.dat)
}

\keyword{print}
