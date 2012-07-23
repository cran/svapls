\name{svapls-package}
\alias{svapls-package}
\alias{svapls}
\docType{package}
\title{
Surrogate variable analysis using Partial Least Squares in a gene expression
data
}
\description{
The package \code{svapls} contains functions that are intended for the 
identification, correction and visualization of the hidden variability owing
to a variety of unknown subject/sample specific effects of residual heterogeneity 
in a gene expression data. 
}
\details{
\tabular{ll}{
Package: \tab svapls\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-07-23\cr
License: \tab GPL-3\cr
}
The package can be used to find the genes that are truly differentially expressed
between two types of samples (tissue types, biological conditions like Cancer/Non-
Cancer samples, etc.), after adjusting for the hidden factors of residual
heterogeneity in the data. The function \code{svpls} detects the truly positive genes
after correcting for the hidden variation and also provides a modified gene
expression matrix which is free from the spurious effects of the residual 
expression heterogeneity. Another important function \code{hfp} produces a heat-
map representing the intensity of latent variability due to the unknown sample-
specific factors, for any specified set of genes and subjects. 

\code{fitModel}, \code{svpls} and \code{hfp} 
}
\author{
Sutirtha Chakraborty, Somnath Datta and Susmita Datta.

Maintainer: Sutirtha Chakraborty <s0chak10@louisville.edu>
}
\references{
Sutirtha Chakraborty, Somnath Datta and Susmita Datta. (2012) 
Surrogate Variable Analysis Using Partial Least Squares in Gene 
Expression Studies. Bioinformatics.
}

\keyword{package}

\examples{
data(batch.dat)
fit <- svpls(10,10,batch.dat,pmax = 5)
fit$genes
Y.corrected <- fit$Y.corr

data(hidden_fac.dat)
gen <- c(1:15,50:65)
sub <- c(1:5,11:17)

hfp(fit,gen,sub,hidden_fac.dat)
}
