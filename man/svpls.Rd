\name{svpls}
\alias{svpls}
\alias{summary.svpls}
\alias{print.svpls}
\title{
Function for identfying the optimal ANCOVA model and detecting the genes
that are truly differentially expressed between the two types of samples.
}
\description{
This function calls \code{fitModel} repeatedly to fit a series of ANCOVA
models along with the standard ANOVA model, to the log transformed gene expression 
data. The model with the minimum AIC is selected as the optimal one and its 
corresponding estimated effects are then used to perform a multiple testing of
differential expression, over all the genes, using the Benjamini-Hochberg correction.
}
\usage{
svpls(k1, k2, Y, pmax = 3, fdr = 0.05)
}

\arguments{
  \item{k1}{
     Number of subjects/samples under variety 1.
  }
  \item{k2}{
     Number of subjects/samples under variety 2.
  }
  \item{Y}{
     The log transformed gene expression data, with genes along the rows and
     subjects/samples along the columns.
  }
  \item{pmax}{
     Maximum number of surrogate variables to be incorporated in the ANCOVA
     model (means \code{pmax} ANCOVA models are fitted to the data). 
     By default, it is taken as 3. 
  }

  \item{fdr}{
     The specified False Discovery Rate (FDR) for multiple testing of 
     differential expression, using the Benjamini-Hochberg correction.
     By Default it is taken as 0.05.
  }
}

\value{
\code{svpls} returns an object of \code{\link{class}} "\code{svpls}". 
The object contains a list of the following components:

  \item{opt.model }{The optimal model. 1 denotes the standard ANOVA model.}
  \item{PLS.imp }{PlS imputed estimate of the hidden expression heterogeneity,
evaluated from the optimal model (applicable only when \code{opt.model}>1).}
  \item{Y.corr }{Corrected gene expression matrix after adjusting for 
the hidden effects (applicable only when \code{opt.model}>1).}
  \item{pvalues}{p-values from the tests with the effects estimated from the standard ANOVA model (returned only when \code{opt.model}=1.}
  \item{pvalues.adj}{Adjusted p-values after correcting for the hidden effects (applicable only when \code{opt.model}>1).}
  \item{genes}{Genes that are deemed to be differentially expressed from the multiple hypotheses testing
with effects estimated from the optimal model.}
  \item{AIC.opt}{AIC value for the optimal model.}
}
\references{
Hirotsugu, A. (1980) Likelihood and the Bayes Procedure. The Institute of
Statistical Mathematics, Tokyo., Benjamini, Y and Hochberg, Y (1995) Controlling
the false discovery rate : a practical and powerful approach to multiple testing.
Journal of the Royal Statistical Society. 
}
\author{
  Sutirtha Chakraborty, Somnath Datta and Susmita Datta.
}

\seealso{
 \code{\link{fitModel}}, \code{\link{hfp}}
}
\examples{
## Loading the first dataset
data(batch.dat)

## Fitting the optimal ANCOVA model to the data gives:
fit <- svpls(10,10,batch.dat,pmax = 5)

## The optimal ANCOVA model, its AIC value and the positive genes detected from it are given by:
fit$opt.model 

fit$AIC.opt

fit$genes

## The corrected gene expression matrix obtained after removing the effects of the hidden variability is given by:
Y.corrected <- fit$Y.corr
}

\keyword{classes}
\keyword{print}
\keyword{methods}

