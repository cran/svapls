\name{fitModel}
\alias{fitModel}
\alias{summary.fitModel}
\alias{print.fitModel}
\title{
Function to fit an ANCOVA model to the log transformed gene expression
data, with a certain specified number of surrogate variables. 
}
\description{
This function begins its operation by fitting a standard ANOVA model to 
the gene expression data, with the gene, variety main effects and their mutual 
interaction. The residuals from the fit of this model and the original 
gene expression values are then respectively organized into two matrices E and Y,
where each column corresponds to a certain gene. Now E is regressed on Y by Partial
Least Squares (PLS) and a specified number of scores are extracted as the estimates
of the latent components from their respective column spaces. The scores in the
Y-space are used as surrogate variables along with the gene and variety interaction
effects with the first score and the usual effects from the standard ANOVA model,
in order to fit an ANCOVA model to the data. The function returns the results from 
this fit.
}
\usage{
fitModel(k1, k2, Y, n.surr)
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
  \item{n.surr}{
     The specified number of surrogate variables.
}
}
\value{
\code{fitModel} returns an object of \code{\link{class}} "\code{fitModel}". The object 
contains a list of the following components:

  \item{mu.hat }{Intercept (general mean effect).}
  \item{G.hat }{Main effects of the genes.}
  \item{V.hat }{Main effects of the varieties.}
  \item{GV.hat }{Gene-Variety interaction effects.}
  \item{sc }{Values of the Surrogate variables (computed only when \code{n.surr}>0).}
  \item{beta.hat }{Coefficients of the surrogate variables (computed only when \code{n.surr}>0).}
  \item{GZ1.hat }{Interaction effects of the genes with the first surrogate variable (computed only when \code{n.surr}>0).}
  \item{VZ1.hat }{Interaction effects of the varieties with the fist surrogate variable (computed only when \code{n.surr}>0).}
  \item{vhat.gvh }{Variances of the estimators for the gene-variety interaction effects.}
  \item{MSE }{Mean Squarred Error for the fitted model.}
  \item{AIC }{Value of the Akaike's Information Criterion (AIC) for the fitted model.}
}


\references{
Sutirtha Chakraborty, Somnath Datta and Susmita Datta. (2012) 
Surrogate Variable Analysis Using Partial Least Squares in Gene 
Expression Studies. Bioinformatics.
Martens, H., Naes, T. (1989) Multivariate Calibration. Chicestor:Wiley.
}

\author{
Sutirtha Chakraborty, Somnath Datta and Susmita Datta.
}

\seealso{
\code{\link{svpls}}, \code{\link{hfp}}
}
\examples{
data(batch.dat)

## Fitting an ANCOVA model with 5 surrogate variables
fit <- fitModel(10,10,batch.dat,n.surr = 5)
print(fit)

}

\keyword{classes}
\keyword{print}
\keyword{methods}