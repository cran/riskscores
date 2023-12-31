% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic.R
\name{coef.risk_mod}
\alias{coef.risk_mod}
\title{Extract Model Coefficients}
\usage{
\method{coef}{risk_mod}(object, ...)
}
\arguments{
\item{object}{An object of class "risk_mod", usually a result of a call to
\code{\link[=risk_mod]{risk_mod()}}.}

\item{...}{Additional arguments.}
}
\value{
Numeric vector with coefficients.
}
\description{
Extracts a vector of model coefficients (both nonzero and zero) from a
"risk_mod" object. Equivalent to accessing the \code{beta} attribute of a
"risk_mod" object.
}
\examples{
y <- breastcancer[[1]]
X <- as.matrix(breastcancer[,2:ncol(breastcancer)])

mod <- risk_mod(X, y, lambda0 = 0.01)
coef(mod)

}
