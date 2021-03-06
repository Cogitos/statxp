% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corstars.R
\name{corstars}
\alias{corstars}
\title{Compute a correlation matrix with stars to indicate significant correlations
 
A function to compute a correlation matrix with stars 
  to indicate significant correlations (p<.05 = *, 
  p<.01 = **, p<.001).
Return a data frame.}
\usage{
corstars(x)
}
\arguments{
\item{x}{A data frame with only the variables to correlate.}
}
\value{
Return a data frame representing the correlation matrix.
}
\description{
Compute a correlation matrix with stars to indicate significant correlations
 
A function to compute a correlation matrix with stars 
  to indicate significant correlations (p<.05 = *, 
  p<.01 = **, p<.001).
Return a data frame.
}
\author{
Guillaume T. Vallet \email{gtvallet@gmail.com}, University of de Montreal (Canada);
}
\keyword{correlation,}
\keyword{matrix,}
\keyword{significance}

