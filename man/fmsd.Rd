% Generated by roxygen2 (4.0.0): do not edit by hand
\name{fmsd}
\alias{fmsd}
\title{Compute the mean and the sd of a vector and format them to APA format}
\usage{
fmsd(data, pm = FALSE)
}
\arguments{
\item{data}{A vector of values to used to compute the mean and
the standard deviation (sd).}

\item{pm}{A logical value to format the mean and the sd all in
parentheses. Defaults to FALSE.}
}
\value{
Return a string reporting an inline mean and standard
 deviation (sd) according to the APA guideline.
}
\description{
A function to compute the mean and the standard deviation (sd)
 and return a formatted string as mean (sd) or as (mean +- sd)
 to be used a Markdown/LaTeX format according to the APA guidelines.
}
\examples{
# Generate fake data:
data = rnorm(10, mean=555, sd=35)

# Formatting as mean (sd)
fmsd(data)

# Formatting as (mean +- sd)
fmsd(data, pm=T)
}
\author{
Guillaume T. Vallet \email{gtvallet@gmail.com}, University of de Montreal (Canada);
}
\keyword{APA,}
\keyword{Knitr/Sweave}
\keyword{mean,}
\keyword{sd,}
