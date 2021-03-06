% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figtheme.R
\name{figtheme}
\alias{figtheme}
\title{Apply a standard clean theme to a ggplot figure}
\usage{
figtheme(fig, legend = "right", brewer = "Set3")
}
\arguments{
\item{fig}{A ggplot figure}

\item{legend}{Position of the legend according to a string (top/down/left/right)
or a vector of coordinates between 0 and 1}

\item{brewer}{A string designate a set of brewer colors. Defaults to 'Set3'}
}
\value{
Return a new ggplot object with the new theme embedded
}
\description{
Apply a standard clean theme to a ggplot figure
}
\examples{
figtheme(fig, legend)
}
\author{
Guillaume T. Vallet \email{gtvallet@gmail.com}, University of Montreal (Canada)
}
\keyword{colors}
\keyword{figure,}
\keyword{ggplot,}
\keyword{plot,}
\keyword{theme,}

