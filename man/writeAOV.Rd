% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeAOV.R
\name{writeAOV}
\alias{writeAOV}
\title{Export ezANOVA results to an csv file}
\usage{
writeAOV(fname, anova, data = NULL, posthoc = NULL)
}
\arguments{
\item{fname}{A string indicating the file name (and the relative path to it).}

\item{anova}{An ezANOVA object.}

\item{data}{A data frame of descriptive statistics (mean, sd, se...).
Defaults to NULL.}

\item{posthoc}{A data frame of posthoc results such as those done by
the pairwise.t.test function.}
}
\description{
A function to export an ezANOVA (package "ez")
 object to a csv file. The function exports the classic ANOVA table
 as well as the Mauchly/Levene test and the appropriated corrections.
Optionally, the function can export the descriptive statistics
 as well ad the related posthoc analyses.
}
\examples{
# Generate fake data with a subject number in the first colum, a
#  fake experimental condition in the second column and fake
#  reaction times in the third column
df = rbind(data.frame(Subj=1:10,  Cond="Control", RT=rnorm(10, mean=720, sd=59)),
           data.frame(Subj=11:20, Cond="Test", RT=rnorm(10, mean=680, sd=45)),
           data.frame(Subj=21:30, Cond="Test2", RT=rnorm(10, mean=650, sd=49)))
# Convert subject and independant variable to factors
df$Subj = as.factor(df$Subj)
df$Cond = as.factor(df$Cond)

# Compute the ANOVA 
anova = ezANOVA(df, wid=.(Subj), between=.(Cond), dv=RT)
# Compute descriptive statistics       
dstats = ezStats(df, wid=.(Subj), between=.(Cond), dv=RT)
# Compute posthoc analysis
posthoc = pairwise.t.test(df$RT, df$Cond, paired=F)

# Export the ANOVA as a csv file
writeAOV('test.csv', anova, data=dstats, posthoc=posthoc)
          
}
\author{
Guillaume T. Vallet \email{gtvallet@gmail.com}, University of de Montreal (Canada);
}
\keyword{ANOVA,}
\keyword{csv,}
\keyword{file}
\keyword{write}

