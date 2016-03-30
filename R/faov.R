#' Export ANOVA summary results to Knitr/Sweave in APA format
#' 
#' A function to format a given treatement (i.e. row) of an 
#'   ANOVA (functions aov, manova...) to a Markdown/LaTeX format according
#'   to the APA guidelines.
#' The output can be directly used in a Knitr/Sweave document.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' @param anova A summary aov object.
#' @param rnb A digit indicating which row to format.
#' @param apa A boolean index to format results according to the APA 
#'  guideline (remove 0 before the decimal point). Default to False.
#' @return Return a string reporting an inline ANOVA results according
#'  to the APA guideline.
#' @keywords ANOVA, APA, Knitr/Sweave
#' @export
#' @seealso \code{\link{ftt}}, \code{\link{fmsd}} 
faov  <- function(anova, rnb, apa=FALSE){
  # GT Vallet -- Université de Montréal -- CRIUGM
  #  2016/02/08 -- v01
  
  # Extract the degree of freedom of the numerator and denominator
  dfn = anova[rnb, 1]
  dfd = anova["Residuals", 1]
  # Extract the F value and round it
  f = round(anova[rnb, "F value"], 2)
  # Extract the p value and format the outupt according to its significance
  p = round(anova[rnb, 5], 2)
  # Format p and eta values according to APA guidelines if apa=T
  if( apa ){
    f = fapa(f)
    p = fapa(p, p=T)
  } 
  
  # Format the data according to the APA guidelines
  if( as.numeric(f) < 1 ){
    str.aov = paste("$F(", dfn, ",~", dfd, ")~<~1$", sep="")
  }else {
    str.aov = paste("$F(", dfn, ",~", dfd, ")~=~", f, "$, ", p, sep="")
  }
  return(str.aov)
}