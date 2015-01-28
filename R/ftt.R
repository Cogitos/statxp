#' Export t test results to APA format
#' 
#' A function to format a t test of Student according to the 
#'  APA guidelines. The output can be directly used in a 
#'  Knitr/Sweave document.
#' The function can optionally export the related d of Cohen.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' @param ttest An t.test object.
#' @param d A number representing the associated d of Cohen.
#' @keywords t test, format, APA, knitr/Sweave
#' @export
#' @seealso \code{\link{faov}}, \code{\link{fmsd}} 
#' @examples
#' # Generate fake data of two groups to be compared
#' cond1 = rnorm(10, mean=555, sd=45)
#' cond2 = rnorm(10, mean=639, sd=33)
#' 
#' # Compute the t test
#' ttest = t.test(cond1, cond2, paired=F)
#' 
#' # Export the t test result
#' ftt(ttest)
#'            
ftt  <- function(ttest, d=NULL){
  # GT Vallet -- Lyon2 University
  #  2013/05/31 -- v01
  
  # Extract the degree of freedom of the numerator and denominator
  df = round(ttest$parameter, 2)
  # Extract the t value and round it
  t = round(ttest$statistic, 2)
  # Extract the p value and format the outupt according to its significance
  p = ttest$p.value
  if( p < .05){
    p = ", p < .05"
  }else{
    p = paste(", p = ", round(p, 2), sep="")
  }
  if( length(d) > 0 ){
    str.ttest = paste("$t(", df, ") = ", t, p, ", d = ", round(d, 2) ,"$", sep="")
  }else{ str.ttest = paste("$t(", df, ") = ", t, p, "$", sep="") }
  
  return(str.ttest)
}
