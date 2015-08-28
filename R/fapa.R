#' Format a number according to the APA guidelines
#' 
#' A function to format a given number according to the APA 
#'   guidelines, i.e. removing the 0 before the decimal point,
#'   and rounding the number to two digits.
#' If the number to format is a p value, then add  'p = ' or
#'   'p < .05'.
#' The output can be directly used in a Knitr/Sweave document.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' @param nbr A number to format.
#' @param p A boolean index to precise if the number to format is a p
#'  value. 
#' @return Return a string reporting an inline number results according
#'  to the APA guideline.
#' @keywords p-value, APA, Knitr/Sweave
#' @export
#' @seealso \code{\link{ftt}}, \code{\link{fmsd}}, \code{\link{faov}} 
#' 
fapa  <- function(nbr, p=FALSE){
  # GT Vallet -- University of Montreal
  #  2014/09/25 -- v01
  
  ### 
  if( nbr < 1 ){
    nbr.ft = substring(nbr, 2, nchar(round(nbr, 2)))
  }else{
    nbr.ft = round(nbr, 2)  
  }
  if( p ){
    if( nbr < .05 ){
      nbr.ft = "$p~<~.05$"
    }else{
      nbr.ft = paste("$p~=~", nbr.ft, "$", sep="")
    }
  }
  
return(nbr.ft)
}