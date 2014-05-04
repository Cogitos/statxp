#' Compute the mean and the sd of a vector and format them to APA format
#' 
#' A function to compute the mean and the standard deviation (sd) 
#'  and return a formatted string as mean (sd) or as (mean +- sd) 
#'  to be used a Markdown/LaTeX format according to the APA guidelines.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' @param data A vector of values to used to compute the mean and 
#'  the standard deviation (sd).
#' @param pm A logical value to format the mean and the sd all in 
#'  parentheses. Defaults to FALSE.
#' @return Return a string reporting an inline mean and standard
#'  deviation (sd) according to the APA guideline.
#' @keywords mean, sd, APA, Knitr/Sweave
#' @export
#' @examples
#' # Generate fake data:
#' data = rnorm(10, mean=555, sd=35)
#' 
#' # Formatting as mean (sd)
#' fmsd(data)
#' 
#' # Formatting as (mean +- sd)
#' fmsd(data, pm=T)    
#' 
fmsd = function(data, pm=FALSE ){
    m = round(mean(data, na.rm=T), 2)
    sd = round(sd(data, na.rm=T), 1)
    if( pm ){
      return(paste('(', m, ' $\\pm$ ', sd, ')', sep=''))
    }
    return(paste(m, ' (', sd, ')', sep=''))
}