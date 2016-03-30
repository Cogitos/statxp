#' Export ezANOVA results to Knitr/Sweave in APA format
#' 
#' A function to format a given treatement (i.e. row) of an 
#'   ezANOVA (package "ez") to a Markdown/LaTeX format according
#'   to the APA guidelines.
#' The output can be directly used in a Knitr/Sweave document.
#' The function tests if the Mauchly's test for sphericity is 
#'  significant, and if so, report the corrected values using
#'  the Greenhouse-Geisser correction.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' @param anova An ezANOVA object.
#' @param ttt A digit indicating which row to format, or the name of
#'  the treatment in the ANOVA.
#' @param apa A boolean index to format results according to the APA 
#'  guideline (remove 0 before the decimal point). Default to False.
#' @return Return a string reporting an inline ANOVA results according
#'  to the APA guideline.
#' @keywords ANOVA, APA, Knitr/Sweave
#' @export
#' @seealso \code{\link{ftt}}, \code{\link{fmsd}} 
#' @examples
#' # Generate fake data with a subject number in the first colum, a
#' #  fake experimental condition in the second column and fake
#' #  reaction times in the third column
#' df = rbind(data.frame(Subj=1:10,  Cond="Control", RT=rnorm(10, mean=720, sd=59)),
#'            data.frame(Subj=11:20, Cond="Test", RT=rnorm(10, mean=680, sd=45)),
#'            data.frame(Subj=21:30, Cond="Test2", RT=rnorm(10, mean=650, sd=49)))
#' # Convert subject and independant variable to factors
#' df$Subj = as.factor(df$Subj)
#' df$Cond = as.factor(df$Cond)
#' 
#' # Compute the ANOVA 
#' anova = ezANOVA(df, wid=.(Subj), between=.(Cond), dv=RT)       
#' 
#' # Format the results
#' fezAov(anova, ttt=1)
#' 
fezAov  <- function(anova, ttt, apa=FALSE){
  # GT Vallet -- Lyon2 University -- CRIUGM
  #  2013/05/31 -- v01
  #  2014/09/24 -- v01.02 -- add insecable spaces and correction for subscripts placement
  #  2014/09/25 -- v01.03 -- add apa option to remove 0 before the decimal point
  #  2015/08/26 -- v01.04 -- update script to new ez behavior
  #  2016/03/29 -- v02    -- rename variable, more flexible behavior and better Sphericity violation handling 

  ### Define the name of the treatment (name of the line) if a digit is provided 
  if( is.numeric(ttt) ){
    ttt = anova[['ANOVA']][ttt,1]
  }
  
  ### Define if treatment to report need to be corrected
  if( length(anova) < 2 ){
    corr = FALSE
  }else{ 
    if( ttt %in% anova[[2]]["Effect"][,1] && anova[[2]][anova[[2]]$"Effect"==ttt,4]=='*' ){
      corr = TRUE      
    }else{
      corr = FALSE
    }
  }
  
  curttt = anova[['ANOVA']][anova[['ANOVA']]$Effect==ttt, ]
  # Extract the degree of freedom of the numerator and denominator
  dfn = curttt[, 2]
  dfd = curttt[, 3]
  # Extract the F value and round it
  f = round(curttt[, 4], 2)
  
  if( corr ){
    corttt = anova[[3]][anova[[3]]$Effect==ttt, ]
    # Apply the correction of Greenhouse-Geiser
    dfn = round(dfn * corttt[, 2], 2)
    dfd = round(dfd * corttt[, 2], 2)    
    # Extract the p value and format the outupt according to its significance
    p = round(corttt[, 3], 2)
  }else{
    # Extract the eta square
    eta = round(curttt[, 7], 2)
    # Extract the p value and format the outupt according to its significance
    p = round(curttt[, 5], 2)
  }
  # Format p and eta values according to APA guidelines if apa=T
  if( apa ){
    f = fapa(f)
    p = fapa(p, p=T)
    if( exists("eta") ){
      eta = fapa(eta)
    }
  }else{
    if( p < .5 ){
      p = "p < 0.05"
    }else{
      p = paste("p = ", p, sep="")  
    }
  }
  
  # Format the data into markdown compatible format
  if( as.numeric(f) < 1 ){
    str.aov = paste("$F(", dfn, ",~", dfd, ")~<~1$", sep="")
  }else if( !corr ){
    str.aov = paste("$F(", dfn, ",~", dfd, ")~=~", f, "$, ", p, ", $\\eta^{2}$\\phantom{}$_{g}~=~", eta, "$", sep="")
  }else {
    str.aov = paste("$F(", dfn, ",~", dfd, ")~=~", f, "$, ", p, sep="")
  }
  return(str.aov)
}
