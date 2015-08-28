#' Export ezANOVA results to Knitr/Sweave in APA format
#' 
#' A function to format a given treatement (i.e. row) of an 
#'   ezANOVA (package "ez") to a Markdown/LaTeX format according
#'   to the APA guidelines.
#' The output can be directly used in a Knitr/Sweave document.
#' The function tests if the Mauchly's test for sphericity is 
#'  significant, and if so, report the corrected values.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' @param anova An ezANOVA object.
#' @param rnb A digit indicating which row to format.
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
#' faov(anova, rnb=1)
#' 
faov  <- function(anova, rnb, apa=FALSE){
  # GT Vallet -- Lyon2 University
  #  2013/05/31 -- v01
  #  2014/09/24 -- v01.02 -- add insecable spaces and correction for subscripts placement
  #  2014/09/25 -- v01.03 -- add apa option to remove 0 before the decimal point
  #  2015/08/26 -- v01.04 -- update script to new ez behavior

  ### Allow to define rnb as a name or an index
  rownames(anova[['ANOVA']]) = anova[['ANOVA']][,1]
  
  ### Define if treatment to report need to be corrected
  if( length(anova) < 2 ){
    corr = FALSE
  }else{ 
    if( anova[[2]][rnb,4]=='*' ){
      corr = TRUE      
    }else{
      corr = FALSE
    }
  }
  
  # Extract the degree of freedom of the numerator and denominator
  dfn = anova[['ANOVA']][rnb, 2]
  dfd = anova[['ANOVA']][rnb, 3]
  # Extract the F value and round it
  f = round(anova[['ANOVA']][rnb, 4], 2)
  
  if( corr ){
    ttt = anova[[3]]
    # Apply the correction of Greenhouse-Geiser
    dfn = dfn * ttt[rnb, 2]
    dfd = dfd * ttt[rnb, 2]    
    # Extract the p value and format the outupt according to its significance
    p = round(ttt[rnb, 5], 2)
  }else{
    ttt = anova[[1]]
    # Extract the F value and round it
    f = round(ttt[rnb, 4], 2)
    # Extract the eta square
    eta = round(ttt[rnb, 7], 2)
    # Extract the p value and format the outupt according to its significance
    p = round(ttt[rnb, 5], 2)
    # Format p and eta values according to APA guidelines if apa=T
    if( apa ){
      f = fapa(f)
      eta = fapa(eta)
      p = fapa(p, p=T)
    } 
  }

  # Format the data according to the APA guidelines
  if( as.numeric(f) < 1 ){
    str.aov = paste("$F(", dfn, ",~", dfd, ")~<~1$", sep="")
  }else if( !corr ){
    str.aov = paste("$F(", dfn, ",~", dfd, ")~=~", f, "$, ", p, ", $\\eta^{2}$\\phantom{}$_{g}~=~", eta, "$", sep="")
  }else {
    str.aov = paste("$F(", dfn, ",~", dfd, ")~=~", f, "$, ", p, sep="")
  }
  return(str.aov)
}
