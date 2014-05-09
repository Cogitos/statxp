#' Filter reaction times values with low/high values and/or standard
#'  deviation form the mean
#' 
#' A function to remove outliers of a given data frame. Based on the
#'  outliers function of the present package. filtRT applies the 
#'  outliers function according to each subject and/or condition.
#' Data can be excluded based on a minimal/maximal values (thresholds)
#'  and/or based on a given standard deviation value from the mean. 
#' Outliers are excluded either from all values or regarding each
#'  subject and condition. 
#'  Return a data frame of filtered data as well as the percentage
#'  of data filtered.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' Benoit A. Riou \email{riouba@@gmail.com}, Lyon2 University (France)
#' 
#' @param data A data frame in the long format (one row per record).
#' @param RT A string indicating the column names the data to filter.
#'  Defaults to 'RT'.
#' @param vars A vector of name to indicate the column names of variables
#'  to use to filter the data by condition, usually subjects and at least
#'  one independent variable.
#' @param fpass A vector length two with the minimal and maximal accepted
#'  value to use for a first filtering of the reaction times. 
#'  To filter only the lowest or highest values, indicate NA as value.
#'  For instance c(100, NA) will only remove RT < 100ms.
#'  Defaults to NULL.
#' @param sdv A number indicating how many standard deviations should
#'  be used to filter the data. Defaults to NULL.
#' @return Return a list with a data frame of filtered data and 
#'  a data frame of number of data excluded and its relative percentage
#'  per condition.
#' @keywords reaction times, filter, outliers
#' @export
#' @seealso \code{\link{outliers}} 
#' @examples
#' # Generate fake data with a subject number in the first colum, a fake experimental condition
#'  in the second column and fake reaction times in the third column
#' df = rbind(data.frame(Subj=1, Cond="Test", RT=rnorm(25, mean=550, sd=48)),
#'            data.frame(Subj=1, Cond="Control", RT=rnorm(25, mean=680, sd=62)),
#'            data.frame(Subj=2, Cond="Test", RT=rnorm(25, mean=585, sd=54)),
#'            data.frame(Subj=2, Cond="Control", RT=rnorm(25, mean=720, sd=59)))
#' 
#' # Adding extreme values
#' df[75,3]  = df[5,3]+300
#' df[25,3] = df[5,3]+500
#' df[79,3] = df[19,3]-350
#' df[33,3] = df[33,3]+420
#' df[40,3] = df[40,3]-520
#' df[27,3] = df[27,3]-350
#' df[9,3]  = 50
#' df[86,3] = 4250
#' df[65,3] = 99
#' df[3,3]  = 1999
#' 
#' Filter with low and high thresolds and with 3 standard deviations
#'   by subject and condition
#' filtRT(df, RT='RT', vars=c('Subj', 'Cond'), fpass=c(100,1000), sdv=3)
#' 
#' # Filter with only a low thresold with 2 standard deviations by subjects
#' filtRT(df, RT='RT', vars='Subj', fpass=c(100, NA), sdv=2)
#'  
filtRT = function(dt, RT='RT', vars=NULL, fpass=NULL, sdv=NULL ){
  # GT Vallet    --  Lyon 2 University
  #   2013/07/01 --  v01
  #   2014/05/01 --  v02 Adding vars as an option.
  #                       Filtering only lowest or highest RT value. 
  #                       Adding the percentage of data filtered.

    if( is.null(vars) ){
      dt.fil = outliers(dt, fpass, RT, sdv)
      dt.filtered = dt.fil[[1]]
      filtered = dt.fil[[2]]
    }else{
      dt.fil = dlply(dt, vars, function(x) outliers(x, fpass, RT, sdv)) 
      dt.filtered  = ldply(dt.fil, function(x) rbind(x[[1]]))
      filtered = ldply(dt.fil, function(x) rbind(x[[2]]))
    }
  
  return(list(Data_Filtered=dt.filtered, Nb_Data_Filtered=filtered))
}
