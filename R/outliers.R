#' Remove extreme with thresolds and values below and above the mean
#' 
#' A function to remove outliers of a given data frame. Extreme values
#'  can be excluded based on a minimal/maximal values and/or based on 
#'  a given standard deviation value.
#' Return a data frame of filtered data as well as the number and
#'  relative percentage of data filtered.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' Benoit A. Riou \email{riouba@@gmail.com}, Lyon2 University (France)
#' 
#' @param data A data frame in the long format (one row per record).
#' @param target A string indicating the column names of the data to filter.
#'  Defaults to 'RT'.
#' @param fpass A vector length two with the minimal and maximal accepted
#'  value to use for a first filtering of the reaction times. 
#'  To filter only the lowest or highest values, indicate NA as value.
#'  For instance c(100, NA) will only remove RT < 100ms.
#'  Defaults to NULL.
#' @param sdv A number indicating how many standard deviations should be
#'  used to filter the data. Defaults to 3.
#' @return Return a list with a data frame of filtered data and 
#'  a data frame of number of data excluded and its relative percentage
#'  per condition.
#' @keywords reaction times, filter, outilers
#' @export
#' @seealso \code{\link{filtRT}}
#' @examples
#' # Generate fake data with a subject number in the first colum, 
#' #   a fake experimental condition in the second column and fake
#' #   reaction times in the third column
#' df = data.frame(Subj=1, Cond="Test", RT=rnorm(25, mean=550, sd=50))
#' 
#' # Adding extreme values
#' df[5,3]  <- df[5,3]+300
#' df[25,3] <- df[5,3]+500
#' df[19,3] <- df[19,3]-350
#' df[12,3] <- 55
#' df[15,3] <- 2340
#' 
#' # Filter with low and high thresolds and with 3 standard deviations
#' outliers(df, RT='RT', fpass=c(100,1000), sdv=3)
#' 
#' # Filter with only a low thresold with 2 standard deviations
#' outliers(df, target='RT', fpass=c(100, NA), sdv=2)
#'
outliers  <- function(data, fpass=NULL, target, sdv=NULL){
    # GTV and BR - 13/10/2011 - v01
  
    dfnames = NULL
    nb_tot = nrow(data)
    ### Filtering lowest and highest values from given thresholds ---------------
    if( !is.null(fpass) ){
      cur_obs = nrow(data)
      if( !is.na(fpass[1]) ){  
        data = data[ data[,target]>=fpass[1], ]
        dfnames = c(dfnames, 'low-cutoff')
      }
      filtered = cur_obs - nrow(data)
      cur_obs = nrow(data)
      if( !is.na(fpass[2]) ){
        data = data[ data[,target]<=fpass[2], ]
        dfnames = c(dfnames, 'high-cutoff')
      }
      filtered = c(filtered, cur_obs - nrow(data))
    }
  
    if( !is.null(sdv) ){
      ### Filtering lowest and highest values from a given standard deviation -----
      cur_obs = nrow(data)
      
      mean_cond  <- mean(data[,target], na.rm=T)  # Compute the mean of the matrix
      std_dev    <- sd(data[,target], na.rm=T)    # Compute the standard deviation of the matrix
      
      cut_off_high  <- mean_cond + sdv*std_dev    # Compute the high cut-off score
      cut_off_low   <- mean_cond - sdv*std_dev    # Compute the low  cut-off score
      
      # Find RT values within the two cut-off scores
      data  <- data[ data[,target]<= cut_off_high & data[,target]>= cut_off_low,]
      filtered = c(filtered, cur_obs-nrow(data))  
      dfnames = c(dfnames, 'by_sdv')
    }
    ptot = round((sum(filtered)/nb_tot)*100, 2)
    filtered = c(filtered, ptot)
    names(filtered) = c(dfnames, 'percent_subtot')
                 
    return(list(data_filtered=data, filtered=filtered))
}
