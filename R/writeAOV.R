#' Export ezANOVA results to an csv file
#' 
#' A function to export an ezANOVA object to a csv file. The function
#'  exports the classic ANOVA table as well as the Mauchly/Levene test
#'  and the appropriated corrections. Optionally, the function can 
#'  export the descriptive statistics as well ad the related posthoc
#'  analyses.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of de Montreal (Canada);
#' 
#' @param fname A string indicating the file name (and the relative path to it).
#' @param anova An ezANOVA object.
#' @param data A data frame of descriptive statistics (mean, sd, se...).
#'  Defaults to NULL.
#' @param posthoc A data frame of posthoc results such as those done by
#'  the pairwise.t.test function.
#' @keywords ANOVA, csv, write file
#' @export
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
#' # Compute descriptive statistics       
#' dstats = ezStats(df, wid=.(Subj), between=.(Cond), dv=RT)
#' # Compute posthoc analysis
#' posthoc = pairwise.t.test(df$RT, df$Cond, paired=F)
#' 
#' # Export the ANOVA as a csv file
#' writeAOV('test.csv', anova, data=dstats, posthoc=posthoc)
#'           
writeAOV  <- function(fname, anova, data=NULL, posthoc=NULL){
  # GT Vallet - Lyon 2 University 
  # 2013/04/18 - v01
  
  if( length(anova)>2 ){ 
    if( length(anova[[2]][1,])>3){
      spher=T
    }else{
      spher=F
    }
  }else{ 
    spher=F
  }
  # format the dataframes to round the numbers
  anova[[1]][,c(4:5,7)]  <- round(anova[[1]][,c(4:5,7)], digits=2)
  if( length(anova)>1 & spher ){
    anova[[2]][,c(-1,-4)] <- round(anova[[2]][,c(-1,-4)], digits=2)
    anova[[3]][,c(2:3,5:6)] <- round(anova[[3]][,c(2:3,5:6)], digits=2)
  }
  # open the file and write the ANOVA table
  f  <- file(fname, open="w")
  writeLines("ANOVA\n", con=f)
  write.table(format(as.matrix(anova[[1]], digits=2)), sep=';', file=f, quote=F, na="", append=T, col.names=NA)  
  # write the Mauchly test and the corrected ANOVA table only if Mauchly is significant
  if( length(anova)>1 & spher ){
      writeLines("\n\nTest de Mauchly; (sphéricité)\n", con=f)
      write.table(format(as.matrix(anova[[2]], digits=2)), sep=';', file=f, quote=F, na="", append=T, col.names=NA)
      writeLines("\n\nANOVA corrigée\n", con=f)
      write.table(format(as.matrix(anova[[3]], digits=2)), sep=';', file=f, quote=F, na="", append=T, col.names=NA)
  }
  # write data's means and standard deviation if such a table is provided
  if( !is.null(data) ){
    writeLines("\n\nStats", con=f)
    write.table(format(as.matrix(data, digit=2)), sep=';', file=f, quote=F, na="", append=T, col.names=NA)
  }
  # write the post-hoc results if such results are provided
  if( !is.null(posthoc) ){
    writeLines(paste("\n\nPost-hoc; (", posthoc$p.adjust.method, ")\n", sep=''), con=f)
    write.table(round(posthoc$p.value, digits=2), sep=';', file=f, quote=F, na="", append=T, col.names=NA)
  }
  # close the file
  close(f)   
}
