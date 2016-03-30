#' Apply a standard clean theme to a ggplot figure
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of Montreal (Canada)
#' 
#' @param fig A ggplot figure
#' @param legend Position of the legend according to a string (top/down/left/right)
#'  or a vector of coordinates between 0 and 1
#' @param brewer A string designate a set of brewer colors. Defaults to 'Set3'
#' @return Return a new ggplot object with the new theme embedded
#' @keywords ggplot, figure, plot, theme, colors
#' @export
#' @examples
#' figtheme(fig, legend)

figtheme = function(fig, legend='right', brewer='Set3'){
  require('grid')
  newfig = fig + theme_bw() + 
            theme(# Grid ---------------------------
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              # Legend -------------------------
              legend.title = element_blank(), 
              legend.text  = element_text(size = 18),
              legend.key   = element_rect(colour = 'black'),
              legend.justification = c(0, 1), 
              legend.position = legend,
              legend.key.width = unit(4, "lines"), legend.key.height=unit(2, "lines"),
              # Axis ---------------------------
              axis.title.x = element_text(face = 'bold', size = 20, vjust =-1), 
              axis.title.y = element_text(face = 'bold', size = 20, vjust = 2),
              axis.text.x  = element_text(size = 18), 
              axis.text.y  = element_text(size = 18),
              # Panel --------------------------
              strip.text.x = element_text(size=18, face="bold"),
              strip.text.y = element_text(size=18, face="bold"),
              plot.margin  = unit(c(.5,0.5,1.5,.9), "cm")
      ) +
      scale_fill_brewer(palette=brewer)
  
  return(newfig)
}