NULL
#' 
#' Visualize asc raster map
#' 
#' Visualize .asc raster map (GEOtop map output) with ggplot
#' 
#' 
#' 
#' @param map  character, full path and name of .asc map to visualize
#' @param date character, date characterization
#' @param variable		character, variable characterization

#' @param layer layer to plot
#' @param limits limits to plot
#' @param legend   character, legend characterization, e.g. \code{"vol\%"} for soil moisture data
#' @param lowcol   minimum colour for visualization, default is \code{"#f7fbff"}
#' @param highcol  maximum colour for visualization, default is \code{"#08306b"}; default is creating a blue colour range
#' 
#' 
#' 
#' @export
#' 
#' 
#' @importFrom raster raster rasterToPoints
#' @importFrom RSAGA read.ascii.grid
#' @importFrom ggplot2 ggtitle ggplot aes ggsave theme_bw theme geom_raster coord_equal scale_fill_gradient element_text element_blank 
#' 

#}
#\details{
##### @param numeric  numeric, vector setting y-limits for plot
#	%%  ~~ If necessary, more details than the description above ~~
#}
#' @return	ggplot object
#}
#\references{
#	%% ~put references to the literature/web site here ~
#}
#\author{
#' @author	Johannes Brenner \email{Johannes.Brenner@eurac.edu}
#}
#\note{
#	%%  ~~further notes~~
#}
#
#%% ~Make other sections like Warning with \section{Warning }{....} ~
#		
#		\seealso{
#			%% ~~objects to See Also as \code{\link{help}}, ~~~
#		}
#\examples{ dB_getSWP
#	
#}
#% Add one or more standard keywords, see file 'KEYWORDS' in the
#		% R documentation directory.
#		\keyword{ ~kwd1 }
#\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
#		
#		
##'		
#
#
## plot maps from raster 

GEOtop_ReadPlotRst <- function(map, date, variable, layer, limits, legend,#
                               lowcol="#f7fbff", highcol="#08306b")
{
  map <- raster(map)
  
  #convert the raster to points for plotting
  map.p <- rasterToPoints(map)
  #Make the points a dataframe for ggplot
  df <- data.frame(map.p)
  #Make appropriate column headings
  colnames(df) <- c("Longitude", "Latitude", "MAP")
  ## ec 20160526
  Longitude <- NULL
  Latitude <- NULL
  MAP <- NULL
  ## end ec 20160526
#   
  if (is.null(layer)) {
    var_layer_date <- paste(variable, " | ", date, sep=" ")
  } else {
    var_layer_date <- paste(variable, " | layer ", layer, " | ", date, sep=" ")
  }
  
  ggp <- ggplot(data=df, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=MAP)) +
    # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
    theme_bw() +
    coord_equal() +
    scale_fill_gradient(legend, limits=limits, low = lowcol, high = highcol, space = "Lab", na.value = "grey50", 
                        guide = "colourbar") +
    theme(axis.title.x = element_text(size=13),
          axis.title.y = element_text(size=13),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12, angle=75),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank(),
          plot.title =element_text(size=14) ) +
    ggtitle(var_layer_date)

  return(ggp)
}