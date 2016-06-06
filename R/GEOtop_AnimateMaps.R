NULL

# GEOtop_animateMAPS}
#
#%- Also NEED an '\alias' for EACH other topic documented here.

#\usage{
#	GEOtop_animateMAPS(path, mapkey, layers, soil_files, variable, limits, legend, 
#			lowcol="#f7fbff", highcol="#08306b", delay_value=NULL)
#}
#%- maybe also 'usage' for other objects documented here.

#		\title{
#'  GEOtop output map animation
#'
#'  GEOtop output map animation. Using GEOtop_ReadPlotRst() to visualize GEOtop output maps in pdf format and ImageMagick to animate images.
#' 
#' @param wpath path into GEOtop simulation
#' @param mapkey keyword name; see package \emph{geotopbricks}, function \code{\link{get.geotop.inpts.keyword.value}}, argument \emph{keyword}
#' @param layers default = NULL, for variables without layering e.g. snow water eqivalent; integer vector defining layers which should be processed for data with layerinf, e.g. soil moisture
#' @param soil_files boolean, TRUE: soil files are provided as GEOtop input. FALSE: soil is parameterized in the geotop.inpts file
#' @param variable character, variable characterization
#' @param limits argument for \code{\link{GEOtop_ReadPlotRst}}
#' @param legend character, legend characterization, e.g. \code{"vol\%"} for soil moisture data
#' @param lowcol minimum colour for visualization, default = \code{"#f7fbff"}
#' @param highcol maximum colour for visualization, default = "#08306b"; default is creating a blue       colour
#' @param delay_value default = NULL, argument for function convert (ImageMagick), display the next image after pausing
#' 
#' 
#' @export
#' 
#' @return 	.gif animation file in folder "path/gif"
#}
##### @param map numeric, vector setting y-limits for plot
#\references{
#	%% ~put references to the literature/web site here ~
#}
#\author{
#' @author	Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
#}
#\note{
#' @note 	for further information on ImageMagick see \url{http://www.imagemagick.org/}, for information on how to install on LINUX (UHREL, Ubuntu) and CentOS see \url{http://tecadmin.net/install-imagemagick-on-linux/}
#}
#
#%% ~Make other sections like Warning with \section{Warning }{....} ~
#		
#		\seealso{
#			%% ~~objects to See Also as \code{\link{help}}, ~~~
#		}
#\examples{
#	
#}
#% Add one or more standard keywords, see file 'KEYWORDS' in the
#		% R documentation directory.
#		\keyword{ ~kwd1 }
#\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
#		
# TO DO 
































# Animate GEOtop maps

# need to install ImageMagick from http://www.imagemagick.org/
# LINUX CentOS UHREL Ubuntu see here: http://tecadmin.net/install-imagemagick-on-linux/

# use IrfanView to display .gif image
# press G to pause animation

# TEST

# library(geotopbricks)
# library(ggplot2)
# # # 
# wpath <- "Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/Discharge/WG1_005/"
# mapkey <- "SoilLiqContentTensorFile"
# mapkey <- "SWEMapFile"
# wpath <- "/data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/Discharge/WG1_005/"
# 
# layers=NULL
# soil_files=F
# variable="SWE"
# limits=c(0,300)
# legend="mm"
# lowcol="#f7fbff"
# highcol="#08306b"

GEOtop_animateMAPS <- function(wpath, mapkey, layers, soil_files,
                               variable, limits, legend, 
                               lowcol="#f7fbff", highcol="#08306b",
                               delay_value=NULL)
{
  # set animation options, convert executable
  #oopt <- ani.options(convert = "convert")
  
  # extract start and end of simulation
  start <- get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,
                                          tz="UTC") + 24*60*60
  start <- as.Date(start)
  end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="UTC")
  end <- as.Date(end)
  
  dates <- seq(from = start, to = end, by = 1)
  
  # number of layers
  # soil saturation and layer thickness from soil input 
  if (soil_files) {
    nr_soiltypes <- get.geotop.inpts.keyword.value(keyword="SoilLayerTypes", wpath=wpath, numeric=TRUE)
    soil_map <- get.geotop.inpts.keyword.value(keyword = "SoilMapFile", raster = T, wpath=wpath, isNA = -9999)
    soil_map@data@values[soil_map@data@values==-9999] <- NA
    soil_type_summary <- summary(as.factor(soil_map@data@values))
    soil_type_ratio   <- soil_type_summary[!names(soil_type_summary)=="NA's"] / sum(soil_type_summary[!names(soil_type_summary)=="NA's"])
    
    soil_input <- get.geotop.inpts.keyword.value(keyword="SoilParFile", wpath=wpath, data.frame=TRUE, 
                                                 level = 1:nr_soiltypes)
    
    soil_thickness_header <- get.geotop.inpts.keyword.value(keyword="HeaderSoilDz", wpath=wpath)
    soil_saturation_header <- get.geotop.inpts.keyword.value(keyword="HeaderThetaSat", wpath=wpath)
    
    if (is.list(soil_input)) {
      soil_thickness <- soil_input[[1]][,soil_thickness_header]
      
      saturation_ratio_mat <- c()
      for (i in names(soil_type_ratio))
      {
        saturation_ratio_mat <- cbind(saturation_ratio_mat, soil_input[[as.integer(i)]][,soil_saturation_header] * soil_type_ratio[i]) 
      }
      soil_saturation <- rowSums(saturation_ratio_mat)
    } else {
      soil_thickness <- soil_input[,soil_thickness_header]
      soil_saturation <- soil_input[,soil_saturation_header]
    }
    
  } else {
    soil_saturation <- get.geotop.inpts.keyword.value(keyword="ThetaSat", wpath=wpath, numeric=T)
    soil_thickness <- get.geotop.inpts.keyword.value("SoilLayerThicknesses", numeric = T, wpath=wpath)
  }
  
  nlayers <- length(soil_thickness)
  
  name_maps <- get.geotop.inpts.keyword.value(keyword = mapkey, wpath=wpath)
  
  dir.create(file.path(wpath,"gif"))
  dir.create(file.path(wpath,"pdf"))
  
  if (is.null(layers)) {
    pointerMAPS <- pointer.to.maps.xyz.time(wpath, map.prefix = name_maps, 
                                            suffix = "N%04d.asc", 
                                            zoo.index = NULL, ntime=1,
                                            nlayers=length(dates))
    
    print("create maps in pdf format for each time step")
    for (d in 1:length(dates))
    {
      p <- GEOtop_ReadPlotRst(map = pointerMAPS[[d]], date = dates[d], variable = variable, layer = layers, 
                              limits = limits, legend = legend, lowcol = lowcol, highcol = highcol)
      ggsave(filename = file.path(wpath,paste("pdf/",variable,"_D",dates[d],".pdf",sep="")), plot = p)
    }
    
    setwd(file.path(wpath,"pdf"))
    file.remove("Rplot.pdf")
    
    # for linux system
    # for linux system
    print("image processing with ImageMagick")
    if (is.null(delay_value)) {
      system(paste("convert -loop 1 *.pdf ", file.path(wpath, paste("gif/", variable,".gif",sep=""))) )
    } else {
      system(paste("convert -loop 1 -delay ", delay_value, " *.pdf ", file.path(wpath, paste("gif/", variable,".gif",sep=""))) )
    }

    file.remove(dir())

  } else {
    pointerMAPS <- pointer.to.maps.xyz.time(wpath, map.prefix = name_maps, 
                                            suffix = "L%04dN%04d.asc", 
                                            zoo.index = NULL, ntime=length(dates), 
                                            nlayers=nlayers)
    
    for (i in layers)
    {
      print(paste("create maps in pdf format for each time step, layer ", i, sep=""))
      for (d in 1:length(dates))
      {
        p <- GEOtop_ReadPlotRst(map = pointerMAPS[[i]][d], date = dates[d], variable = variable, layer = i, 
                                limits = limits, legend = legend, lowcol = lowcol, highcol = highcol)
        ggsave(filename = file.path(wpath,paste("pdf/",variable,"_L",i,"_D",dates[d],".pdf",sep="")), plot = p)
      }
      
      setwd(file.path(wpath,"pdf"))
      file.remove("Rplots.pdf")
      
      # for linux system
      print("image processing with ImageMagick")
      if (is.null(delay_value)) {
        system(paste("convert -loop 1 *.pdf ", file.path(wpath, paste("gif/", variable,"_L",i,".gif",sep=""))) )
      } else {
        system(paste("convert -loop 1 -delay ", delay_value, " *.pdf ", file.path(wpath, paste("gif/", variable,"_L",i,".gif",sep=""))) )
      }
     
      file.remove(dir())
    }
  }

}
