% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GEOtop_AnimateMaps.R
\name{GEOtop_animateMAPS}
\alias{GEOtop_animateMAPS}
\title{GEOtop output map animation}
\usage{
GEOtop_animateMAPS(wpath, mapkey, layers, soil_files, variable, limits, legend,
  lowcol = "#f7fbff", highcol = "#08306b", delay_value = NULL)
}
\arguments{
\item{wpath}{path into GEOtop simulation}

\item{mapkey}{keyword name; see package \emph{geotopbricks}, function \code{\link{get.geotop.inpts.keyword.value}}, argument \emph{keyword}}

\item{layers}{default = NULL, for variables without layering e.g. snow water eqivalent; integer vector defining layers which should be processed for data with layerinf, e.g. soil moisture}

\item{soil_files}{boolean, TRUE: soil files are provided as GEOtop input. FALSE: soil is parameterized in the geotop.inpts file}

\item{variable}{character, variable characterization}

\item{limits}{argument for \code{\link{GEOtop_ReadPlotRst}}}

\item{legend}{character, legend characterization, e.g. \code{"vol\%"} for soil moisture data}

\item{lowcol}{minimum colour for visualization, default = \code{"#f7fbff"}}

\item{highcol}{maximum colour for visualization, default = "#08306b"; default is creating a blue       colour}

\item{delay_value}{default = NULL, argument for function convert (ImageMagick), display the next image after pausing}
}
\value{
.gif animation file in folder "path/gif"
}
\description{
GEOtop output map animation. Using GEOtop_ReadPlotRst() to visualize GEOtop output maps in pdf format and ImageMagick to animate images.
}
\note{
for further information on ImageMagick see \url{http://www.imagemagick.org/}, for information on how to install on LINUX (UHREL, Ubuntu) and CentOS see \url{http://tecadmin.net/install-imagemagick-on-linux/}
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}

