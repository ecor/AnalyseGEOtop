NULL

#' Calculation of latent and sensible heat fluxes over canopy
#'	
#' Calculation of latent and sensible heat fluxes over canopy from GEOtop point output file
#'  
#' @param data list, containing zoo objects of latent and sensible heat fluxes (ground veg - g_veg, ground bare - g_unveg, canopy - veg, canopy fraction - cf); list names: "LE"  and "H", zoo header refers to GEOtop point file
#' @param canopy_fraction numeric, \code{default = NULL}, canopy fraction for simulation point, set if not available from simulation point output
#' 
#' 
#' @export
#' 

#		}
#\details{
#	%%  ~~ If necessary, more details than the description above ~~
#}
#\value{
#	%%  ~Describe the value returned
#	%%  If it is a LIST, use
#	%%  \item{comp1 }{Description of 'comp1'}
#	%%  \item{comp2 }{Description of 'comp2'}
#	%% ...
#}
#\references{
#	%% ~put references to the literature/web site here ~
#}
#\author{
#	%%  ~~who you are~~
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
#\examples{
#}
#% Add one or more standard keywords, see file 'KEYWORDS' in the
#		% R documentation directory.
#		\keyword{ ~kwd1 }
#\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
#		
#


GEOtop_EfluxOcanopy <- function(data, canopy_fraction=NULL)
{
  overcanopy <- list()
  
  for (i in names(data)) 
  {
    if (!is.null(canopy_fraction)) {
      overcanopy[[i]] <- canopy_fraction * (data[[i]]$g_veg + data[[i]]$veg) + (1-canopy_fraction) * data[[i]]$g_unveg
    } else {
      overcanopy[[i]] <- data[[i]]$cf * (data[[i]]$g_veg + data[[i]]$veg) + (1-data[[i]]$cf) * data[[i]]$g_unveg
    }
  }
  
  return(overcanopy)
}