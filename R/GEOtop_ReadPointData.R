NULL
#' Function to load GEOtop point simulation output based on observations
#'

#' @param wpath path pointing into simulation folder
#' @param soil_output_files output file keywords used in the function. Default is \code{c("SoilLiqContentProfileFile","SoilIceContentProfileFile", "SoilLiqWaterPressProfileFile", "SoilAveragedTempProfileFile")}
#' @param soil_files				boolean, \code{TRUE}: soil files are provided as GEOtop input. \code{FALSE}: soil is parameterized in the geotop.inpts file
#' @param save_rData				boolean, if \code{TRUE} (default) data is stored in working directory (simulation folder)
#' 
#' 
#' @export
#' 
#' 
#' 
#' 
#' @importFrom dplyr '%>%'
#' @importFrom geotopbricks declared.geotop.inpts.keywords
#' @importFrom data.table as.data.table
#' 
#' 
#' @examples
#'  
#' wpath <- system.file('simulation/Muntatschini_pnt_1_225_B2_004',package="AnalyseGeotop")
#' data("observations_B2")
#' 
#' out <- GEOtop_ReadPointData(wpath, soil_files=TRUE, save_rData=TRUE)
#' 
#' 

#
# 
#
# load(file.path(wpath, "obs", "observation.RData"))
# names(observation) <- c("hour", "day")
# obs <- observation
# 
# obs   <- list(hour=B2_h, day=B2_d)
# 
# 


#### ### @param wpath name with complete working path of a GEOtop simulation folder
# 
# ##"/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,
# ##share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/
# ###HiResAlp/1D/Montecini_pnt_1_225_B2_007/"
# ##wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,
# ###share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/
# MonaLisa/1D/Kaltern/sim006"
##

GEOtop_ReadPointData <- function(wpath, 
                                 soil_output_files=c("SoilLiqContentProfileFile","SoilIceContentProfileFile", "SoilLiqWaterPressProfileFile", "SoilAveragedTempProfileFile"), 
                                 soil_files=TRUE, save_rData=TRUE)
{
  
	#print("blo!!")	
#  # Evapotranspiration  
	Evap_surface.mm.  <- NULL
	Trasp_canopy.mm. <- NULL
	# partitioning: 1 means full evaporation - 0 means full transpiration  
	Evapotranspiration_Partitioning <- NULL 
	Evap_surface.mm. <- NULL
	Evapotranspiration.mm.<- NULL
#  # precipitation
	PrainPsnow_over_canopy.mm. <- NULL 
	Psnow_over_canopy.mm.  <- NULL 
	Prain_over_canopy.mm. <- NULL
#  # partitioning: 1 means full rain - 0 means full snow  
	Precipitation_part_over_canopy  <- NULL 
	Prain_over_canopy.mm. <- NULL
	PrainPsnow_over_canopy.mm. <- NULL
#  # net shortwave energy flux  
	Net_shortwave_flux_W.m2. <- NULL
	SWin.W.m2. <- NULL
	SWup.W.m2. <- NULL
# # net shortwave energy flux  
	Net_longwave_flux_W.m2. <- NULL 
	LWin.W.m2. <- NULL
	LWup.W.m2. <- NULL
	##  # net radiation 
	Net_radiation_W.m2.  <- NULL 
	Net_shortwave_flux_W.m2. <- NULL
	Net_longwave_flux_W.m2. <- NULL
	### latent heat flux in air
	Latent_heat_flux_over_canopy_W.m2. <- NULL 
	Canopy_fraction... <- NULL
	LEg_veg.W.m2. <- NULL
	LEv.W.m2.  <- NULL
	Canopy_fraction... <- NULL 
	LEg_unveg.W.m2. <- NULL
	### sensible heat flux in air
	##  Sensible_heat_flux_over_canopy_W.m2. <- NULL
	Canopy_fraction... <- NULL
	Hg_veg.W.m2. <- NULL
	Hv.W.m2. <- NULL
	Canopy_fraction... <- NULL 
	Hg_unveg.W.m2.<- NULL
	### energy budget
	Energy_budget_storage_W.m2. <- NULL
	Net_radiation_W.m2. <- NULL
	Latent_heat_flux_over_canopy_W.m2.  <- NULL 
	Sensible_heat_flux_over_canopy_W.m2. <- NULL
	Soil_heat_flux.W.m2. <- NULL
#  
	#print("ba")
	#a <- ls()
	#print(a)
	#### end ec 20160526
	
	
	
	
	
	
	
# get x- , y-coordinates of output points
  if (file.exists(file.path(wpath,"listpoints.txt")))
  {
    listpoints <- read.csv(file.path(wpath,"listpoints.txt"))
    xpoints <- listpoints$xcoord
    ypoints <- listpoints$ycoord
  } else {
    xpoints <- get.geotop.inpts.keyword.value("CoordinatePointX",wpath=wpath,numeric=T)
    ypoints <- get.geotop.inpts.keyword.value("CoordinatePointY",wpath=wpath,numeric=T)
  }
  
  level <- 1:length(xpoints)
# read point data with specified keyword  
	
 ########### print("ba ba!!")		
  point_data <- get.geotop.inpts.keyword.value(keyword="PointOutputFile", wpath=wpath,
                                                 raster=FALSE,
                                                 data.frame=TRUE,
                                                 level=level, 
                                                 date_field="Date12.DDMMYYYYhhmm.",
												 tz="Etc/GMT+1")
  
										 
  #### ec 20160
  ############print(names(point_data))
  
										 
  							 
										 
										 
  dt <- as.data.table(point_data)
  
#LWnet.W.m2. and SWnet.W.m2. is below the canopy, also LE and H 
  
 
# get variables direct or postprocessed from point data 
	
  out <- 
  dt %>%
  # Evapotranspiration  
    dplyr::mutate(Evapotranspiration.mm. = Evap_surface.mm. + Trasp_canopy.mm.) %>%
  # partitioning: 1 means full evaporation - 0 means full transpiration  
    dplyr::mutate(Evapotranspiration_Partitioning = Evap_surface.mm. / Evapotranspiration.mm.) %>%
  # precipitation
    dplyr::mutate(PrainPsnow_over_canopy.mm. = Psnow_over_canopy.mm. + Prain_over_canopy.mm.)  %>%
  # partitioning: 1 means full rain - 0 means full snow  
    dplyr::mutate(Precipitation_part_over_canopy = Prain_over_canopy.mm. / PrainPsnow_over_canopy.mm.) %>%
  # net shortwave energy flux  
    dplyr::mutate(Net_shortwave_flux_W.m2. = SWin.W.m2. - SWup.W.m2.) %>%
  # net shortwave energy flux  
    dplyr::mutate(Net_longwave_flux_W.m2. = LWin.W.m2. - LWup.W.m2.) %>% 
  # net radiation 
    dplyr::mutate(Net_radiation_W.m2. = Net_shortwave_flux_W.m2. + Net_longwave_flux_W.m2.) %>%
  # latent heat flux in air
    dplyr::mutate(Latent_heat_flux_over_canopy_W.m2. = Canopy_fraction... * (LEg_veg.W.m2. + LEv.W.m2.) + (1-Canopy_fraction...) * LEg_unveg.W.m2.) %>%
  # sensible heat flux in air
    dplyr::mutate(Sensible_heat_flux_over_canopy_W.m2. = Canopy_fraction... * (Hg_veg.W.m2. + Hv.W.m2.) + (1-Canopy_fraction...) * Hg_unveg.W.m2.) %>%
  # energy budget
    dplyr::mutate(Energy_budget_storage_W.m2. = Net_radiation_W.m2. - Latent_heat_flux_over_canopy_W.m2. - Sensible_heat_flux_over_canopy_W.m2. - Soil_heat_flux.W.m2.) 
  
# get available keywords
  keywords <- declared.geotop.inpts.keywords(wpath = wpath)$Keyword
  
# get soil information
    if (soil_files) {
      soil_input <- get.geotop.inpts.keyword.value(keyword="SoilParFile", wpath=wpath, data.frame=TRUE)
      soil_thickness <- soil_input[,1]
    } else {
      Dz <- soil_thickness <- get.geotop.inpts.keyword.value("SoilLayerThicknesses", numeric = T, wpath=wpath)
      Kh <-     get.geotop.inpts.keyword.value("NormalHydrConductivity", numeric = T, wpath=wpath)
      Kv <-     get.geotop.inpts.keyword.value("LateralHydrConductivity", numeric = T, wpath=wpath)
      vwc_r <-  get.geotop.inpts.keyword.value("ThetaRes", numeric = T, wpath=wpath)
      vwc_w <-  get.geotop.inpts.keyword.value("WiltingPoint", numeric = T, wpath=wpath)
      vwc_fc <- get.geotop.inpts.keyword.value("FieldCapacity", numeric = T, wpath=wpath)
      vwc_s <-  get.geotop.inpts.keyword.value("ThetaSat", numeric = T, wpath=wpath)
      alpha <-  get.geotop.inpts.keyword.value("AlphaVanGenuchten", numeric = T, wpath=wpath) 
      n <-      get.geotop.inpts.keyword.value("NVanGenuchten", numeric = T, wpath=wpath)
      soil_input <- data.frame(Dz,Kh,Kv,vwc_r,vwc_w,vwc_fc,vwc_s,alpha,n)
    }
    
# output depth in mm
    soil_head <- diff(c(0,cumsum(soil_thickness)))/2 + c(0,cumsum(soil_thickness))[-length(soil_thickness)-1]
    
# soil moisture content liquid, soil moisture content ice, ...
    # add liquid and ice water content
    if ("SoilLiqContentProfileFile" %in% soil_output_files & "SoilIceContentProfileFile" %in% soil_output_files &
        "SoilLiqContentProfileFile" %in% keywords & "SoilIceContentProfileFile" %in% keywords) {
      
    soil_file_liq <- get.geotop.inpts.keyword.value(keyword="SoilLiqContentProfileFile", wpath=wpath, data.frame=TRUE)
    names(soil_file_liq)[7:length(soil_file_liq)] <-  paste(substr("SoilLiqContentProfileFile",1,14), soil_head, sep="")
    
    soil_file_ice <- get.geotop.inpts.keyword.value(keyword="SoilIceContentProfileFile", wpath=wpath, data.frame=TRUE)
    names(soil_file_ice)[7:length(soil_file_ice)] <-  paste(substr("SoilIceContentProfileFile",1,14), soil_head, sep="")
    
    soil_file_tot <- soil_file_liq[,7:length(soil_file_liq)] + soil_file_ice[,7:length(soil_file_ice)]
    names(soil_file_tot) <-  paste("TotalSoilWaterContent", soil_head, sep="")
    soil_file_tot <- data.frame(soil_file_liq[,1:6],soil_file_tot)
      
    out <- 
      out %>% 
      dplyr::left_join(as.data.table(soil_file_liq[,-1])) %>% 
      dplyr::left_join(as.data.table(soil_file_ice[,-1]))%>% 
      dplyr::left_join(as.data.table(soil_file_tot[,-1]))
      
    soil_output_files <- soil_output_files[-c(grep("SoilLiqContentProfileFile",soil_output_files),grep("SoilIceContentProfileFile",soil_output_files))]
    
    }
    
    for (i in soil_output_files) {
      if (i %in% keywords)
      {
        soil_file <- get.geotop.inpts.keyword.value(keyword=i, wpath=wpath, data.frame=TRUE)
        soil_header <- paste(substr(i,1,14), soil_head, sep="")
        names(soil_file)[7:length(soil_file)] <- soil_header
        
        out <- 
          out %>% 
          dplyr::left_join(as.data.table(soil_file[,-1]))
      }
    }
  
	
# zoo object
  out <- zoo(out[, -c(1:5), with=F], time(point_data))  
    
# save or not    
  if(save_rData) save(list = "out", file = file.path(wpath,"PointOut.RData"))
  return(out)
}