###Function to import NCRMP grids and save as R data

#Purpose:
# 1. imports sample frame grids for various regions
# 2. reprojects from UTM to WGS 1984
# 3. Saves as R.data for use in extract grid function
# 4. calculates Ntot and outputs to a csv

# L Bauer
# July 2016

# Last update
# Mar 2020

# Even though weights have already been calculated for 2013-2015 data, they are included for consistency

#' Import NCRMP grids and save as R data
#'
#' This function has 4 purposes:
#'  (1) Import sample frame grids for various regions;
#'  (2) Reproject from UTM to WGS 1984;
#'  (3) Save as R.data for use in extract grid function;
#'  (4) Calculate Ntot and output to a csv file
#'
#' @param region A string indicating the region
#' @param year A numeric indicating the year
#' @return A list of two dataframes: sampling_grid and strata_ntot
#' @export

import_grids <- function (region, year){

  library(rgdal)
  library(maptools)
  library(magrittr)

  ## define coordinate system for reprojection (WGS 1984)
  crs.wgs84 <- CRS("+init=epsg:4326")


  ##### Puerto Rico ######
  if(region == "PRICO") {

    if(year == 2014) {
      #read geodatabase
      PR_2014_sample_design <- "T:\\Projects\\NCRMP\\2014 Missions\\Puerto Rico 2014\\Sample Design\\Final Sample Design\\PR_2014_SamplingDesignPackage.gdb"

      # List all feature classes in the geodatabase
      subset(ogrDrivers(), grepl("GDB", name))
      fc_list = ogrListLayers(PR_2014_sample_design)
      print(fc_list)

      # read the final sample frame feature class
      PR_2014_sample_frame <- readOGR(dsn=PR_2014_sample_design, layer="PR_2014_SAMPLE_FRAME")
      summary(PR_2014_sample_frame)

      # reproject to WGS 1984
      PR_2014_sample_frame_WGS1984 <- spTransform(PR_2014_sample_frame, crs.wgs84)

      # save as R.data
      save(PR_2014_sample_frame_WGS1984, file = "T:\\Projects\\NCRMP\\Data Analysis\\FY14\\Puerto Rico 2014\\grids\\PR_2014_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(PR_2014_sample_frame_WGS1984) #convert to dataframe
      strata_ntot <- ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "DEPTH", "ADMIN"),
                           summarize, NTOT=length(GRID_ID))
      # add region and grid size fields to match FL
      strata_ntot$REGION <- "PRICO"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2014
      # export to csv
      write.csv(strata_ntot, "T:\\Projects\\NCRMP\\Data Analysis\\FY14\\Puerto Rico 2014\\grids\\PR_2014_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = PR_2014_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2016) {

      #read geodatabase
      #PR_2016_sample_design <- "T:\\Projects\\NCRMP\\2016_Missions\\Puerto_Rico_2016\\Sample_Design\\Final Sample Design\\PR_2016_Sample_Design.gdb"

      # or just read shapefile
      PRICO_2016_sample_frame <- readOGR(dsn="T:\\NCRMP\\2016_Missions\\Puerto_Rico_2016\\Sample_Design\\Final Sample Design", layer="PR_2016_SAMPLE_FRAME_020819")

      # Below is needed if ou read in gdb
      # # List all feature classes in the geodatabase
      # subset(ogrDrivers(), grepl("GDB", name))
      # fc_list = ogrListLayers(PR_2016_sample_design)
      # print(fc_list)
      #
      # # read the final sample frame feature class
      # PR_2016_sample_frame <- readOGR(dsn=PR_2016_sample_design, layer="PR_2016_SAMPLE_FRAME")
      # summary(PR_2016_sample_frame)

      # reproject to WGS 1984
      PRICO_2016_sample_frame_WGS1984 <- spTransform(PRICO_2016_sample_frame, crs.wgs84)

      # save as R.data
      save(PRICO_2016_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY16\\Puerto Rico 2016\\grids\\PRICO_2016_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(PRICO_2016_sample_frame_WGS1984)
      strata_ntot <- plyr::ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "DEPTH", "ADMIN"),
                           dplyr::summarize, NTOT=length(GRID_ID))

      # add region and grid size fields to match FL
      strata_ntot$REGION <- "PRICO"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2016

      strata_ntot <- strata_ntot %>%
        dplyr::mutate(REGION = "PRICO",
                      GRID_SIZE = 50,
                      SUB_REGION = BIOTOPE,
                      YEAR = 2016,
                      WH = NTOT/sum(NTOT)) %>%
        dplyr::select("REGION", "YEAR", "STRATUM", "HABITAT", "SUB_REGION", "DEPTH", "ADMIN", "NTOT", "GRID_SIZE", "WH")

       # Add grid and allocation NTOT to QAQC package
      usethis::use_data(PRICO_2016_sample_frame_WGS1984,  overwrite = TRUE)

      PRICO_2016_strata_allocation <- strata_ntot

      usethis::use_data(PRICO_2016_strata_allocation,  overwrite = TRUE)

      # export to csv
      write.csv(strata_ntot, "T:\\NCRMP\\Data Analysis\\FY16\\Puerto Rico 2016\\grids\\PR_2016_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = PR_2016_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2019) {

      # read the final sample frame feature class
      PRICO_2019_sample_frame <- readOGR(dsn="T:\\NCRMP\\2019_Missions\\Puerto_Rico_2019\\Sample_Design\\Final_Sample_Design", layer="PR2019_SAMPLE_FRAME_05132019")
      #summary(PRICO_2019_sample_frame)

      # reproject to WGS 1984
      PRICO_2019_sample_frame_WGS1984 <- spTransform(PRICO_2019_sample_frame, crs.wgs84)

      # save as R.data
      save(PRICO_2019_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY19\\PRICO 2019\\grids\\PR_2019_sample_frame_WGS1984.RData")
      save(PRICO_2019_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY19\\Sample_design_analysis\\grids\\PR_2019_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(PRICO_2019_sample_frame_WGS1984)

      strata_ntot <- plyr::ddply(sampling_grid_df, c("STRATUM", "HABITAT", "SUBREGION", "DEPTH", "ADMIN"),
                                 dplyr::summarize, NTOT=length(GRID_ID))

      # add region and grid size fields to match FL
      strata_ntot <- strata_ntot %>%
        dplyr::mutate(REGION = "PRICO",
                      GRID_SIZE = 50,
                      SUB_REGION = SUBREGION,
                      YEAR = 2019,
                      WH = NTOT/sum(NTOT)) %>%
        dplyr::select("REGION", "YEAR", "STRATUM", "HABITAT", "SUB_REGION", "DEPTH", "ADMIN", "NTOT", "GRID_SIZE", "WH")

       # Add grid and allocation NTOT to QAQC package
      usethis::use_data(PRICO_2019_sample_frame_WGS1984,  overwrite = TRUE)

      PRICO_2019_strata_allocation <- strata_ntot

      usethis::use_data(PRICO_2019_strata_allocation,  overwrite = TRUE)


      # export to csv
      write.csv(strata_ntot, "T:\\NCRMP\\Data Analysis\\FY19\\PRICO 2019\\grids\\PR_2019_strata.csv", row.names = FALSE)
      write.csv(strata_ntot, "T:\\NCRMP\\Data Analysis\\FY19\\Sample_design_analysis\\allocation ntot\\PR_2019_strata.csv", row.names = FALSE)


      # return output
      output <- list("sampling_grid" = PR_2016_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2021) {

      # read the final sample frame feature class
      PRICO_2021_sample_frame <- readOGR(dsn="K:\\_BioGeoProjects\\NCRMP\\2021_Missions\\Puerto_Rico_2021\\Sample_Design\\Final_Sample_Design", layer="PR2021_SAMPLE_FRAME")
      #summary(PRICO_2021_sample_frame)

      # reproject to WGS 1984
      PRICO_2021_sample_frame_WGS1984 <- spTransform(PRICO_2021_sample_frame, crs.wgs84)


      # Add grid and allocation NTOT to QAQC package
      usethis::use_data(PRICO_2021_sample_frame_WGS1984,  overwrite = TRUE)

      # return output
      output <- list("sampling_grid" = PRICO_2021_sample_frame_WGS1984)
      return(output)
    }


  }

  ##### St. Thomas / St. John ######
  if(region == "STTSTJ") {

    if(year == 2013) {
      #read geodatabase
      ##Need to confirm 2013 sample design grid
      STTSTJ_2013_sample_design <- "T:\\Projects\\NCRMP\\2013 Missions\\St. Thomas-St. John 2013\\Sample Design\\Final Sample Design"

      # List all feature classes in the geodatabase
      subset(ogrDrivers(), grepl("GDB", name))
      fc_list = ogrListLayers(STTSTJ_2013_sample_design)
      print(fc_list)

      # read the final sample frame feature class
      STTSTJ_2013_sample_frame <- readOGR(dsn=STTSTJ_2013_sample_design, layer="STTSTJ_2013_SAMPLE_FRAME")
      summary(STTSTJ_2013_sample_frame)
      proj4string(STTSTJ_2013_sample_frame) <- CRS("+proj=utm +zone=20 +datum=WGS84")

      # reproject to WGS 1984
      STTSTJ_2013_sample_frame_WGS1984 <- spTransform(STTSTJ_2013_sample_frame, crs.wgs84)

      # save as R.data
      save(STTSTJ_2013_sample_frame_WGS1984, file = "T:\\Projects\\NCRMP\\Data Analysis\\FY13\\St. Thomas-St. John 2013\\grids\\STTSTJ_2013_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(STTSTJ_2013_sample_frame_WGS1984) #convert to dataframe
      strata_ntot <- ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "DEPTH", "ADMIN"),
                           summarize, NTOT=length(GRID_ID))
      # add region and grid size fields to match FL
      strata_ntot$REGION <- "STTSTJ"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2013
      # export to csv
      write.csv(strata_ntot, "T:\\Projects\\NCRMP\\Data Analysis\\FY13\\St. Thomas-St. John 2013\\grids\\STTSTJ_2013_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = STTSTJ_2013_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2015) {
      #read geodatabase
      STTSTJ_2015_sample_design <- "T:\\Projects\\NCRMP\\2015 Missions\\St. Thomas-St. John 2015\\Sample Design\\Final Sample Design\\STTSTJ_2015_SamplingDesignPackage.gdb"

      # List all feature classes in the geodatabase
      subset(ogrDrivers(), grepl("GDB", name))
      fc_list = ogrListLayers(STTSTJ_2015_sample_design)
      print(fc_list)

      # read the final sample frame feature class
      STTSTJ_2015_sample_frame <- readOGR(dsn=STTSTJ_2015_sample_design, layer="STTSTJ_2015_SAMPLE_FRAME")
      #summary(STTSTJ_2015_sample_frame)

      # reproject to WGS 1984
      STTSTJ_2015_sample_frame_WGS1984 <- spTransform(STTSTJ_2015_sample_frame, crs.wgs84)

      # save as R.data
      save(STTSTJ_2015_sample_frame_WGS1984, file = "T:\\Projects\\NCRMP\\Data Analysis\\FY15\\USVI 2015\\grids\\STTSTJ_2015_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(STTSTJ_2015_sample_frame_WGS1984)
      strata_ntot <- ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "DEPTH", "ADMIN"),
                           summarize, NTOT=length(GRID_ID))
      # add region and grid size fields to match FL
      strata_ntot$REGION <- "STTSTJ"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2015
      # export to csv
      write.csv(strata_ntot, "T:\\Projects\\NCRMP\\Data Analysis\\FY15\\USVI 2015\\grids\\STTSTJ_2015_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = STTSTJ_2015_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2017) {
      #read geodatabase
      STTSTJ_2017_sample_design <- "T:\\NCRMP\\2017_Missions\\St. John St. Thomas 2017\\Sample Design\\Final Sample Design\\STTSTJ_2017_Sample_Design.gdb"

      # List all feature classes in the geodatabase
      subset(ogrDrivers(), grepl("GDB", name))
      fc_list = ogrListLayers(STTSTJ_2017_sample_design)
      print(fc_list)

      # read the final sample frame feature class
      STTSTJ_2017_sample_frame <- readOGR(dsn=STTSTJ_2017_sample_design, layer="STTSTJ_2017_SAMPLEFRAME_FINAL")
      #summary(STTSTJ_2017_sample_frame)

      # reproject to WGS 1984
      STTSTJ_2017_sample_frame_WGS1984 <- spTransform(STTSTJ_2017_sample_frame, crs.wgs84)

      # save as R.data
      save(STTSTJ_2017_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY17\\St Thomas St John 2017\\grids\\STTSTJ_2017_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(STTSTJ_2017_sample_frame_WGS1984)
      strata_ntot <- ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "DEPTH", "ADMIN"),
                           summarize, NTOT=length(GRID_ID))
      # add region and grid size fields to match FL
      strata_ntot$REGION <- "STTSTJ"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2017
      # export to csv
      write.csv(strata_ntot, "T:\\Projects\\NCRMP\\Data Analysis\\FY17\\St Thomas St John 2017\\grids\\STTSTJ_2017_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = STTSTJ_2017_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2019) {
      #read geodatabase
      # STTSTJ_2019_sample_design <- "T:\\NCRMP\\2019_Missions\\St_John_St_Thomas_2019\\Sample_Design\\Final_Sample_Design\\STTSTJ_2019_Sample_Design.gdb"
      #
      # # List all feature classes in the geodatabase
      # subset(ogrDrivers(), grepl("GDB", name))
      # fc_list = ogrListLayers(STTSTJ_2017_sample_design)
      # print(fc_list)

      # read the final sample frame feature class
      STTSTJ_2019_sample_frame <- readOGR(dsn="T:\\NCRMP\\2019_Missions\\St_John_St_Thomas_2019\\Sample_Design\\Final_Sample_Design", layer="STTSTJ_2019_SAMPLE_FRAME_DRAFT")
      #summary(STTSTJ_2019_sample_frame)

      # reproject to WGS 1984
      STTSTJ_2019_sample_frame_WGS1984 <- spTransform(STTSTJ_2019_sample_frame, crs.wgs84)

      # save as R.data
      save(STTSTJ_2019_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY19\\USVI 2019\\grids\\STTSTJ_2019_sample_frame_WGS1984.RData")
      save(STTSTJ_2019_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY19\\Sample_design_analysis\\grids\\STTSTJ_2019_sample_frame_WGS1984.RData")

      # calculate allocation Ntot from grid
      sampling_grid_df <- as.data.frame(STTSTJ_2019_sample_frame_WGS1984)

      strata_ntot <- plyr::ddply(sampling_grid_df, c("STRATUM", "HABITAT", "SUB_REGION", "DEPTH", "ADMIN"),
                                 dplyr::summarize, NTOT=length(GRID_ID))

      # add region and grid size fields to match FL
      strata_ntot <- strata_ntot %>%
        dplyr::mutate(REGION = "STTSTJ",
                      GRID_SIZE = 50,
                      YEAR = 2019,
                      WH = NTOT/sum(NTOT)) %>%
        dplyr::select("REGION", "YEAR", "STRATUM", "HABITAT", "SUB_REGION", "DEPTH", "ADMIN", "NTOT", "GRID_SIZE", "WH")

      # export to csv
      write.csv(strata_ntot, "T:\\NCRMP\\Data Analysis\\FY19\\USVI 2019\\grids\\STTSTJ_2019_strata.csv", row.names = FALSE)
      write.csv(strata_ntot, "T:\\NCRMP\\Data Analysis\\FY19\\Sample_design_analysis\\allocation ntot\\STTSTJ_2019_strata.csv", row.names = FALSE)


      # Add grid and allocation NTOT to QAQC package
      usethis::use_data(STTSTJ_2019_sample_frame_WGS1984,  overwrite = TRUE)

      STTSTJ_2019_strata_allocation <- strata_ntot

      usethis::use_data(STTSTJ_2019_strata_allocation,  overwrite = TRUE)

      # return output
      output <- list("sampling_grid" = STTSTJ_2019_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2021) {

      # read the final sample frame feature class
      STTSTJ_2021_sample_frame <- readOGR(dsn="K:\\_BioGeoProjects\\NCRMP\\2021_Missions\\St_John_St_Thomas_2021\\Sample_Design\\Final_Sample_Design", layer="STTSTJ_2021_SAMPLE_FRAME")
      #summary(STTSTJ_2021_sample_frame)

      # reproject to WGS 1984
      STTSTJ_2021_sample_frame_WGS1984 <- spTransform(STTSTJ_2021_sample_frame, crs.wgs84)


      # Add grid and allocation NTOT to QAQC package
      usethis::use_data(STTSTJ_2021_sample_frame_WGS1984,  overwrite = TRUE)

      # return output
      output <- list("sampling_grid" = STTSTJ_2021_sample_frame_WGS1984)
      return(output)
    }

  }

  if(region == "STX") {

    if(year == 2015) {
      #read geodatabase
      STC_2015_sample_design <- "T:\\Projects\\NCRMP\\2015 Missions\\St. Croix 2015\\Sample Design\\Final Sample Design\\STC_2015_SamplingDesignPackage.gdb"

      # List all feature classes in the geodatabase
      subset(ogrDrivers(), grepl("GDB", name))
      fc_list = ogrListLayers(STC_2015_sample_design)
      print(fc_list)

      # read the final sample frame feature class
      STC_2015_sample_frame <- readOGR(dsn=STC_2015_sample_design, layer="STC_2015_SAMPLE_FRAME")
      #summary(STC_2015_sample_frame)

      # reproject to WGS 1984
      STC_2015_sample_frame_WGS1984 <- spTransform(STC_2015_sample_frame, crs.wgs84)

      # save as R.data
      save(STC_2015_sample_frame_WGS1984, file = "T:\\Projects\\NCRMP\\Data Analysis\\FY15\\USVI 2015\\grids\\STC_2015_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(STC_2015_sample_frame_WGS1984)
      strata_ntot <- ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "DEPTH", "ADMIN"),
                           summarize, NTOT=length(GRID_ID))
      # add region and grid size fields to match FL
      strata_ntot$REGION <- "STC"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2015
      # export to csv
      write.csv(strata_ntot, "T:\\Projects\\NCRMP\\Data Analysis\\FY15\\USVI 2015\\grids\\STC_2015_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = STC_2015_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2017) {
      #read geodatabase
      STX_2017_sample_design <- "T:\\Projects\\NCRMP\\2017_Missions\\St. Croix 2017\\Sample_Design\\Final Sample Design\\STX_2017_Sample_Design_Package.gdb"

      # List all feature classes in the geodatabase
      subset(ogrDrivers(), grepl("GDB", name))
      fc_list = ogrListLayers(STX_2017_sample_design)
      print(fc_list)

      # read the final sample frame feature class
      STX_2017_sample_frame <- readOGR(dsn=STX_2017_sample_design, layer="STX_2017_SAMPLE_FRAME")
      #summary(STX_2017_sample_frame)

      # reproject to WGS 1984
      STX_2017_sample_frame_WGS1984 <- spTransform(STX_2017_sample_frame, crs.wgs84)

      # save as R.data
      save(STX_2017_sample_frame_WGS1984, file = "T:\\Projects\\NCRMP\\Data Analysis\\FY17\\St Croix 2017\\grids\\STX_2017_sample_frame_WGS1984.rda")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(STX_2017_sample_frame_WGS1984)
      strata_ntot <- ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "DEPTH", "ADMIN"),
                           summarize, NTOT=length(GRID_ID))
      # add region and grid size fields to match FL
      strata_ntot$REGION <- "STX"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2017
      # export to csv
      write.csv(strata_ntot, "T:\\Projects\\NCRMP\\Data Analysis\\FY17\\USVI 2017\\grids\\STX_2017_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = STX_2017_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2019) {
      #read geodatabase
      # STX_2019_sample_design <- "T:\\NCRMP\\2019_Missions\\St_Croix_2019\\Sample_Design\\Final_Sample_Design\\STX2019_Sample_Design_Package.gdb"
      #
      # # List all feature classes in the geodatabase
      # subset(ogrDrivers(), grepl("GDB", name))
      # fc_list = ogrListLayers(STX_2019_sample_design)
      # print(fc_list)

      # read the final sample frame feature class
      STX_2019_sample_frame <- readOGR(dsn="T:\\NCRMP\\2019_Missions\\St_Croix_2019\\Sample_Design\\Final_Sample_Design", layer="STX2019_SAMPLE_FRAME_DRAFT_Revised")
      #summary(STX_2017_sample_frame)

      # reproject to WGS 1984
      STX_2019_sample_frame_WGS1984 <- spTransform(STX_2019_sample_frame, crs.wgs84)

      # save as R.data
      save(STX_2019_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY19\\USVI 2019\\grids\\STX_2019_sample_frame_WGS1984.Rdata")
      save(STX_2019_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY19\\Sample_design_analysis\\grids\\STX_2019_sample_frame_WGS1984.RData")

      # calculate allocation Ntot from grid
      sampling_grid_df <- as.data.frame(STX_2019_sample_frame_WGS1984)
      strata_ntot <- plyr::ddply(sampling_grid_df, c("STRATUM", "HABITAT", "SUBREGION", "DEPTH", "ADMIN"),
                                 dplyr::summarize, NTOT=length(GRID_ID))

      # add region and grid size fields to match FL
      strata_ntot <- strata_ntot %>%
        dplyr::mutate(REGION = "STX",
                      GRID_SIZE = 50,
                      SUB_REGION =SUBREGION,
                      YEAR = 2019,
                      WH = NTOT/sum(NTOT)) %>%
        dplyr::select("REGION", "YEAR", "STRATUM", "HABITAT", "SUB_REGION", "DEPTH", "ADMIN", "NTOT", "GRID_SIZE", "WH") %>%
        dplyr::filter(!is.na(STRATUM))

      # export to csv
      write.csv(strata_ntot, "T:\\NCRMP\\Data Analysis\\FY19\\USVI 2019\\grids\\STX_2019_strata.csv", row.names = FALSE)
      write.csv(strata_ntot, "T:\\NCRMP\\Data Analysis\\FY19\\Sample_design_analysis\\allocation ntot\\STX_2019_strata.csv", row.names = FALSE)

      # Add grid and allocation NTOT to QAQC package
      usethis::use_data(STX_2019_sample_frame_WGS1984,  overwrite = TRUE)

      STX_2019_strata_allocation <- strata_ntot

      usethis::use_data(STX_2019_strata_allocation,  overwrite = TRUE)

      # return output
      output <- list("sampling_grid" = STX_2019_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2021) {

      # load the sample frame R data file created during the sample allocation process
      load("K:/_BioGeoProjects/NCRMP/Data Analysis/FY21/USVI 2021/grids/STX_2021_sample_frame_WGS1984.Rdata")
      #summary(STTSTJ_2021_sample_frame)


      # Add grid and allocation NTOT to QAQC package
      usethis::use_data(STX_2021_sample_frame_WGS1984,  overwrite = TRUE)

      # return output
      output <- list("sampling_grid" = STTSTJ_2019_sample_frame_WGS1984)
      return(output)
    }



  }

  ##### FGBNMS ######
  if(region == "FGBNMS") {

    if(year == 2013) {
      #read shapefile
      ##no geodatabase for 2013 FGB
      FGBNMS_2013_sample_frame <- readOGR("T:\\Projects\\NCRMP\\2013 Missions\\FGBNMS 2013\\Sample Design\\Final Sample Design", "FGB_2013_final_GRID")

      # reproject to WGS 1984
      FGBNMS_2013_sample_frame_WGS1984 <- spTransform(FGBNMS_2013_sample_frame, crs.wgs84)

      # save as R.data
      save(FGBNMS_2013_sample_frame_WGS1984, file = "T:\\Projects\\NCRMP\\Data Analysis\\FY13\\FGBNMS 2013\\grids\\FGBNMS_2013_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(FGBNMS_2013_sample_frame_WGS1984) #convert to dataframe
      #add stratum field
      sampling_grid_df$STRATUM <- with(sampling_grid_df, paste(HABITAT, BIOTOPE, ADMIN, sep=""))
      strata_ntot <- ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "ADMIN"),
                           summarize, NTOT=length(GRID_ID))
      # add region and grid size fields to match FL
      strata_ntot$REGION <- "FGBNMS"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2013
      # export to csv
      write.csv(strata_ntot, "T:\\Projects\\NCRMP\\Data Analysis\\FY13\\FGBNMS 2013\\grids\\FGBNMS_2013_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = FGBNMS_2013_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2015) {
      #read geodatabase
      FGBNMS_2015_sample_design <- "T:\\Projects\\NCRMP\\2015 Missions\\FGBNMS 2015\\Sample Design\\Final Sample Design\\FGBNMS_2015_Sample_Design.gdb"

      # List all feature classes in the geodatabase
      subset(ogrDrivers(), grepl("GDB", name))
      fc_list = ogrListLayers(FGBNMS_2015_sample_design)
      print(fc_list)

      # read the final sample frame feature class
      FGBNMS_2015_sample_frame <- readOGR(dsn=FGBNMS_2015_sample_design, layer="FGBNMS_2015_SAMPLE_FRAME")
      #summary(FGBNMS_2015_sample_frame)

      # reproject to WGS 1984
      FGBNMS_2015_sample_frame_WGS1984 <- spTransform(FGBNMS_2015_sample_frame, crs.wgs84)

      # save as R.data
      save(FGBNMS_2015_sample_frame_WGS1984, file = "T:\\Projects\\NCRMP\\Data Analysis\\FY15\\FGBNMS 2015\\grids\\FGBNMS_2015_sample_frame_WGS1984.RData")

      # calculate Ntot from grid
      sampling_grid_df <- as.data.frame(FGBNMS_2015_sample_frame_WGS1984)
      sampling_grid_df$STRATUM <- with(sampling_grid_df, paste(HABITAT, BIOTOPE, ADMIN, sep=""))
      strata_ntot <- ddply(sampling_grid_df, c("STRATUM", "HABITAT", "BIOTOPE", "ADMIN"),
                           summarize, NTOT=length(GRID_ID))

      # add region and grid size fields to match FL
      strata_ntot$REGION <- "FGBNMS"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2015
      # export to csv
      write.csv(strata_ntot, "T:\\Projects\\NCRMP\\Data Analysis\\FY15\\FGBNMS 2015\\grids\\FGBNMS_2015_strata.csv", row.names = FALSE)

      # return output
      output <- list("sampling_grid" = FGBNMS_2015_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

    if(year == 2018) {
      #read geodatabase
      FGBNMS_2018_sample_design <- "T:\\NCRMP\\2018_Missions\\FGBNMS 2018\\Sample_Design\\Final_Sample_Design\\FGBNMS_2018_Sample_Design_Package.gdb"

      # List all feature classes in the geodatabase
      subset(ogrDrivers(), grepl("GDB", name))
      fc_list = ogrListLayers(FGBNMS_2018_sample_design)
      print(fc_list)

      # read the final sample frame feature class
      FGBNMS_2018_sample_frame <- readOGR(dsn=FGBNMS_2018_sample_design, layer="FGBNMS_2018_SAMPLE_FRAME")
      #summary(FGBNMS_2015_sample_frame)

      # reproject to WGS 1984
      FGBNMS_2018_sample_frame_WGS1984 <- spTransform(FGBNMS_2018_sample_frame, crs.wgs84)

      # save as R.data
      save(FGBNMS_2018_sample_frame_WGS1984, file = "T:\\NCRMP\\Data Analysis\\FY18\\FGB 2018\\grids\\FGBNMS_2018_sample_frame_WGS1984.RData")

      # calculate allocation Ntot from grid
      sampling_grid_df <- as.data.frame(FGBNMS_2018_sample_frame_WGS1984)
      sampling_grid_df$STRATUM <- with(sampling_grid_df, paste(HABITAT, SUB_REGION, ADMIN, sep=""))
      strata_ntot <- plyr::ddply(sampling_grid_df, c("STRATUM", "HABITAT", "SUB_REGION", "DEPTH", "ADMIN"),
                                 dplyr::summarize, NTOT=length(GRID_ID))

      # add region and grid size fields to match FL
      strata_ntot$REGION <- "GOM"
      strata_ntot$GRID_SIZE <- 50
      strata_ntot$YEAR <- 2018
      # export to csv
      write.csv(strata_ntot, "T:\\NCRMP\\Data Analysis\\FY18\\FGB 2018\\grids\\FGBNMS_2018_strata.csv", row.names = FALSE)

      # Add grid to QAQC package

      usethis::use_data(FGBNMS_2018_sample_frame_WGS1984,  overwrite = TRUE)

      # return output
      output <- list("sampling_grid" = FGBNMS_2018_sample_frame_WGS1984, "strata_ntot" = strata_ntot)
      return(output)
    }

  }


}


