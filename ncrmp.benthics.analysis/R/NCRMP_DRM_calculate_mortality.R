## Function to calculate colony mean (old and new) mortality at the site and strata level

# Purpose:
# creates csv files with mean mortality.


## Tag: data analysis


# outputs created in this file --------------
# old_mortality_site
# new_mortality_site
# Domain estimates



# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Mar 2020


##############################################################################################################################

#' Creates colony mortality summary dataframes
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param region A string indicating the region
#' @param species_filter A boolean value indicating whether to filter to a subset of species
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_DRM_calculate_mortality <- function(project, region, species_filter = "NULL"){

  tmp <- load_NCRMP_DRM_demo_data(project = project,
                                  region = region,
                                  species_filter = species_filter)

  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  # Calculate mean mortality

  # Old mortality

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    dat1_1stage <- dat_1stage %>%
      # filter to corals present, remove Marquesas and any corals not sampled for mortality
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    OLD_MORT != "NA",
                    OLD_MORT <= 100) %>%
      # change PROT to factor
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # calculate site level mortality
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(avsitemort = mean(OLD_MORT), .groups = "keep") %>%
      dplyr::mutate(MORT_TYPE = "Old") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    dat1_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    OLD_MORT != "NA",
                    OLD_MORT <= 100) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # when data is two stage (two transects or more) calculate transect mean before site mean.
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(transect_mort = mean(OLD_MORT), .groups = "keep") %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(avsitemort = mean(transect_mort), .groups = "keep") %>%
      dplyr::mutate(MORT_TYPE = "Old") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    old_mortality_site <- rbind(dat1_1stage, dat1_2stage)

  } else {

    old_mortality_site <- dat_1stage %>%
      # filter to corals present, remove Marquesas and any corals not sampled for mortality
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    OLD_MORT != "NA",
                    OLD_MORT <= 100) %>%
      # change PROT to factor
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # calculate site level mortality
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(avsitemort = mean(OLD_MORT), .groups = "keep") %>%
      dplyr::mutate(MORT_TYPE = "Old") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

  }

  # Apply weighting scheme and calculate strata and regional means

  tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = old_mortality_site, region, datatype = "mortality")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])

  old_mortality_strata <- mortality_strata %>%
    dplyr::mutate(MORT_TYPE = "Old")
  Domain_est_old_mort <- Domain_est %>%
    dplyr::mutate(MORT_TYPE = "Old")


  # Recent mortality

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI"||
     project == "NCRMP" && region == "Tortugas") {

    dat1_1stage <- dat_1stage %>%
      # filter to corals present, remove Marquesas and any corals not sampled for mortality
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    RECENT_MORT != "NA",
                    RECENT_MORT <= 100) %>%
      # change PROT to factor
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # calculate site level mortality
      dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(avsitemort = mean(RECENT_MORT)) %>%
      dplyr::mutate(MORT_TYPE = "Recent") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    dat1_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    RECENT_MORT != "NA",
                    RECENT_MORT <= 100) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # when data is two stage (two transects or more) calculate transect mean before site mean.
      dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(transect_mort = mean(RECENT_MORT)) %>%
      dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(avsitemort = mean(transect_mort)) %>%
      dplyr::mutate(MORT_TYPE = "Recent") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    recent_mortality_site <- rbind(dat1_1stage, dat1_2stage)

  } else {

    recent_mortality_site <- dat_1stage %>%
      # filter to corals present, remove Marquesas and any corals not sampled for mortality
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    RECENT_MORT != "NA",
                    RECENT_MORT <= 100) %>%
      # change PROT to factor
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # calculate site level mortality
      dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(avsitemort = mean(RECENT_MORT)) %>%
      dplyr::mutate(MORT_TYPE = "Recent") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

  }

  # Apply weighting scheme and calculate strata and regional means

  tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = recent_mortality_site, region, datatype = "mortality")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  rec_mortality_strata <- mortality_strata %>%
    dplyr::mutate(MORT_TYPE = "Recent")
  Domain_est_rec_mort <- Domain_est %>%
    dplyr::mutate(MORT_TYPE = "Recent")



  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "old_mortality_site" = old_mortality_site,
    "recent_mortality_site" = recent_mortality_site,
    "old_mortality_strata" =old_mortality_strata,
    "rec_mortality_strata" = rec_mortality_strata,
    "Domain_est_old_mort" = Domain_est_old_mort,
    "Domain_est_rec_mort" = Domain_est_rec_mort)

  return(output)

}
