## Function to calculate colony mean (old and new) mortality at the site and strata level for DRM and SCREAM data 1999-2013

# Purpose:
# creates csv files with mean mortality.


## Tag: data analysis


# outputs created in this file --------------
# old_mortality_site
# new_mortality_site
# wh_old_mortality_strata
# wh_new_mortality_strata
# Domain estimates


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Nov 2018


##############################################################################################################################

#' Creates colony density and colony size summary dataframes
#'
#'
#'
#'
#'
#' @param project A string indicating the project, DRM or DRM and SCREAM combined
#' @param species_filter A boolean value indicating whether to filter to a subset of species
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

DRM_SCREAM_calculate_mortality <- function(project = "NULL", species_filter = "NULL"){

  ### Create species filters

  # First 6 species are from sample allocation, remainder are ESA listed species
  FLK_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                     "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN"
                  ,
                     "SID SIDE",
                     "POR PORI"
                  )

  Tort_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                     "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN"
                   ,
                     "SID SIDE",
                     "POR PORI"
                   )

  SEFCRI_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                     "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN"
                     ,
                     "SID SIDE",
                     "POR PORI"
                     )



  if(project == "DRM_SCREAM"||
     project == "NULL"){

    if(species_filter == "FALSE"||
       species_filter == "NULL"){

      # Load data

      dat <- preNCRMP_SCREAM_DRM_coral_demographics %>%
        # Set LAT and LON to the same # of digits - helps with grouping
        dplyr::mutate(LAT_DEGREES = sprintf("%0.5f", LAT_DEGREES),
                      LON_DEGREES = sprintf("%0.5f", LON_DEGREES)) %>%
        # Filter out Sand
        dplyr::filter(STRAT != "SAND_NA")

    }

    if(species_filter == "TRUE"){

      dat <- preNCRMP_SCREAM_DRM_coral_demographics %>%
        # Set LAT and LON to the same # of digits - helps with grouping
        dplyr::mutate(LAT_DEGREES = sprintf("%0.5f", LAT_DEGREES),
                      LON_DEGREES = sprintf("%0.5f", LON_DEGREES)) %>%
        # Filter out Sand
        dplyr::filter(STRAT != "SAND_NA")

      # Subset by region and filter with region specific filter

      SEFCRI <- dat %>%
        dplyr::filter(REGION == "SEFCRI",
                      SPECIES_CD %in% SEFCRI_filter)

      FLK <- dat %>%
        dplyr::filter(REGION == "FLK",
                      SPECIES_CD %in% FLK_filter)

      Tort <- dat %>%
        dplyr::filter(REGION == "Tortugas",
                      SPECIES_CD %in% Tort_filter)


      dat <- dplyr::bind_rows(SEFCRI, FLK, Tort)

    }

  }


  if(project == "DRM"){

    if(species_filter == "FALSE"||
       species_filter == "NULL"){

      # Load data

      dat <- preNCRMP_SCREAM_DRM_coral_demographics %>%
        # Set LAT and LON to the same # of digits - helps with grouping
        dplyr::mutate(LAT_DEGREES = sprintf("%0.5f", LAT_DEGREES),
                      LON_DEGREES = sprintf("%0.5f", LON_DEGREES)) %>%
        # Filter out Sand
        dplyr::filter(STRAT != "SAND_NA",
                      SURVEY == "DRM")

    }

    if(species_filter == "TRUE"){

      dat <- preNCRMP_SCREAM_DRM_coral_demographics %>%
        # Set LAT and LON to the same # of digits - helps with grouping
        dplyr::mutate(LAT_DEGREES = sprintf("%0.5f", LAT_DEGREES),
                      LON_DEGREES = sprintf("%0.5f", LON_DEGREES)) %>%
        # Filter out Sand
        dplyr::filter(STRAT != "SAND_NA",
                      SURVEY == "DRM")

      # Subset by region and filter with region specific filter

      SEFCRI <- dat %>%
        dplyr::filter(REGION == "SEFCRI",
                      SPECIES_CD %in% SEFCRI_filter)

      FLK <- dat %>%
        dplyr::filter(REGION == "FLK",
                      SPECIES_CD %in% FLK_filter)

      Tort <- dat %>%
        dplyr::filter(REGION == "Tortugas",
                      SPECIES_CD %in% Tort_filter)


      dat <- dplyr::bind_rows(SEFCRI, FLK, Tort)

    }

  }


  # Calculate mean mortality

  # Old mortality

  old_mortality_site <- dat %>%
    dplyr::filter(N == 1,
                  SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                  OLD_MORT != "NA",
                  OLD_MORT <= 100) %>%
    dplyr::mutate(PROT = as.factor(PROT)) %>%
    # when data is two stage (two transects or more) calculate transect mean before site mean.
    dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>%
    dplyr::summarise(transect_mort = mean(OLD_MORT)) %>%
    dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>%
    dplyr::summarise(avsitemort = mean(transect_mort)) %>%
    dplyr::mutate(MORT_TYPE = "Old") %>%
    dplyr::ungroup() %>%
    # update some column classes to make them compatible with current NCRMP data
    dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                  LAT_DEGREES = as.numeric(LAT_DEGREES),
                  LON_DEGREES = as.numeric(LON_DEGREES),
                  PROT = as.factor(PROT))


  # Apply weighting scheme and calculate strata and regional means

  tmp  <- DRM_SCREAM_make_weighted_demo_data(inputdata = old_mortality_site, datatype = "mortality")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])

  old_mortality_strata <- mortality_strata %>%
    dplyr::mutate(MORT_TYPE = "Old")

  Domain_est_old_mort <- Domain_est %>%
    dplyr::mutate(MORT_TYPE = "Old")


  # Recent mortality


  recent_mortality_site <- dat %>%
    dplyr::filter(N == 1,
                  SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                  RECENT_MORT != "NA",
                  RECENT_MORT <= 100) %>%
    dplyr::mutate(PROT = as.factor(PROT)) %>%
    # when data is two stage (two transects or more) calculate transect mean before site mean.
    dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>% #No need to include region, will be added from ntot in wh. function
    dplyr::summarise(transect_mort = mean(RECENT_MORT)) %>%
    dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>%
    dplyr::summarise(avsitemort = mean(transect_mort)) %>%
    dplyr::mutate(MORT_TYPE = "Recent") %>%
    dplyr::ungroup() %>%
    # update some column classes to make them compatible with current NCRMP data
    dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                  LAT_DEGREES = as.numeric(LAT_DEGREES),
                  LON_DEGREES = as.numeric(LON_DEGREES),
                  PROT = as.factor(PROT))


  # Apply weighting scheme and calculate strata and regional means

  tmp  <- DRM_SCREAM_make_weighted_demo_data(inputdata = recent_mortality_site, datatype = "mortality")

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
    "old_mortality_strata" = old_mortality_strata,
    "rec_mortality_strata" = rec_mortality_strata,
    "Domain_est_old_mort" = Domain_est_old_mort,
    "Domain_est_rec_mort" = Domain_est_rec_mort)

  return(output)

}
