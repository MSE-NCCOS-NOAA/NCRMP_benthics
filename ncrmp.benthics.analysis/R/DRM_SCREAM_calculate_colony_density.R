## Function to calculate colony density and colony size structure for combined SCREAM and DRM data
## from 1999 to 2013

# Purpose:
# creates csv files with colony density and colony size structure.
# Creates figures with size distribution curves.


## Tag: data analysis


# outputs created in this file --------------
# density_site
# unwh_density_strata,
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
#' @param project A string indicating the project, DRM or DRM and SCREAM combined
#' @param species_filter A boolean value indicating whether to filter to a subset of species
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


DRM_SCREAM_calculate_colony_density <- function(project = "NULL", species_filter = "NULL"){


  ### Create species filters

  FLK_filter <- c("ACR CERV", "MON CAVE", "ACR PALM","PSE STRI", "PSE CLIV",
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

  # Calculate coral density



  density_site <- dat %>%
    dplyr::ungroup() %>%
    dplyr::filter(N == 1,
                  SUB_REGION_NAME != "Marquesas",
                  SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
    dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
    dplyr::summarise(ABUNDANCE = sum(N)) %>%
    dplyr::mutate(DENSITY_transect = ABUNDANCE/METERS_COMPLETED) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
    dplyr::summarise(DENSITY = mean(DENSITY_transect)) %>%
    dplyr::ungroup() %>%
    # update some column classes to make them compatible with current NCRMP data
    dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT),
                  LAT_DEGREES = as.numeric(LAT_DEGREES),
                  LON_DEGREES = as.numeric(LON_DEGREES),
                  PROT = as.factor(PROT))


  tmp  <- DRM_SCREAM_make_weighted_demo_data(inputdata = density_site, datatype = "density")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])



  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "density_site" = density_site,
    "density_strata" = density_strata,
    "Domain_est" = Domain_est)

  return(output)

}



