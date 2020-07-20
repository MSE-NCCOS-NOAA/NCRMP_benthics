## Function to create a complete  calculate disease prevalence for combined SCREAM and DRM data from 1999 to 2013

# Purpose:
# creates csv files with disease prevalence by region


## Tag: data analysis


# outputs created in this file --------------
# disease_prev_site
# disease_prev_strata
# Domain estimates

# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Nov 2018


##############################################################################################################################

#' Creates species list, species richness and species diversity dataframes from NCRMP and FRRP data
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


DRM_SCREAM_calculate_disease_prevalence <- function(project = "NULL", species_filter = "NULL"){

  ### Create species filters

   FLK_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                  "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN",
                  "SID SIDE",
                  "POR PORI"
  )

  Tort_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                   "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN",
                   "SID SIDE",
                   "POR PORI"
  )

  SEFCRI_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                     "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN",
                     "SID SIDE",
                     "POR PORI"
  )

if(project == "DRM_SCREAM"||
     project == "NULL"){

  # Load data
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

  # Load data
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


  # Calulate site level disease prevalence


    disease_prev_site <- dat %>%
      dplyr::filter(N == 1,
                    DISEASE != "NA",
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = mean(Total_dis),
                       Total_col = mean(Total_col),
                       DIS_PREV = mean(DIS_PREV)) %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)) )





  tmp  <- DRM_SCREAM_make_weighted_demo_data(inputdata = disease_prev_site, datatype = "disease")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])





  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "disease_prev_site" = disease_prev_site,
    "unwh_dis_prev_strata" = unwh_dis_prev_strata,
    "Domain_est" = Domain_est)

  return(output)






}
