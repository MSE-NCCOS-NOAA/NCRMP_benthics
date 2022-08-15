## Function to calculate mean colony size for combined NCRMP and DRM  data

# Purpose:
# creates csv files with mean colony size


## Tag: data analysis


# outputs created in this file --------------
# mean colony size_site
# mean colony size_strata,
# Domain_est


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Nov 2021


##############################################################################################################################

#' Creates mean colony size summary dataframes
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param region A string indicating the region
#' @param species_filter A string indicating whether to filter to a subset of species
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_DRM_calculate_mean_colony_size <- function(project = "NULL", region, species_filter = "NULL"){

  tmp <- load_NCRMP_DRM_demo_data(project = project,
                                  region = region,
                                  species_filter = species_filter)

  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  p = 1.6

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    size_species_1stage <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    (OLD_MORT + RECENT_MORT) != 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2) - ((4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)*(OLD_MORT+RECENT_MORT)/100)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d),
                       avg_cm3 = mean(size_3d),
                       var_cm2 = var(size_2d),
                       var_cm3 = var(size_3d),
                       n_colonies = length(unique(size_3d)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()

    size_site_1stage <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    (OLD_MORT + RECENT_MORT) != 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2) - ((4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)*(OLD_MORT+RECENT_MORT)/100)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d),
                       avg_cm3 = mean(size_3d),
                       var_cm2 = var(size_2d),
                       var_cm3 = var(size_3d),
                       n_colonies = length(unique(size_3d)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()


    size_species_2stage <- dat_2stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    (OLD_MORT + RECENT_MORT) != 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d),
                       avg_cm3 = mean(size_3d),
                       var_cm2 = var(size_2d),
                       var_cm3 = var(size_3d),
                       n_colonies = length(unique(size_3d)), .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(avg_cm2 = mean(avg_cm2),
                       avg_cm3 = mean(avg_cm3),
                       var_cm2 = var(avg_cm2),
                       var_cm3 = var(avg_cm3),
                       n_colonies = length(unique(avg_cm3)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()


    size_site_2stage <- dat_2stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    (OLD_MORT + RECENT_MORT) != 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d),
                       avg_cm3 = mean(size_3d),
                       var_cm2 = var(size_2d),
                       var_cm3 = var(size_3d),
                       n_colonies = length(unique(size_3d)), .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED) %>%
      dplyr::summarise(avg_cm2 = mean(avg_cm2),
                       avg_cm3 = mean(avg_cm3),
                       var_cm2 = var(avg_cm2),
                       var_cm3 = var(avg_cm3),
                       n_colonies = length(unique(avg_cm3)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()

    size_species <- dplyr::bind_rows(size_species_1stage, size_species_2stage)

    size_site <- dplyr::bind_rows(size_site_1stage, size_site_2stage)

  } else {

    size_species <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    (OLD_MORT + RECENT_MORT) != 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
        # equation for surface area of half of an ellipsoid
        size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d),
                       avg_cm3 = mean(size_3d),
                       var_cm2 = var(size_2d),
                       var_cm3 = var(size_3d),
                       n_colonies = length(unique(size_3d)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()


    size_site <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    (OLD_MORT + RECENT_MORT) != 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d),
                       avg_cm3 = mean(size_3d),
                       var_cm2 = var(size_2d),
                       var_cm3 = var(size_3d),
                       n_colonies = length(unique(size_3d)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()


  }




  # Run through the weighting function
  tmp  <- NCRMP_make_weighted_demo_data(project,
                                        inputdata = size_site,
                                        region,
                                        datatype = "size",
                                        species_filter = species_filter)




  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  ################
  # Export
  ################



    # Create list to export
    output <- list(
      "size_species" = size_species,
      "size_site" = size_site,
      "size_est_cm2_strata" = size_est_cm2_strata,
      "size_est_cm3_strata" = size_est_cm3_strata,
      "Domain_est" = Domain_est)



  return(output)

}




