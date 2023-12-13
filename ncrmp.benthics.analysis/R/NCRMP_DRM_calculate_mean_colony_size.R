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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Jan 2023


##############################################################################################################################

#' Creates mean colony size summary dataframes
#'
#' Calculates mean colony size (3D, 2D, and maximum diameter)
#' by species and site, by site, by strata, and regional
#' estimates. Also calculates species-specific regional estimates.
#' Regional estimates of size are weighted by the
#' number of grid cells of a stratum in the sample frame. Note calculations
#' for 3D and 2D means aver not available for NCRMP+DRM data because DRM
#' does not collect perpendicular diameter.
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined ("NCRMP_DRM").
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param species_filter An optional string indicating whether to filter to a subset of species
#' @return A list of dataframes including at most 1) mean size by species for each
#' site, 2) mean size by site, 3) mean 2D size by strata, 4) mean 3D size by strata,
#' 5) mean 2D size by species and strata, 6) mean 3D size by species and strata,
#' 7) mean maximum diameter by species and strata, 8) regional estimates for size,
#' and 9), regional estimates for maximum diameter by species.
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
      dplyr::mutate(total_mort = OLD_MORT + RECENT_MORT) %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    total_mort < 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2) - ((4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)*(OLD_MORT+RECENT_MORT)/100)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d, na.rm = T),
                       avg_cm3 = mean(size_3d, na.rm=T),
                       var_cm2 = var(size_2d, na.rm=T),
                       var_cm3 = var(size_3d, na.rm=T),
                       avg_maxdiam = mean(MAX_DIAMETER, na.rm = T),
                       var_maxdiam = var(MAX_DIAMETER, na.rm=T),
                       n_colonies = length(unique(size_3d)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()

    size_site_1stage <- dat_1stage %>%
      dplyr::mutate(total_mort = OLD_MORT + RECENT_MORT) %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    total_mort < 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2) - ((4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)*(OLD_MORT+RECENT_MORT)/100)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d, na.rm=T),
                       avg_cm3 = mean(size_3d, na.rm=T),
                       var_cm2 = var(size_2d, na.rm=T),
                       var_cm3 = var(size_3d, na.rm=T),
                       avg_maxdiam = mean(MAX_DIAMETER, na.rm = T),
                       var_maxdiam = var(MAX_DIAMETER, na.rm=T),
                       n_colonies = length(unique(size_3d)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()


    size_species_2stage <- dat_2stage %>%
      dplyr::mutate(total_mort = OLD_MORT + RECENT_MORT) %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    total_mort < 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d, na.rm=T),
                       avg_cm3 = mean(size_3d, na.rm=T),
                       var_cm2 = var(size_2d, na.rm=T),
                       var_cm3 = var(size_3d, na.rm=T),
                       avg_maxdiam = mean(MAX_DIAMETER, na.rm = T),
                       var_maxdiam = var(MAX_DIAMETER, na.rm=T),
                       n_colonies = length(unique(size_3d)), .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(avg_cm2 = mean(avg_cm2, na.rm=T),
                       avg_cm3 = mean(avg_cm3, na.rm=T),
                       var_cm2 = var(avg_cm2, na.rm=T),
                       var_cm3 = var(avg_cm3, na.rm=T),
                       avg_maxdiam = mean(avg_maxdiam, na.rm = T),
                       var_maxdiam = var(avg_maxdiam, na.rm=T),
                       n_colonies = length(unique(avg_cm3)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()


    size_site_2stage <- dat_2stage %>%
      dplyr::mutate(total_mort = OLD_MORT + RECENT_MORT) %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    total_mort < 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d, na.rm=T),
                       avg_cm3 = mean(size_3d, na.rm=T),
                       var_cm2 = var(size_2d, na.rm=T),
                       var_cm3 = var(size_3d, na.rm=T),
                       avg_maxdiam = mean(MAX_DIAMETER, na.rm = T),
                       var_maxdiam = var(MAX_DIAMETER, na.rm=T),
                       n_colonies = length(unique(size_3d)), .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED) %>%
      dplyr::summarise(avg_cm2 = mean(avg_cm2, na.rm=T),
                       avg_cm3 = mean(avg_cm3, na.rm=T),
                       var_cm2 = var(avg_cm2, na.rm=T),
                       var_cm3 = var(avg_cm3, na.rm=T),
                       avg_maxdiam = mean(avg_maxdiam, na.rm = T),
                       var_maxdiam = var(avg_maxdiam, na.rm=T),
                       n_colonies = length(unique(avg_cm3)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()

    size_species <- dplyr::bind_rows(size_species_1stage, size_species_2stage)

    size_site <- dplyr::bind_rows(size_site_1stage, size_site_2stage)

  } else {

    size_species <- dat_1stage %>%
      dplyr::mutate(total_mort = OLD_MORT + RECENT_MORT) %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    total_mort < 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
        # equation for surface area of half of an ellipsoid
        size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d),
                       avg_cm3 = mean(size_3d),
                       var_cm2 = var(size_2d),
                       var_cm3 = var(size_3d),
                       avg_maxdiam = mean(MAX_DIAMETER, na.rm = T),
                       var_maxdiam = var(MAX_DIAMETER, na.rm=T),
                       n_colonies = length(unique(size_3d)),
                       DEPTH_M = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup()


    size_site <- dat_1stage %>%
      dplyr::mutate(total_mort = OLD_MORT + RECENT_MORT) %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0,
                    total_mort < 100) %>%

      dplyr::mutate(size_2d = ((MAX_DIAMETER*PERP_DIAMETER)/2)-(((MAX_DIAMETER*PERP_DIAMETER)/2)*(OLD_MORT+RECENT_MORT)/100),
                    # equation for surface area of half of an ellipsoid
                    size_3d = (4*pi*(((((MAX_DIAMETER/2)*(PERP_DIAMETER/2)) + ((MAX_DIAMETER/2)*(HEIGHT/2)) + ((MAX_DIAMETER/2*(HEIGHT/2))))/3)^1/p)/2)) %>%

      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PROT, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, METERS_COMPLETED) %>%
      dplyr::summarise(avg_cm2 = mean(size_2d, na.rm=T),
                       avg_cm3 = mean(size_3d, na.rm=T),
                       var_cm2 = var(size_2d, na.rm=T),
                       var_cm3 = var(size_3d, na.rm=T),
                       avg_maxdiam = mean(MAX_DIAMETER, na.rm = T),
                       var_maxdiam = var(MAX_DIAMETER, na.rm=T),
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

  # rename exports?


  # Run through the weighting function by species
  tmp2  <- NCRMP_make_weighted_demo_data(project,
                                         inputdata = size_species,
                                         region,
                                         datatype = "size_species",
                                         species_filter = species_filter)

  for(k in 1:length(tmp2))assign(names(tmp2)[k], tmp2[[k]])


  ################
  # Export
  ################



    # Create list to export
  if(project == "NCRMP"){
    output <- list(
      "size_species" = size_species,
      "size_site" = size_site,
      "size_est_cm2_strata" = size_est_cm2_strata,
      "size_est_cm3_strata" = size_est_cm3_strata,
      "size_est_cm2_strata_species" = size_est_cm2_strata_species,
      "size_est_cm3_strata_species" = size_est_cm3_strata_species,
      "size_est_maxdiam_strata_species" = size_est_maxdiam_strata_species,
      "Domain_est_species" = Domain_est_species,
      "Domain_est" = Domain_est)
  }
  if(project == "NCRMP_DRM"){
    output <- list(
      "size_species" = size_species,
      "size_site" = size_site,
      "size_est_maxdiam_strata_species" = size_est_maxdiam_strata_species,
      "Domain_est_species" = Domain_est_species,
      "Domain_est" = Domain_est)
  }



  return(output)

}




