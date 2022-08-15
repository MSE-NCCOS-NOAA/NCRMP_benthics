## Function to calculate colony density for combined NCRMP and DRM  data

# Purpose:
# creates csv files with colony density.


## Tag: data analysis


# outputs created in this file --------------
# density_site
# density_strata,
# Domain_est


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Mar 2022


##############################################################################################################################

#' Creates colony density summary dataframes
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


NCRMP_DRM_calculate_colony_density <- function(project = "NULL", region, species_filter = "NULL"){


  tmp <- load_NCRMP_DRM_demo_data(project = project,
                                  region = region,
                                  species_filter = species_filter)

  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  # Calculate coral density

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    dat1_1stage <- dat_1stage %>%
      dplyr::ungroup() %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::select(-ABUNDANCE, -METERS_COMPLETED)%>%
      dplyr::ungroup() %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    density_species_1stage <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    #N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::select(-METERS_COMPLETED) %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))


    dat1_2stage <- dat_2stage %>%
      dplyr::ungroup() %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N), .groups = "keep") %>%
      dplyr::mutate(DENSITY_transect = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(DENSITY = mean(DENSITY_transect),
                       MIN_DEPTH = mean(MIN_DEPTH),
                       MAX_DEPTH = mean(MAX_DEPTH), .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    density_species_2stage <- dat_2stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES)) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(ABUNDANCE = sum(N),.groups = "keep") %>%
      dplyr::mutate(DENSITY_transect = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(DENSITY = mean(DENSITY_transect),
                       ABUNDANCE = sum(ABUNDANCE), .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    density_site <- dplyr::bind_rows(dat1_1stage, dat1_2stage)

    density_species <- dplyr::bind_rows(density_species_1stage, density_species_2stage)

  } else {

    density_species <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, PROT, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(ABUNDANCE = sum(N), .groups = "keep") %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::select(-METERS_COMPLETED) %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))


    density_site <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N), .groups = "keep") %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::select(-ABUNDANCE, -METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))


  }




  # Run through the weighting function
  tmp  <- NCRMP_make_weighted_demo_data(project,
                                        inputdata = density_site,
                                        region,
                                        datatype = "density",
                                        species_filter = species_filter,
                                        species_data = density_species)




  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  ################
  # Export
  ################

  if(species_filter == "TRUE"){

    # Create list to export
    output <- list(
      "density_species" = density_species,
      "Species_regional_means_CV" = Species_regional_CV,
      "density_site" = density_site,
      "density_strata" = density_strata,
      "Domain_est" = Domain_est,
      'g1' = g1,
      'g2' = g2,
      'g.mid' = g.mid)

  } else {

    # Create list to export
    output <- list(
      "density_species" = density_species,
      "density_site" = density_site,
      "density_strata" = density_strata,
      "Domain_est" = Domain_est)



  }

  return(output)

}



