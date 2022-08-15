
## Function to calculate weighted coral cover by species

# Purpose:
# creates csv files with weighted mean density & CVs.


## Tag: data analysis


# outputs created in this file --------------
# region_means
# strata_means
#


# CallS:
# analysis ready data

# output gets called by:
# NCRMP_colony_percent_cover
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Nov 2021


##############################################################################################################################

#' Calculate weighted mean density & CVs
#'
#'
#'
#'
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "n"
#' @export
#'
#'


# function to Calculate weights based on the most recent sampling grid
NCRMP_make_weighted_species_coral_cover_data <- function(region, sppcvr) {


  ntot <- load_NTOT(region = region,
                    inputdata = sppcvr,
                    project = "NCRMP")



  ## coral data processing
  species_cvr_wide <- sppcvr %>%
    # select hard corals only
    dplyr::filter(cover_group == "HARD CORALS") %>%
    # filter out spp columns
    dplyr::filter(
      COVER_CAT_NAME != "Solenastrea spp",
      COVER_CAT_NAME != "Siderastrea spp",
      COVER_CAT_NAME != "Scolymia spp",
      COVER_CAT_NAME != "Agaricia spp",
      COVER_CAT_NAME != "Orbicella spp",
      COVER_CAT_NAME != "Madracis spp",
      COVER_CAT_NAME != "Other coral",
      COVER_CAT_NAME != "Isophyllia spp",
      COVER_CAT_NAME != "Porites spp",
      COVER_CAT_NAME != "Meandrina spp",
      COVER_CAT_NAME != "Pseudodiploria spp",
      COVER_CAT_NAME != "Orbicella annularis species complex") %>%
    # add in zeros for species that didn't occur per site. Easiest to flip to wide format ( 1 row per site) for this
    # dplyr::select(REGION, YEAR, PRIMARY_SAMPLE_UNIT, SPECIES_NAME, DENSITY) %>%
    dplyr::select(-COVER_CAT_CD) %>%
    tidyr::spread(., COVER_CAT_NAME, Percent_Cvr,
                  fill = 0)

  species_cvr_long <- tidyr::gather(species_cvr_wide, SPECIES_NAME, cvr, 17:ncol(species_cvr_wide))

  ## Calculate weighted means and cv
  # strata_means
  strata_means <- species_cvr_long %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, ANALYSIS_STRATUM, HABITAT_CD) %>%  # removed  DEPTH_STRAT, b/c it's in STRAT_ANALYSIS
    # sample variance of density in stratum
    dplyr::summarize(mean = mean(cvr),
                     svar = var(cvr),
                     N_LPI_CELLS_SAMPLED = length(PRIMARY_SAMPLE_UNIT),
                     .groups = "keep") %>%
    # replace zeros with very small number
    dplyr::mutate(svar=dplyr::case_when(svar==0 ~ 0.00000001,
                                        TRUE ~ svar)) %>%
    #variance of mean density in stratum
    dplyr::mutate(Var = svar/N_LPI_CELLS_SAMPLED,
                  # std dev of density in stratum
                  std = sqrt(svar),
                  #SE of the mean density in stratum
                  SE = sqrt(Var),
                  CV_perc = (SE/mean)*100,
                  CV = (SE/mean))


  # region/population means
  region_means <- strata_means %>%
    dplyr::left_join(ntot) %>%
    dplyr::mutate(wh_mean = wh*mean,
                  wh_var = wh^2*Var) %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME) %>%
    dplyr::summarize(avCvr = sum(wh_mean),
                     Var = sum(wh_var,
                               na.rm = TRUE),
                     SE = sqrt(Var),
                     CV_perc = (SE/avCvr)*100,
                     CV = (SE/avCvr),
                     n_sites = sum(N_LPI_CELLS_SAMPLED),
                     .groups = "keep") %>%
    dplyr::mutate(STRAT_ANALYSIS = "ALL_STRAT",
                  DEPTH_STRAT = "ALL_DEPTHS",
                  HABITAT_CD = "ALL_HABS") %>%  #add svar variable
    dplyr::select(REGION, YEAR, STRAT_ANALYSIS, SPECIES_NAME, avCvr, Var, SE, CV_perc, CV, n_sites, HABITAT_CD, DEPTH_STRAT)

  # Calculate n sites present for each species
  strata_presence <- species_cvr_long %>%
    # remove sites where species not present
    dplyr::filter(cvr > 0) %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME, ANALYSIS_STRATUM, HABITAT_CD) %>%
    dplyr::summarize(n_sites = length(cvr),
                     .groups = "keep") %>%
    dplyr::ungroup()

  region_presence <- strata_presence %>%
    dplyr::group_by(REGION, YEAR, SPECIES_NAME) %>%
    dplyr::summarize(n_sites_present = sum(n_sites),
                     .groups = "keep") %>%
    dplyr::ungroup()

  region_means <- dplyr::left_join(region_means, region_presence) %>%
    dplyr::select(REGION, YEAR, STRAT_ANALYSIS, SPECIES_NAME, avCvr, Var, SE, CV_perc, CV, n_sites_present, n_sites, HABITAT_CD, DEPTH_STRAT) %>%
    # drop rows with NA as species code
    tidyr::drop_na(SPECIES_NAME) %>%
    # exclude rows with -spp in name
    dplyr::filter(., !grepl('spp', SPECIES_NAME)) %>%
    dplyr::mutate(n_sites_present = tidyr::replace_na(n_sites_present, 0))




  ################
  # Export
  ################


  # Create list to export
  output <- list(
    "region_means" = region_means,
    "strata_means" = strata_means)

  return(output)

}
