
## Function to calculate colony density and cv by species

# Purpose:
# creates csv files with weighted mean density & CVs.


## Tag: data analysis


# outputs created in this file --------------
# region_means
#
#


# CallS:
# sppdens

# output gets called by:
# NCRMP_colony_density_CV_and_occurrence
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Feb 2023


##############################################################################################################################

#' Calculate weighted mean density & CVs
#'
#'
#'
#'
#' @param region A string indicating the region
#' @param sppden A dataframe
#' @param project A string indicating the project, "NCRMP", "DRM", or "MIR"
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "n"
#' @export
#'
#'


# function to Calculate weights based on the most recent sampling grid
NCRMP_make_weighted_density_CV_data <- function(region, sppdens, project = "NULL") {

  ntot <- load_NTOT(region = region,
                    inputdata = sppdens,
                    project = project)

  ## coral data processing
  species_dens_wide <- sppdens %>%
    # select for most recent year
    # remove abundance column
    dplyr::select(-ABUNDANCE) %>%
    # filter out spp columns
    dplyr::filter(
      SPECIES_NAME != "Orbicella spp",
      SPECIES_NAME != "Agaricia spp",
      SPECIES_NAME != "Undaria spp",
      SPECIES_NAME != "Siderastrea spp",
      SPECIES_NAME != "Orbicella annularis species complex",
      SPECIES_NAME != "Other coral") %>%
    # add in zeros for species that didn't occur per site. Easiest to flip to wide format (1 row per site) for this
    dplyr::select(-SPECIES_CD) %>%
    tidyr::spread(., SPECIES_NAME, DENSITY,
                  fill = 0)

  if(project == "NCRMP_DRM"){

      species_dens_long <- tidyr::gather(species_dens_wide, SPECIES_CD, dens, 12:ncol(species_dens_wide))

  }
  if(project == "NCRMP"){

    if(region == "SEFCRI" || region == "Tortugas"){
      species_dens_long <- tidyr::gather(species_dens_wide, SPECIES_CD, dens, 12:ncol(species_dens_wide))

    } else{

      species_dens_long <- tidyr::gather(species_dens_wide, SPECIES_CD, dens, 14:ncol(species_dens_wide))


    }
  }

  if(region=="STX" || region=="STTSTJ" || region == "PRICO" || region == "GOM") {

    ## Calculate weighted means and cv
    # strata_means
    strata_means <- species_dens_long %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(REGION, YEAR, SPECIES_CD, ANALYSIS_STRATUM, HABITAT_CD) %>%
      # sample variance of density in stratum
      dplyr::summarize(mean = mean(dens),
                       svar = var(dens),
                       N_DEMO_CELLS_SAMPLED = length(PRIMARY_SAMPLE_UNIT),
                       .groups = "keep") %>%
      # replace zeros with very small number
      dplyr::mutate(svar=dplyr::case_when(svar==0 ~ 0.00000001,
                                          TRUE ~ svar)) %>%
      #variance of mean density in stratum
      dplyr::mutate(Var = svar/N_DEMO_CELLS_SAMPLED,
                    # std dev of density in stratum
                    std = sqrt(svar),
                    #SE of the mean density in stratum
                    SE = sqrt(Var),
                    CV_perc = (SE/mean)*100,
                    CV = (SE/mean))

  } else {

    strata_means <- species_dens_long %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(REGION, YEAR, SPECIES_CD, ANALYSIS_STRATUM, HABITAT_CD) %>%
      # sample variance of density in stratum
      dplyr::summarize(mean = mean(dens),
                       svar = var(dens),
                       N_DEMO_CELLS_SAMPLED = length(PRIMARY_SAMPLE_UNIT),
                       .groups = "keep") %>%
      # replace zeros with very small number
      dplyr::mutate(svar=dplyr::case_when(svar==0 ~ 0.00000001,
                                          TRUE ~ svar)) %>%
      #variance of mean density in stratum
      dplyr::mutate(Var = svar/N_DEMO_CELLS_SAMPLED,
                    # std dev of density in stratum
                    std = sqrt(svar),
                    #SE of the mean density in stratum
                    SE = sqrt(Var),
                    CV_perc = (SE/mean)*100,
                    CV = (SE/mean))

  }


  # region/population means
  region_means <- strata_means %>%
    dplyr::left_join(ntot) %>%
    dplyr::mutate(wh_mean = wh*mean,
                  wh_var = wh^2*Var) %>%
    dplyr::group_by(REGION, YEAR, SPECIES_CD) %>%
    dplyr::summarize(avDen = sum(wh_mean),
                     Var = sum(wh_var,
                               na.rm = TRUE),
                     SE = sqrt(Var),
                     CV_perc = (SE/avDen)*100,
                     CV = (SE/avDen),
                     n_sites = sum(N_DEMO_CELLS_SAMPLED),
                     .groups = "keep") %>%
    dplyr::mutate(ANALYSIS_STRATUM = "ALL_STRAT",
                  DEPTH_STRAT = "ALL_DEPTHS",
                  HABITAT_CD = "ALL_HABS") %>%  #add svar variable
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_CD, avDen, Var, SE, CV_perc, CV, n_sites, HABITAT_CD, DEPTH_STRAT)

  # Calculate n sites present for each species
  region_sites <- species_dens_long %>%
    # remove sites where species not present
    dplyr::group_by(REGION, YEAR) %>%
    dplyr::summarize(n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
                     .groups = "keep") %>%
    dplyr::ungroup()

  region_presence <- species_dens_long %>%
    dplyr::filter(dens > 0) %>%
    dplyr::group_by(REGION, YEAR, SPECIES_CD) %>%
    dplyr::summarize(n_sites_present = length(dens),
                     .groups = "keep") %>%
    dplyr::ungroup()

  region_means <- dplyr::left_join(region_means, region_presence) %>%
    dplyr::mutate(occurrence = n_sites_present/n_sites) %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_CD, avDen, Var, SE, CV_perc, CV, occurrence, n_sites_present, n_sites, HABITAT_CD, DEPTH_STRAT) %>%
    # drop rows with NA as species code
    tidyr::drop_na(SPECIES_CD) %>%
    # exclude rows with -spp in name
    dplyr::filter(., !grepl('spp', SPECIES_CD)) %>%
    dplyr::mutate(n_sites_present = tidyr::replace_na(n_sites_present, 0))

  return(region_means)
}
