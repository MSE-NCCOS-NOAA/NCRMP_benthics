
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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams, Sturm
# Last update: Sep 2024


##############################################################################################################################

#' Calculate weighted mean coral density & CVs
#'
#' Calculates regional weighted mean coral density and coefficient of variation (CV), by species,
#' from coral demographic data. NCRMP utilizes a stratified random
#' sampling design. Regional estimates of density are weighted by the number of grid cells of a stratum
#' in the sample frame. Function calculates strata means, weighted strata means,
#' and weighted regional estimates for various measures from coral demographic data.
#' Also calculates occurrence of each species in each year.
#' Support function called by [NCRMP_colony_density_CV_and_occurrence()].
#'
#'
#'
#'
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param sppdens A dataframe of coral density by species at each individual site in given region.
#' @param project A string indicating the project, "NCRMP", "NCRMP_DRM", or "MIR".
#' @return A dataframe of regional weighted means of coral density and CV.
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

  ################UPDATE ALLOCATION SPECIES################################
  ## Define allocation species for each region
  allocation_species_list <- list(
    STTSTJ = c("Colpophyllia natans", "Diploria labyrinthiformis", "Madracis decactis", "Meandrina meandrites", "Montastraea cavernosa", "Orbicella annularis", "Orbicella faveolata", "Pseudodiploria strigosa", "Siderastrea siderea"),
    STX = c("Colpophyllia natans", "Dichocoenia stokesii", "Madracis decactis", "Montastraea cavernosa", "Orbicella annularis", "Orbicella franksi", "Pseudodiploria strigosa"),
    PRICO = c("Colpophyllia natans", "Diploria labyrinthiformis", "Madracis decactis", "Meandrina meandrites", "Montastraea cavernosa", "Orbicella annularis", "Orbicella faveolata", "Orbicella franksi", "Pseudodiploria strigosa"),
    FLK = c("Colpophyllia natans", "Montastraea cavernosa", "Orbicella faveolata", "Porites astreoides", "Siderastrea siderea", "Solenastrea bournoni"),
    Tortugas = c("Colpophyllia natans", "Montastraea cavernosa", "Orbicella faveolata", "Porites astreoides", "Orbicella franksi", "Stephanocoenia intersepta"),
    SEFCRI = c("Acropora cervicornis", "Dichocoenia stokesii", "Montastraea cavernosa", "Porites astreoides", "Pseudodiploria strigosa", "Siderastrea siderea")
  )


  # Get the list of allocation species for the given region
  coral_species <- allocation_species_list[[region]]

  ## coral data processing
  species_dens_wide <- sppdens %>%
    # remove abundance column, THIS WAS COMMENTED OUT BUT MAYBE NEEDED?
    #dplyr::select(-ABUNDANCE) %>% # abundance was eliminated in Dec. 2023 when we updated to include 0's in all density calculations for all species ever observed in the region
    # filter out spp columns
    dplyr::filter(
      SPECIES_NAME != "Orbicella spp",
      SPECIES_NAME != "Agaricia spp",
      SPECIES_NAME != "Undaria spp",
      SPECIES_NAME != "Siderastrea spp",
      SPECIES_NAME != "Orbicella annularis species complex",
      SPECIES_NAME != "Other coral") %>%
    # Replace "Isophyllastrea rigida" with "Isophyllia rigida"
    dplyr::mutate(SPECIES_NAME = dplyr::recode(SPECIES_NAME,
                                               "Isophyllastrea rigida" = "Isophyllia rigida")) %>%
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
      dplyr::group_by(REGION, YEAR, SPECIES_CD, ANALYSIS_STRATUM) %>% # HABITAT was previously included here, which is WRONG for GOM data...
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
      dplyr::group_by(REGION, YEAR, SPECIES_CD, ANALYSIS_STRATUM) %>% # HABITAT was previously included here, which is WRONG for FL data...
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


  # Add the allocation_species column
  region_means <- region_means %>%
    dplyr::mutate(allocation_species = ifelse(SPECIES_CD %in% coral_species, "Y", "N"))


  return(region_means)
}
