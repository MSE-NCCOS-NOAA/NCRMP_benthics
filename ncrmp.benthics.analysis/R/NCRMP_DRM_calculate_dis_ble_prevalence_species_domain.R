## Function to calculate species domain estimates for disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data (FL only).

# Purpose:
# creates csv files with disease/bleaching prevalence by species,  region and year


## Tag: data analysis


# outputs created in this file --------------

# Species domain estimates

# CallS:
# NCRMP_REGION_YEARS_dis_ble_prev_species

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Sep 2021


##############################################################################################################################

#' Species domain estimates for disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'
NCRMP_DRM_calculate_dis_ble_prevalence_species_domain <- function(project, region){


  if(project == "NCRMP"){


    if(region == "STTSTJ"){

      # Load NTOTs
      ntot <- USVI_2019_NTOT %>%
        dplyr::filter(REGION == "STTSTJ") %>%
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT),
                         #.groups is experimental with dplyr
                         .groups = "keep") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                      ngrtot = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(wh = NTOT/ngrtot)


      # Load species/site level bleaching & disease data
      dat <- NCRMP_STTSTJ_2013_19_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               `ORB ANCX`="ORB SPE."))

    }

    if(region == "STX"){

      ntot <- USVI_2019_NTOT %>%
        dplyr::filter(REGION == "STX") %>%
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT),
                         #.groups is experimental with dplyr
                         .groups = "keep") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                      ngrtot = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(wh = NTOT/ngrtot)

      dat <- NCRMP_STX_2015_19_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               `ORB ANCX`="ORB SPE."))

    }

    if(region == "PRICO"){

      ntot <- PRICO_2019_NTOT %>%
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT),
                         #.groups is experimental with dplyr
                         .groups = "keep") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                      ngrtot = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(wh = NTOT/ngrtot)

      dat <- NCRMP_PRICO_2014_19_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               `ORB ANCX`="ORB SPE."))
    }

  }


  dat1 <- dat %>%
    dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
    dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD) %>% # Modify this line to changes analysis stratum
    dplyr::summarise(# compute average density
      avDprev = mean(DIS_PREV),
      avBprev = mean(BLE_PREV),
      # compute stratum variance
      svarD = var(DIS_PREV, na.rm = T),
      svarB = var(BLE_PREV, na.rm = T),
      # calculate N
      n_sites = length(PRIMARY_SAMPLE_UNIT),
      #.groups is experimental with dplyr
      .groups = "keep") %>%
    # convert 0 for stratum variance so that the sqrt is a small # but not a 0
    dplyr::mutate(svarD = dplyr::case_when(svarD == 0 ~ 0.00000001,
                                           TRUE ~ svarD)) %>%
    dplyr::mutate(stdD = sqrt(svarD))%>%
    # convert 0 for stratum variance so that the sqrt is a small # but not a 0
    dplyr::mutate(svarB = dplyr::case_when(svarB == 0 ~ 0.00000001,
                                           TRUE ~ svarB)) %>%
    dplyr::mutate(stdB = sqrt(svarB))

  dat2 <-dat1 %>%
    # Merge ntot with coral_est_spp
    dplyr::full_join(., ntot) %>%
    # stratum estimates
    dplyr::mutate(whavDprev = wh * avDprev,
                  whavBprev = wh * avBprev,
                  whsvarD = wh^2 * svarD,
                  whsvarB = wh^2 * svarB,
                  whstdD = wh * stdD,
                  whstdB = wh * stdB,
                  n_sites = tidyr::replace_na(n_sites, 0))  %>%
    dplyr::ungroup()

  ## Domain Estimates
  DomainEst <- dat2 %>%
    dplyr::group_by(REGION, YEAR, SPECIES_CD) %>%
    dplyr::summarise(avDisPrev = sum(whavDprev, na.rm = T), # This accounts for strata with 0 species of interest present
                     avBlePrev = sum(whavBprev, na.rm = T),
                     VarD = sum(whsvarD, na.rm = T),
                     VarB = sum(whsvarB, na.rm = T),# This accounts for strata with N = 1
                     SE_D=sqrt(VarD),
                     SE_B=sqrt(VarB),
                     n_sites = sum(n_sites),
                     n_strat = length(unique(ANALYSIS_STRATUM)),
                     ngrtot = sum(NTOT),
                     #.groups is experimental with dplyr
                     .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::left_join(., ncrmp_frrp_sppcodes %>%
                       dplyr::select(fl_ncrmp_code, species_name),
                     by = c('SPECIES_CD' = 'fl_ncrmp_code'))

  DomainEst_bl <- DomainEst %>%
    dplyr::select(REGION, YEAR, species_name, avBlePrev, SE_B) %>%
    dplyr::filter(avBlePrev > 0,
                  species_name != "Orbicella spp",
                  species_name != "Agaricia spp",
                  species_name != "Undaria spp",
                  species_name != "Madracis spp",
                  species_name != "Siderastrea spp")


  DomainEst_dis <- DomainEst %>%
    dplyr::select(REGION, YEAR, species_name, avDisPrev, SE_D) %>%
    dplyr::filter(avDisPrev > 0,
                  species_name != "Orbicella spp",
                  species_name != "Agaricia spp",
                  species_name != "Undaria spp",
                  species_name != "Siderastrea spp")



  ################
  # Export
  ################

  # Create list to export
  output <- list(
    'DomainEst_bl' = DomainEst_bl,
    "DomainEst_dis" = DomainEst_dis)

  return(output)




}
