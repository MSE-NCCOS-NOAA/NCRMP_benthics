## Function to calculate weighted invert density by strata

# Purpose:
# support function to calculate weighted invert density data


## Tag: data analysis


# outputs created in this file --------------
# invert_density_site
# unwh_invert_strata
# Domain est

# Current weighting scheme:
# STRAT + PROT (FL)
# STRAT (Carib/GOM)

# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_invert_density.R
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Mar 2020


##############################################################################################################################

#' Creates weighted Invert density data
#'
#'
#'
#'
#' @param inputdata A dataframe
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_invert_density_data <- function(inputdata, region)
{

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")


  #### Read in ntot ####

  ## Florida
  # SE FL
  if(region == "SEFCRI") {

    ntot14 <- SEFL_2014_NTOT %>%
      dplyr::filter(STRAT == "MIDR1" | STRAT == "MIDR0") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))

    ntot16 <- SEFL_2016_NTOT %>%
      dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH2"~"NEAR1",
                                             STRAT == "PTDP0"~"OFFR0",
                                             STRAT == "PTDP1"~"OFFR1", TRUE ~ as.character(STRAT))) %>%
      dplyr::group_by(YEAR, REGION, STRAT, PROT, GRID_SIZE) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(STRAT != "RGDP1" & STRAT != "RGDP0") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))

    ntot18 <- SEFL_2018_NTOT %>%
      dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH2"~"NEAR1",
                                             STRAT == "PTDP0"~"OFFR0",
                                             STRAT == "PTDP1"~"OFFR1", TRUE ~ as.character(STRAT))) %>%
      dplyr::group_by(YEAR, REGION, STRAT, PROT, GRID_SIZE) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(STRAT != "RGDP1" & STRAT != "RGDP0") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))

    # Combine NTOT files
    ntot <- rbind(ntot16, ntot14, ntot18)

  }


  if(region == "FLK") {

    # Filter NTOT to only strata sampled that year - this is done manually for NCRMP only for now

    ntot18 <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "FLK") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot16 <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "FLK") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    YEAR = 2016) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot14 <- FL_2018_NTOT %>%
      # Subset by region of interest, filter out strata not sampled
      dplyr::filter(REGION == "FLK",
                    STRAT != "FDLR") %>%
      # Rename region to current NCRMP code, add Rugosity code and create ANALYSIS STRATUM column
      dplyr::mutate(YEAR = 2014,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    # Combine NTOT files
    ntot <- rbind(ntot16, ntot14, ntot18)

  }

  if(region == "Tortugas") {

    ntot18 <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "Tortugas",
                    STRAT != "SPGR_LR") %>% # Not sampled in 2018
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0",
                    ANALYSIS_STRATUM != "ISOL_LR / PROT = 1") %>% # Not sampled in 2018
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot16 <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "Tortugas") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    YEAR = 2016) %>%
      dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0")%>%  # Not sampled in 2016
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot14 <- FL_2018_NTOT %>%
      dplyr::filter(REGION == "Tortugas",
                    STRAT != "SPGR_LR") %>% # Not sampled in 2014
      dplyr::mutate(YEAR = 2014,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- rbind(ntot18, ntot16, ntot14)

  }

  if(region == "STTSTJ"){

    ntot13 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ",
                    STRAT != "HARD_SHLW") %>% # Hard shlw was not sampled in 2013
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2013,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot15 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2015,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot17 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2017,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot19 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2019,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot21 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()


    ntot <- dplyr::bind_rows(ntot13, ntot15, ntot17, ntot19, ntot21)


  }

  if(region == "STX"){

    ntot15 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STX",
                    STRAT != "HARD_SHLW", # Hard shlw was not sampled in 2015
                    STRAT != "HARD_DEEP") %>% # Hard deep was not sampled in 2015
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2015,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot17 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STX",
                    STRAT != "HARD_SHLW") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2017,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot19 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STX") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2019,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot21 <- USVI_2021_NTOT %>%
      dplyr::filter(REGION == "STX") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))


    ntot <- dplyr::bind_rows(ntot15, ntot17, ntot19, ntot21)

  }

  if(region == "PRICO"){

    ntot14 <- PRICO_2021_NTOT %>%
      dplyr::filter(STRAT != "HARD_DEEP", # Hard shlw was not sampled in 2014
                    STRAT != "HARD_SHLW") %>% # Hard deep was not sampled in 2014
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2014,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot16 <- PRICO_2021_NTOT %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2016,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot19 <- PRICO_2021_NTOT %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2019,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot21 <- PRICO_2021_NTOT %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot14, ntot16, ntot19, ntot21)

  }

  if(region == "GOM"){

    ntot13 <- FGBNMS_2018_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                    PROT = NA_character_,
                    YEAR = 2013) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
      dplyr::summarise(NTOT = sum(NTOT),
                       ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()



    ntot15 <- FGBNMS_2018_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                    PROT = NA_character_,
                    YEAR = 2015) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
      dplyr::summarise(NTOT = sum(NTOT),
                       ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot18 <- FGBNMS_2018_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                    PROT = NA_character_) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
      dplyr::summarise(NTOT = sum(NTOT),
                       ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot <- rbind(ntot13, ntot15, ntot18)

  }



  ntot <- ntot %>%
    # calculate percentage of NTOT within grid total
    dplyr::mutate(wh = NTOT/ngrtot) %>%
    dplyr::mutate(PROT = as.factor(PROT))


  if(region %in% FL)
  {
    #### Calculate avdns, svar, n and std at the strata + PROT level ####
    dens_est <- inputdata %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      # group by analysis level strata
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
      dplyr::summarise(
        # calculate mean density
        avden = mean(Diadema_dens),
        # calculate stratum variance
        svar = var(Diadema_dens),
        # calculate N
        n = length(Diadema_dens)) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(std = sqrt(svar))

    dens_est <- dens_est %>%
      # Merge ntot with coral_est_spp
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavden = wh * avden,
                    whsvar = wh^2 * svar,
                    whstd = wh * std,
                    n = tidyr::replace_na(n, 0))
  }


  if(region %in% GOM |
     region %in% Carib)
  {

    #### Calculate avdns, svar, n and std at the strata ####
    dens_est <- inputdata %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      # group by analysis level strata
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to change analysis stratum
      dplyr::summarise(
        # compute average density
        avden = mean(Diadema_dens),
        # compute stratum variance
        svar = var(Diadema_dens),
        # calculate N
        n = length(Diadema_dens)) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(std = sqrt(svar))

    dens_est <- dens_est %>%
      # Merge ntot with coral_est_spp
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavden = wh * avden,
                    whsvar = wh^2 * svar,
                    whstd = wh * std,
                    n = tidyr::replace_na(n, 0),
                    # Add the following to match FL format
                    PROT = NA,
                    RUG_CD = NA)
  }

  # Reformat output
  dens_est <- dens_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avden, svar, std, whavden, whsvar, whstd)

  # cover, unweighted by strata
  invert_strata <- dens_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avden, svar, std)


  ## Domain Estimates
  Domain_est <- dens_est %>%
    dplyr::group_by(REGION, YEAR) %>%
    dplyr::summarise(avDen = sum(whavden, na.rm = T),
                     var = sum(whsvar, na.rm = T),
                     std = sqrt(var),
                     ngrtot = sum(NTOT) )


  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "invert_strata" = invert_strata,
    "Domain_est" = Domain_est

  )



}
