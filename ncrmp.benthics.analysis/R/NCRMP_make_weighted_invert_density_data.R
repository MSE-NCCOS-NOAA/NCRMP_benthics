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
# Last update: Nov 2018


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

    ntot14 <- FL_2016_NTOT %>%
      # Filter to region of interest
      dplyr::filter(REGION == "SEFCRI") %>%
      # Create a STRAT column
      dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
      # Filter out strata not sampled that year
      dplyr::filter(STRAT == "MIDR1" | STRAT == "MIDR0") %>%
      # Create analysis strata column and change the sampling year to match the data
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    YEAR = 2014) %>%
      # Calculate total number of grid cells, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))


    ntot16 <- FL_2016_NTOT %>%
      dplyr::filter(REGION == "SEFCRI") %>%
      dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    # Combine NTOT files
    ntot <- rbind(ntot16, ntot14)

  }


  if(region == "FLK") {

    # Filter NTOT to only strata sampled that year - this is done manually for NCRMP only for now

    ntot16 <- FL_2016_NTOT %>%
      dplyr::filter(REGION == "FL KEYS") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::mutate(REGION = "FLK",
                    ngrtot = sum(NTOT))

    ntot14 <- FL_2016_NTOT %>%
      # Subset by region of interest, filter out strata not sampled
      dplyr::filter(REGION == "FL KEYS",
                    STRAT != "FDLR") %>%
      # Rename region to current NCRMP code, add Rugosity code and create ANALYSIS STRATUM column
      dplyr::mutate(YEAR = 2014,
                    REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    # Combine NTOT files
    ntot <- rbind(ntot16, ntot14)

  }

  if(region == "Tortugas") {

  ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0")%>%  # Not sampled in 2016
        dplyr::mutate(ngrtot = sum(NTOT),
                      REGION = "Tortugas")

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT",
                      STRAT != "SPGR_LR") %>% # Not sampled in 2014
        dplyr::mutate(YEAR = 2014,
                      REGION = "Tortugas") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot16, ntot14)

  }

  if(region == "STTSTJ"){

    ntot13 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STTSTJ",
                    STRAT != "HARD_SHLW") %>% # Hard shlw was not sampled in 2013
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2013,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot15 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2015,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot17 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot <- rbind(ntot13, ntot15, ntot17)


  }

  if(region == "STX"){

    ntot15 <- USVI_2017_NTOT %>%
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

    ntot17 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STX",
                    STRAT != "HARD_SHLW") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot <- rbind(ntot15, ntot17)

  }

  if(region == "PRICO"){

    ntot14 <- PRICO_2016_NTOT %>%
      dplyr::filter(STRAT != "HARD_DEEP", # Hard shlw was not sampled in 2014
                    STRAT != "HARD_SHLW") %>% # Hard deep was not sampled in 2014
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2014,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot16 <- PRICO_2016_NTOT %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot <- rbind(ntot14, ntot16)

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
  unwh_invert_strata <- dens_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avden, svar, std)


  ## Domain Estimates
  Domain_est <- dens_est %>%
    dplyr::group_by(REGION, YEAR) %>%
    dplyr::summarise(avDen = sum(whavden),
                     var = sum(whsvar, na.rm = T),
                     std = sqrt(var),
                     ngrtot = sum(NTOT) )


  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "unwh_invert_strata" = unwh_invert_strata,
    "Domain_est" = Domain_est

  )



}
