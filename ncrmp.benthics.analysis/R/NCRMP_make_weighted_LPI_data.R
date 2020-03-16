## Function to calculate weighted percent cover by strata and protected area and then at the regional level

# Purpose:
# support function to calculate weighted percent cover data


## Tag: data analysis


# outputs created in this file --------------
# unwh_cover_strata
# domain estimates

# Current weighting scheme:
# STRAT + PROT


# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_cover.R
#

# NCRMP Caribbean Benthic analytics team: Groves and viehman
# Last update: Apr 2019


##############################################################################################################################

#' Creates weighted LPI data
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

NCRMP_make_weighted_LPI_data <- function(inputdata, region)
{

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")


  #### Read in ntot ####

  ## Florida
  # SE FL
  if(region == "SEFCRI"){

    if(year == 2014)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::filter(STRAT == "MIDR1" | STRAT == "MIDR0") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2014)
    }

    if(year == 2016)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2016)
    }

    if(year == 2018)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
    }

  }

  # FL Keys
  if(region == "FLK"){

    if(year == 2014)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK",
                      STRAT != "FDLR") %>% #Remove strat that were not sampled from ntot so they are not counted in ngrtot
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2014)

    }

    if(year == 2016)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(YEAR = 2016,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
    }

    if(year == 2018)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
    }

  }

  # Tortugas
  if(region == "Tortugas"){

    if(year == 2014)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas",
                      STRAT != "SPGR_LR") %>% # Not sampled in 2014
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2014)
    }

    if(year == 2016)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2016) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0")
    }


    if(year == 2018)
    {
      ntot <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas",
                      STRAT != "SPGR_LR") %>% # Not sampled in 2018
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0",
                      ANALYSIS_STRATUM != "ISOL_LR / PROT = 1")  # Not sampled in 2018
    }
  }

  ## USVI
  # St Thomas - St John
  if(region == "STTSTJ"){

    ntot13 <- USVI_2019_NTOT %>%
      dplyr::filter(REGION == "STTSTJ",
                    STRAT != "HARD_SHLW") %>% # Hard shlw was not sampled in 2013
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2013,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot15 <- USVI_2019_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2015,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot17 <- USVI_2019_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2017,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

      ntot19 <- USVI_2019_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()


    ntot <- rbind(ntot13, ntot15, ntot17, ntot19)
    }

  # St Croix
  if(region == "STX"){

  ntot15 <- USVI_2019_NTOT %>%
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

    ntot17 <- USVI_2019_NTOT %>%
      dplyr::filter(REGION == "STX",
                    STRAT != "HARD_SHLW") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(YEAR = 2017,
                    ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))

    ntot19 <- USVI_2019_NTOT %>%
      dplyr::filter(REGION == "STX") %>%
      dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                    PROT = NA_character_,
                    ngrtot = sum(NTOT))


    ntot <- rbind(ntot15, ntot17, ntot19)
  }

  ## Puerto Rico
  if(region == "PRICO")
  {
    if(year == 2014)
    {
      ntot <- PRICO_2016_NTOT %>%
        dplyr::filter(STRAT != "HARD_DEEP", # Hard shlw was not sampled in 2014
                      STRAT != "HARD_SHLW") %>% # Hard deep was not sampled in 2014
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(YEAR = 2014,
                      ANALYSIS_STRATUM = STRAT,
                      PROT = NA_character_)
    }

    if(year == 2016)
    {
      ntot <- PRICO_2016_NTOT %>%
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                      PROT = NA_character_) %>%
        dplyr::ungroup()
    }
  }

  ## Flower Garden Banks National Marine Sanctuary (GOM)
  if(region == "GOM"){

    if(year == 2013){

      ntot <- FGBNMS_2018_NTOT %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                      PROT = NA_character_,
                      YEAR = 2013) %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup()
    }

    if(year == 2015){
      ntot <- FGBNMS_2018_NTOT %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                      PROT = NA_character_,
                      YEAR = 2015) %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup()
    }

    if(year == 2018){
      ntot <- FGBNMS_2018_NTOT %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                      PROT = NA_character_) %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup()
    }


  }

  ntot <- ntot %>%
    dplyr::mutate(PROT = as.factor(PROT),
                  wh = NTOT/ngrtot)


  if(region %in% FL)
  {
    #### Calculate avcvr, svar, n and std at the strata + PROT level ####
    cover_est <- inputdata %>%
      # make avcvr
      dplyr::group_by(ANALYSIS_STRATUM, STRAT, PROT, cover_group) %>% # Modify this line to changes analysis stratum
      dplyr::summarise(
        # calculate mean cover
        avcvr = mean(Percent_Cvr),
        # calculate stratum variance
        svar = var(Percent_Cvr),
        # calculate N
        n = length(Percent_Cvr),
        # calculate mean stratum depth
        MIN_DEPTH = mean(MIN_DEPTH, na.rm = T),
        MAX_DEPTH = mean(MAX_DEPTH, na.rm = T),
        DEPTH_M = (MIN_DEPTH+MAX_DEPTH)/2) %>%

      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                    std = sqrt(svar), # std dev of density in stratum
                    SE=sqrt(Var), #SE of the mean density in stratum
                    CV_perc=(SE/avcvr)*100)

    cover_est <- cover_est %>%
      # Merge ntot with coral_est_spp
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavcvr = wh * avcvr,
                    whsvar = wh^2 * Var,
                    n = tidyr::replace_na(n, 0)) %>%
      dplyr::filter(cover_group != "NA") # This will remove any strata that was not sampled that year
  }


  if(region %in% GOM |
     region %in% Carib){

    #### Calculate avcvr, svar, n and std at the strata + PROT level ####
    cover_est <- inputdata %>%
      # make avcvr
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, cover_group) %>% # Modify this line to changes analysis stratum
      dplyr::summarise(
        # compute average cover
        avcvr = mean(Percent_Cvr),
        # compute stratum variance
        svar = var(Percent_Cvr),
        # calculate N
        n = length(unique(PRIMARY_SAMPLE_UNIT)),
        # calculate mean stratum depth
        MIN_DEPTH = mean(MIN_DEPTH, na.rm = T),
        MAX_DEPTH = mean(MAX_DEPTH, na.rm = T),
        DEPTH_M = (MIN_DEPTH+MAX_DEPTH)/2) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                    std = sqrt(svar), # std dev of density in stratum
                    SE=sqrt(Var), #SE of the mean density in stratum
                    CV_perc=(SE/avcvr)*100)

    cover_est <- cover_est %>%
      # Merge ntot with cover_est
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavcvr = wh * avcvr,
                    whsvar = wh^2 * Var,
                    n = tidyr::replace_na(n, 0),
                    # Add the following to match FL format
                    PROT = NA,
                    RUG_CD = NA)  %>%
      dplyr::ungroup()
  }

  # Reformat output

  # strata_means
  cover_strata <- cover_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, DEPTH_M, cover_group, n, avcvr, Var, SE, CV_perc) %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0),
                  RUG_CD = as.factor(RUG_CD)) %>%
    # replace inf values so we can add the strata means
    dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf)))


  ## Domain Estimates
  Domain_est <- cover_est %>%
    # replace inf values so we can add the strata means
    dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) %>%
    dplyr::group_by(REGION, YEAR, cover_group) %>%
    dplyr::summarise(avCvr = sum(whavcvr),
                     Var = sum(whsvar, na.rm = T),
                     SE=sqrt(Var),
                     CV_perc=(SE/avCvr)*100,
                     n_sites = sum(n),
                     n_strat = length(unique(ANALYSIS_STRATUM)),
                     ngrtot = sum(NTOT) )  %>%
    dplyr::ungroup()


  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "cover_strata" = cover_strata,
    "Domain_est" = Domain_est
  )



}
