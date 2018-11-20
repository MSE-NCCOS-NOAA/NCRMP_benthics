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

# NCRMP Caribbean Benthic analytics team: Viehman, Groves, Bauer
# Last update: May 2018
# Current status: In prep


##############################################################################################################################

#' Creates weighted LPI data
#'
#'
#'
#'
#' @param inputdata A dataframe
#' @param region A string indicating the region
#' @param year A string indicating the year
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_LPI_data <- function(inputdata, region, year)
{

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")


  #### Read in ntot ####

  ## Florida
  # SE FL
  if(region == "SEFCRI")
  {

    if(year == 2014)
    {
      ntot <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::filter(STRAT == "MIDR1" | STRAT == "MIDR0") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2014)
    }

    if(year == 2016)
    {
      ntot <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
    }
  }

  # FL Keys
  if(region == "FLK")
  {
    if(year == 2014)
    {
      ntot <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS",
                      STRAT != "FDLR") %>% #Remove strat that were not sampled from ntot so they are not counted in ngrtot
        dplyr::mutate(REGION = "FLK",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2014)

    }

    if(year == 2016)
    {
      ntot <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS") %>%
        dplyr::mutate(REGION = "FLK",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
    }
  }

  # Tortugas
  if(region == "Tortugas")
  {
    if(year == 2014)
    {
      ntot <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT",
                      STRAT != "SPGR_LR") %>% # Not sampled in 2014
        dplyr::mutate(REGION = "Tortugas",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2014)
    }

    if(year == 2016)
    {
      ntot <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      REGION = "Tortugas") %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0")
    }
  }

  ## USVI
  # St Thomas - St John
  if(region == "STTSTJ"){

    if(year == 2013){

      ntot <- USVI_2017_NTOT %>%
        dplyr::filter(REGION == "STTSTJ",
                      STRAT != "HARD_SHLW") %>% # Hard shlw was not sampled in 2013
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(YEAR = 2013,
                      ANALYSIS_STRATUM = STRAT,
                      PROT = NA_character_)

    }

    if(year == 2015){

      ntot <- USVI_2017_NTOT %>%
        dplyr::filter(REGION == "STTSTJ") %>%
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(YEAR = 2015,
                      ANALYSIS_STRATUM = STRAT,
                      PROT = NA_character_)

    }

    if(year == 2017){

      ntot <- USVI_2017_NTOT %>%
        dplyr::filter(REGION == "STTSTJ") %>%
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                      PROT = NA_character_,
                      ngrtot = sum(NTOT))

    }

  }
  # St Croix
  if(region == "STX")
  {
    if(year == 2015){

      ntot <- USVI_2017_NTOT %>%
        dplyr::filter(REGION == "STX",
                      STRAT != "HARD_SHLW", # Hard shlw was not sampled in 2015
                      STRAT != "HARD_DEEP") %>% # Hard deep was not sampled in 2015
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(YEAR = 2015,
                      ANALYSIS_STRATUM = STRAT,
                      PROT = NA_character_)
    }

    if(year == 2017){

      ntot <- USVI_2017_NTOT %>%
        dplyr::filter(REGION == "STX",
                      STRAT != "HARD_SHLW") %>%
        dplyr::group_by(REGION, YEAR, STRAT, HABITAT_CD, DEPTH_STRAT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                      PROT = NA_character_)
    }
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

      ntot <- FGBNMS_2015_NTOT %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                      PROT = NA_character_,
                      REGION = "GOM",
                      YEAR = 2013) %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup()
    }

    if(year == 2015){
      ntot <- FGBNMS_2015_NTOT %>%
        dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                      PROT = NA_character_,
                      REGION = "GOM") %>%
        dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup()
    }

  }

  ntot <- ntot %>%
    dplyr::mutate(ngrtot = sum(NTOT),
                  PROT = as.factor(PROT))  %>%
    dplyr::mutate(wh = NTOT/ngrtot)


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
        n = sum(n),
        # calculate mean stratum depth
        MIN_DEPTH = mean(MIN_DEPTH, na.rm = T),
        MAX_DEPTH = mean(MAX_DEPTH, na.rm = T),
        DEPTH_M = (MIN_DEPTH+MAX_DEPTH)/2) %>%

      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(std = sqrt(svar))

    cover_est <- cover_est %>%
      # Merge ntot with coral_est_spp
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavcvr = wh * avcvr,
                    whsvar = wh^2 * svar,
                    whstd = wh * std,
                    n = tidyr::replace_na(n, 0)) %>%
      dplyr::filter(cover_group != "NA") # This will remove any strata that was not sampled that year
  }


  if(region %in% GOM |
     region %in% Carib)
  {

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
        n = sum(n),
        # calculate mean stratum depth
        MIN_DEPTH = mean(MIN_DEPTH, na.rm = T),
        MAX_DEPTH = mean(MAX_DEPTH, na.rm = T),
        DEPTH_M = (MIN_DEPTH+MAX_DEPTH)/2) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(std = sqrt(svar))

    cover_est <- cover_est %>%
      # Merge ntot with cover_est
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavcvr = wh * avcvr,
                    whsvar = wh^2 * svar,
                    whstd = wh * std,
                    n = tidyr::replace_na(n, 0),
                    # Add the following to match FL format
                    PROT = NA,
                    RUG_CD = NA)
  }

  # Reformat output
  cover_est <- cover_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, DEPTH_M, NTOT, ngrtot, wh, cover_group, n, avcvr, svar, std, whavcvr, whsvar, whstd)

  # cover, unweighted by strata
  unwh_cover_strata <- cover_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, DEPTH_M, NTOT, ngrtot, wh, cover_group, n, avcvr, svar, std) %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0))


  ## Domain Estimates
  Domain_est <- cover_est %>%
    dplyr::group_by(REGION, YEAR, cover_group) %>%
    dplyr::summarise(avCvr = sum(whavcvr),
                     var = sum(whsvar, na.rm = T),
                     std = sqrt(var),
                     ngrtot = sum(NTOT) )


  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "unwh_cover_strata" = unwh_cover_strata,
    "Domain_est" = Domain_est
  )



}
