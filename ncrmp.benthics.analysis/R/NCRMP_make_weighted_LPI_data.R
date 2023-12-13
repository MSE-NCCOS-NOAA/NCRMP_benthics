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

# NCRMP Caribbean Benthic analytics team: Groves, viehman, Williams
# Last update: Nov 2023


##############################################################################################################################

#' Creates weighted benthic cover data
#'
#' Calcualtes weighted benthic cover data. NCRMP utilizes a stratified random
#' sampling design. Regional estimates of benthic cover are weighted by the
#' number of grid cells of a stratum in the sample frame.
#' Function produces strata means, weighted strata means,
#' and weighted regional estimates for benthic cover data.
#' Support function called by [NCRMP_calculate_cover()].
#'
#'
#'
#'
#'
#' @param inputdata A dataframe of benthic cover data summarized by cover group at each site in a single region.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param project A string indicating the project. "NCRMP" is the only option.
#' @return A list of dataframes, including a dataframe of strata means of cover groups
#' and a dataframe of weighted regional estimates of cover groups, for specified region.
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_LPI_data <- function(inputdata, region, project = "NULL")
{

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")

  ntot <- load_NTOT(region = region,
                    inputdata = inputdata,
                    project = project)

  if(region %in% FL)
  {
    #### Calculate avcvr, svar, n and std at the strata + PROT level ####
    cover_est <- inputdata %>%
      # make avcvr
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT, cover_group) %>% # Modify this line to changes analysis stratum
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
      # Merge ntot with coral_est
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavcvr = wh * avcvr,
                    whsvar = wh^2 * Var,
                    n = tidyr::replace_na(n, 0)) %>%
      dplyr::filter(cover_group != "NA")
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
                    PROT = NA)  %>%
      dplyr::ungroup()
  }

  # Reformat output

  # strata_means
  cover_strata <- cover_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, DEPTH_M, cover_group, n, avcvr, Var, SE, CV_perc) %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0)) %>%
    # replace inf values so we can add the strata means
    #dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) # this line throws errors for some people?
    dplyr::mutate(CV_perc = case_when(CV_perc == Inf ~ NA_real_, TRUE ~ CV_perc))


  ## Domain Estimates
  Domain_est <- cover_est %>%
    # replace inf values so we can add the strata means
    #dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) # this line throws errors for some people?
    dplyr::mutate(CV_perc = case_when(CV_perc == Inf ~ NA_real_, TRUE ~ CV_perc)) %>%
    dplyr::group_by(REGION, YEAR, cover_group) %>%
    dplyr::summarise(avCvr = sum(whavcvr, na.rm = T),
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
