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
# Last update: Feb 2023


##############################################################################################################################

#' Creates weighted Invert density data
#'
#'
#'
#'
#' @param inputdata A dataframe
#' @param region A string indicating the region
#' @param project A string indicating the project: "NCRMP" or "MIR". Default is NCRMP.
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_invert_density_data <- function(inputdata, region, project = "NULL")
{

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")


  #### Read in ntot ####

  ntot <- load_NTOT(region = region,
                    inputdata = inputdata,
                    project = project)


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
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, NTOT, ngrtot, wh, n, avden, svar, std, whavden, whsvar, whstd)

  # cover, unweighted by strata
  invert_strata <- dens_est %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, NTOT, ngrtot, wh, n, avden, svar, std)


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
