## Function to calculate weighted coral density and species richness by strata and protected area for pre NCRMP
## DRM and SCREAM data from 1999 to 2013

# Purpose:
# support function to calculate weighted coral density and species richness


## Tag: data analysis


# outputs created in this file --------------
# wh_richness_strata
# unwh_richness_strata
# comb_richness_strata

# Current weighting scheme:
# STRAT + PROT

# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_species_richness_diversity.R
#

# NCRMP Caribbean Benthic analytics team: Viehman, Bauer, Groves
# Last update: Apr 2019


##############################################################################################################################

#' Creates weighted demo data
#'
#'
#'
#'
#' @param inputdata A dataframe
#' @param datatype A string indicating the datatype
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

DRM_SCREAM_make_weighted_demo_data <- function(inputdata, datatype){



  #### Read in ntot ####

  FL_NTOT <- FL_2018_NTOT %>%
    # Combine rugosity cd and strat for SEFCRI
    dplyr::mutate(STRAT = dplyr::case_when(REGION == "SEFCRI" ~ paste(STRAT, RUG_CD, sep = ""), TRUE ~
                                             as.character(STRAT))) %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

  # Filter NTOT to only strata sampled that year

  # Make a dataframe of just the YEAR and STRAT
  tmp <- inputdata %>%
    dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
    dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
    dplyr::summarise(N = length(ANALYSIS_STRATUM))

  # Make a list of all the years
  Years <- sort(unique(tmp$YEAR))
  # And an empty list to populate with strata sampled for each year
  Filter <- vector('list', length(Years))

  # Create an empty dataframe to populate with new ntot files
  ntot_all <- data.frame()

  # Use a loop to create a unique list for each year of strata sampled
  for(i in Years){
    a <- tmp %>% dplyr::filter(YEAR == i)
    Filter[[i]] = unique(a$ANALYSIS_STRATUM)
    assign(paste("Filter", i, sep = "_"), Filter[[i]])

  ##### Create NTOT for each year - to do: Loop this!

    ntot <- FL_NTOT %>%
      # Assign year
      dplyr::mutate(YEAR = i)  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter[[i]]) %>%
      # Calculate total grid (cell) size, based on strata sampled
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, PROT) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::mutate(ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

  # Create a dataframe for each individual NTOT for reference
  assign(paste("ntot", i, sep = "_"), ntot)

  # Combine into a single ntot
  ntot_all <- dplyr::bind_rows(ntot_all, ntot)

}

  ntot <- ntot_all %>%
    dplyr::mutate(wh = NTOT/ngrtot) %>%
    # Add rugosity code
    dplyr::mutate(RUG_CD = dplyr::case_when(REGION == "SEFCRI" ~ as.character(substr(ANALYSIS_STRATUM, 5, 5)),
                                            TRUE ~ NA_character_)) %>%
    dplyr::mutate(PROT = as.factor(PROT),
                  RUG_CD = as.factor(RUG_CD))


  #### Calculate weighted species richness ####

  if(datatype == "richness"){

    # Calculate avdns, svar, n and std
    richness_est <- inputdata %>%
      # group by analysis level strata
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
      dplyr::summarise(
        # compute average richness
        avspr = mean(SPP_RICHNESS),
        # compute stratum variance
        svar = var(SPP_RICHNESS),
        n = length(SPP_RICHNESS)) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(std = sqrt(svar))

    richness_est <- richness_est %>%
      # Merge ntot with richness_est
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavspr = wh * avspr,
                    whsvar = wh^2 * svar,
                    whstd = wh * std,
                    n = tidyr::replace_na(n, 0))


    # Reformat output

    richness_est <- richness_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avspr, svar, std, whavspr, whsvar, whstd)

    unwh_richness_strata <-  richness_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avspr, svar, std)


    ## Domain Estimates
    Domain_est <- richness_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avSpR = sum(whavspr),
                       var = sum(whsvar, na.rm = T),
                       std = sqrt(var),
                       ngrtot = sum(NTOT) ) %>%
      dplyr::ungroup()



    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "unwh_richness_strata" = unwh_richness_strata,
      "Domain_est" = Domain_est)

    return(output)

  }

  #### Calculate weighted density ####

  if(datatype == "density"){

    # Calculate avdns, svar, n and std
    density_est <- inputdata %>%
      # group by analysis level strata
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(REGION, YEAR,ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
      dplyr::summarise(
        # compute average density
        avden = mean(DENSITY),
        # compute stratum variance
        svar = var(DENSITY),
        n = length(DENSITY)) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(std = sqrt(svar))

    density_est <- density_est %>%
      # Merge ntot with density_est
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavden = wh * avden,
                    whsvar = wh^2 * svar,
                    whstd = wh * std,
                    n = tidyr::replace_na(n, 0))

    # Reformat output

    density_est <- density_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avden, svar, std, whavden, whsvar, whstd)

    unwh_density_strata <-  density_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avden, svar, std)


    ## Domain Estimates
    Domain_est <- density_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avDen = sum(whavden),
                       var = sum(whsvar, na.rm = T),
                       std = sqrt(var),
                       ngrtot = sum(NTOT) ) %>%
      dplyr::ungroup()

    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "unwh_density_strata" = unwh_density_strata,
      "Domain_est" = Domain_est)

    return(output)

  }


  if(datatype == "mortality"){


    # Calculate avdns, svar, n and std
    mortality_est <- inputdata %>%
      # group by analysis level strata
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT, MORT_TYPE) %>% # Modify this line to changes analysis substrate
      dplyr::summarise(
        # compute average density
        avmort = mean(avsitemort),
        # compute stratum variance
        svar = var(avsitemort),
        n = length(avsitemort)) %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(std = sqrt(svar))

    mortality_est <- mortality_est %>%
      # Merge ntot with  mortality_est
      dplyr::full_join(., ntot) %>%
      # stratum estimates
      dplyr::mutate(whavmort = wh * avmort,
                    whsvar = wh^2 * svar,
                    whstd = wh * std,
                    n = tidyr::replace_na(n, 0))


    # Reformat output

    mortality_est <- mortality_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avmort, svar, std, whavmort, whsvar, whstd)

    unwh_mortality_strata <-  mortality_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avmort, svar, std)

    ## Domain Estimates
    Domain_est <- mortality_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avMort = sum(whavmort),
                       var = sum(whsvar, na.rm = T),
                       std = sqrt(var),
                       ngrtot = sum(NTOT) )%>%
      dplyr::ungroup()


    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "unwh_mortality_strata" = unwh_mortality_strata,
      "Domain_est" = Domain_est)

    return(output)
  }

   if(datatype == "disease"){


      # Calculate avprev, svar, n and std
      disease_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avprev = mean(DIS_PREV),
          # compute stratum variance
          svar = var(DIS_PREV),
          # calculate N
          n = length(DIS_PREV)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(std = sqrt(svar))

      disease_est <- disease_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavprev = wh * avprev,
                      whsvar = wh^2 * svar,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))


    # Reformat output

    disease_est <- disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avprev, svar, std, whavprev, whsvar, whstd)

    unwh_dis_prev_strata <-  disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avprev, svar, std)

    ## Domain Estimates
    Domain_est <- disease_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avPrev = sum(whavprev, na.rm = T), # This accounts for strata with 0 species of interest present
                       var = sum(whsvar, na.rm = T),    # This accounts for strata with N = 1
                       std = sqrt(var),
                       ngrtot = sum(NTOT) ) %>%
      dplyr::ungroup()

    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "unwh_dis_prev_strata" = unwh_dis_prev_strata,
      "Domain_est" = Domain_est)

    return(output)



  }


}















