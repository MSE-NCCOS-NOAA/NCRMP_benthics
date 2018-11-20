## Function to calculate weighted coral density, species richness and mortality by strata

# Purpose:
# support function to calculate weighted coral density and species richness


## Tag: data analysis


# outputs created in this file --------------
# unwh_density_strata
# unwh_richness_strata
# unwh_mortality_strata
# and Domain estimates for each of the above metric

# Weighting scheme:
# STRAT

# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_species_richness_diversity.R
# NCRMP_FRRP_calculate_colony_density.R
# NCRMP_FRRP_calculate_mortality.R

# NCRMP Caribbean Benthic analytics team: Viehman, Bauer, Groves
# Last update: Oct 2018
# Current status: In prep


##############################################################################################################################

#' Creates weighted demo data
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and FRRP combined
#' @param inputdata A dataframe
#' @param region A string indicating the region
#' @param datatype A string indicating the datatype
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_demo_data_RC <- function(project, inputdata, region, datatype){

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")


  #### Read in ntot ####

  if(region == "SEFCRI") {

    if(project == "NCRMP"){

      ntot14 <- FL_2016_NTOT %>%
        # Filter to region
        dplyr::filter(REGION == "SEFCRI") %>%
        # Add rugosity code to strat
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        # Filter to strata sampled
        dplyr::filter(STRAT == "MIDR1" | STRAT == "MIDR0") %>%
        # Rename the Strat column
        dplyr::mutate(ANALYSIS_STRATUM = STRAT,
                      YEAR = 2014) %>%
        # Calculate total grid (cell) size, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))


      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot16, ntot14)

    }

    if(project == "NCRMP_FRRP"){

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # And an empty list to populate with strata sampled for each year
      Filter <- vector('list', length(Years))

      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter[[i]] = unique(a$ANALYSIS_STRATUM)
        assign(paste("Filter", i, sep = "_"), Filter[[i]])
      }

      ntot17 <- FL_2016_NTOT %>%
        # Subet to region of interest
        dplyr::filter(REGION == "SEFCRI") %>%
        # Add rugosity code to strat, YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = ""),
                      YEAR = 2017,
                      ANALYSIS_STRATUM = STRAT)  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
        # Calculate total grid (cell) size, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = ""),
                      ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = ""),
                      YEAR = 2015,
                      ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(YEAR = 2014,
                      STRAT = paste(STRAT, RUG_CD, sep = ""),
                      ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      # Combine all ntot
      ntot <- rbind(ntot17, ntot16, ntot15, ntot14)

    }

  }

  if(region == "FLK") {

    if(project == "NCRMP"){

      ntot16 <- FL_2016_NTOT %>%
        # Filter to region
        dplyr::filter(REGION == "FL KEYS") %>%
        # Rename region to current NCRMP code. Rename the Strat column
        dplyr::mutate(REGION = "FLK",
                      ANALYSIS_STRATUM = STRAT) %>%
        # Roll up PROT - group by strat only and calculate new NTOT
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        # Calculate total grid (cell) size, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS",
                      # Remove strata not sampled
                      STRAT != "FDLR") %>%
        dplyr::mutate(REGION = "FLK",
                      YEAR = 2014,
                      ANALYSIS_STRATUM = STRAT) %>%# Roll up PROT - group by strat only and calculate new NTOT
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot16, ntot14)

    }

    if(project == "NCRMP_FRRP") {

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # And an empty list to populate with strata sampled for each year
      Filter <- vector('list', length(Years))

      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter[[i]] = unique(a$ANALYSIS_STRATUM)
        assign(paste("Filter", i, sep = "_"), Filter[[i]])
      }


      ntot17 <- FL_2016_NTOT %>%
        # Subet to region of interest
        dplyr::filter(REGION == "FL KEYS") %>%
        # Rename region to current NCRMP code, change YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(REGION = "FLK",
                      YEAR = 2017,
                      ANALYSIS_STRATUM = STRAT)  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
        # Roll up PROT - group by strat only and calculate new NTOT
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        # Calculate total grid (cell) size, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS") %>%
        dplyr::mutate(REGION = "FLK",
                      ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS") %>%
        dplyr::mutate(REGION = "FLK",
                      YEAR = 2015) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS") %>%
        dplyr::mutate(REGION = "FLK",
                      YEAR = 2014) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot17, ntot16, ntot15, ntot14)

    }


  }

  if(region == "Tortugas") {

    if(project == "NCRMP"){

      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(REGION = "Tortugas",
                      ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT",
                      STRAT != "SPGR_LR") %>% # Not sampled in 2014
        dplyr::mutate(YEAR = 2014,
                      REGION = "Tortugas") %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot16, ntot14)

    }

    if(project == "NCRMP_FRRP") {

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # And an empty list to populate with strata sampled for each year
      Filter <- vector('list', length(Years))

      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter[[i]] = unique(a$ANALYSIS_STRATUM)
        assign(paste("Filter", i, sep = "_"), Filter[[i]])
      }


      ntot17 <- FL_2016_NTOT %>%
        # Subet to region of interest
        dplyr::filter(REGION == "TORT") %>%
        # Rename region to current NCRMP code, change YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(REGION = "Tortugas",
                      YEAR = 2017,
                      ANALYSIS_STRATUM = STRAT)  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
        # Roll up PROT - group by strat only and calculate new NTOT
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        # Calculate total grid (cell) size, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(REGION = "Tortugas",
                      ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(REGION = "Tortugas",
                      RUG_CD = NA_character_,
                      YEAR = 2015) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(REGION = "Tortugas",
                      YEAR = 2014) %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::group_by(REGION, YEAR, RUG_CD, STRAT, GRID_SIZE, ANALYSIS_STRATUM) %>%
        dplyr::summarise(NTOT = sum(NTOT)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot17, ntot16, ntot15, ntot14)

    }

  }

  if(region == "STTSTJ"){

    ntot13 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STTSTJ",
                    STRAT != "HARD_SHLW") %>% # Hard shlw was not sampled in 2013
      dplyr::mutate(YEAR = 2013,
                    ANALYSIS_STRATUM = HABITAT_CD) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot15 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::mutate(YEAR = 2015,
                    ANALYSIS_STRATUM = HABITAT_CD) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot17 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::mutate(ANALYSIS_STRATUM = HABITAT_CD) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- rbind(ntot13, ntot15, ntot17)


  }

  if(region == "STX"){

    ntot15 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STX",
                    STRAT != "HARD_SHLW", # Hard shlw was not sampled in 2015
                    STRAT != "HARD_DEEP") %>% # Hard deep was not sampled in 2015
      dplyr::mutate(YEAR = 2015,
                    ANALYSIS_STRATUM = HABITAT_CD) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot17 <- USVI_2017_NTOT %>%
      dplyr::filter(REGION == "STX",
                    STRAT != "HARD_SHLW") %>%
      dplyr::mutate(ANALYSIS_STRATUM = HABITAT_CD) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- rbind(ntot15, ntot17)

  }

  if(region == "PRICO"){

    ntot14 <- PRICO_2016_NTOT %>%
      dplyr::filter(STRAT != "HARD_DEEP", # Hard shlw was not sampled in 2014
                    STRAT != "HARD_SHLW") %>% # Hard deep was not sampled in 2014
      dplyr::mutate(YEAR = 2014,
                    ANALYSIS_STRATUM = HABITAT_CD) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot16 <- PRICO_2016_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = HABITAT_CD) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>%
      dplyr::summarise(NTOT = sum(NTOT)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- rbind(ntot14, ntot16)

  }

  if(region == "GOM"){

    ntot13 <- FGBNMS_2015_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                    REGION = "GOM",
                    YEAR = 2013) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH) %>%
      dplyr::summarise(NTOT = sum(NTOT),
                       ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()



    ntot15 <- FGBNMS_2015_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                    REGION = "GOM",
                    YEAR = 2015) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH) %>%
      dplyr::summarise(NTOT = sum(NTOT),
                       ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot <- rbind(ntot13, ntot15)

  }


  ntot <- ntot %>%
    dplyr::mutate(wh = NTOT/ngrtot)

  #### Calculate weighted species richness ####

  if(datatype == "richness"){

    if(region %in% FL) {

      # Calculate avdns, svar, n and std
      richness_est <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis strata
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
        # Merge with ntot
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavspr = wh * avspr,
                      whsvar = wh^2 * svar,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))

    }

    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avspr, svar, n and std
      richness_est <- inputdata %>%
        # make avspr
        dplyr::mutate(ANALYSIS_STRATUM = HABITAT_CD) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>% # Modify this line to changes analysis strata
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
        # Merge with ntot
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavspr = wh * avspr,
                      whsvar = wh^2 * svar,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format
                      RUG_CD = NA)

    }



    # Reformat output

    richness_est <- richness_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, RUG_CD, NTOT, ngrtot, wh, n, avspr, svar, std, whavspr, whsvar, whstd)

    unwh_richness_strata <-  richness_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, RUG_CD, NTOT, ngrtot, wh, n, avspr, svar, std)

    ## Domain Estimates
    Domain_est <- richness_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avSpR = sum(whavspr),
                       var = sum(whsvar, na.rm = T),
                       std = sqrt(var),
                       ngrtot = sum(NTOT) )



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

    if(region %in% FL) {

      # Calculate avdns, svar, n and std
      density_est <- inputdata %>%
        # make avdns
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
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
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whsvar = wh^2 * svar,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))

    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avdns, svar, n and std
      density_est <- inputdata %>%
        # make avdns
        dplyr::mutate(ANALYSIS_STRATUM = HABITAT_CD) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, HABITAT_CD) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          n = length(DENSITY)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(std = sqrt(svar))

      density_est <- density_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavden = wh * avden,
                      whsvar = wh^2 * svar,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format
                      RUG_CD = NA)

    }


    # Reformat output

    density_est <- density_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, RUG_CD, NTOT, ngrtot, wh, n, avden, svar, std, whavden, whsvar, whstd)

    unwh_density_strata <-  density_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, RUG_CD, NTOT, ngrtot, wh, n, avden, svar, std)

    ## Domain Estimates
    Domain_est <- density_est %>%
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
      "unwh_density_strata" = unwh_density_strata,
      "Domain_est" = Domain_est)

    return(output)

  }


  if(datatype == "mortality"){

    if(region %in% FL) {

      # Calculate avdns, svar, n and std
      mortality_est <- inputdata %>%
        # make avdns
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, MORT_TYPE) %>% # Modify this line to changes analysis substrate
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
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whsvar = wh^2 * svar,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0))

    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avmort, svar, n and std
      mortality_est <- inputdata %>%
        # make avmort
        dplyr::mutate(ANALYSIS_STRATUM = HABITAT_CD) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, HABITAT_CD, MORT_TYPE) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(
          # compute average mortality
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          n = length(avsitemort)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(std = sqrt(svar))

      mortality_est <- mortality_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whsvar = wh^2 * svar,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format temporarily
                      RUG_CD = NA)
    }


    # Reformat output

    mortality_est <- mortality_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, RUG_CD, NTOT, ngrtot, wh, n, avmort, svar, std, whavmort, whsvar, whstd)

    unwh_mortality_strata <-  mortality_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, RUG_CD, NTOT, ngrtot, wh, n, avmort, svar, std)

    ## Domain Estimates
    Domain_est <- mortality_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avMort = sum(whavmort, na.rm = T),
                       var = sum(whsvar, na.rm = T),
                       std = sqrt(var),
                       ngrtot = sum(NTOT) )

    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "unwh_mortality_strata" = unwh_mortality_strata,
      "Domain_est" = Domain_est)

    return(output)
  }

}















