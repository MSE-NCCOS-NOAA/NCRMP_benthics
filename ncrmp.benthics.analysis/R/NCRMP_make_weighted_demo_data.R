## Function to calculate weighted coral density, species richness, mortality and disease prevalence by strata and protected area

# Purpose:
# support function to calculate weighted coral density, species richness, mortality and disease prevalence


## Tag: data analysis


# outputs created in this file --------------
# unwh_density_strata
# unwh_richness_strata
# unwh_mortality_strata
# unwh_dis_prev_strata
# and Domain estimates for each of the above metric

# Weighting scheme:
# STRAT + PROT
# STRAT (Carib/GOM)


# CallS:
# analysis ready data

# output gets called by:
# NCRMP_calculate_species_richness_diversity.R
# NCRMP_FRRP_calculate_colony_density.R
# NCRMP_FRRP_calculate_mortality.R
# NCRMP_FRRP_calculate_disease_prevalence.R

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Oct 2018


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

NCRMP_make_weighted_demo_data <- function(project, inputdata, region, datatype){

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")


  #### Read in ntot ####

  if(region == "SEFCRI") {

    if(project == "NCRMP"){

      ntot14 <- FL_2016_NTOT %>%
        # Filter to region of interest
        dplyr::filter(REGION == "SEFCRI") %>%
        # Create a STRAT column
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        # Filter out strata not sampled that year
        dplyr::filter(STRAT == "MIDR1" | STRAT == "MIDR0") %>%
        # Create analysis strata column and change the sampling year
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2014) %>%
        # Calculate total number of grid cells
        dplyr::mutate(ngrtot = sum(NTOT))


      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot16, ntot14)

    }

    if(project == "NCRMP_FRRP"){

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
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
        # Calculate total grid (cell) n, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = ""),
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(YEAR = 2015,
                      STRAT = paste(STRAT, RUG_CD, sep = ""),
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(YEAR = 2014,
                      STRAT = paste(STRAT, RUG_CD, sep = ""),
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot17, ntot16, ntot15, ntot14)

    }

  }

  if(region == "FLK") {

    if(project == "NCRMP"){

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

      ntot <- rbind(ntot16, ntot14)

    }

    if(project == "NCRMP_FRRP") {

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

      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter[[i]] = unique(a$ANALYSIS_STRATUM)
        assign(paste("Filter", i, sep = "_"), Filter[[i]])
      }


      ntot17 <- FL_2016_NTOT %>%
        # Subet to region of interest
        dplyr::filter(REGION == "FL KEYS") %>%
        # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(REGION = "FLK",
                      YEAR = 2017,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
        # Calculate total grid (cell) n, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS") %>%
        dplyr::mutate(REGION = "FLK",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS") %>%
        dplyr::mutate(REGION = "FLK",
                      YEAR = 2015) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "FL KEYS") %>%
        dplyr::mutate(REGION = "FLK",
                      YEAR = 2014) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot17, ntot16, ntot15, ntot14)

    }


  }

  if(region == "Tortugas") {

    if(project == "NCRMP"){

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

    if(project == "NCRMP_FRRP") {

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

      # Use a loop to create a unique lists for each year of strata sampled
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i)
        Filter[[i]] = unique(a$ANALYSIS_STRATUM)
        assign(paste("Filter", i, sep = "_"), Filter[[i]])
      }


      ntot17 <- FL_2016_NTOT %>%
        # Subet to region of interest
        dplyr::filter(REGION == "TORT") %>%
        # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(REGION = "Tortugas",
                      YEAR = 2017,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
        # Calculate total grid (cell) n, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(REGION = "Tortugas",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(REGION = "Tortugas",
                      YEAR = 2015) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2016_NTOT %>%
        dplyr::filter(REGION == "TORT") %>%
        dplyr::mutate(REGION = "Tortugas",
                      YEAR = 2014) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot17, ntot16, ntot15, ntot14)

    }

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

    ntot13 <- FGBNMS_2015_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                    PROT = NA_character_,
                    REGION = "GOM",
                    YEAR = 2013) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
      dplyr::summarise(NTOT = sum(NTOT),
                       ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()



    ntot15 <- FGBNMS_2015_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                    PROT = NA_character_,
                    REGION = "GOM",
                    YEAR = 2015) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH, PROT) %>%
      dplyr::summarise(NTOT = sum(NTOT),
                       ngrtot = sum(NTOT)) %>%
      dplyr::ungroup()

    ntot <- rbind(ntot13, ntot15)

  }


  ntot <- ntot %>%
    dplyr::mutate(wh = NTOT/ngrtot) %>%
    dplyr::mutate(PROT = as.factor(PROT))

  #### Calculate weighted species richness ####

  if(datatype == "richness"){

    if(region %in% FL) {

      # Calculate avspr, svar, n and std
      richness_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average richness
          avspr = mean(SPP_RICHNESS),
          # compute stratum variance
          svar = var(SPP_RICHNESS),
          n = length(SPP_RICHNESS)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(std = sqrt(svar))

      richness_est <- richness_est %>%
        # Merge ntot with coral_est_spp
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
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average richness
          avspr = mean(SPP_RICHNESS),
          # compute stratum variance
          svar = var(SPP_RICHNESS),
          n = length(SPP_RICHNESS)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(std = sqrt(svar))

      richness_est <- richness_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavspr = wh * avspr,
                      whsvar = wh^2 * svar,
                      whstd = wh * std,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format temporarily
                      PROT = NA,
                      RUG_CD = NA)

    }



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
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
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
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
          avden = mean(DENSITY),
          # compute stratum variance
          svar = var(DENSITY),
          # calculate N
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
                      PROT = NA,
                      RUG_CD = NA)

    }


    # Reformat output

    density_est <- density_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avden, svar, std, whavden, whsvar, whstd)

    unwh_density_strata <-  density_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avden, svar, std)

    ## Domain Estimates
    Domain_est <- density_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avDen = sum(whavden, na.rm = T), # This accounts for strata with 0 species of interest present
                       var = sum(whsvar, na.rm = T),    # This accounts for strata with N = 1
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

  #### Calculate mortality ####

  if(datatype == "mortality"){

    if(region %in% FL) {

      # Calculate avdns, svar, n and std
      mortality_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT, MORT_TYPE) %>% # Modify this line to changes analysis substrate
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
                      n = tidyr::replace_na(n, 0))

    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avdns, svar, n and std
      mortality_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, MORT_TYPE) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(# compute average density
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
                      PROT = NA,           # Add the following to match FL format temporarily
                      RUG_CD = NA)

    }


    # Reformat output

    mortality_est <- mortality_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avmort, svar, std, whavmort, whsvar, whstd)

    unwh_mortality_strata <-  mortality_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avmort, svar, std)

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

  #### Calculate weighted diversity ####

  if(datatype == "diversity"){

    if(region %in% FL) {

      # Calculate avdiv, svar, n and std
      diversity_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(
          # compute average diversity
          avSimp = mean(Simpson),
          avInvSimp = mean(Inv_Simpson),
          avShannon = mean(Shannon),
          # compute stratum variance
          svar_Simp = var(Simpson),
          svar_InvSimp = var(Inv_Simpson),
          svar_Shan = var(Shannon),
          n = length(Simpson)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar_Simp = dplyr::case_when(svar_Simp == 0 ~ 0.00000001,
                                                   TRUE ~ svar_Simp),
                      svar_InvSimp = dplyr::case_when(svar_InvSimp == 0 ~ 0.00000001,
                                                      TRUE ~ svar_InvSimp),
                      svar_Shan = dplyr::case_when(svar_Shan == 0 ~ 0.00000001,
                                                   TRUE ~ svar_Shan)) %>%
        dplyr::mutate(std_Simp = sqrt(svar_Simp),
                      std_InvSimp = sqrt(svar_InvSimp),
                      std_Shan = sqrt(svar_Shan))

      diversity_est <- diversity_est %>%
        # Merge ntot with diversity_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavSimp = wh * avSimp,
                      whavInvSimp = wh * avInvSimp,
                      whavShan = wh * avShannon,
                      whsvar_Simp = wh^2 * svar_Simp,
                      whsvar_InvSimp = wh^2 * svar_InvSimp,
                      whsvar_Shan = wh^2 * svar_Shan,
                      whstd_Simp = wh * std_Simp,
                      whstd_InvSimp = wh * std_InvSimp,
                      whstd_Shan = wh * std_Shan,
                      n = tidyr::replace_na(n, 0))

    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avdiv, svar, n and std
      diversity_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(
          # compute average diversity
          avSimp = mean(Simpson),
          avInvSimp = mean(Inv_Simpson),
          avShannon = mean(Shannon),
          # compute stratum variance
          svar_Simp = var(Simpson),
          svar_InvSimp = var(Inv_Simpson),
          svar_Shan = var(Shannon),
          n = length(Simpson)) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar_Simp = dplyr::case_when(svar_Simp == 0 ~ 0.00000001,
                                                   TRUE ~ svar_Simp),
                      svar_InvSimp = dplyr::case_when(svar_InvSimp == 0 ~ 0.00000001,
                                                      TRUE ~ svar_InvSimp),
                      svar_Shan = dplyr::case_when(svar_Shan == 0 ~ 0.00000001,
                                                   TRUE ~ svar_Shan)) %>%
        dplyr::mutate(std_Simp = sqrt(svar_Simp),
                      std_InvSimp = sqrt(svar_InvSimp),
                      std_Shan = sqrt(svar_Shan))

      diversity_est <- diversity_est %>%
        # Merge ntot with diversity_est
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavSimp = wh * avSimp,
                      whavInvSimp = wh * avInvSimp,
                      whavShan = wh * avShannon,
                      whsvar_Simp = wh^2 * svar_Simp,
                      whsvar_InvSimp = wh^2 * svar_InvSimp,
                      whsvar_Shan = wh^2 * svar_Shan,
                      whstd_Simp = wh * std_Simp,
                      whstd_InvSimp = wh * std_InvSimp,
                      whstd_Shan = wh * std_Shan,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL
                      PROT = NA,
                      RUG_CD = NA)

    }


    # Reformat output

    diversity_est <- diversity_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n,
                    avSimp, avInvSimp, avShannon,svar_Simp, svar_InvSimp, svar_Shan,
                    std_Simp, std_InvSimp, std_Shan, whavSimp, whavInvSimp, whavShan,
                    whsvar_Simp, whsvar_InvSimp, whsvar_Shan, whstd_Simp, whstd_InvSimp, whstd_Shan)

    unwh_diversity_strata <-  diversity_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n,
                    avSimp, avInvSimp, avShannon, svar_Simp, svar_InvSimp, svar_Shan,
                    std_Simp, std_InvSimp, std_Shan)

    ## Domain Estimates
    Domain_est <- diversity_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avSimp = sum(whavSimp),
                       avInvSimp = sum(whavInvSimp),
                       avShan = sum(whavShan),
                       var_Simp = sum(whsvar_Simp, na.rm = T),
                       var_InvSimp = sum(whsvar_InvSimp, na.rm = T),
                       var_Shan = sum(whsvar_Shan, na.rm = T),
                       std_Simp = sqrt(var_Simp),
                       std_InvSimp = sqrt(var_InvSimp),
                       std_Shan = sqrt(var_Shan),
                       ngrtot = sum(NTOT) )

    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "unwh_diversity_strata" = unwh_diversity_strata,
      "Domain_est_div" = Domain_est)

    return(output)

  }

  if(datatype == "disease"){

     if(region %in% FL) {

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

    }


    if(region %in% GOM |
       region %in% Carib) {

      # Calculate avprev, svar, n and std
      disease_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(
          # compute average density
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
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format
                      PROT = NA,
                      RUG_CD = NA)

    }


    # Reformat output

    disease_est <- disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avprev, svar, std, whavprev, whsvar, whstd)

    unwh_dis_prev_strata <-  disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avprev, svar, std) %>%
      dplyr::mutate(RUG_CD = as.factor(RUG_CD))

    ## Domain Estimates
    Domain_est <- disease_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avPrev = sum(whavprev, na.rm = T), # This accounts for strata with 0 species of interest present
                       var = sum(whsvar, na.rm = T),    # This accounts for strata with N = 1
                       std = sqrt(var),
                       ngrtot = sum(NTOT) )

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















