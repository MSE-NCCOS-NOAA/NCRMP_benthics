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
# NCRMP_DRM_calculate_colony_density.R
# NCRMP_DRM_calculate_mortality.R
# NCRMP_DRM_calculate_disease_prevalence.R

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Mar 2020


##############################################################################################################################

#' Creates weighted demo data
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param inputdata A dataframe
#' @param region A string indicating the region
#' @param datatype A string indicating the datatype
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_make_weighted_demo_data <- function(project, inputdata, region, datatype, species_filter, species_data = NULL){

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")


  #### Read in ntot ####

  if(region == "SEFCRI") {

    if(project == "NCRMP"){

      ntot14 <- FL_2018_NTOT %>%
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


      ntot16 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2016) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot18 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = "")) %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot18, ntot16, ntot14)

    }

    if(project == "NCRMP_DRM"){

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

      ntot18 <- FL_2018_NTOT %>%
        # Subet to region of interest
        dplyr::filter(REGION == "SEFCRI") %>%
        # Add rugosity code to strat, YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = ""),
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2018) %>%
        # Calculate total grid (cell) n, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot17 <- FL_2018_NTOT %>%
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

      ntot16 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(STRAT = paste(STRAT, RUG_CD, sep = ""),
                      YEAR = 2016,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(YEAR = 2015,
                      STRAT = paste(STRAT, RUG_CD, sep = ""),
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "SEFCRI") %>%
        dplyr::mutate(YEAR = 2014,
                      STRAT = paste(STRAT, RUG_CD, sep = ""),
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot18, ntot17, ntot16, ntot15, ntot14)

    }

  }

  if(region == "FLK") {

    if(project == "NCRMP"){

      # Filter NTOT to only strata sampled that year - this is done manually for NCRMP only for now

      ntot18 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(YEAR = 2016,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
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

      ntot <- rbind(ntot18, ntot16, ntot14)

    }

    if(project == "NCRMP_DRM") {

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

      ntot18 <- FL_2018_NTOT %>%
        # Subet to region of interest
        dplyr::filter(REGION == "FLK") %>%
        # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2018) %>%
        # Calculate total grid (cell) n, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot17 <- FL_2018_NTOT %>%
        # Subet to region of interest
        dplyr::filter(REGION == "FLK") %>%
        # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(YEAR = 2017,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
        # Calculate total grid (cell) n, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(YEAR = 2016,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(YEAR = 2015,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "FLK") %>%
        dplyr::mutate(YEAR = 2014,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot18, ntot17, ntot16, ntot15, ntot14)

    }


  }

  if(region == "Tortugas") {

    if(project == "NCRMP"){

      ntot18 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas",
                      STRAT != "SPGR_LR") %>% # Not sampled in 2018
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0",
                      ANALYSIS_STRATUM != "ISOL_LR / PROT = 1") %>%  # Not sampled in 2018
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                      YEAR = 2016) %>%
        dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0") %>%  # Not sampled in 2016
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas",
                      STRAT != "SPGR_LR") %>% # Not sampled in 2014
        dplyr::mutate(YEAR = 2014,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot18, ntot16, ntot14)

    }

    if(project == "NCRMP_DRM") {

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

      ntot18 <- FL_2018_NTOT %>%
        # Subset to region of interest
        dplyr::filter(REGION == "Tortugas") %>%
        # Rename YEAR to sampling year and create ANALYSIS STRATUM column
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        # Filter to just strata sampled
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2018) %>%
        # Calculate total grid (cell) n, based on strata sampled
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot17 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(YEAR = 2017,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot16 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(YEAR = 2016,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot15 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(YEAR = 2015,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot14 <- FL_2018_NTOT %>%
        dplyr::filter(REGION == "Tortugas") %>%
        dplyr::mutate(YEAR = 2014,
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- rbind(ntot18, ntot17, ntot16, ntot15, ntot14)

    }

  }

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
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
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
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()

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
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
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
                      RUG_CD = NA)  %>%
        dplyr::ungroup()

    }



    # Reformat output

    richness_est <- richness_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avspr, svar, std, whavspr, whsvar, whstd)

    richness_strata <-  richness_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avspr, svar, std) %>%
      dplyr::mutate(RUG_CD = as.factor(RUG_CD))

    ## Domain Estimates
    Domain_est <- richness_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avSpR = sum(whavspr),
                       var = sum(whsvar, na.rm = T),
                       std = sqrt(var),
                       ngrtot = sum(NTOT) )  %>%
      dplyr::ungroup()



    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "richness_strata" = richness_strata,
      "Domain_est" = Domain_est)

    return(output)

  }

  #### Calculate weighted density ####



  if(datatype == "density"){

    if(species_filter == "FALSE" ||
       species_filter == "NULL"){

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
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/avden)*100)

        density_est <- density_est %>%
          # Merge ntot with coral_est_spp
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavden = wh * avden,
                        whvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0))  %>%
          dplyr::ungroup()

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
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/avden)*100)

        density_est <- density_est %>%
          # Merge ntot with coral_est_spp
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavden = wh * avden,
                        whvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0),
                        # Add the following to match FL format
                        PROT = NA,
                        RUG_CD = NA)  %>%
          dplyr::ungroup()

      }


      # Reformat output

      # strata_means
      density_strata <-  density_est %>%
        dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, n, avden, Var, SE, CV_perc) %>%
        dplyr::mutate(RUG_CD = as.factor(RUG_CD))

      ## Domain Estimates
      # region/population means
      Domain_est <- density_est %>%
        dplyr::group_by(REGION, YEAR) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T), # This accounts for strata with 0 species of interest present
                         Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                         SE=sqrt(Var),
                         CV_perc=(SE/avDen)*100,
                         n_sites = sum(n),
                         n_strat = length(unique(ANALYSIS_STRATUM)),
                         ngrtot = sum(NTOT) )  %>%
        dplyr::ungroup()

      ################
      # Export
      ################

      # Create list to export
      output <- list(
        "density_strata" = density_strata,
        "Domain_est" = Domain_est)

      return(output)

    }

    if(species_filter == "TRUE"){

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
            n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/avden)*100)

        density_est <- density_est %>%
          # Merge ntot with coral_est_spp
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavden = wh * avden,
                        whvar = wh^2 * Var,
                        n = tidyr::replace_na(n, 0))  %>%
          dplyr::ungroup()

        # calculate species CVS
        strata_CV <- species_data %>%
          dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
          group_by(YEAR, SPECIES_CD, ANALYSIS_STRATUM, STRAT, PROT) %>%
          summarize(mean=mean(DENSITY),
                    svar=var(DENSITY),
                    N_species=length(DENSITY)) %>% # sample variance of density in stratum
          mutate(svar=dplyr::case_when(svar==0 ~ 0.00000001, # replace zeros with very small number
                                       TRUE ~ svar)) %>%
          dplyr::left_join(., density_est %>% dplyr::select(YEAR, ANALYSIS_STRATUM, n)) %>%

          mutate(Var=svar/n, #variance of mean density in stratum
                 std = sqrt(svar), # std dev of density in stratum
                 SE=sqrt(Var), #SE of the mean density in stratum
                 CV_perc=(SE/mean)*100)

        # region/population means
        region_CV <- strata_CV %>%
          dplyr::full_join(., ntot) %>%
          dplyr::mutate(wh_mean=wh*mean, wh_var = wh^2*Var) %>%
          dplyr::group_by(YEAR, SPECIES_CD) %>%
          dplyr::summarize(mean=sum(wh_mean), Var=sum(wh_var, na.rm=TRUE),
                           SE=sqrt(Var), CV_perc=(SE/mean)*100, n= sum(N_species)) %>%
          dplyr::mutate(ANALYSIS_STRATUM="ALL_STRAT", STRAT="ALL_HABS", PROT="ALL_PROT") %>%  #add svar variable
          dplyr::select(YEAR, ANALYSIS_STRATUM, SPECIES_CD, n, mean, Var, SE, CV_perc, STRAT, PROT)


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
            n_sites = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
          # convert 0 for stratum variance so that the sqrt is a small # but not a 0
          dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                                TRUE ~ svar)) %>%
          dplyr::mutate(Var=svar/n_sites, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/avden)*100)

        density_est <- density_est %>%
          # Merge ntot with coral_est_spp
          dplyr::full_join(., ntot) %>%
          # stratum estimates
          dplyr::mutate(whavden = wh * avden,
                        whvar = wh^2 * Var,
                        n_sites = tidyr::replace_na(n_sites, 0),
                        # Add the following to match FL format
                        PROT = NA,
                        RUG_CD = NA)  %>%
          dplyr::ungroup()

        # calculate species CVS
        strata_CV <- species_data %>%
          dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
          dplyr::group_by(YEAR, SPECIES_CD, ANALYSIS_STRATUM, STRAT) %>%
          dplyr::summarize(mean=mean(DENSITY),
                           svar=var(DENSITY),
                           n_colonies = sum(ABUNDANCE),
                           n_sites_present=length(DENSITY)) %>% # sample variance of density in stratum
          dplyr::mutate(svar=dplyr::case_when(svar==0 ~ 0.00000001, # replace zeros with very small number
                                              TRUE ~ svar)) %>%
          dplyr::left_join(., density_est %>% dplyr::select(YEAR, ANALYSIS_STRATUM, n_sites)) %>%

          dplyr::mutate(Var=svar/n_sites, #variance of mean density in stratum
                        std = sqrt(svar), # std dev of density in stratum
                        SE=sqrt(Var), #SE of the mean density in stratum
                        CV_perc=(SE/mean)*100,
                        Occurrence = n_sites_present/n_sites)

        if(region %in% GOM) {

          # region/population means - FGB has no strata currently so this is equivalent to regional CVs
          region_CV <- strata_CV


        } else {

          Total_N <- density_est %>%
            dplyr::group_by(REGION) %>%
            dplyr::summarise(n_sites = sum(n_sites),
                             n_strat = length(unique(ANALYSIS_STRATUM)))  %>%
            dplyr::ungroup()

          # region/population means
          region_CV <- strata_CV %>%
            dplyr::full_join(., ntot, by=c("YEAR", "ANALYSIS_STRATUM")) %>%
            dplyr::mutate(wh_mean=wh*mean, wh_var = wh^2*Var) %>%
            dplyr::group_by(REGION, SPECIES_CD) %>%
            dplyr::summarize(avDen=sum(wh_mean), Var=sum(wh_var, na.rm=TRUE),
                             SE=sqrt(Var), CV_perc=(SE/avDen), n_colonies=sum(n_colonies), n_sites_present = sum(n_sites_present)) %>%
            dplyr::mutate(YEAR = "ALL_YEARS", ANALYSIS_STRATUM="ALL_STRAT", STRAT="ALL_HABS", PROT="ALL_PROT", n_sites = Total_N$n_sites,
                          Occurrence = n_sites_present/n_sites) %>%
            dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_CD, n_colonies, n_sites_present, n_sites, avDen, Var, SE, CV_perc, Occurrence, STRAT, PROT)



        }

         g.mid <- ggplot(region_CV, aes(x=1,y=reorder(SPECIES_CD, CV_perc)))+geom_text(aes(label=SPECIES_CD))+
            geom_segment(aes(x=0.94,xend=0.96,yend=SPECIES_CD))+
            geom_segment(aes(x=1.04,xend=1.065,yend=SPECIES_CD))+
            ggtitle(region_CV$REGION)+
            ylab(NULL)+
            scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
            theme(axis.title=element_blank(),
                  panel.grid=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  panel.background=element_blank(),
                  axis.text.x=element_text(color=NA),
                  axis.ticks.x=element_line(color=NA),
                  plot.margin = unit(c(1,-1,1,-1), "mm"),
                  plot.title = element_text(hjust = 0.5))

          g1 <- ggplot(data = region_CV, aes(x = reorder(SPECIES_CD, CV_perc), y = Occurrence, fill = 'even')) +
            geom_bar(stat = "identity") + ggtitle("Occurrence") +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.margin = unit(c(1,-1,1,0), "mm"),
                  plot.title = element_text(hjust = 0.5)) +
            scale_y_reverse() + coord_flip() + guides(fill = FALSE) + scale_fill_manual(values= c( "#0a4595"))

          g2 <- ggplot(data = region_CV, aes(x = reorder(SPECIES_CD, CV_perc), y = CV_perc, fill = 'even')) +xlab(NULL)+
            geom_bar(stat = "identity") + ggtitle("Coefficient of Variation of mean density") +
            theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                  axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                  plot.margin = unit(c(1,0,1,-1), "mm"), plot.title = element_text(hjust = 0.5)) +
            coord_flip() + guides(fill = FALSE) + scale_fill_manual(values= c( "#58babb"))


      }


      # Reformat output

      # strata_means
      density_strata <-  density_est %>%
        dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, n_sites, avden, Var, SE, CV_perc) %>%
        dplyr::mutate(RUG_CD = as.factor(RUG_CD))

      ## Domain Estimates
      # region/population means
      Domain_est <- density_est %>%
        dplyr::group_by(REGION, YEAR) %>%
        dplyr::summarise(avDen = sum(whavden, na.rm = T), # This accounts for strata with 0 species of interest present
                         Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                         SE=sqrt(Var),
                         CV_perc=(SE/avDen)*100,
                         n_sites = sum(n_sites),
                         n_strat = length(unique(ANALYSIS_STRATUM)),
                         ngrtot = sum(NTOT) )  %>%
        dplyr::ungroup()

      ################
      # Export
      ################


      # Create list to export
      output <- list(
        "Species_regional_CV" = region_CV,
        "density_strata" = density_strata,
        "Domain_est" = Domain_est,
        'g1' = g1,
        'g2' = g2,
        'g.mid' = g.mid)

      return(output)


    }


  }


  #### Calculate mortality ####

  if(datatype == "mortality"){

    if(region %in% FL) {

      # Calculate avmort, svar, n and std
      mortality_est <- inputdata %>%
        # group by analysis level strata
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, PROT, MORT_TYPE) %>% # Modify this line to changes analysis substrate
        dplyr::summarise(
          # compute average mortality
          avmort = mean(avsitemort),
          # compute stratum variance
          svar = var(avsitemort),
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var), #SE of the mean density in stratum
                      CV_perc=(SE/avmort)*100)

      mortality_est <- mortality_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()

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
          n = length(unique(PRIMARY_SAMPLE_UNIT))) %>%
        # convert 0 for stratum variance so that the sqrt is a small # but not a 0
        dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                              TRUE ~ svar)) %>%
        dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                      std = sqrt(svar), # std dev of density in stratum
                      SE=sqrt(Var), #SE of the mean density in stratum
                      CV_perc=(SE/avmort)*100)

      mortality_est <- mortality_est %>%
        # Merge ntot with coral_est_spp
        dplyr::full_join(., ntot) %>%
        # stratum estimates
        dplyr::mutate(whavmort = wh * avmort,
                      whvar = wh^2 * Var,
                      n = tidyr::replace_na(n, 0),
                      # Add the following to match FL format
                      PROT = NA,
                      RUG_CD = NA)  %>%
        dplyr::ungroup()

    }


    # Reformat output

    # strata_means
    mortality_strata <-  mortality_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, n, avmort, Var, SE, CV_perc) %>%
      dplyr::mutate(RUG_CD = as.factor(RUG_CD))

    ## Domain Estimates
    # region/population means
    Domain_est <- mortality_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avMort = sum(whavmort, na.rm = T), # This accounts for strata with 0 species of interest present
                       Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                       SE=sqrt(Var),
                       CV_perc=(SE/avMort)*100,
                       n_sites = sum(n),
                       n_strat = length(unique(ANALYSIS_STRATUM)),
                       ngrtot = sum(NTOT) )  %>%
      dplyr::ungroup()

    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "mortality_strata" = mortality_strata,
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
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()

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
                      RUG_CD = NA)  %>%
        dplyr::ungroup()

    }


    # Reformat output

    diversity_est <- diversity_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n,
                    avSimp, avInvSimp, avShannon,svar_Simp, svar_InvSimp, svar_Shan,
                    std_Simp, std_InvSimp, std_Shan, whavSimp, whavInvSimp, whavShan,
                    whsvar_Simp, whsvar_InvSimp, whsvar_Shan, whstd_Simp, whstd_InvSimp, whstd_Shan)

    diversity_strata <-  diversity_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n,
                    avSimp, avInvSimp, avShannon, svar_Simp, svar_InvSimp, svar_Shan,
                    std_Simp, std_InvSimp, std_Shan) %>%
      dplyr::mutate(RUG_CD = as.factor(RUG_CD))

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
                       ngrtot = sum(NTOT) )  %>%
      dplyr::ungroup()

    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "diversity_strata" = diversity_strata,
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
                      n = tidyr::replace_na(n, 0))  %>%
        dplyr::ungroup()

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
                      RUG_CD = NA)  %>%
        dplyr::ungroup()

    }


    # Reformat output

    disease_est <- disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avprev, svar, std, whavprev, whsvar, whstd)

    dis_prev_strata <-  disease_est %>%
      dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, STRAT, RUG_CD, PROT, NTOT, ngrtot, wh, n, avprev, svar, std) %>%
      dplyr::mutate(RUG_CD = as.factor(RUG_CD))

    ## Domain Estimates
    Domain_est <- disease_est %>%
      dplyr::group_by(REGION, YEAR) %>%
      dplyr::summarise(avPrev = sum(whavprev, na.rm = T), # This accounts for strata with 0 species of interest present
                       var = sum(whsvar, na.rm = T),    # This accounts for strata with N = 1
                       std = sqrt(var),
                       ngrtot = sum(NTOT) )  %>%
      dplyr::ungroup()

    ################
    # Export
    ################

    # Create list to export
    output <- list(
      "dis_prev_strata" = dis_prev_strata,
      "Domain_est" = Domain_est)

    return(output)



  }

}















