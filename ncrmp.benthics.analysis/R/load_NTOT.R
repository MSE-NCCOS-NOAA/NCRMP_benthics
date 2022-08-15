## Function to load NTOT files and calculate wh

# Purpose:
# support function to load NTOT files and calculate wh


## Tag: data analysis


# outputs created in this file --------------
# ntot


# CallS:
# NTOTs

# output gets called by:
# NCRMP_make_weighted_demo_data.R

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: March 2022


##############################################################################################################################

#' Creates weighted demo data
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param inputdata A dataframe
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

load_NTOT <- function(region, inputdata, project){


#### Read in ntot ####

if(region == "SEFCRI") {

  if(project == "NCRMP" || project == "NULL"){

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

    ntot20 <- SEFL_2020_NTOT %>%
      dplyr::ungroup() %>%
      dplyr::filter(STRAT != "RGDP1" & STRAT != "RGDP0") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot14, ntot16, ntot18, ntot20) %>% dplyr::mutate(PROT = as.factor(PROT))

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

    ntot21 <- SEFL_2020_NTOT %>%
      # create ANALYSIS STRATUM column
      dplyr::mutate(YEAR = 2021,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2021) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot20 <- SEFL_2020_NTOT %>%
      # create ANALYSIS STRATUM column
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2020) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot19 <- SEFL_2018_NTOT %>%
      # create ANALYSIS STRATUM column
      dplyr::mutate(YEAR = 2019,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2019) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot18 <- SEFL_2018_NTOT %>%
      # create ANALYSIS STRATUM column
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2018) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot17 <- SEFL_2018_NTOT %>%
      dplyr::mutate(YEAR = 2017,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot16 <- SEFL_2016_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot15 <- SEFL_2014_NTOT %>%
      dplyr::mutate(YEAR = 2015,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot14 <- SEFL_2014_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot21, ntot20, ntot19, ntot18, ntot17, ntot16, ntot15, ntot14)

  }

}

if(region == "FLK") {

  if(project == "NCRMP" || project == "NULL"){

    # Filter NTOT to only strata sampled that year - this is done manually for NCRMP only for now

    ntot14 <- FLK_2014_NTOT %>%
      dplyr::filter(STRAT != "FDLR") %>% #Remove strat that were not sampled from ntot so they are not counted in ngrtot
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    REGION = "FLK",
                    ngrtot = sum(NTOT))

    ntot16 <- FLK_2016_NTOT %>%
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))

    ntot18 <- FLK_2018_NTOT %>%
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))

    ntot20 <- FLK_2020_NTOT %>%
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot14, ntot16, ntot18, ntot20) %>% dplyr::mutate(PROT = as.factor(PROT))

  }

  if(project == "NCRMP_DRM") {

    # Filter NTOT to only strata sampled that year
    # Make a dataframe of just the YEAR and STRAT
    tmp <- inputdata %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
      dplyr::summarise(N = length(ANALYSIS_STRATUM), .groups = "keep")

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

    ntot21 <- FLK_2020_NTOT %>%
      # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
      dplyr::mutate(REGION = "FLK",
                    YEAR = 2021,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2021) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot20 <- FLK_2020_NTOT %>%
      # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2020) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot19 <- FLK_2018_NTOT %>%
      # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
      dplyr::mutate(REGION = "FLK",
                    YEAR = 2019,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2019) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot18 <- FLK_2018_NTOT %>%
      # Subet to region of interest
      # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2018) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot17 <- FLK_2018_NTOT %>%
      # Subet to region of interest
      # Rename region to current NCRMP code, YEAR to sampling year and create ANALYSIS STRATUM column
      dplyr::mutate(REGION = "FLK",
                    YEAR = 2017,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot16 <- FLK_2016_NTOT %>%
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot15 <- FLK_2016_NTOT %>%
      dplyr::mutate(REGION = "FLK",
                    YEAR = 2015,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot14 <- FLK_2014_NTOT %>%
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot21, ntot20, ntot19, ntot18, ntot17, ntot16, ntot15, ntot14)

  }


}

if(region == "Tortugas") {

  if(project == "NCRMP" || project == "NULL"){

    ntot14 <- Tort_2014_NTOT %>%
      dplyr::filter(STRAT != "SPGR_LR") %>% # Not sampled in 2014
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))

    ntot16 <- Tort_2016_NTOT %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    REGION = "Tortugas") %>%
      dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0") %>% #Not sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot18 <- Tort_2018_NTOT %>%
      dplyr::filter(STRAT != "SPGR_LR") %>% # Not sampled in 2018
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM != "ISOL_LR / PROT = 0",
                    ANALYSIS_STRATUM != "ISOL_LR / PROT = 1") %>% # Not sampled in 2018
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot20 <- Tort_2020_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(ANALYSIS_STRATUM != "T09 / PROT = 0",
                    ANALYSIS_STRATUM != "T09 / PROT = 1",
                    ANALYSIS_STRATUM != "T03 / PROT = 2",
                    ANALYSIS_STRATUM != "T06 / PROT = 2") %>% #Not sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot14, ntot16, ntot18, ntot20)

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

    ntot21 <- Tort_2020_NTOT %>%
      # Rename region and year if necessary, add analysis strat
      dplyr::mutate(REGION = "Tortugas",
                    YEAR = 2021,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      # Filter to just strata sampled
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2021) %>%
      # Calculate total grid (cell) n, based on strata sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot20 <- Tort_2020_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2020) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot19 <- Tort_2018_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    YEAR = 2019,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2019) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot18 <- Tort_2018_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2018) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot17 <- Tort_2018_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    YEAR = 2017,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2017) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot16 <- Tort_2016_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2016) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot15 <- Tort_2016_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    YEAR = 2015,
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2015) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot14 <- Tort_2014_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))  %>%
      dplyr::filter(ANALYSIS_STRATUM %in% Filter_2014) %>%
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot21, ntot20, ntot19, ntot18, ntot17, ntot16, ntot15, ntot14)

  }

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

  ntot <- dplyr::bind_rows(ntot13, ntot15, ntot18)

}


ntot <- ntot %>%
  dplyr::mutate(wh = NTOT/ngrtot) %>%
  dplyr::mutate(PROT = as.factor(PROT))

################
# Export
################

return(ntot)
}
