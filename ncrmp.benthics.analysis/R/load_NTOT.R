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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Dec 2023


##############################################################################################################################

#' Creates weights for coral demographic data
#'
#' Creates weighting scheme for coral demographic data from a single region.
#' NCRMP utilizes a stratified random sampling design. Generally, weighting is
#' based on the number of grid cells in the sample frame in each stratum, to provide
#' regional estimates. This function is called by broader calculation functions
#' such as [NCRMP_make_weighted_demo_data()],
#' which will provide the inputdata object.
#'
#'
#'
#' @param project A string indicating the project, "NCRMP" or NCRMP and DRM combined ("NCRMP_DRM").
#' @param inputdata A dataframe of coral demographic data. Can be in various forms.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @return A dataframe of weighting scheme for corresponding region.
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

    ntot22 <- SEFL_2022_NTOT %>%
      dplyr::filter(STRAT != "RGDP1" & STRAT != "RGDP0") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    ngrtot = sum(NTOT))


    ntot <- dplyr::bind_rows(ntot14, ntot16, ntot18, ntot20, ntot22) %>% dplyr::mutate(PROT = as.factor(PROT))

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
    # add an empty data frame to populate with the filtered NTOTs
    ntot <- data.frame()
    # create a data frame of the full NTOTs for FLK
    NTOT_all <- dplyr::bind_rows(SEFL_2014_NTOT, SEFL_2014_NTOT %>% dplyr::mutate(YEAR = 2015),
                                 SEFL_2016_NTOT, SEFL_2018_NTOT %>% dplyr::mutate(YEAR = 2017),
                                 SEFL_2018_NTOT, SEFL_2018_NTOT %>% dplyr::mutate(YEAR = 2019),
                                 SEFL_2020_NTOT, SEFL_2020_NTOT %>% dplyr::mutate(YEAR = 2021),
                                 SEFL_2022_NTOT) %>%
      dplyr::mutate(REGION = "SEFCRI",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

    # Use a loop to create a unique lists for each year of strata sampled
    for(i in Years){
      a <- tmp %>% dplyr::filter(YEAR == i)
      Filter = unique(a$ANALYSIS_STRATUM)

      ntot_filt <- NTOT_all %>%
        # filter to year i
        dplyr::filter(YEAR == i) %>%
        # filter to strata present in year i
        dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
        # re-calculate ntot
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- dplyr::bind_rows(ntot, ntot_filt)
    }

    ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

  }

}

if(region == "FLK") {

  if(project == "NCRMP" || project == "NULL"){

    # Use a loop to create a unique lists for each year of strata sampled
    # Filter NTOT to only strata sampled that year (previously done manually)
    tmp <- inputdata %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM) %>%
      dplyr::summarise(N = length(ANALYSIS_STRATUM), .groups = "keep")

    # Make a list of all the years
    Years <- sort(unique(tmp$YEAR))
    # add an empty data frame to populate with the filtered NTOTs
    ntot <- data.frame()
    # create a data frame of the full NTOTs for FLK
    NTOT_all <- dplyr::bind_rows(FLK_2014_NTOT, FLK_2016_NTOT, FLK_2018_NTOT, FLK_2020_NTOT,FLK_2022_NTOT) %>%
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

    # Use a loop to create a unique lists for each year of strata sampled
    for(i in Years){
      a <- tmp %>% dplyr::filter(YEAR == i)
      Filter = unique(a$ANALYSIS_STRATUM)

      ntot_filt <- NTOT_all %>%
        # filter to year i
        dplyr::filter(YEAR == i) %>%
        # filter to strata present in year i
        dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
        # re-calculate ntot
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- dplyr::bind_rows(ntot, ntot_filt)
    }


    ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

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
    # add an empty data frame to populate with the filtered NTOTs
    ntot <- data.frame()
    # create a data frame of the full NTOTs for FLK
    NTOT_all <- dplyr::bind_rows(FLK_2014_NTOT, FLK_2016_NTOT %>% dplyr::mutate(YEAR = 2015),
                                 FLK_2016_NTOT, FLK_2018_NTOT %>% dplyr::mutate(YEAR = 2017),
                                 FLK_2018_NTOT, FLK_2018_NTOT %>% dplyr::mutate(YEAR = 2019),
                                 FLK_2020_NTOT, FLK_2020_NTOT %>% dplyr::mutate(YEAR = 2021),
                                 FLK_2022_NTOT) %>%
      dplyr::mutate(REGION = "FLK",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

    # Use a loop to create a unique lists for each year of strata sampled
    for(i in Years){
      a <- tmp %>% dplyr::filter(YEAR == i)
      Filter = unique(a$ANALYSIS_STRATUM)

      ntot_filt <- NTOT_all %>%
        # filter to year i
        dplyr::filter(YEAR == i) %>%
        # filter to strata present in year i
        dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
        # re-calculate ntot
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- dplyr::bind_rows(ntot, ntot_filt)
    }

    ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))


  }


  if(project == "MIR"){

    ntot22 <- FLK_MIR_2022_NTOT %>%
      dplyr::ungroup() %>%
      # Remove strata not sampled - this will be unique to each year and need to be updated. See Tortugas for examples.
      dplyr::filter(STRAT_CORA != "CS05",
                    STRAT_CORA != "CS16",
                    ANALYSIS_STRATUM != "CS02 / PROT = 1",
                    ANALYSIS_STRATUM != "CS07 / PROT = 1",
                    ANALYSIS_STRATUM != "CS12 / PROT = 0",
                    ANALYSIS_STRATUM != "CS14 / PROT = 1",
                    ANALYSIS_STRATUM != "CS15 / PROT = 1") %>%
      dplyr::mutate(YEAR = 2022,
                    ngrtot = sum(NTOT))

    ntot <- ntot22

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
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "),
                    PROT = as.integer(PROT)) %>%
      dplyr::filter(ANALYSIS_STRATUM != "T09 / PROT = 0",
                    ANALYSIS_STRATUM != "T09 / PROT = 1",
                    ANALYSIS_STRATUM != "T03 / PROT = 2",
                    ANALYSIS_STRATUM != "T06 / PROT = 2") %>% #Not sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot22 <- Tort_2022_NTOT %>%
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::filter(PROT != 0) %>%
      dplyr::filter(ANALYSIS_STRATUM != "T07 / PROT = 1",
                    ANALYSIS_STRATUM != "T09 / PROT = 1",
                    ANALYSIS_STRATUM != "T04 / PROT = 2") %>% #Not sampled
      dplyr::mutate(ngrtot = sum(NTOT))

    ntot <- dplyr::bind_rows(ntot14, ntot16, ntot18, ntot20, ntot22)

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
    # add an empty data frame to populate with the filtered NTOTs
    ntot <- data.frame()
    # create a data frame of the full NTOTs for FLK
    NTOT_all <- dplyr::bind_rows(Tort_2014_NTOT, Tort_2016_NTOT %>% dplyr::mutate(YEAR = 2015),
                                 Tort_2016_NTOT, Tort_2018_NTOT %>% dplyr::mutate(YEAR = 2017),
                                 Tort_2018_NTOT, Tort_2018_NTOT %>% dplyr::mutate(YEAR = 2019),
                                 Tort_2020_NTOT, Tort_2020_NTOT %>% dplyr::mutate(YEAR = 2021),
                                 Tort_2022_NTOT) %>%
      dplyr::mutate(REGION = "Tortugas",
                    ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

    # Use a loop to create a unique lists for each year of strata sampled
    for(i in Years){
      a <- tmp %>% dplyr::filter(YEAR == i)
      Filter = unique(a$ANALYSIS_STRATUM)

      ntot_filt <- NTOT_all %>%
        # filter to year i
        dplyr::filter(YEAR == i) %>%
        # filter to strata present in year i
        dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
        # re-calculate ntot
        dplyr::mutate(ngrtot = sum(NTOT))

      ntot <- dplyr::bind_rows(ntot, ntot_filt)
    }

    ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

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

  ntot13 <- FGBNMS_2022_NTOT %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                  PROT = NA_character_,
                  YEAR = 2013) %>%
    dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH_STRAT, PROT) %>%
    dplyr::summarise(NTOT = sum(NTOT),
                     ngrtot = sum(NTOT)) %>%
    dplyr::ungroup()


  ntot15 <- FGBNMS_2022_NTOT %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                  PROT = NA_character_,
                  YEAR = 2015) %>%
    dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH_STRAT, PROT) %>%
    dplyr::summarise(NTOT = sum(NTOT),
                     ngrtot = sum(NTOT)) %>%
    dplyr::ungroup()

  ntot18 <- FGBNMS_2022_NTOT %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                  PROT = NA_character_,
                  YEAR = 2018) %>%
    dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH_STRAT, PROT) %>%
    dplyr::summarise(NTOT = sum(NTOT),
                     ngrtot = sum(NTOT)) %>%
    dplyr::ungroup()

  ntot22 <- FGBNMS_2022_NTOT %>%
    dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS",
                  PROT = NA_character_) %>%
    dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, DEPTH_STRAT, PROT) %>%
    dplyr::summarise(NTOT = sum(NTOT),
                     ngrtot = sum(NTOT)) %>%
    dplyr::ungroup()

  ntot <- dplyr::bind_rows(ntot13, ntot15, ntot18, ntot22)

}


ntot <- ntot %>%
  dplyr::mutate(wh = NTOT/ngrtot) %>%
  dplyr::mutate(PROT = as.factor(PROT))

################
# Export
################

return(ntot)
}
