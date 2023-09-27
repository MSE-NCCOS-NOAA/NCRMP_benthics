
## Function to load NTOT files and calculate wh for species specific analyses

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
# Last update: Aug 2023


##############################################################################################################################




#' Creates weighted demo data, for individual species
#'
#' @param region A string indicating the region
#' @param inputdata A dataframe
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#'
#' @importFrom magrittr "%>%"
#' @export
#'



load_NTOT_species <- function(region, inputdata, project){


  if(region == "FLK"){

    if(project == "NCRMP" || project == "NULL"){
      # Use a loop to create a unique lists for each year of strata sampled
      # Filter NTOT to only strata sampled that year (previously done manually)
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM), .groups = "keep")

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # make a list of all the species
      spp <- unique(tmp$SPECIES_CD)
      # add an empty data frame to populate with the filtered NTOTs
      ntot <- data.frame()
      # create a data frame of the full NTOTs for FLK
      NTOT_all <- dplyr::bind_rows(FLK_2014_NTOT, FLK_2016_NTOT, FLK_2018_NTOT, FLK_2020_NTOT,FLK_2022_NTOT) %>%
        dplyr::mutate(REGION = "FLK",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

      # Use a loop to create a unique lists for each year of strata sampled
      for(s in spp){
        for(i in Years){
          a <- tmp %>% dplyr::filter(YEAR == i & SPECIES_CD == s)
          Filter = unique(a$ANALYSIS_STRATUM)

          ntot_filt <- NTOT_all %>%
            # filter to year i
            dplyr::filter(YEAR == i) %>%
            # filter to strata present in year i
            dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
            # re-calculate ntot
            dplyr::mutate(ngrtot = sum(NTOT),
                          SPECIES_CD = s)

          ntot <- dplyr::bind_rows(ntot, ntot_filt)
        }
      }


      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))
    }

    if(project == "NCRMP_DRM"){
      # Filter NTOT to only strata each species occurs in each year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM), .groups = "keep")

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # make a list of all the species
      spp <- unique(tmp$SPECIES_CD)
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

      # Use a loop to create a unique lists for each year of strata sampled for each species
      for(s in spp){
        for(i in Years){
          a <- tmp %>% dplyr::filter(YEAR == i & SPECIES_CD == s)
          Filter = unique(a$ANALYSIS_STRATUM)

          ntot_filt <- NTOT_all %>%
            # filter to year i and species s
            dplyr::filter(YEAR == i) %>%
            # filter to strata present in year i
            dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
            # re-calculate ntot
            dplyr::mutate(ngrtot = sum(NTOT),
                          SPECIES_CD = s)

          ntot <- dplyr::bind_rows(ntot, ntot_filt)
        }
      }

      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))
    }
  }




  #### Read in ntot ####

  if(region == "SEFCRI") {

    if(project == "NCRMP" || project == "NULL"){

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # make a list of all the species
      spp <- unique(tmp$SPECIES_CD)
      # add an empty data frame to populate with the filtered NTOTs
      ntot <- data.frame()
      # create a data frame of the full NTOTs for FLK
      NTOT_all <- dplyr::bind_rows(SEFL_2014_NTOT %>% dplyr::filter(STRAT == "MIDR1" | STRAT == "MIDR0"),
                                   SEFL_2016_NTOT %>% dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH2"~"NEAR1",
                                                                                             STRAT == "PTDP0"~"OFFR0",
                                                                                             STRAT == "PTDP1"~"OFFR1", TRUE ~ as.character(STRAT))),
                                   SEFL_2018_NTOT %>% dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH2"~"NEAR1",
                                                                                             STRAT == "PTDP0"~"OFFR0",
                                                                                             STRAT == "PTDP1"~"OFFR1", TRUE ~ as.character(STRAT))),
                                   SEFL_2020_NTOT,
                                   SEFL_2022_NTOT) %>%
        dplyr::mutate(REGION = "SEFCRI",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

      # Use a loop to create a unique lists for each year of strata sampled
      for(s in spp){
        for(i in Years){
          a <- tmp %>% dplyr::filter(YEAR == i)
          Filter = unique(a$ANALYSIS_STRATUM)

          ntot_filt <- NTOT_all %>%
            # filter to year i
            dplyr::filter(YEAR == i) %>%
            # filter to strata present in year i
            dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
            # re-calculate ntot
            dplyr::mutate(ngrtot = sum(NTOT),
                          SPECIES_CD = s)

          ntot <- dplyr::bind_rows(ntot, ntot_filt)
        }
      }

      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

    }

    if(project == "NCRMP_DRM"){

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # make a list of all the species
      spp <- unique(tmp$SPECIES_CD)
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
      for(s in spp){
        for(i in Years){
          a <- tmp %>% dplyr::filter(YEAR == i)
          Filter = unique(a$ANALYSIS_STRATUM)

          ntot_filt <- NTOT_all %>%
            # filter to year i
            dplyr::filter(YEAR == i) %>%
            # filter to strata present in year i
            dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
            # re-calculate ntot
            dplyr::mutate(ngrtot = sum(NTOT),
                          SPECIES_CD = s)

          ntot <- dplyr::bind_rows(ntot, ntot_filt)
        }
      }

      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

    }

  }



  if(region == "Tortugas") {

    if(project == "NCRMP" || project == "NULL"){

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # make list of species
      spp <- unique(tmp$SPECIES_CD)
      # add an empty data frame to populate with the filtered NTOTs
      ntot <- data.frame()
      # create a data frame of the full NTOTs for FLK
      NTOT_all <- dplyr::bind_rows(Tort_2014_NTOT,
                                   Tort_2016_NTOT,
                                   Tort_2018_NTOT,
                                   Tort_2020_NTOT,
                                   Tort_2022_NTOT) %>%
        dplyr::mutate(REGION = "Tortugas",
                      ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))

      # Use a loop to create a unique lists for each year of strata sampled
      for(s in spp){
        for(i in Years){
          a <- tmp %>% dplyr::filter(YEAR == i & SPECIES_CD == s)
          Filter = unique(a$ANALYSIS_STRATUM)

          ntot_filt <- NTOT_all %>%
            # filter to year i
            dplyr::filter(YEAR == i) %>%
            # filter to strata present in year i
            dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
            # re-calculate ntot
            dplyr::mutate(ngrtot = sum(NTOT),
                          SPECIES_CD = s)

          ntot <- dplyr::bind_rows(ntot, ntot_filt)
        }
      }

      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))


    }

    if(project == "NCRMP_DRM") {

      # Filter NTOT to only strata sampled that year
      # Make a dataframe of just the YEAR and STRAT
      tmp <- inputdata %>%
        dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
        dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
        dplyr::summarise(N = length(ANALYSIS_STRATUM))

      # Make a list of all the years
      Years <- sort(unique(tmp$YEAR))
      # make list of species
      spp <- unique(tmp$SPECIES_CD)
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
      for(s in spp){
        for(i in Years){
          a <- tmp %>% dplyr::filter(YEAR == i & SPECIES_CD == s)
          Filter = unique(a$ANALYSIS_STRATUM)

          ntot_filt <- NTOT_all %>%
            # filter to year i
            dplyr::filter(YEAR == i) %>%
            # filter to strata present in year i
            dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
            # re-calculate ntot
            dplyr::mutate(ngrtot = sum(NTOT),
                          SPECIES_CD = s)

          ntot <- dplyr::bind_rows(ntot, ntot_filt)
        }
      }

      ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

    }

  }

  if(region == "STTSTJ"){
    # Filter NTOT to only strata sampled that year
    # Make a dataframe of just the YEAR and STRAT
    tmp <- inputdata %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(N = length(ANALYSIS_STRATUM))

    # Make a list of all the years
    Years <- sort(unique(tmp$YEAR))
    # make list of species
    spp <- unique(tmp$SPECIES_CD)
    # add an empty data frame to populate with the filtered NTOTs
    ntot <- data.frame()
    # create a data frame of the full NTOTs for FLK
    NTOT_all <- dplyr::bind_rows(USVI_2021_NTOT %>% dplyr::filter(REGION == "STTSTJ") %>% dplyr::mutate(YEAR = 2013),
                                 USVI_2021_NTOT %>% dplyr::filter(REGION == "STTSTJ") %>% dplyr::mutate(YEAR = 2015),
                                 USVI_2021_NTOT %>% dplyr::filter(REGION == "STTSTJ") %>% dplyr::mutate(YEAR = 2017),
                                 USVI_2021_NTOT %>% dplyr::filter(REGION == "STTSTJ") %>% dplyr::mutate(YEAR = 2019),
                                 USVI_2021_NTOT %>% dplyr::filter(REGION == "STTSTJ")) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT)


    # Use a loop to create a unique lists for each year of strata sampled
    for(s in spp){
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i & SPECIES_CD == s)
        Filter = unique(a$ANALYSIS_STRATUM)

        ntot_filt <- NTOT_all %>%
          # filter to year i
          dplyr::filter(YEAR == i) %>%
          # filter to strata present in year i
          dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
          # re-calculate ntot
          dplyr::mutate(ngrtot = sum(NTOT),
                        SPECIES_CD = s)

        ntot <- dplyr::bind_rows(ntot, ntot_filt)
      }
    }

    ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

  }

  if(region == "STX"){

    # Filter NTOT to only strata sampled that year
    # Make a dataframe of just the YEAR and STRAT
    tmp <- inputdata %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(N = length(ANALYSIS_STRATUM))

    # Make a list of all the years
    Years <- sort(unique(tmp$YEAR))
    # make list of species
    spp <- unique(tmp$SPECIES_CD)
    # add an empty data frame to populate with the filtered NTOTs
    ntot <- data.frame()
    # create a data frame of the full NTOTs for FLK
    NTOT_all <- dplyr::bind_rows(USVI_2021_NTOT %>% dplyr::filter(REGION == "STX") %>% dplyr::mutate(YEAR = 2013),
                                 USVI_2021_NTOT %>% dplyr::filter(REGION == "STX") %>% dplyr::mutate(YEAR = 2015),
                                 USVI_2021_NTOT %>% dplyr::filter(REGION == "STX") %>% dplyr::mutate(YEAR = 2017),
                                 USVI_2021_NTOT %>% dplyr::filter(REGION == "STX") %>% dplyr::mutate(YEAR = 2019),
                                 USVI_2021_NTOT %>% dplyr::filter(REGION == "STX")) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT)


    # Use a loop to create a unique lists for each year of strata sampled
    for(s in spp){
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i & SPECIES_CD == s)
        Filter = unique(a$ANALYSIS_STRATUM)

        ntot_filt <- NTOT_all %>%
          # filter to year i
          dplyr::filter(YEAR == i) %>%
          # filter to strata present in year i
          dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
          # re-calculate ntot
          dplyr::mutate(ngrtot = sum(NTOT),
                        SPECIES_CD = s)

        ntot <- dplyr::bind_rows(ntot, ntot_filt)
      }
    }

    ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

  }

  if(region == "PRICO"){

    # Filter NTOT to only strata sampled that year
    # Make a dataframe of just the YEAR and STRAT
    tmp <- inputdata %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(N = length(ANALYSIS_STRATUM))

    # Make a list of all the years
    Years <- sort(unique(tmp$YEAR))
    # make list of species
    spp <- unique(tmp$SPECIES_CD)
    # add an empty data frame to populate with the filtered NTOTs
    ntot <- data.frame()
    # create a data frame of the full NTOTs for FLK
    NTOT_all <- dplyr::bind_rows(PRICO_2021_NTOT %>% dplyr::mutate(YEAR = 2014),
                                 PRICO_2021_NTOT %>% dplyr::mutate(YEAR = 2016),
                                 PRICO_2021_NTOT %>% dplyr::mutate(YEAR = 2019),
                                 PRICO_2021_NTOT) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT)


    # Use a loop to create a unique lists for each year of strata sampled
    for(s in spp){
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i & SPECIES_CD == s)
        Filter = unique(a$ANALYSIS_STRATUM)

        ntot_filt <- NTOT_all %>%
          # filter to year i
          dplyr::filter(YEAR == i) %>%
          # filter to strata present in year i
          dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
          # re-calculate ntot
          dplyr::mutate(ngrtot = sum(NTOT),
                        SPECIES_CD = s)

        ntot <- dplyr::bind_rows(ntot, ntot_filt)
      }
    }

    ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))


  }

  if(region == "GOM"){

    # Filter NTOT to only strata sampled that year
    # Make a dataframe of just the YEAR and STRAT
    tmp <- inputdata %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(N = length(ANALYSIS_STRATUM))

    # Make a list of all the years
    Years <- sort(unique(tmp$YEAR))
    # make list of species
    spp <- unique(tmp$SPECIES_CD)
    # add an empty data frame to populate with the filtered NTOTs
    ntot <- data.frame()
    # create a data frame of the full NTOTs for FLK
    NTOT_all <- dplyr::bind_rows(FGBNMS_2022_NTOT %>% dplyr::mutate(YEAR = 2013),
                                 FGBNMS_2022_NTOT %>% dplyr::mutate(YEAR = 2015),
                                 FGBNMS_2022_NTOT %>% dplyr::mutate(YEAR = 2018),
                                 FGBNMS_2022_NTOT) %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")


    # Use a loop to create a unique lists for each year of strata sampled
    for(s in spp){
      for(i in Years){
        a <- tmp %>% dplyr::filter(YEAR == i & SPECIES_CD == s)
        Filter = unique(a$ANALYSIS_STRATUM)

        ntot_filt <- NTOT_all %>%
          # filter to year i
          dplyr::filter(YEAR == i) %>%
          # filter to strata present in year i
          dplyr::filter(ANALYSIS_STRATUM %in% Filter) %>%
          # re-calculate ntot
          dplyr::mutate(ngrtot = sum(NTOT),
                        SPECIES_CD = s)

        ntot <- dplyr::bind_rows(ntot, ntot_filt)
      }
    }

    ntot <- ntot %>% dplyr::mutate(PROT = as.factor(PROT))

  }


  ntot <- ntot %>%
    dplyr::mutate(wh = NTOT/ngrtot) %>%
    dplyr::mutate(PROT = as.factor(PROT))

  ################
  # Export
  ################

  return(ntot)
}
