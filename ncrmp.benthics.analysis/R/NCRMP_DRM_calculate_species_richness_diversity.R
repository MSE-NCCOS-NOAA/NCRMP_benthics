## Function to create a complete species list, calculate species richness and species diversity for combined NCRMP and FRRP data (FL only)

# Purpose:
# creates csv files with species list, species richness and species diversity


## Tag: data analysis


# outputs created in this file --------------
# species_list
# richness_site
# unwh_richness_strata
# Domain_est
# species_diversity_site
# species_diversity_strata


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Viehman and Groves
# Last update: Apr 2019


##############################################################################################################################

#' Creates species list, species richness and species diversity dataframes from NCRMP and FRRP data
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and FRRP combined
#' @param region A string indicating the region
#' @param analysis_strat A string indicating the analysis level strata
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @importFrom vegan "diversity"
#' @export
#'
#'


NCRMP_DRM_calculate_species_richness_diversity <- function(project, region){


  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")

  # Load data

  # Florida

  if(project == "NCRMP_FRRP"){

    if(region == "SEFCRI"){


      tmp1 <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- SEFCRI_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- FRRP_SEFCRI_2017_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "FRRP")

      dat <- rbind(tmp1, tmp2, tmp3) %>%
        dplyr::mutate(PROT = as.factor(PROT))

      #Combine 1 stage or 2 stage data
      dat_1stage <- rbind(tmp2)

      dat_2stage <- rbind(tmp1, tmp3)

    }

    if(region == "FLK"){

      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- FRRP_FLK_2017_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "FRRP")

      dat <- rbind(tmp1, tmp2, tmp3)

      #Combine 1 stage or 2 stage data
      dat_1stage <- rbind(tmp1, tmp2)

      dat_2stage <- rbind(tmp3)

    }

    if(region == "Tortugas"){

      tmp1 <- TortugasMarq_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- TortugasMarq_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM")

      tmp4 <- FRRP_Tort_2017_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "DRM")

      dat <- rbind(tmp1, tmp2, tmp3, tmp4)

      #Combine 1 stage or 2 stage data
      dat_1stage <- rbind(tmp1, tmp2)

      dat_2stage <- rbind(tmp3, tmp4)
    }

  }

  if(project == "NCRMP"){

    if(region == "SEFCRI"){


      dat_2stage <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      dat_1stage <- SEFCRI_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      dat <- rbind(dat_1stage, dat_2stage)


    }

    if(region == "FLK"){

      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      YEAR = 2014)  # Some sampling occurred in 2015 - set all to 2014

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      dat <- rbind(tmp1, tmp2)

      #Combine 1 stage or 2 stage data
      dat_1stage <- rbind(tmp1, tmp2)

    }

    if(region == "Tortugas"){

      tmp1 <- TortugasMarq_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- TortugasMarq_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM")

      dat <- rbind(tmp1, tmp2, tmp3)

      #Combine 1 stage or 2 stage data
      dat_1stage <- rbind(tmp1, tmp2)

      dat_2stage <- tmp3


    }

    ## Carib / GOM

    if(region == "STTSTJ"){

      tmp1 <- USVI_2013_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp2 <- USVI_2015_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp3 <- USVI_2017_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp4 <- USVI_2019_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      # this get used for diversity
      dat <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
        dplyr::mutate(SURVEY = "NCRMP")

      # this get used for richness
      #Combine 1 stage or 2 stage data
      dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
        dplyr::mutate(SURVEY = "NCRMP")


    }

    if(region == "STX"){

      tmp1 <- USVI_2015_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      tmp2 <- USVI_2017_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      tmp3 <- USVI_2019_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      dat <- dplyr::bind_rows(tmp1, tmp2) %>%
        dplyr::mutate(SURVEY = "NCRMP")

      #Combine 1 stage or 2 stage data
      dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
        dplyr::mutate(SURVEY = "NCRMP")


    }

    if(region == "PRICO"){

      tmp1 <- PRICO_2014_coral_demographics

      tmp2 <- PRICO_2016_coral_demographics %>%
        dplyr::mutate(YEAR = 2016)

      tmp3 <- PRICO_2019_coral_demographics

      dat <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
        dplyr::mutate(SURVEY = "NCRMP")

      #Combine 1 stage or 2 stage data
      dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
        dplyr::mutate(SURVEY = "NCRMP")

    }

    if(region == "GOM"){

      tmp1 <- FGBNMS_2013_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      STRAT = "FGBNMS",
                      REGION = "GOM")

      tmp2 <- FGBNMS_2015_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      STRAT = "FGBNMS",
                      REGION = "GOM")

       tmp3 <- FGBNMS_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      dat <- rbind(tmp1, tmp2, tmp3)

      #Combine 1 stage or 2 stage data
      dat_1stage <- rbind(tmp1, tmp2, tmp3)

    }

  }

  # Clean up species names

  # this get used later for diversity
  dat <- dat %>%
    dplyr::mutate(SPECIES_NAME = dplyr::case_when(SPECIES_CD == "MEAN JACK" ~ "Meandrina jacksoni", TRUE ~ as.character(SPECIES_NAME) ),
                  SPECIES_NAME = dplyr::case_when(SPECIES_CD == "DIP STRI" ~ "Pseudodiploria strigosa", TRUE ~ as.character(SPECIES_NAME)),
                  SPECIES_NAME = dplyr::case_when(SPECIES_CD == "CLA ARBU" ~ "Cladacora arbuscula", TRUE ~ as.character(SPECIES_NAME)),
                  SPECIES_NAME = dplyr::case_when(SPECIES_CD == "DIP CLIV" ~ "Pseudodiploria clivosa", TRUE ~ as.character(SPECIES_NAME))) %>%
    dplyr::mutate(SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Pseudodiploria strigosa" ~ "PSE STRI", TRUE ~ as.character(SPECIES_CD)),
                  SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Pseudodiploria clivosa" ~ "PSE CLIV", TRUE ~ as.character(SPECIES_CD)),
                  SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Meandrina jacksoni" ~ "MEA JACK", TRUE ~ as.character(SPECIES_CD)))

  dat_1stage <- dat_1stage %>%
    dplyr::mutate(SPECIES_NAME = dplyr::case_when(SPECIES_CD == "MEAN JACK" ~ "Meandrina jacksoni", TRUE ~ as.character(SPECIES_NAME) ),
                  SPECIES_NAME = dplyr::case_when(SPECIES_CD == "DIP STRI" ~ "Pseudodiploria strigosa", TRUE ~ as.character(SPECIES_NAME)),
                  SPECIES_NAME = dplyr::case_when(SPECIES_CD == "CLA ARBU" ~ "Cladacora arbuscula", TRUE ~ as.character(SPECIES_NAME)),
                  SPECIES_NAME = dplyr::case_when(SPECIES_CD == "DIP CLIV" ~ "Pseudodiploria clivosa", TRUE ~ as.character(SPECIES_NAME))) %>%
    dplyr::mutate(SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Pseudodiploria strigosa" ~ "PSE STRI", TRUE ~ as.character(SPECIES_CD)),
                  SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Pseudodiploria clivosa" ~ "PSE CLIV", TRUE ~ as.character(SPECIES_CD)),
                  SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Meandrina jacksoni" ~ "MEA JACK", TRUE ~ as.character(SPECIES_CD)))


  if(project == "NCRMP_FRRP" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    dat_2stage <- dat_2stage %>%
      dplyr::mutate(SPECIES_NAME = dplyr::case_when(SPECIES_CD == "MEAN JACK" ~ "Meandrina jacksoni", TRUE ~ as.character(SPECIES_NAME) ),
                    SPECIES_NAME = dplyr::case_when(SPECIES_CD == "DIP STRI" ~ "Pseudodiploria strigosa", TRUE ~ as.character(SPECIES_NAME)),
                    SPECIES_NAME = dplyr::case_when(SPECIES_CD == "CLA ARBU" ~ "Cladacora arbuscula", TRUE ~ as.character(SPECIES_NAME)),
                    SPECIES_NAME = dplyr::case_when(SPECIES_CD == "DIP CLIV" ~ "Pseudodiploria clivosa", TRUE ~ as.character(SPECIES_NAME))) %>%
      dplyr::mutate(SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Pseudodiploria strigosa" ~ "PSE STRI", TRUE ~ as.character(SPECIES_CD)),
                    SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Pseudodiploria clivosa" ~ "PSE CLIV", TRUE ~ as.character(SPECIES_CD)),
                    SPECIES_CD = dplyr::case_when(SPECIES_NAME == "Meandrina jacksoni" ~ "MEA JACK", TRUE ~ as.character(SPECIES_CD)))

  }

  # Calculate species richness by site

  # Create a list of species present


  if(project == "NCRMP_FRRP" ||
     project == "NCRMP" && region == "SEFCRI"||
     project == "NCRMP" && region == "Tortugas") {
    # Clean up species
    species_1stage <- dat_1stage %>%
      dplyr::filter(N == 1,
                    !grepl('SPE.', SPECIES_CD),
                    !grepl('ANCX', SPECIES_CD),
                    SPECIES_CD != "OTH CORA") %>% # Filter out SPE
      dplyr::select(SPECIES_NAME, SPECIES_CD) %>%
      dplyr::distinct(SPECIES_NAME, SPECIES_CD)

    species_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    !grepl('SPE.', SPECIES_CD),
                    !grepl('ANCX', SPECIES_CD),
                    SPECIES_CD != "OTH CORA") %>%  # Filter out SPE
      dplyr::select(SPECIES_NAME, SPECIES_CD) %>%
      dplyr::distinct(SPECIES_NAME, SPECIES_CD)

    species_list <- species_1stage %>%
      rbind(., species_2stage) %>%
      unique(.)

    dat1_1stage <- dat_1stage %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    !grepl('SPE.', SPECIES_CD),
                    !grepl('ANCX', SPECIES_CD)) %>% # Filter out SPE
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_NAME) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(SppSumSite = sum(N)) %>%
      dplyr::mutate(present = 1) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(SPP_RICHNESS = sum(present)) %>%
      dplyr::ungroup()


    dat1_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    !grepl('SPE.', SPECIES_CD),
                    !grepl('ANCX', SPECIES_CD)) %>% # Filter out SPE
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_NAME) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(IndSumSite = sum(N))  %>%
      dplyr::mutate(present = 1) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(SppSumSite = sum(present)) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(SPP_RICHNESS = mean(SppSumSite)) %>%
      dplyr::ungroup()

    dat1 <- rbind(dat1_1stage, dat1_2stage)


  } else {

    # Clean up species
    species_list <- dat_1stage %>%
      dplyr::filter(N == 1,
                    !grepl('SPE.', SPECIES_CD),
                    !grepl('ANCX', SPECIES_CD),
                    SPECIES_CD != "OTH CORA") %>% # Filter out SPE
      dplyr::select(SPECIES_NAME, SPECIES_CD) %>%
      dplyr::distinct(SPECIES_NAME, SPECIES_CD) %>%
      unique(.)

    dat1_1stage <- dat_1stage %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    !grepl('SPE.', SPECIES_CD),
                    !grepl('ANCX', SPECIES_CD)) %>% # Filter out SPE
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, SPECIES_NAME) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(SppSumSite = sum(N)) %>%
      dplyr::mutate(present = 1) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(SPP_RICHNESS = sum(present)) %>%
      dplyr::ungroup()

    dat1 <- dat1_1stage

  }


  richness_site <- dat1

 # Run through the weighting function
  tmp  <- NCRMP_make_weighted_demo_data(project,
                                        inputdata = richness_site,
                                        region,
                                        datatype = "richness",
                                        species_filter = "FALSE")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  ###### Calculate coral diversity with Simpson, Inv. Simpson and Shannon Indices #####


    # Site level
    sites <- dat %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    !grepl('SPE.', SPECIES_CD),
                    !grepl('ANCX', SPECIES_CD))  %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT),
                    REGION = region) %>%
      dplyr::group_by(REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT) %>%
      dplyr::summarize(PSU = unique(PRIMARY_SAMPLE_UNIT)) %>%
      dplyr::ungroup()

    species_wide_site <- dat %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    !grepl('SPE.', SPECIES_CD),
                    !grepl('ANCX', SPECIES_CD)) %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT),
                    REGION = region) %>% #Convert PSU to character to make sure vegan::diversity does not use it as counts
      dplyr::group_by(REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT, SPECIES_NAME) %>%
      dplyr::summarise(SPP_Count = sum(N)) %>%
      tidyr::spread(., key = SPECIES_NAME, value = SPP_Count, fill = 0) %>%
      dplyr::ungroup()

    species_only_site <-  species_wide_site[, c(4, 10:ncol(species_wide_site))]

    species_diversity_site <- species_only_site %>%
      dplyr::mutate(Simpson =  vegan::diversity(species_only_site[, -1], index = "simpson")) %>%
      dplyr::mutate(Inv_Simpson =  vegan::diversity(species_only_site[, -1], index = "invsimpson")) %>%
      dplyr::mutate(Shannon =  vegan::diversity(species_only_site[, -1], index = "shannon")) %>%
      dplyr::select(PRIMARY_SAMPLE_UNIT, Simpson, Inv_Simpson, Shannon ) %>%
      dplyr::inner_join(., sites) %>%
      dplyr::select(REGION, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, PROT, Simpson, Inv_Simpson, Shannon) %>%
      dplyr::ungroup()


    # Calculate regional diversity using weighting function

    tmp  <- NCRMP_make_weighted_demo_data(project,
                                          inputdata = species_diversity_site,
                                          region,
                                          datatype = "diversity",
                                          species_filter = "FALSE")

    # unpack list
    for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])



  ################
  # Export
  ################


    # Create list to export
    output <- list(
      "species_list" = species_list,
      "richness_site" = richness_site,
      "richness_strata" = richness_strata,
      "Domain_est" = Domain_est,
      "species_diversity_site" = species_diversity_site,
      "diversity_strata" = diversity_strata,
      "Domain_est_div" = Domain_est_div
    )

    return(output)


}






