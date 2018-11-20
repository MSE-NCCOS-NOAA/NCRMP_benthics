## Function to calculate colony density for combined NCRMP and FRRP data

# Purpose:
# creates csv files with colony density.


## Tag: data analysis


# outputs created in this file --------------
# density_site
# unwh_density_strata,
# Domain_est


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Viehman, Bauer, Groves
# Last update: Sept 2018
# Current status: Needs review


##############################################################################################################################

#' Creates colony density summary dataframes
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and FRRP combined
#' @param region A string indicating the region
#' @param analysis_strat A string indicating the analysis level strata
#' @param species_filter A string indicating whether to filter to a subset of species
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_FRRP_calculate_colony_density <- function(project, region, species_filter = "NULL", analysis_strat = "NULL"){

  ### Create species filters

  # First 6 species are from sample allocation, remainder are ESA listed species
  FLK_filter <- c("COL NATA", "MON CAVE", "ORB FAVE", "POR PORI", "SID SIDE", "SOL BOUR", "ACR CERV", "ACR PALM",
                  "ORB ANNU", "ORB FRAN", "DEN CYLI", "MYC FERO")

  Tort_filter <- c("COL NATA", "MON CAVE", "ORB FAVE", "POR PORI", "ORB FRAN", "STE INTE", "ACR CERV", "ACR PALM",
                   "ORB ANNU", "DEN CYLI", "MYC FERO")

  SEFCRI_filter <- c("ACR CERV", "DIC STOK", "MON CAVE", "POR ASTR", "PSE STRI", "SID SIDE","ACR CERV", "ACR PALM",
                     "ORB ANNU", "ORB FRAN", "ORB FAVE", "DEN CYLI", "MYC FERO")

  STX_filter <- c("COL NATA", "ORB ANNU", "MEA MEAN", "MAD DECA", "ORB FRAN", "ORB FAVE", "ACR CERV", "ACR PALM",
                  "ORB ANNU", "DEN CYLI", "MYC FERO")

  # First 5 species are from sample allocation, remainder are ESA listed species
  STTSTJ_filter <- c("COL NATA", "ORB ANNU", "DIP LABY", "MAD DECA", "ORB FAVE", "ACR CERV", "ACR PALM",
                     "ORB FRAN", "DEN CYLI", "MYC FERO")

  PR_filter <- c("COL NATA", "ORB ANNU", "DIP LABY", "MAD DECA", "ORB FAVE", "ACR CERV", "ACR PALM",
                 "ORB FRAN", "DEN CYLI", "MYC FERO")

  # Top 10 most abundant species - minus POR ASTR
  GOM_filter <- c("ORB FRAN", "PSE STRI", "AGA AGAR", "MON CAVE", "STE INTE", "COL NATA", "MAD DECA",
                  "ORB FAVE", "MAD AURE")


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


      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp2)

        dat_2stage <- rbind(tmp1, tmp3)
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp2) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

        dat_2stage <- rbind(tmp1, tmp3) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)
      }

    }

    if(region == "FLK"){

      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- FRRP_FLK_2017_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "FRRP")


      if(species_filter == "FALSE" ||
         species_filter == "NULL") {

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2)

        dat_2stage <- rbind(tmp3)
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)

        dat_2stage <- rbind(tmp3) %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)
      }
    }

    if(region == "Tortugas"){

      tmp1 <- TortugasMarq_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- TortugasMarq_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- FRRP_Tort_2017_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "FRRP")


      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2)

        dat_2stage <- rbind(tmp3)
      }

      if(species_filter == "TRUE"){

        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

        dat_2stage <- rbind(tmp3) %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)
      }
    }

  }

  if(project == "NCRMP"){

    if(region == "SEFCRI"){

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        dat_2stage <- SEFCRI_2014_2stage_coral_demographics %>%
          dplyr::mutate(SURVEY = "NCRMP")

        dat_1stage <- SEFCRI_2016_coral_demographics %>%
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){
        dat_2stage <- SEFCRI_2014_2stage_coral_demographics %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

        dat_1stage <- SEFCRI_2016_coral_demographics %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)
      }
    }

    if(region == "FLK"){


      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(YEAR = 2014)

      tmp2 <- FLK_2016_coral_demographics


      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2)  %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)

      }
    }

    if(region == "Tortugas"){


      tmp1 <- TortugasMarq_2014_coral_demographics

      tmp2 <- TortugasMarq_2016_coral_demographics

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

      }
    }

    # Carib / GOM

    if(region == "STTSTJ"){

      tmp1 <- USVI_2013_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp2 <- USVI_2015_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      tmp3 <- USVI_2017_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% STTSTJ_filter)
      }
    }

    if(region == "STX"){

      tmp1 <- USVI_2015_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      tmp2 <- USVI_2017_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% STX_filter)
      }

    }

    if(region == "PRICO"){

      tmp1 <- PRICO_2014_coral_demographics

      tmp2 <- PRICO_2016_coral_demographics %>%
        dplyr::mutate(YEAR = 2016)

      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP")

      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% PR_filter)

      }

    }

    if(region == "GOM"){

      tmp1 <- FGBNMS_2013_coral_demographics

      tmp2 <- FGBNMS_2015_coral_demographics

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM")
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM") %>%
          dplyr::filter(SPECIES_CD %in% GOM_filter)
      }

    }

  }



  # Calculate coral density

  if(project == "NCRMP_FRRP" ||
     project == "NCRMP" && region == "SEFCRI") {

    dat1_1stage <- dat_1stage %>%
      dplyr::ungroup() %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::select(-ABUNDANCE, -METERS_COMPLETED)%>%
      dplyr::ungroup()

    dat1_2stage <- dat_2stage %>%
      dplyr::filter(N == 1) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY_transect = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(DENSITY = mean(DENSITY_transect)) %>%
      dplyr::select(-METERS_COMPLETED)%>%
      dplyr::ungroup() %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    density_site <- rbind(dat1_1stage, dat1_2stage)

  } else {

    density_site <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::select(-ABUNDANCE, -METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))


  }


  # Run through the weighting function based on what you analysis stratum is

  if(analysis_strat == "STRAT_PROT" ||
     analysis_strat == "NULL"){

    # FL Analysis strat = STRAT + PROT (default; will work for Carib and GOM regions if analysis_strat is not specified.)
    tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = density_site, region, datatype = "density")

  }

  if(analysis_strat == "STRAT"){

    # FL Analysis strat = STRAT
    tmp  <- NCRMP_make_weighted_demo_data_RC(project, inputdata = density_site, region, datatype = "density")

  }

  if(analysis_strat == "HABITAT_DEPTH"){

    # Carib/GOM Analysis strat = HABITAT CODE + DEPTH STRAT
    tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = density_site, region, datatype = "density")

  }

  if(analysis_strat == "HABITAT"){

    # Carib/GOM Analysis strat = HABITAT CODE
    tmp  <- NCRMP_make_weighted_demo_data_RC(project, inputdata = density_site, region, datatype = "density")

  }



  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "density_site" = density_site,
    "unwh_density_strata" = unwh_density_strata,
    "Domain_est" = Domain_est)

  return(output)

}



