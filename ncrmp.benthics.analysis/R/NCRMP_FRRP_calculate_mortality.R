## Function to calculate colony mean (old and new) mortality at the site and strata level

# Purpose:
# creates csv files with mean mortality.


## Tag: data analysis


# outputs created in this file --------------
# old_mortality_site
# new_mortality_site
# wh_old_mortality_strata
# wh_new_mortality_strata
# Domain estimates



# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Nov 2018


##############################################################################################################################

#' Creates colony density and colony size summary dataframes
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param region A string indicating the region
#' @param species_filter A boolean value indicating whether to filter to a subset of species
#' @param analysis_strat A string indicating the analysis level strata
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'

NCRMP_FRRP_calculate_mortality <- function(project, region, species_filter = "NULL", analysis_strat = "NULL"){

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


  # Florida

  if(project == "NCRMP_FRRP"){

    if(region == "SEFCRI"){


      tmp1 <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- SEFCRI_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- FRRP_SEFCRI_2017_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "FRRP")

      if(species_filter == "FALSE"||
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

      tmp3 <- FGBNMS_2018_coral_demographics

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM")
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM") %>%
          dplyr::filter(SPECIES_CD %in% GOM_filter)
      }

    }



  }

  # Calculate mean mortality

  # Old mortality

  if(project == "NCRMP_FRRP" ||
     project == "NCRMP" && region == "SEFCRI") {

    dat1_1stage <- dat_1stage %>%
      # filter to corals present, remove Marquesas and any corals not sampled for mortality
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    OLD_MORT != "NA",
                    OLD_MORT <= 100) %>%
      # change PROT to factor
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # calculate site level mortality
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(avsitemort = mean(OLD_MORT)) %>%
      dplyr::mutate(MORT_TYPE = "Old") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    dat1_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    OLD_MORT != "NA",
                    OLD_MORT <= 100) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # when data is two stage (two transects or more) calculate transect mean before site mean.
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(transect_mort = mean(OLD_MORT)) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(avsitemort = mean(transect_mort)) %>%
      dplyr::mutate(MORT_TYPE = "Old") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    old_mortality_site <- rbind(dat1_1stage, dat1_2stage)

  } else {

    old_mortality_site <- dat_1stage %>%
      # filter to corals present, remove Marquesas and any corals not sampled for mortality
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    OLD_MORT != "NA",
                    OLD_MORT <= 100) %>%
      # change PROT to factor
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # calculate site level mortality
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(avsitemort = mean(OLD_MORT)) %>%
      dplyr::mutate(MORT_TYPE = "Old") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

  }

  # Apply weighting scheme and calculate strata and regional means

  if(analysis_strat == "STRAT_PROT" ||
     analysis_strat == "NULL"){

    # FL Analysis strat = STRAT + PROT (default; will work for Carib and GOM regions if analysis_strat is not specified.)
    tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = old_mortality_site, region, datatype = "mortality")

  }

  if(analysis_strat == "STRAT"){

    # FL Analysis strat = STRAT
    tmp  <- NCRMP_make_weighted_demo_data_RC(project, inputdata = old_mortality_site, region, datatype = "mortality")

  }

  if(analysis_strat == "HABITAT_DEPTH"){

    # Carib/GOM Analysis strat = HABITAT CODE + DEPTH STRAT
    tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = old_mortality_site, region, datatype = "mortality")

  }

  if(analysis_strat == "HABITAT"){

    # Carib/GOM Analysis strat = HABITAT CODE
    tmp  <- NCRMP_make_weighted_demo_data_RC(project, inputdata = old_mortality_site, region, datatype = "mortality")

  }

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])

  unwh_old_mortality_strata <- unwh_mortality_strata %>%
    dplyr::mutate(MORT_TYPE = "Old")
  Domain_est_old_mort <- Domain_est %>%
    dplyr::mutate(MORT_TYPE = "Old")


  # Recent mortality

  if(project == "NCRMP_FRRP" ||
     project == "NCRMP" && region == "SEFCRI") {

    dat1_1stage <- dat_1stage %>%
      # filter to corals present, remove Marquesas and any corals not sampled for mortality
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    RECENT_MORT != "NA",
                    RECENT_MORT <= 100) %>%
      # change PROT to factor
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # calculate site level mortality
      dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(avsitemort = mean(RECENT_MORT)) %>%
      dplyr::mutate(MORT_TYPE = "Recent") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    dat1_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    RECENT_MORT != "NA",
                    RECENT_MORT <= 100) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # when data is two stage (two transects or more) calculate transect mean before site mean.
      dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(transect_mort = mean(RECENT_MORT)) %>%
      dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(avsitemort = mean(transect_mort)) %>%
      dplyr::mutate(MORT_TYPE = "Recent") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    recent_mortality_site <- rbind(dat1_1stage, dat1_2stage)

  } else {

    recent_mortality_site <- dat_1stage %>%
      # filter to corals present, remove Marquesas and any corals not sampled for mortality
      dplyr::filter(N == 1,
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    RECENT_MORT != "NA",
                    RECENT_MORT <= 100) %>%
      # change PROT to factor
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      # calculate site level mortality
      dplyr::group_by(SURVEY, YEAR, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>% #No need to include region, will be added from ntot in wh. function
      dplyr::summarise(avsitemort = mean(RECENT_MORT)) %>%
      dplyr::mutate(MORT_TYPE = "Recent") %>%
      dplyr::ungroup() %>%
      # update some column classes to make them compatible with pre NCRMP data
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

  }

  # Apply weighting scheme and calculate strata and regional means

  if(analysis_strat == "STRAT_PROT" ||
     analysis_strat == "NULL"){

    # FL Analysis strat = STRAT + PROT (default; will work for Carib and GOM regions if analysis_strat is not specified.)
    tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = recent_mortality_site, region, datatype = "mortality")

  }

  if(analysis_strat == "STRAT"){

    # FL Analysis strat = STRAT
    tmp  <- NCRMP_make_weighted_demo_data_RC(project, inputdata = recent_mortality_site, region, datatype = "mortality")

  }

  if(analysis_strat == "HABITAT_DEPTH"){

    # Carib/GOM Analysis strat = HABITAT CODE + DEPTH STRAT
    tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = recent_mortality_site, region, datatype = "mortality")

  }

  if(analysis_strat == "HABITAT"){

    # Carib/GOM Analysis strat = HABITAT CODE
    tmp  <- NCRMP_make_weighted_demo_data_RC(project, inputdata = recent_mortality_site, region, datatype = "mortality")

  }

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  unwh_rec_mortality_strata <- unwh_mortality_strata %>%
    dplyr::mutate(MORT_TYPE = "Recent")
  Domain_est_rec_mort <- Domain_est %>%
    dplyr::mutate(MORT_TYPE = "Recent")



  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "old_mortality_site" = old_mortality_site,
    "recent_mortality_site" = recent_mortality_site,
    "unwh_old_mortality_strata" = unwh_old_mortality_strata,
    "unwh_rec_mortality_strata" = unwh_rec_mortality_strata,
    "Domain_est_old_mort" = Domain_est_old_mort,
    "Domain_est_rec_mort" = Domain_est_rec_mort)

  return(output)

}
