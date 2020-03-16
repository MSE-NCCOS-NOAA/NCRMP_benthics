## Function to calculate colony mean (old and new) mortality at the site and strata level

# Purpose:
# creates csv files with mean mortality.


## Tag: data analysis


# outputs created in this file --------------
# old_mortality_site
# new_mortality_site
# Domain estimates



# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Mar 2020


##############################################################################################################################

#' Creates colony mortality summary dataframes
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

NCRMP_DRM_calculate_mortality <- function(project, region, species_filter = "NULL", analysis_strat = "NULL"){

  ### Create species filters - developed during 2019 status report juridictional meetings

  FLK_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                  "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN"
                  ,
                     "SID SIDE",
                     "POR PORI"
                  )

  Tort_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                   "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN"
                   ,
                     "SID SIDE",
                     "POR PORI"
                   )

  SEFCRI_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                     "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN"
                     ,
                     "SID SIDE",
                     "POR PORI"
                     )

  STX_filter <- c("COL NATA", "ORB ANNU", "MEA MEAN", "ORB FRAN", "ORB FAVE", "ACR CERV", "ACR PALM",
                  "ORB ANNU", "DIP LABY", "MON CAVE", "POR PORI", "PSE STRI", "PSE CLIV", "SID SIDE", "STE INTE")

  STTSTJ_filter <- c("COL NATA", "ORB ANNU", "ORB FAVE", "ACR CERV", "ACR PALM",
                     "ORB FRAN", "DIP LABY", "MON CAVE", "POR PORI", "PSE STRI", "PSE CLIV",  "MEA MEAN", "SID SIDE", "STE INTE")

  PR_filter <- c("COL NATA", "ORB ANNU", "DIP LABY", "POR PORI", "ORB FAVE", "ACR CERV", "ACR PALM",
                 "ORB FRAN", "DEN CYLI", "MON CAVE", "PSE STRI", "SID SIDE", "AGA AGAR", "AGA LAMA")


  GOM_filter <- c("ORB FRAN", "PSE STRI", "AGA AGAR", "MON CAVE", "STE INTE", "COL NATA", "MAD DECA",
                  "ORB FAVE", "MAD AURE", "DIP LABY", "ORB ANNU", "POR ASTR", "PSE CLIV", "SID SIDE")


  # Florida

  if(project == "NCRMP_DRM"){

    if(region == "SEFCRI"){


      tmp1 <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp2 <- SEFCRI_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- DRM_SEFCRI_2018_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "DRM")

      tmp4 <- SEFCRI_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp2, tmp4)

        dat_2stage <- rbind(tmp1, tmp3)

      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp2, tmp4) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

        dat_2stage <- rbind(tmp1, tmp3) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)
      }
    }

    if(region == "FLK"){

      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP") %>%
        dplyr::mutate(YEAR = 2014)

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")

      tmp3 <- DRM_FLK_2018_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "DRM")

      tmp4 <- FLK_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP")


      if(species_filter == "FALSE" ||
         species_filter == "NULL") {

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp4)

        dat_2stage <- rbind(tmp3)
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp4) %>%
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

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM")

      tmp4 <- FRRP_Tort_2017_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "DRM")


      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2)

        dat_2stage <- rbind(tmp3, tmp4)
      }

      if(species_filter == "TRUE"){

        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

        dat_2stage <- rbind(tmp3, tmp4) %>%
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

        dat_1stage <- dplyr::bind_rows(SEFCRI_2016_coral_demographics, SEFCRI_2018_coral_demographics)%>%
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

      tmp2 <- dplyr::bind_rows(FLK_2016_coral_demographics, FLK_2018_coral_demographics)


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

      tmp3 <- Tortugas_2018_coral_demographics

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP")

        dat_2stage <- tmp3 %>%
          dplyr::mutate(SURVEY = "NCRMP/DRM")
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

        dat_2stage <- tmp3 %>%
          dplyr::mutate(SURVEY = "NCRMP/DRM")%>%
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

      tmp4 <- USVI_2019_coral_demographics %>%
        dplyr::filter(REGION == "STTSTJ")

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% STTSTJ_filter)
      }

    }

    if(region == "STX"){

      tmp1 <- USVI_2015_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      tmp2 <- USVI_2017_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      tmp3 <- USVI_2019_coral_demographics %>%
        dplyr::filter(REGION == "STX")

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% STX_filter)
      }

    }

    if(region == "PRICO"){

      tmp1 <- PRICO_2014_coral_demographics

      tmp2 <- PRICO_2016_coral_demographics %>%
        dplyr::mutate(YEAR = 2016)

      tmp3 <- PRICO_2019_coral_demographics


      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP")

      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp3) %>%
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

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

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

  tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = old_mortality_site, region, datatype = "mortality")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])

  old_mortality_strata <- mortality_strata %>%
    dplyr::mutate(MORT_TYPE = "Old")
  Domain_est_old_mort <- Domain_est %>%
    dplyr::mutate(MORT_TYPE = "Old")


  # Recent mortality

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI"||
     project == "NCRMP" && region == "Tortugas") {

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

  tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = recent_mortality_site, region, datatype = "mortality")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  rec_mortality_strata <- mortality_strata %>%
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
    "old_mortality_strata" =old_mortality_strata,
    "rec_mortality_strata" = rec_mortality_strata,
    "Domain_est_old_mort" = Domain_est_old_mort,
    "Domain_est_rec_mort" = Domain_est_rec_mort)

  return(output)

}
