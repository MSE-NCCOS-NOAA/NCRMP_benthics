## Function to create a complete  calculate disease prevalence for NCRMP and NCRMP + DRM data (FL only)

# Purpose:
# creates csv files with disease prevalence by region


## Tag: data analysis


# outputs created in this file --------------
# disease_prev_site
# disease_prev_strata
# Domain estimates

# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Apr 2019


##############################################################################################################################

#' Creates species list, species richness and species diversity dataframes from NCRMP and DRM data
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_DRM_calculate_disease_prevalence <- function(project, region, species_filter = "NULL"){

  ### Create species filters

  FLK_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                  "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN",
                  "SID SIDE",
                  "POR PORI"
  )

  Tort_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                   "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN",
                   "SID SIDE",
                   "POR PORI"
  )

  SEFCRI_filter <- c("ACR CERV", "MON CAVE", "ACR PALM", "PSE STRI", "PSE CLIV",
                     "ORB ANNU", "ORB FRAN", "ORB FAVE",  "COL NATA", "DIP LABY","STE INTE", "MEA MEAN",
                     "SID SIDE",
                     "POR PORI"
  )

  STX_filter <- c("COL NATA", "MEA MEAN", "ORB FRAN", "ORB FAVE", "ACR CERV", "ACR PALM", "AGA AGAR", "AGA LAMA",
                  "ORB ANNU", "DIP LABY", "MON CAVE", "POR PORI", "PSE STRI", "PSE CLIV", "SID SIDE", "STE INTE")

  STTSTJ_filter <- c("COL NATA", "ORB ANNU", "ORB FAVE", "ACR CERV", "ACR PALM", "AGA AGAR", "AGA LAMA",
                     "ORB FRAN", "DIP LABY", "MON CAVE", "POR PORI", "PSE STRI", "PSE CLIV",  "MEA MEAN", "SID SIDE", "STE INTE")

  PR_filter <- c("COL NATA", "ORB ANNU", "DIP LABY", "POR PORI", "ORB FAVE", "ACR CERV", "ACR PALM",
                 "ORB FRAN", "MON CAVE", "PSE STRI", "SID SIDE", "AGA AGAR", "AGA LAMA", "DEN CYLI")

  GOM_filter <- c("ORB FRAN", "PSE STRI", "AGA AGAR", "MON CAVE", "STE INTE", "COL NATA", "MAD DECA",
                  "ORB FAVE", "MAD AURE", "DIP LABY", "ORB ANNU", "POR ASTR", "PSE CLIV", "SID SIDE")


  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")

  # Load data
  # Florida

  if(project == "NCRMP_DRM"){

    if(region == "SEFCRI"){


      tmp1 <- SEFCRI_2014_2stage_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp2 <- SEFCRI_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp3 <- DRM_SEFCRI_2018_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp4 <- SEFCRI_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))



      if(species_filter == "FALSE" ||
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
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      YEAR = 2014)

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp3 <- DRM_FLK_2018_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

       tmp4 <- FLK_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


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
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp2 <- TortugasMarq_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp3 <- Tortugas_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp4 <- FRRP_Tort_2017_2stage_demo_data %>%
        dplyr::mutate(SURVEY = "DRM",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


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


      tmp1 <- SEFCRI_2014_2stage_coral_demographics

      tmp2 <- SEFCRI_2016_coral_demographics

      tmp3 <- SEFCRI_2018_coral_demographics

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        dat_2stage <- tmp1 %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

         dat_1stage <- dplyr::bind_rows(tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


      }

      if(species_filter == "TRUE"){
        dat_2stage <- tmp1 %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

        dat_1stage <- rbind(tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)
      }
    }

    if(region == "FLK"){


      tmp1 <- FLK_2014_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      YEAR = 2014)

      tmp2 <- FLK_2016_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

       tmp3 <- FLK_2018_coral_demographics %>%
        dplyr::mutate(SURVEY = "NCRMP",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp3)
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp3)  %>%
          dplyr::filter(SPECIES_CD %in% FLK_filter)

      }
    }

    if(region == "Tortugas"){


      tmp1 <- TortugasMarq_2014_coral_demographics

      tmp2 <- TortugasMarq_2016_coral_demographics

      tmp3 <- Tortugas_2018_coral_demographics

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

        dat_2stage <- tmp3 %>%
          dplyr::mutate(SURVEY = "NCRMP/DRM",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% Tort_filter)

        dat_2stage <- tmp3 %>%
          dplyr::mutate(SURVEY = "NCRMP/DRM",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))%>%
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
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
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
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))
      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% STX_filter)
      }

    }

    if(region == "PRICO"){

      tmp1 <- PRICO_2014_coral_demographics %>%
        dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

      tmp2 <- PRICO_2016_coral_demographics %>%
        dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      YEAR = 2016)

      tmp3 <- PRICO_2019_coral_demographics

      if(species_filter == "FALSE"||
         species_filter == "NULL"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP")

      }

      if(species_filter == "TRUE"){

        #Combine 1 stage or 2 stage data
        dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3) %>%
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
                        REGION = "GOM",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" ))
      }

      if(species_filter == "TRUE"){
        #Combine 1 stage or 2 stage data
        dat_1stage <- rbind(tmp1, tmp2, tmp3) %>%
          dplyr::mutate(SURVEY = "NCRMP",
                        STRAT = "FGBNMS",
                        REGION = "GOM",
                        DATE = paste(MONTH, DAY, YEAR, sep = "/" )) %>%
          dplyr::filter(SPECIES_CD %in% GOM_filter)
      }

    }

  }

  # Calulate site level disease prevalence

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    dat1_1stage <- dat_1stage %>%
      dplyr::filter(N == 1,
                    DISEASE != "NA",
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100) %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV))) %>%
      dplyr::ungroup()


    dat1_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    DISEASE != "NA",
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1)) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100) %>%
      dplyr::group_by(SURVEY, REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = mean(Total_dis),
                       Total_col = mean(Total_col),
                       DIS_PREV = mean(DIS_PREV)) %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)) )

    disease_prev_site <-dplyr::bind_rows(dat1_1stage, dat1_2stage)

  } else {

    disease_prev_site <- dat_1stage %>%
      dplyr::filter(N == 1,
                    DISEASE != "NA",
                    SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::mutate(PROT = as.factor(PROT),
                    DISEASE = dplyr::case_when(DISEASE == "A" ~ 0,
                                               DISEASE == "P" ~ 1)) %>%
      dplyr::group_by(REGION, YEAR, DATE, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT) %>%
      dplyr::summarise(Total_dis = sum(DISEASE),
                       Total_col = sum(N),
                       DIS_PREV = (Total_dis/Total_col)*100) %>%
      dplyr::mutate(DIS_PREV = as.numeric(sprintf("%0.1f", DIS_PREV)) )

  }

  # Run through the weighting function

  tmp  <- NCRMP_make_weighted_demo_data(project, inputdata = disease_prev_site, region, datatype = "disease")

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])





  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "disease_prev_site" = disease_prev_site,
    "dis_prev_strata" = dis_prev_strata,
    "Domain_est" = Domain_est)

  return(output)






}
