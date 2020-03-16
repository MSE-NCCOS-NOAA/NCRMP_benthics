## Function to calculate colony density for combined NCRMP and DRM  data

# Purpose:
# creates csv files with colony density.


## Tag: data analysis


# outputs created in this file --------------
# density_site
# density_strata,
# Domain_est


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Mar 2020


##############################################################################################################################

#' Creates colony density summary dataframes
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP or NCRMP and DRM combined
#' @param region A string indicating the region
#' @param species_filter A string indicating whether to filter to a subset of species
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_DRM_calculate_colony_density <- function(project = "NULL", region, species_filter = "NULL"){

  ### Create species filters - species selected during 2019 status report juridictional meetings

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

  STX_filter <- c("COL NATA", "MEA MEAN", "ORB FRAN", "ORB FAVE", "ACR CERV", "ACR PALM", "AGA AGAR", "AGA LAMA",
                  "ORB ANNU", "DIP LABY", "MON CAVE", "POR PORI", "PSE STRI", "PSE CLIV", "SID SIDE", "STE INTE")

  STTSTJ_filter <- c("COL NATA", "ORB ANNU", "ORB FAVE", "ACR CERV", "ACR PALM", "AGA AGAR", "AGA LAMA",
                     "ORB FRAN", "DIP LABY", "MON CAVE", "POR PORI", "PSE STRI", "PSE CLIV",  "MEA MEAN", "SID SIDE", "STE INTE")

  PR_filter <- c("COL NATA", "ORB ANNU", "DIP LABY", "POR PORI", "ORB FAVE", "ACR CERV", "ACR PALM",
                 "ORB FRAN", "MON CAVE", "PSE STRI", "SID SIDE", "AGA AGAR", "AGA LAMA", "DEN CYLI")

  GOM_filter <- c("ORB FRAN", "PSE STRI", "AGA AGAR", "MON CAVE", "STE INTE", "COL NATA", "MAD DECA",
                  "ORB FAVE", "MAD AURE", "DIP LABY", "ORB ANNU", "POR ASTR", "PSE CLIV", "SID SIDE")





  # Load data
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

  if(project == "NCRMP" || project == "NULL"){

    if(region == "SEFCRI"){

      if(species_filter == "FALSE" ||
         species_filter == "NULL"){

        dat_2stage <- SEFCRI_2014_2stage_coral_demographics %>%
          dplyr::mutate(SURVEY = "NCRMP")

        dat_1stage <- dplyr::bind_rows(SEFCRI_2016_coral_demographics, SEFCRI_2018_coral_demographics) %>%
          dplyr::mutate(SURVEY = "NCRMP")
      }

      if(species_filter == "TRUE"){
        dat_2stage <- SEFCRI_2014_2stage_coral_demographics %>%
          dplyr::mutate(SURVEY = "NCRMP") %>%
          dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

        dat_1stage <- dplyr::bind_rows(SEFCRI_2016_coral_demographics, SEFCRI_2018_coral_demographics) %>%
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

        #Combine 1 stage or 2 stage data
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



  # Calculate coral density

  if(project == "NCRMP_DRM" ||
     project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    dat1_1stage <- dat_1stage %>%
      dplyr::ungroup() %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::select(-ABUNDANCE, -METERS_COMPLETED)%>%
      dplyr::ungroup() %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    density_species_1stage <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::select(-METERS_COMPLETED) %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))


    dat1_2stage <- dat_2stage %>%
      dplyr::ungroup() %>%
      dplyr::filter(N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY_transect = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(DENSITY = mean(DENSITY_transect)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-METERS_COMPLETED) %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    density_species_2stage <- dat_2stage %>%
      dplyr::filter(N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY_transect = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(DENSITY = mean(DENSITY_transect),
                       ABUNDANCE = sum(ABUNDANCE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-METERS_COMPLETED)%>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

    density_site <- rbind(dat1_1stage, dat1_2stage)

    density_species <- rbind(density_species_1stage, density_species_2stage)

  } else {

    density_species <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH, PROT, METERS_COMPLETED, SPECIES_CD, SPECIES_NAME) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::select(-METERS_COMPLETED) %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))


    density_site <- dat_1stage %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    N == 1,
                    JUV == 0) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>% # Change PROT to factor for ggplot will recognize it as a grouping variable
      dplyr::group_by(REGION, SURVEY, YEAR, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, STRAT, HABITAT_CD, PROT, METERS_COMPLETED) %>%
      dplyr::summarise(ABUNDANCE = sum(N)) %>%
      dplyr::mutate(DENSITY = ABUNDANCE/METERS_COMPLETED) %>%
      dplyr::select(-ABUNDANCE, -METERS_COMPLETED) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))


  }




  # Run through the weighting function
  tmp  <- NCRMP_make_weighted_demo_data(project,
                                        inputdata = density_site,
                                        region,
                                        datatype = "density",
                                        species_filter = species_filter,
                                        species_data = density_species)




  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])


  ################
  # Export
  ################

  if(species_filter == "TRUE"){

    # Create list to export
    output <- list(
      "density_species" = density_species,
      "Species_regional_means_CV" = Species_regional_CV,
      "density_site" = density_site,
      "density_strata" = density_strata,
      "Domain_est" = Domain_est)

  } else {

    # Create list to export
    output <- list(
      "density_species" = density_species,
      "density_site" = density_site,
      "density_strata" = density_strata,
      "Domain_est" = Domain_est)



  }

  return(output)

}



