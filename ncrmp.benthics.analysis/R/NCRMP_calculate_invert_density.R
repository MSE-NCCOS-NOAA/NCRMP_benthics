## Function to calculate diadema density

# Purpose:
# creates csv files with colony density and colony size structure.
# Creates figures with size distribution curves.


## Tag: data analysis


# outputs created in this file --------------
# diadema_density_site
# diadema_density_strata
# Domain est


# CallS:
# analysis ready data

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams
# Last update: Jan 2024


##############################################################################################################################

#' Creates summary dataframes of Diadema density from NCRMP benthic assessment.
#' NCRMP utilizes a stratified random sampling design.
#' Regional estimates of Diadema density are weighted by the number of
#' grid cells of a stratum in the sample frame.
#'
#'
#'
#'
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param project A string indicating the project: "NCRMP" or "MIR". Default is NCRMP.
#' @return A list dataframes including 1) Diadema density at each site,
#' 2) mean Diadema density by strata, and 3) weighted regional mean Diadema density,
#' all for a given region.
#' @importFrom magrittr "%>%"
#' @export
#'
#'


NCRMP_calculate_invert_density <- function(region, project = "NULL") {

  # Load analysis ready data from package

  # SEFCRI
  if(region == "SEFCRI"){


    dat_2stage <- SEFCRI_2014_2stage_inverts_ESAcorals

    dat_1stage <- dplyr::bind_rows(SEFCRI_2016_inverts_ESAcorals, SEFCRI_2018_inverts_ESAcorals, SEFCRI_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020), SEFCRI_2022_inverts_ESAcorals)


  }

  # FLK
  if(region == "FLK"){

    if(project == "NCRMP" || project == "NULL"){

    dat_2stage <- FLK_2014_2stage_inverts_ESAcorals %>%
      dplyr::mutate(YEAR = 2014)

    tmp1 <- FLK_2016_inverts_ESAcorals
    tmp2 <- FLK_2018_inverts_ESAcorals
    tmp3 <- FLK_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020)
    tmp4 <- FLK_2022_inverts_ESAcorals
    # UPDATE THE PROT (MIR sites initially labeled as PROT=2)
    grid_df <- FLK_2020_sample_frame@data
    new_prots <- grid_df %>% dplyr::select(MAPGRID_NR, PROT) %>% dplyr::rename("PROT_og" = PROT) %>% dplyr::mutate(MAPGRID_NR = as.numeric(MAPGRID_NR), PROT_og = as.numeric(PROT_og))
    tmp4 <- tmp4 %>% dplyr::left_join(., new_prots, by = c("MAPGRID_NR")) %>%
      # fix any that get left out manually, they fell outside the grid and JB fixed them
      dplyr::mutate(PROT_og = case_when(PRIMARY_SAMPLE_UNIT == 1006 ~ 0, PRIMARY_SAMPLE_UNIT == 1382 ~ 1, TRUE ~ PROT_og)) %>%
      dplyr::select(-PROT) %>%
      dplyr::rename("PROT" = PROT_og)

    dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4)

    ### UPDATE IN DEC. 2023!!
    # PROT is re-coded here to 0 for ALL sites as fish and benthics met 12/19/23
    # to determine that it is not appropraite to keep PROT in the analysis strat
    # in FLK because the data aren't allocated that way
    # only affects FLK data
    dat_1stage <- dat_1stage %>% dplyr::mutate(PROT = as.factor(0))
    dat_2stage <- dat_2stage %>% dplyr::mutate(PROT = as.factor(0))

    }

    if(project == "MIR"){

      dat_1stage <- MIR_2022_inverts_ESAcorals_DUMMY %>%
        dplyr::mutate(SURVEY = "MIR",
                      DATE = paste(MONTH, DAY, YEAR, sep = "/" ),
                      REGION = "FLK",
                      SUB_REGION_NAME = MIR_zone)

    }
  }

  # Tortugas
  if(region == "Tortugas"){

    tmp1 <- TortugasMarq_2014_inverts_ESAcorals

    tmp2 <- TortugasMarq_2016_inverts_ESAcorals

    tmp3 <- Tortugas_2018_inverts_ESAcorals

    tmp4 <- Tortugas_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020) %>%
      dplyr::mutate(STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT)))

    tmp5 <- Tortugas_2022_inverts_ESAcorals

    dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp4, tmp5)

    dat_2stage <- dplyr::bind_rows(tmp3)

  }

  # Carib / GOM
  # St. Thomas & St. John
  if(region == "STTSTJ"){

    tmp1 <- USVI_2013_inverts_ESAcorals %>%
      # Filter to region of interest
      dplyr::filter(REGION == "STTSTJ")

    tmp2 <- USVI_2015_inverts_ESAcorals %>%
      dplyr::filter(REGION == "STTSTJ",
                    # Remove sites where diadema were not recorded
                    !is.na(DIADEMA_NUM))

    tmp3 <- USVI_2017_inverts_ESAcorals %>%
      dplyr::filter(REGION == "STTSTJ")

    tmp4 <- USVI_2019_inverts_ESAcorals %>%
      dplyr::filter(REGION == "STTSTJ")

    tmp5 <- USVI_2021_inverts_ESAcorals %>%
      dplyr::filter(REGION == "STTSTJ")

    #Combine 1 stage or 2 stage data
    dat_1stage <-dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5) %>%
      dplyr::mutate(DIADEMA_NUM = as.numeric(DIADEMA_NUM))


  }
  # St. Croix
  if(region == "STX"){

    tmp1 <- USVI_2015_inverts_ESAcorals %>%
      dplyr::filter(REGION == "STX",
                    DIADEMA_NUM != "N/A",
                    STRAT != "BDRK_DEEP")  # There is no BDRK DEEP in the 2017 NTOT for STX - only 1 site is being removed

    tmp2 <- USVI_2017_inverts_ESAcorals %>%
      dplyr::filter(REGION == "STX")

    tmp3 <- USVI_2019_inverts_ESAcorals %>%
      dplyr::filter(REGION == "STX")

    tmp4 <- USVI_2021_inverts_ESAcorals %>%
      dplyr::filter(REGION == "STX")

    #Combine 1 stage or 2 stage data
    dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4)


  }

  # Puerto Rico
  if(region == "PRICO"){

    tmp1 <- PRICO_2014_inverts_ESAcorals

    tmp2 <- PRICO_2016_inverts_ESAcorals %>%
      dplyr::mutate(YEAR = 2016)

    tmp3 <- PRICO_2019_inverts_ESAcorals

    tmp4 <- PRICO_2021_inverts_ESAcorals

    #Combine data
    dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT)

  }

  ## Flower Garden Banks National Marine Sanctuary (GOM)
  if(region == "GOM"){

    tmp1 <- FGBNMS_2013_inverts_ESAcorals %>%
      dplyr::mutate(SURVEY = "NCRMP",
                    ANALYSIS_STRATUM = "FGBNMS",
                    STRAT = "FGBNMS",
                    REGION = "GOM")

    tmp2 <- FGBNMS_2015_inverts_ESAcorals %>%
      dplyr::mutate(SURVEY = "NCRMP",
                    ANALYSIS_STRATUM = "FGBNMS",
                    STRAT = "FGBNMS",
                    REGION = "GOM")

     tmp3 <- FGBNMS_2018_inverts_ESAcorals %>%
      dplyr::mutate(SURVEY = "NCRMP",
                    ANALYSIS_STRATUM = "FGBNMS",
                    STRAT = "FGBNMS",
                    REGION = "GOM",
                    MAPGRID_NR = as.factor(MAPGRID_NR))

     tmp4 <- FGBNMS_2022_inverts_ESAcorals %>%
       dplyr::mutate(SURVEY = "NCRMP",
                     ANALYSIS_STRATUM = "FGBNMS",
                     STRAT = "FGBNMS",
                     REGION = "GOM",
                     MAPGRID_NR = as.factor(MAPGRID_NR))

    #Combine data
    dat_1stage <- dplyr::bind_rows(tmp1, tmp2, tmp3, tmp4)

  }

  ### Account for SEFCRI/FLK 2014 2 stage data - take the transect means

  if(project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "FLK" ||
     project == "NCRMP" && region == "Tortugas") {

    dat1_1stage <- dat_1stage %>%
      # Remove Marquesas
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      # Change column class
      dplyr::mutate(YEAR = as.numeric(YEAR),
                    PROT = as.factor(PROT),
                    DIADEMA_NUM = as.numeric(as.character(DIADEMA_NUM))) %>%
      # could expand here to include lobster and conch density
      # convert counts DIADEMA_NUM to density DIADEMA_DENS per site
      dplyr::mutate(Diadema_dens = dplyr::case_when(REGION == "FLK" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "Tortugas" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "SEFCRI" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "STTSTJ" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "STX" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "PRICO" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "FGBNMS" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                    TRUE ~ DIADEMA_NUM/(25 * 2) )) %>%
      # select columns
      dplyr::select(YEAR, MONTH, DAY, REGION, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                      MIN_DEPTH, MAX_DEPTH, STRAT, PROT, LOBSTER_NUM, CONCH_NUM, DIADEMA_NUM, Diadema_dens)

    dat1_2stage <- dat_2stage %>%
      # Remove Marquesas
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      # Change column class
      dplyr::mutate(YEAR = as.numeric(YEAR),
                    PROT = as.factor(PROT),
                    DIADEMA_NUM = as.numeric(as.character(DIADEMA_NUM))) %>%
      # could expand here to include lobster and conch density
      # convert counts DIADEMA_NUM to density DIADEMA_DENS per transect
      dplyr::mutate(Diadema_dens = dplyr::case_when(REGION == "FLK" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "SEFCRI" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "Tortugas" ~ DIADEMA_NUM/(15 * 2),
                                                    TRUE ~ DIADEMA_NUM/(25 * 2) )) %>%

      # Calculate site density by taking the mean of 2 transects
      dplyr::group_by(YEAR, MONTH, DAY, REGION, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                      STRAT, PROT) %>%
      dplyr::summarise(MIN_DEPTH = mean(MIN_DEPTH),
                       MAX_DEPTH = mean(MAX_DEPTH),
                       LOBSTER_NUM = mean(LOBSTER_NUM),
                       CONCH_NUM = mean(CONCH_NUM),
                       DIADEMA_NUM = mean(DIADEMA_NUM),
                       Diadema_dens = mean(Diadema_dens))


    diadema_density_site <- dplyr::bind_rows(dat1_1stage, dat1_2stage)


  } else {

    diadema_density_site <- dat_1stage %>%
      # Remove Marquesas
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans",
                    DIADEMA_NUM != "NA") %>%
      # Change column class
      dplyr::mutate(YEAR = as.numeric(YEAR),
                    PROT = as.factor(PROT),
                    DIADEMA_NUM = as.numeric(as.character(DIADEMA_NUM))) %>%
      # could expand here to include lobster and conch density
      # convert counts DIADEMA_NUM to density DIADEMA_DENS per site
      dplyr::mutate(Diadema_dens = dplyr::case_when(REGION == "FLK" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "Tortugas" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "SEFCRI" ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "STTSTJ" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "STX" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "PRICO" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                    REGION == "FGBNMS" & YEAR > 2014 ~ DIADEMA_NUM/(15 * 2),
                                                    TRUE ~ DIADEMA_NUM/(25 * 2) )) %>%
      # drop columns
      dplyr::select(YEAR, MONTH, DAY, REGION, SUB_REGION_NAME, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                      MIN_DEPTH, MAX_DEPTH, STRAT, PROT, LOBSTER_NUM, CONCH_NUM, DIADEMA_NUM, Diadema_dens)
  }

  # call function for weighting
  tmp <- NCRMP_make_weighted_invert_density_data(inputdata = diadema_density_site,
                                                region,
                                                project)

  # unpack list
  for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])





  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "diadema_density_site" = diadema_density_site,
    "invert_strata" = invert_strata,
    "Domain_est" = Domain_est)

  return(output)

}


