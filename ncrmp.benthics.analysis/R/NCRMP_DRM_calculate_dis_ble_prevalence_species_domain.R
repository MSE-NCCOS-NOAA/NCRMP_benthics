## Function to calculate species domain estimates for disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data (FL only).

# Purpose:
# creates csv files with disease/bleaching prevalence by species,  region and year


## Tag: data analysis


# outputs created in this file --------------

# Species domain estimates

# CallS:
# NCRMP_REGION_YEARS_dis_ble_prev_species

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Feb 2023


##############################################################################################################################

#' Species domain estimates for disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP, MIR, or NCRMP and DRM combined
#' @param region A string indicating the region
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'
NCRMP_DRM_calculate_dis_ble_prevalence_species_domain <- function(project, region){


  if(project == "MIR"){

    dat <- MIR_2022_dis_ble_prev_species_DUMMY


  }

  if(project == "NCRMP_DRM") {

    if(region == "SEFCRI"){

      # Load species/site level bleaching & disease data
      dat <- NCRMP_DRM_SEFCRI_2014_22_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               "ORB ANCX"="ORB SPE.",
                                               "DIP STRI"="PSE STRI"))

      ntot <- load_NTOT(region = region,
                        inputdata = dat,
                        project = project)
    }

    if(region == "FLK"){

      # Load species/site level bleaching & disease data
      dat <- NCRMP_DRM_FLK_2014_22_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                 "DIP CLIV" = "PSE CLIV",
                                                 "DIP STRI" = "PSE STRI",
                                                 'CLA ARBU' = "CLA ABRU",
                                                 "ORB ANCX"="ORB SPE."))

      ntot <- load_NTOT(region = region,
                        inputdata = dat,
                        project = project)
    }

    if(region == "Tortugas"){

      # Load species/site level bleaching & disease data
      dat <- NCRMP_DRM_Tort_2014_22_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                                 "DIP CLIV" = "PSE CLIV",
                                                 "DIP STRI" = "PSE STRI",
                                                 'CLA ARBU' = "CLA ABRU",
                                                 "ORB ANCX"="ORB SPE.",))

      ntot <- load_NTOT(region = region,
                        inputdata = dat,
                        project = project)
    }
  } else {

    #ntot <- load_NTOT(region = region,
    #                  inputdata = "NULL",
    #                  project = project)
  }

  if(project == "NCRMP"){

    if(region == "SEFCRI"){

      # Load species/site level bleaching & disease data
      dat <- NCRMP_SEFCRI_2014_22_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               "ORB ANCX"="ORB SPE.",
                                               "DIP STRI"="PSE STRI"))
    }

    if(region == "FLK"){

      # Load species/site level bleaching & disease data
      dat <- NCRMP_FLK_2014_22_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               "ORB ANCX"="ORB SPE.",
                                               "DIP STRI"="PSE STRI"))
    }

    if(region == "Tortugas"){

      # Load species/site level bleaching & disease data
      dat <- NCRMP_Tort_2014_22_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               "ORB ANCX"="ORB SPE.",
                                               "DIP STRI"="PSE STRI"))
    }


    if(region == "STTSTJ"){

      # Load species/site level bleaching & disease data
      dat <- NCRMP_STTSTJ_2013_21_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               "ORB ANCX"="ORB SPE."))

    }

    if(region == "STX"){


      dat <- NCRMP_STX_2015_21_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               "ORB ANCX"="ORB SPE."))

    }

    if(region == "PRICO"){

      dat <- NCRMP_PRICO_2014_21_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               "ORB ANCX"="ORB SPE."))
    }

    ## Flower Garden Banks National Marine Sanctuary (GOM)
    if(region == "GOM"){

      # Load species/site level bleaching & disease data
      dat <- NCRMP_FGBNMS_2013_22_dis_ble_prev_species %>%
        dplyr::mutate(SPECIES_CD=dplyr::recode(SPECIES_CD,
                                               "ORB ANCX"="ORB SPE."))
    }

    ntot <- load_NTOT(region = region,
                      inputdata = dat,
                      project = project)

  }



  ## coral data processing

  dat_ble_wide <- dat %>%
    # select for most recent year
    # remove other metrics
    dplyr::select(-Total_ble, -Total_dis, -Total_col, -DIS_PREV) %>%
    # filter out spp columns
    dplyr::filter(
      SPECIES_CD != "ORB SPE.",
      SPECIES_CD != "AGA SPE.",
      SPECIES_CD != "MYC SPE.",
      SPECIES_CD != "SCO SPE.",
      SPECIES_CD != "MAD SPE.",
      SPECIES_CD != "SID SPE.",
      SPECIES_CD != "SOL SPE.",
      SPECIES_CD != "PSE SPE.",
      SPECIES_CD != "OTH CORA",
      SPECIES_CD != "POR SPE.",
      SPECIES_CD != "SCL SPE.")
  # %>%
  #   # add in zeros for species that didn't occur per site. Easiest to flip to wide format ( 1 row per site) for this
  #   tidyr::spread(., SPECIES_CD, BLE_PREV
  #                 ,
  #                 fill = 0
  #                 )

  #dat_ble_long <- tidyr::gather(dat_ble_wide, SPECIES_CD, BLE_PREV, 12:ncol(dat_ble_wide))

  dat_dis_wide <- dat %>%
    # select for most recent year
    # remove other metrics
    dplyr::select(-Total_ble, -Total_dis, -Total_col, -BLE_PREV) %>%
    # filter out spp columns
    dplyr::filter(
      SPECIES_CD != "ORB SPE.",
      SPECIES_CD != "AGA SPE.",
      SPECIES_CD != "MYC SPE.",
      SPECIES_CD != "SCO SPE.",
      SPECIES_CD != "OTH CORA",
      SPECIES_CD != "MAD SPE.",
      SPECIES_CD != "SID SPE.",
      SPECIES_CD != "SOL SPE.",
      SPECIES_CD != "PSE SPE.",
      SPECIES_CD != "POR SPE.",
      SPECIES_CD != "SCL SPE.")
  # %>%
  #   # add in zeros for species that didn't occur per site. Easiest to flip to wide format ( 1 row per site) for this
  #   tidyr::spread(., SPECIES_CD, DIS_PREV,
  #                 fill = 0)

  #dat_dis_long <- tidyr::gather(dat_dis_wide, SPECIES_CD, DIS_PREV, 12:ncol(dat_dis_wide))

  # Define regional groups
  FL <- c("SEFCRI", "FLK", "Tortugas")
  GOM <- "GOM"
  Carib <- c("STTSTJ", "STX", "PRICO")

  if(region %in% FL) {

    dat1 <- dplyr::left_join(dat_dis_wide, dat_ble_wide) %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD) %>% # Modify this line to changes analysis stratum
      dplyr::summarise(# compute average density
        avDprev = mean(DIS_PREV, na.rm = T),
        avBprev = mean(BLE_PREV, na.rm = T),
        # compute stratum variance
        svarD = var(DIS_PREV, na.rm = T),
        svarB = var(BLE_PREV, na.rm = T),
        # calculate N
        n_sites = length(PRIMARY_SAMPLE_UNIT),
        #.groups is experimental with dplyr
        .groups = "keep") %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svarD = dplyr::case_when(svarD == 0 ~ 0.00000001,
                                             TRUE ~ svarD)) %>%
      dplyr::mutate(stdD = sqrt(svarD))%>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svarB = dplyr::case_when(svarB == 0 ~ 0.00000001,
                                             TRUE ~ svarB)) %>%
      dplyr::mutate(stdB = sqrt(svarB))


  } else {

  dat1 <- dplyr::left_join(dat_dis_wide, dat_ble_wide) %>%
    dplyr::group_by(YEAR, STRAT, SPECIES_CD) %>% # Modify this line to changes analysis stratum
    dplyr::summarise(# compute average density
      avDprev = mean(DIS_PREV, na.rm = T),
      avBprev = mean(BLE_PREV, na.rm = T),
      # compute stratum variance
      svarD = var(DIS_PREV, na.rm = T),
      svarB = var(BLE_PREV, na.rm = T),
      # calculate N
      n_sites = length(PRIMARY_SAMPLE_UNIT),
      #.groups is experimental with dplyr
      .groups = "keep") %>%
    # convert 0 for stratum variance so that the sqrt is a small # but not a 0
    dplyr::mutate(svarD = dplyr::case_when(svarD == 0 ~ 0.00000001,
                                           TRUE ~ svarD)) %>%
    dplyr::mutate(stdD = sqrt(svarD))%>%
    # convert 0 for stratum variance so that the sqrt is a small # but not a 0
    dplyr::mutate(svarB = dplyr::case_when(svarB == 0 ~ 0.00000001,
                                           TRUE ~ svarB)) %>%
    dplyr::mutate(stdB = sqrt(svarB))

  }

  dat2 <-dat1 %>%
    # Merge ntot with coral_est_spp
    dplyr::full_join(., ntot) %>%
    # stratum estimates
    dplyr::mutate(whavDprev = wh * avDprev,
                  whavBprev = wh * avBprev,
                  whsvarD = wh^2 * svarD,
                  whsvarB = wh^2 * svarB,
                  whstdD = wh * stdD,
                  whstdB = wh * stdB,
                  n_sites = tidyr::replace_na(n_sites, 0))  %>%
    dplyr::ungroup()

  ## Domain Estimates
  DomainEst <- dat2 %>%
    dplyr::group_by(REGION, YEAR, SPECIES_CD) %>%
    dplyr::summarise(avDisPrev = sum(whavDprev, na.rm = T), # This accounts for strata with 0 species of interest present
                     avBlePrev = sum(whavBprev, na.rm = T),
                     VarD = sum(whsvarD, na.rm = T),
                     VarB = sum(whsvarB, na.rm = T),# This accounts for strata with N = 1
                     SE_D=sqrt(VarD),
                     SE_B=sqrt(VarB),
                     n_sites = sum(n_sites),
                     n_strat = length(unique(ANALYSIS_STRATUM)),
                     ngrtot = sum(NTOT),
                     #.groups is experimental with dplyr
                     .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::left_join(., ncrmp_frrp_sppcodes %>%
                       dplyr::select(fl_ncrmp_code, species_name),
                     by = c('SPECIES_CD' = 'fl_ncrmp_code')) %>%
    dplyr::filter(!is.na(SPECIES_CD))

  DomainEst_bl <- DomainEst %>%
    dplyr::select(REGION, YEAR, species_name, avBlePrev, SE_B)%>%
    dplyr::filter(!is.na(species_name))


  DomainEst_dis <- DomainEst %>%
    dplyr::select(REGION, YEAR, species_name, avDisPrev, SE_D)%>%
    dplyr::filter(!is.na(species_name))



  ################
  # Export
  ################

  # Create list to export
  output <- list(
    'DomainEst_bl' = DomainEst_bl,
    "DomainEst_dis" = DomainEst_dis)

  return(output)




}
