
## Function to load analysis ready NCRMP & DRM demo data
# Purpose:
# creates files with all demo programs/years combined


## Tag: data analysis


# outputs created in this file --------------
# dat_1stage
# dat_2stage


# CallS:
# analysis ready data

# output gets called by:
# NCRMP_DRM_calculate_colony_density
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams, Krampitz, Sturm
# Last update: Jul 2025


##############################################################################################################################

#' Creates combined demo dataframes
#'
#' Loads combined dataframes of coral demographics data from all years of a single region. Can be just NCRMP
#' data, or also NCRMP and DRM data combined.
#' In regions with two stage data (SEFCRI, FLK, Tortugas) a list of dataframes is produces, one
#' containing all the single stage data, one containing all the two stage data.
#' This function is called by nearly all of the functions that run summary calculations from coral demographic data.
#'
#'
#' @param project A string indicating the project, NCRMP, NCRMP and DRM combined, or MIR. Default is NCRMP.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "FGB".
#' @param species_filter An optional string indicating the species to subset to, if desired. Format is the 7 digits species code (e.g. Acropora cervicornis would be ACR CERV).
#' @return A dataframe or list of dataframes containing all demographic data for specified region, and species, if selected. A list is produced for Florida regions, with one dataframe containing all single stage data and the other containing all two stage data.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "case_when"
#' @export
#'
#'
#'
#'

load_NCRMP_DRM_demo_data <- function(project = "NULL", region, species_filter = "NULL"){

  #### clean data function ####
  clean_data <- function(data, survey){
    data<- data %>%
      mutate(SURVEY = survey) %>%
      mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))
  }

  ##### Mutate Undaria spp to Agaricia spp Function ####
  recode_and_clean_species <- function(data) {
    data %>%
      dplyr::mutate(
        SPECIES_CD = dplyr::case_when(
          SPECIES_CD == "MEAN JACK" ~ "MEA JACK",
          SPECIES_CD == "DIP STRI" ~ "PSE STRI",
          SPECIES_CD == "DIP CLIV" ~ "PSE CLIV",
          SPECIES_CD == "CLA ARBU" ~ "CLA ABRU",
          TRUE ~ SPECIES_CD
        ),
        SPECIES_NAME = dplyr::case_when(
          SPECIES_NAME == "Undaria spp" ~ "Agaricia spp",
          SPECIES_CD == "PSE STRI" ~ "Pseudodiploria strigosa",
          SPECIES_CD == "PSE CLIV" ~ "Pseudodiploria clivosa",
          SPECIES_CD == "MEA JACK" ~ "Meandrina jacksoni",
          SPECIES_CD == "CLA ABRU" ~ "Cladocora arbuscula",
          TRUE ~ SPECIES_NAME
        )
      )
  }


  ####Process Florida Regions ####
  process_florida_data <- function(one_stage_data, two_stage_data, filters = NULL){

    dat_1stage <- bind_rows(one_stage_data) %>%
      recode_and_clean_species() %>%
      dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1" ~ "PTSH2", TRUE ~ STRAT))

    dat_2stage <- bind_rows(two_stage_data) %>%
      recode_and_clean_species() %>%
      dplyr::mutate(STRAT = dplyr::case_when(STRAT == "PTSH1" ~ "PTSH2", TRUE ~ STRAT))

    return(list(dat_1stage = dat_1stage, dat_2stage = dat_2stage))
  }

  ####Data Processing Mission: Iconic Reef####
  if(project == "MIR"){
    dat_1stage <- MIR_FLK_2022_coral_demographics_DUMMY %>%
      dplyr::mutate(SURVEY = "MIR", REGION = "FLK", SUB_REGION_NAME = MIR_zone) %>%
      dplyr::filter(!is.na(MAPGRID_NR), !is.na(MIR_zone))
  }

  ####Data Processing Southeast Florida NCRMP_DRM####
  if(project == "NCRMP_DRM" && region == "SEFCRI"){
    # Load and clean one stage datasets
    one_stage_data <- list(
      clean_data(SEFCRI_2016_coral_demographics, "NCRMP"),
      clean_data(SEFCRI_2018_coral_demographics, "NCRMP"),
      clean_data(SEFCRI_2020_coral_demographics, "NCRMP") %>% dplyr::mutate(YEAR = 2020),
      clean_data(SEFCRI_2022_coral_demographics, "NCRMP"),
      clean_data(SEFCRI_2024_coral_demographics, "NCRMP")
    )
    # Load and clean two stage datasets
    two_stage_data <- list(
      clean_data(SEFCRI_2014_2stage_coral_demographics, "NCRMP"),
      DRM_SEFCRI_2014_2024_2stage_coral_demographics %>%
        mutate(STRAT = dplyr::case_when(STRAT == "NEAR2"~"NEAR1", TRUE ~ as.character(STRAT))) %>%
        mutate(SURVEY = "DRM", PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT)) %>%
        filter(STRAT != 'NA0')
    )

    # Process data
    processed_data <- process_florida_data(one_stage_data, two_stage_data, filters = NULL)
    dat_1stage <- processed_data$dat_1stage
    dat_2stage <- processed_data$dat_2stage

    # Apply species filter if needed
    if (!is.null(species_filter) && species_filter == "TRUE"){
      dat_1stage <- dat_1stage %>% filter(SPECIES_CD %in% SEFCRI_filter)
      dat_2stage <- dat_2stage %>% filter(SPECIES_CD %in% SEFCRI_filter)
    }
  }


  ####Data Processing Florida Keys NCRMP_DRM####
  if(project == "NCRMP_DRM" && region == "FLK"){

    #load and clean one stage datasets
    one_stage_data <- list(
      clean_data(FLK_2014_coral_demographics, "NCRMP") %>% mutate(YEAR = 2014),
      clean_data(FLK_2016_coral_demographics, "NCRMP"),
      clean_data(FLK_2018_coral_demographics, "NCRMP"),
      clean_data(FLK_2020_coral_demographics, "NCRMP") %>% mutate(YEAR = 2020),
      clean_data(FLK_2022_coral_demographics, "NCRMP") %>%
        update_protection_status(grid_df = FLK_2020_sample_frame@data), #call NEW function to update PROT/protection status for FLK
      clean_data(FLK_2024_coral_demographics, "NCRMP")
      )

    #load and clean two stage datasets
    two_stage_data <- list(
      clean_data(DRM_FLK_2014_2024_2stage_coral_demographics, "DRM") %>% filter(!is.na(MAPGRID_NR), !is.na(STRAT))
    )

    # Process data
    processed_data <- process_florida_data(one_stage_data, two_stage_data, filters = NULL)
    dat_1stage <- processed_data$dat_1stage %>% mutate(PROT = as.factor(PROT)) %>% dplyr::mutate(PROT = as.factor(0))
    dat_2stage <- processed_data$dat_2stage%>% mutate(PROT = as.factor(PROT))%>% dplyr::mutate(PROT = as.factor(0))

    if (!is.null(species_filter) && species_filter == "TRUE"){
      dat_1stage <- dat_1stage %>% filter(SPECIES_CD %in% FLK_filter)
      dat_2stage <- dat_2stage %>% filter(SPECIES_CD %in% FLK_filter)
    }
  }

  ####Data Processing Tortugas NCRMP_DRM####
  if(project == "NCRMP_DRM" && region == "Tortugas"){

    # 2020 NCRMP data overlaps some with DRM because NCRMP divers went on DRM cruise
    # and entered data in both data entry systems...
    # filter out the sites already in NCRMP data
    filter_out_dupes <- function(data){
      data <- data %>%
        dplyr::filter(!(YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1490 & STATION_NR == 9290 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1562 & STATION_NR == 9508 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1564 & STATION_NR == 9362 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1570 & STATION_NR == 9345 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1571 & STATION_NR == 9366 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 1572 & STATION_NR == 9515 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2166 & STATION_NR == 9297 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2172 & STATION_NR == 9668 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2187 & STATION_NR == 9375 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2188 & STATION_NR == 9376 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2209 & STATION_NR == 9514 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2211 & STATION_NR == 9333 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2215 & STATION_NR == 9344 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2219 & STATION_NR == 9339 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2221 & STATION_NR == 9520 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2224 & STATION_NR == 9289 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2227 & STATION_NR == 9343 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2229 & STATION_NR == 9577 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2231 & STATION_NR == 9492 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2241 & STATION_NR == 9340 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2243 & STATION_NR == 9335 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2255 & STATION_NR == 9365 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2264 & STATION_NR == 9586 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2289 & STATION_NR == 9591 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2300 & STATION_NR == 9334 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2307 & STATION_NR == 9341 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2309 & STATION_NR == 9485 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2312 & STATION_NR == 9589 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2315 & STATION_NR == 9320 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2317 & STATION_NR == 9481 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2319 & STATION_NR == 9288 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2329 & STATION_NR == 9374 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2340 & STATION_NR == 9511 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2512 & STATION_NR == 9576 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2571 & STATION_NR == 9360 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2606 & STATION_NR == 9579 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2646 & STATION_NR == 9578 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2698 & STATION_NR == 9330 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 2716 & STATION_NR == 9315 |
                          YEAR == 2021 & PRIMARY_SAMPLE_UNIT == 3023 & STATION_NR == 9299))
    }


    one_stage_data <- list(
      clean_data(TortugasMarq_2014_coral_demographics, "NCRMP"),
      clean_data(TortugasMarq_2016_coral_demographics, "NCRMP"),
      clean_data(Tortugas_2022_coral_demographics, "NCRMP"),
      clean_data(Tortugas_2024_coral_demographics, "NCRMP")
    )


    two_stage_data <- list(
      clean_data(Tortugas_2018_coral_demographics, "NCRMP"),
      clean_data(DRM_Tort_2014_2024_2stage_coral_demographics, "DRM") %>% filter_out_dupes(),
      Tortugas_2020_coral_demographics %>%
        dplyr::mutate(SURVEY = dplyr::case_when(MONTH == 8 ~ "NCRMP", MONTH == 9 ~ "DRM"), # assign DRM/NCRMP data to DRM so it can be combined with the second stage version of the data in the DRM cruise
                      STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT))) %>%
        mutate( PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))
    )

    # Process data
    processed_data <- process_florida_data(one_stage_data, two_stage_data, filters = NULL)
    dat_1stage <- processed_data$dat_1stage
    dat_2stage <- processed_data$dat_2stage

    if (!is.null(species_filter) && species_filter == "TRUE"){
      dat_1stage <- dat_1stage %>% filter(SPECIES_CD %in% Tort_filter)
      dat_2stage <- dat_2stage %>% filter(SPECIES_CD %in% Tort_filter)
    }
  }


  ####Data Processing Southeast Florida NCRMP####
  if(region == "SEFCRI"&& project == "NCRMP" || project == "NULL") {

    two_stage_data <- SEFCRI_2014_2stage_coral_demographics %>%
      clean_data(survey = "NCRMP") %>%
      recode_and_clean_species()


    one_stage_data <- dplyr::bind_rows(SEFCRI_2016_coral_demographics, SEFCRI_2018_coral_demographics, SEFCRI_2020_coral_demographics %>% dplyr::mutate(YEAR = 2020), SEFCRI_2022_coral_demographics,SEFCRI_2024_coral_demographics) %>%
      clean_data(survey = "NCRMP") %>%
      recode_and_clean_species()

    # Process data
    processed_data <- process_florida_data(one_stage_data, two_stage_data, filters = NULL)
    dat_1stage <- processed_data$dat_1stage
    dat_2stage <- processed_data$dat_2stage

    if(species_filter == "TRUE"){
      dat_2stage <- dat_2stage %>% dplyr::filter(SPECIES_CD %in% SEFCRI_filter)

      dat_1stage <- dat_1stage %>% dplyr::filter(SPECIES_CD %in% SEFCRI_filter)
    }
  }

  ####Data Processing Florida Keys NCRMP####
  if(region == "FLK"&& project == "NCRMP" || project == "NULL"){

    datasets <- list(
      clean_data(FLK_2014_coral_demographics, survey = "NCRMP") %>% dplyr::mutate(YEAR = 2014),
      clean_data(FLK_2016_coral_demographics, survey = "NCRMP"),
      clean_data(FLK_2018_coral_demographics, survey = "NCRMP"),
      clean_data(FLK_2020_coral_demographics, survey = "NCRMP") %>% dplyr::mutate(YEAR = 2020),
      clean_data(FLK_2022_coral_demographics, survey = "NCRMP") %>%
        update_protection_status(grid_df = FLK_2020_sample_frame@data),
      clean_data(FLK_2024_coral_demographics, survey = "NCRMP")
    )

    dat_1stage <- dplyr::bind_rows(datasets) %>%
      clean_data(survey = "NCRMP") %>%
      recode_and_clean_species() %>%
      dplyr::mutate(PROT = as.factor(0))

    if(species_filter == "TRUE"){
      dat_1stage <- dat_1stage %>% dplyr::filter(SPECIES_CD %in% FLK_filter)
    }
  }


  ####Data Processing Tortuga NCRMP####
  if(region == "Tortugas" && project == "NCRMP" || project == "NULL"){

    one_stage_data <- list(
      TortugasMarq_2014_coral_demographics %>% clean_data(survey = "NCRMP"),
      TortugasMarq_2016_coral_demographics %>% clean_data(survey = "NCRMP"),
      Tortugas_2020_coral_demographics %>%
        mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT)) %>%
        dplyr::mutate(SURVEY = "NCRMP/DRM", YEAR = 2020,
                      STRAT = dplyr::case_when(STRAT == "T08" & PROT == 2 ~ 'T09', TRUE ~ as.character(STRAT))),
      Tortugas_2022_coral_demographics %>% clean_data(survey = "NCRMP"),
      Tortugas_2024_coral_demographics %>% clean_data(survey = "NCRMP")
    )

    two_stage_data <- Tortugas_2018_coral_demographics %>% clean_data(survey = "NCRMP/DRM")


    dat_1stage <- dplyr::bind_rows(one_stage_data)  %>%
      dplyr::mutate(across(where(is.character), stringr::str_trim)) %>%  # this may not be necessary
      dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ))

    dat_2stage <- two_stage_data  %>%
      dplyr::mutate(across(where(is.character), stringr::str_trim)) %>%
      dplyr::mutate(DATE = paste(MONTH, DAY, YEAR, sep = "/" ))


    if(species_filter == "TRUE"){
      dat_1stage <- dat_1stage %>% dplyr::filter(SPECIES_CD %in% Tort_filter)
      dat_2stage <- dat_2stage %>% dplyr::filter(SPECIES_CD %in% Tort_filter)
    }
  }

  ####Data Processing STTSTJ####
  if(region == "STTSTJ"){

    STTSTJ_datasets <- list(
      USVI_2013_coral_demographics,
      USVI_2015_coral_demographics,
      USVI_2017_coral_demographics,
      USVI_2019_coral_demographics,
      USVI_2021_coral_demographics,
      USVI_2023_coral_demographics,
      USVI_2025_coral_demographics
    )

    dat_1stage <- dplyr::bind_rows(STTSTJ_datasets) %>%
      dplyr::filter(REGION == "STTSTJ") %>%
      dplyr::mutate(SURVEY = "NCRMP")


    if(species_filter == "TRUE"){
      dat_1stage <- dat_1stage %>% dplyr::filter(SPECIES_CD %in% STTSTJ_filter)
    }
  } #end St Thomas, St John (STTSTJ)


  ####Data Processing STX####
  if(region == "STX"){

    STX_datasets <- list(
      USVI_2015_coral_demographics,
      USVI_2017_coral_demographics,
      USVI_2019_coral_demographics,
      USVI_2021_coral_demographics,
      USVI_2023_coral_demographics,
      USVI_2025_coral_demographics
    )

    dat_1stage <- dplyr::bind_rows(STX_datasets) %>%
      dplyr::filter(REGION == "STX") %>%
      dplyr::mutate(SURVEY = "NCRMP")


    if(species_filter == "TRUE"){
      dat_1stage <- dat_1stage %>% dplyr::filter(SPECIES_CD %in% STX_filter)
    }
  } #end St Croix/STX

  ####Data Processing PRICO####
  if(region == "PRICO"){

    datasets <- list(
      PRICO_2014_coral_demographics %>% dplyr::mutate(PROT = NA),
      PRICO_2016_coral_demographics %>% dplyr::mutate(YEAR = 2016, PROT = NA),
      PRICO_2019_coral_demographics,
      PRICO_2021_coral_demographics,
      PRICO_2023_coral_demographics,
      PRICO_2025_coral_demographics
    )

    dat_1stage <- dplyr::bind_rows(datasets) %>% dplyr::mutate(SURVEY = "NCRMP")

    if(species_filter == "TRUE"){

      dat_1stage <- dat_1stage %>% dplyr::filter(SPECIES_CD %in% PRICO_filter)
    }
  } #end PRICO

  ####Data Processing FGB####
  if(region == "FGB"){

    datasets <- list(
      FGBNMS_2013_coral_demographics,
      FGBNMS_2015_coral_demographics,
      FGBNMS_2018_coral_demographics %>% dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR)),
      FGBNMS_2022_coral_demographics %>% dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR)),
      FGBNMS_2024_coral_demographics %>% dplyr::mutate(MAPGRID_NR = as.factor(MAPGRID_NR))

    )

    dat_1stage <- dplyr::bind_rows(datasets) %>%
      dplyr::mutate(SURVEY = "NCRMP", STRAT = "FGBNMS", REGION = "FGB")

    if (species_filter == "TRUE"){
      dat_1stage <- dat_1stage %>% dplyr::filter(SPECIES_CD %in% FGB_filter)
    }
  } #end FGB


  ####Export####
  if(project == "NCRMP_DRM" || project == "NCRMP" && region == "SEFCRI" ||
     project == "NCRMP" && region == "Tortugas") {

    output <- list(
      "dat_1stage" = dat_1stage,
      "dat_2stage" = dat_2stage)

  } else {
    output <- list(
      "dat_1stage" = dat_1stage)
  }
  return(output)
}
