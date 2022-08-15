

## Function objective:
#  prepare analysis ready 2 stage SCREAM data from 2012, 2009, 2005-2006, 1999-2002.

# Purpose:
# Cleans preNCRMP SCREAM data to match NCRMP AR format

# Outputs:
# AR SREAM data

# Output gets called by:
# Analysis Rmd

# Authors: NCRMP Benthics Analyses Team (Groves)
# latest update: June 2022


##############################################################################################################################

#' Clean and tidy preNCRMP data
#'
#'
#'
#' @param region A string indicating the region
#' @param userlocation A string indicating the user location
#' @return A list of multiple dataframes
#' @importFrom data.table "dcast"
#' @export

make_SCREAM_analysis_ready_data <- function(){


  Kdrivepath <- "K:\\_BFTProjects\\ESA_Corals\\"



  # load species name codes
  ncrmp_frrp_sppcodes <- ncrmp_frrp_sppcodes %>% dplyr::select('scream9909code', 'species_name', 'fl_ncrmp_code')

  flk_grid <- FLK_2020_sample_frame

  tort_grid <- Tort_2020_sample_frame



  ###############
  # Load & Tidy SCREAM data: 2015, 2012, 2009, 2005-2006, 1999-2002
  ###############

  #####
  # Prep & Tidy 1999-2002, 2005, 2006, 2009 demo data

  # 1999-2002 Demo data
  # 1999-2002 Demo data
  SCREAM_raw_9902 <- read.csv(paste(Kdrivepath, "RawData\\SCREAM_updated\\Colony_level_data\\All_corals_demographics\\Raw Data\\Coralsz9902.csv", sep = ""), na.strings = c(NA, -9))
  # 2005, 2006 Demo data
  SCREAM_raw_0506 <- read.csv(paste(Kdrivepath, "RawData\\SCREAM_updated\\Colony_level_data\\All_corals_demographics\\Raw Data\\Coralsz0506.csv", sep = ""), na.strings = c(NA, -9))
  # 2009 demo data
  SCREAM_raw_09   <- read.csv(paste(Kdrivepath, "RawData\\SCREAM_updated\\Colony_level_data\\All_corals_demographics\\Raw Data\\Coralsz2009.csv", sep = ""), na.strings = c(NA, -9))

  # make placeholder df of non-scleractinian coral species
  nonscl <- c("UNHC", "MALC", "MCOM", "NOSC", "AAAA", "xxxx")

  # Combine and tidy years
  SCREAM_9909 <- dplyr::bind_rows(SCREAM_raw_9902,
                                  SCREAM_raw_0506,
                                  SCREAM_raw_09) %>%
    ## Filter
    # remove non-scleractinian corals
    dplyr::filter(!specode %in% nonscl) %>%
    # remove years = 9999 (SAS remnant placeholder)
    dplyr::filter(year < 9999) %>%
    # Remove 0 for maxdiam
    dplyr::filter(maxdiam > 0) %>%
    ## new columns:
    # calculate difference in depth for rugosity
    ## note: all 1999 and some 2000 sites do NOT have a mindep -> set these to NA
    ## note: many 2005 sites have 0 mindep and 0 maxdep -> set these to NA
    dplyr::mutate(rugosity = ifelse(!is.na(mindep),
                                    abs(maxdep - mindep),
                                    NA),
                  rugosity = ifelse(mindep == 0 & maxdep == 0,
                                    NA,
                                    rugosity),
                  # Add a present column
                  PRESENT = 1) %>%
    # select specific columns to keep
    dplyr::select(year, site, stn, latdd, londd, rugosity, maxdep, mindep, region, subreg,
                  hab, area, specode, maxdiam, maxht, pdiam, recdead,
                  lngdead, bleach, disease) %>%
    ## Tidy & cleanup
    # capitalize all columns
    setNames(toupper(names(.))) %>%
    # convert REGION COLUMN TO TEXT
    dplyr::mutate(REGION = ifelse(REGION == 1 | # Biscayne
                                    REGION == 2 | # Upper Keys
                                    REGION == 3 | # Middle Keys
                                    REGION == 4, "FLK", # Lower Keys
                                  "Tortugas")  ) %>% # Dry Tortugas
    # Rename some columns to match NCRMP
    dplyr::rename(
      PRIMARY_SAMPLE_UNIT = SITE,
      MAX_DEPTH = MAXDEP,
      MIN_DEPTH = MINDEP,
      SUB_REGION_NR = SUBREG,
      LAT_DEGREES = LATDD,
      LON_DEGREES = LONDD,
      HABITAT_CD = HAB,
      MAX_DIAMETER = MAXDIAM,
      PERP_DIAMETER = PDIAM,
      HEIGHT = MAXHT,
      OLD_MORT = LNGDEAD,
      RECENT_MORT = RECDEAD,
      BLEACH_CONDITION = BLEACH,
      METERS_COMPLETED = AREA,
      STATION_NR = STN
    ) %>%
    # convert disease to present and absent to P and A, consistent w/ other years and geographies
    dplyr::mutate(DISEASE = dplyr::case_when(DISEASE >= 1 ~ "P",
                                             DISEASE == 0 ~ "A", TRUE ~ NA_character_)) %>%# convert disease to present and absent to P and A, consistent w/ other years and geographies
    dplyr::mutate(BLEACH_CONDITION = dplyr::case_when(BLEACH_CONDITION >= 1 ~ "P",
                                                      BLEACH_CONDITION == 0 ~ "A", TRUE ~ NA_character_))


  #####
  # SCREAM 2012 Presence/Absence only

  # Prep & tidy SCREAM 2012 coral data
  SCREAM_12_DEN <- read.csv(paste(Kdrivepath, "RawData\\SCREAM_updated\\Corals2012.csv", sep = ""), na.strings = NA) %>%
    # select columns to keep
    dplyr::select(Year, Site, Stn, Area, Species, Size) %>%

    ## Filtering
    # remove 9999 year placeholder columns
    dplyr::filter(Year < 9999) %>%
    # remove the non-scleractinian corals
    dplyr::filter(!Species %in% nonscl) %>%
    # remove size bin 0 then remove the size column
    dplyr::filter(Size > 0) %>%

    ## Add, remove, rename reformat columns
    # add a present column
    dplyr::mutate(PRESENT = 1) %>%
    # remove size column (data are presence - absence only)
    dplyr::select(-Size) %>%
    # Rename some columns
    dplyr::rename(PRIMARY_SAMPLE_UNIT = Site,
                  METERS_COMPLETED = Area,
                  STATION_NR = Stn,
                  SPECODE = Species) %>%
    # Add in columns to make the 2012 data match the 2005, 2006, and 2009 dat so you can calculate the weights at the same time
    dplyr::mutate(MAX_DIAMETER = NA,
                  PERP_DIAMETER = NA,
                  HEIGHT = NA,
                  OLD_MORT = NA,
                  RECENT_MORT = NA,
                  BLEACH_CONDITION = NA,
                  SUBREGION_NR = NA,
                  DISEASE = NA) %>%
    # capitalize all columns
    setNames(toupper(names(.)))

  # Prep & tidy SCREAM 2012 site data
  SCREAM_12_sites <- read.csv(paste(Kdrivepath, "RawData\\SCREAM_updated\\Stations2012.csv", sep = ""), na.strings = NA) %>%
    # select columns to keep
    dplyr::select(Year, Site, Stn, Region, Habitat, LatDD, LonDD, Dep, MaxDep, MinDep) %>%
    # add rugosity (MaxDep - MinDep)
    dplyr::mutate(rugosity = abs(MaxDep - MinDep)) %>%
    ## Filtering
    # remove 9999 year placeholder columns
    dplyr::filter(Year < 9999) %>%
    # capitalize all columns
    setNames(toupper(names(.))) %>%
    # rename columns to match NCRMP
    dplyr::rename(PRIMARY_SAMPLE_UNIT = SITE,
                  STATION_NR = STN,
                  HABITAT_CD = HABITAT,
                  LAT_DEGREES = LATDD,
                  LON_DEGREES = LONDD,
                  MAX_DEPTH = MAXDEP,
                  MIN_DEPTH = MINDEP)

  # Merge 2012 demo and site information
  SCREAM_12 <- merge(SCREAM_12_sites,
                     SCREAM_12_DEN,
                     by = c("YEAR",
                            "PRIMARY_SAMPLE_UNIT",
                            "STATION_NR")) %>%
    # Change region column
    dplyr::mutate(REGION = ifelse(REGION == 1 |  # Biscayne
                                    REGION == 2 | # Upper Keys
                                    REGION == 3 | # Middle Keys
                                    REGION == 4, "FLK",  # Lower Keys
                                  "Tortugas")  #Dry Tortugas
    ) %>%
    dplyr::mutate(REGION = as.factor(REGION))


  ### Combine 1999-2009 with 2012 SCREAM data
  SCREAM_demo <- dplyr::bind_rows(SCREAM_9909, SCREAM_12)  %>%
    dplyr::mutate(DISEASE = as.factor(DISEASE),
                  region = as.factor(REGION),
                  SPECODE = as.character(SPECODE),
                  PRESENT = 1,
                  fieldid = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "_"))

  # make df of unique sites
  SCREAM_demo_sites <- unique(SCREAM_demo[, c("REGION", "YEAR", "fieldid", "PRIMARY_SAMPLE_UNIT", "STATION_NR", "HABITAT_CD",
                                              "METERS_COMPLETED", "LAT_DEGREES", "LON_DEGREES", "MIN_DEPTH", "MAX_DEPTH", "RUGOSITY")])

  SCREAM_demo <-  SCREAM_demo %>%
    dplyr::select(SPECODE, fieldid,
                  MAX_DIAMETER, PERP_DIAMETER, HEIGHT, OLD_MORT,
                  RECENT_MORT, BLEACH_CONDITION, DISEASE, PRESENT)


  SCREAM_demo2 <- SCREAM_demo  %>%
    # add 0s in for species not observed
    tidyr::expand(.,SPECODE, fieldid) %>%
    # connect back to demo_data to fill in with NAs
    dplyr::full_join(., SCREAM_demo) %>%
    dplyr::mutate(N = dplyr::case_when(PRESENT ==1 ~ 1,
                                       TRUE ~ 0)) %>% # Reformat species codes to NCRMP
    dplyr::left_join(., ncrmp_frrp_sppcodes %>%
                       dplyr::select("fl_ncrmp_code", "scream9909code", "species_name"),
                     by = c( "SPECODE" = "scream9909code")) %>%
    dplyr::rename(CORAL_CD = fl_ncrmp_code,
                  SPECIES_NAME = species_name) %>%
    dplyr::select(-SPECODE) %>%
    dplyr::full_join(., SCREAM_demo_sites) %>%
    dplyr::select(REGION, YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR, LAT_DEGREES, LON_DEGREES, HABITAT_CD, METERS_COMPLETED,
                  MIN_DEPTH, MAX_DEPTH, RUGOSITY, CORAL_CD, SPECIES_NAME, MAX_DIAMETER, PERP_DIAMETER, HEIGHT, OLD_MORT,
                  RECENT_MORT, BLEACH_CONDITION, DISEASE, N)





  # There is no SEFCRI data in this SCREAM dataset so move right into FLK (!!!!!)

  ######################
  # FLK



    # select FLK demo
    SCREAM_FLK <- SCREAM_demo2 %>% dplyr::filter(REGION != "Tortugas")

    # select FLK sites
    SCREAM_FLK_sites  <- SCREAM_demo_sites %>% dplyr::filter(REGION != "Tortugas")

    library(rgdal)

    # merge FLK with FL NCRMP 50m grid
    data_grid <- extract_grid_data(field_sites = SCREAM_FLK_sites,
                                   grid = flk_grid,
                                   region = "FLK")


    # Create foredep variable
    data_grid$foredep <- with(data_grid,
                              ifelse(ZONE_NR == 4 & MAX_DEPTH < 6, "1",
                                     ifelse(ZONE_NR == 4 & 6.0 <= MAX_DEPTH & MAX_DEPTH < 18.0, "2",
                                            ifelse(ZONE_NR == 4 & MAX_DEPTH >= 18.0, "3", 0))))

    data_grid$foredep <- with(data_grid, ifelse(habclass_grid == "SPGR_HR", "1", foredep))

    # add in management field (prot=1 in SPAs)
    data_grid$PROT <- with(data_grid, ifelse(MPA_NR > 0, 1, 0))


    demo_data_FLK <- dplyr::full_join(SCREAM_FLK, data_grid) %>%


      dplyr::mutate(HABITAT_CD = dplyr::case_when(habclass_grid == "SPGR_HR" & is.na(RUGOSITY) ~ "SPGR_HR",
                                                  habclass_grid == "SPGR_LR" & is.na(RUGOSITY) ~ "SPGR_LR",
                                                  habclass_grid == "ISOL_MR" & is.na(RUGOSITY) ~ "ISOL_MR",
                                                  habclass_grid == "ISOL_LR" & is.na(RUGOSITY) ~ "ISOL_LR",
                                                  habclass_grid == "SPGR_HR" & RUGOSITY >= 0.7 ~ "SPGR_HR",
                                                  habclass_grid == "SPGR_HR" & RUGOSITY < 0.7 ~ "SPGR_LR",
                                                  habclass_grid == "SPGR_LR" & RUGOSITY >= 0.7 ~ "SPGR_HR",
                                                  habclass_grid == "SPGR_LR" & RUGOSITY < 0.7 ~ "SPGR_LR",
                                                  habclass_grid == "CONT_LR" ~ "CONT_LR",
                                                  habclass_grid == "ISOL_MR" & RUGOSITY >= 0.7 ~ "ISOL_MR",
                                                  habclass_grid == "ISOL_MR" & RUGOSITY < 0.7 & RUGOSITY >= 0.3 ~ "ISOL_MR",
                                                  habclass_grid == "ISOL_MR" & RUGOSITY < 0.3 ~ "ISOL_LR",
                                                  habclass_grid == "ISOL_LR" & RUGOSITY >= 0.7 ~ "ISOL_MR",
                                                  habclass_grid == "ISOL_LR" & RUGOSITY < 0.7 & RUGOSITY >= 0.3 ~ "ISOL_MR",
                                                  habclass_grid == "ISOL_LR" & RUGOSITY < 0.3 ~ "ISOL_LR",
                                                  habclass_grid == "RUBB_LR" ~ "RUBB_LR", TRUE ~ as.character(habclass_grid)))  %>%
      # add STRAT
      dplyr::mutate(STRAT = dplyr::case_when(HABITAT_CD == "SPGR_HR" ~ "HRRF",
                                             ZONE_NR == 1 ~ "INPR",
                                             ZONE_NR == 2 ~ "MCPR",
                                             ZONE_NR == 3 ~ "OFPR",
                                             HABITAT_CD == "CONT_LR" & foredep == 1 ~ "FSLR",
                                             HABITAT_CD == "ISOL_LR" & foredep == 1 ~ "FSLR",
                                             HABITAT_CD == "RUBB_LR" & foredep == 1 ~ "FSLR",
                                             HABITAT_CD == "SPGR_LR" & foredep == 1 ~ "FSLR",
                                             HABITAT_CD == "ISOL_MR" & foredep == 1 ~ "FSLR",
                                             foredep == 2 ~ "FMLR",
                                             foredep == 3 & PROT == 0 ~ "FDLR",
                                             foredep == 3 & PROT == 1 ~ "FMLR", TRUE ~ NA_character_)) %>%

      dplyr::mutate(STRAT = as.factor(STRAT)) %>%
      setNames(toupper(names(.))) %>%
      dplyr::mutate(RUGOSITY_CD = NA_character_,
                    WTD_RUG = NA_character_,
                    ADMIN = NA_character_,
                    SURVEY = "SCREAM",
                    MONTH = NA_character_,
                    DAY = NA_character_,
                    SUB_REGION_NAME = SUB_REGION,
                    SPECIES_CD = CORAL_CD,
                    DEPTH_STRAT = NA_character_,
                    MIN_DEPTH = (MIN_DEPTH/3.28084),
                    MAX_DEPTH = (MAX_DEPTH/3.28084),
                    REGION = "FLK") %>%
      dplyr::select(SURVEY, REGION, YEAR, MONTH, DAY, PRIMARY_SAMPLE_UNIT, STATION_NR,
                    LAT_DEGREES, LON_DEGREES, MAPGRID_NR, RUGOSITY_CD, WTD_RUG,
                    SUB_REGION_NAME, SUB_REGION_NR, ZONE_NAME, ZONE_NR, PROT, ADMIN,  MPA_NAME, MPA_NR,
                    HABITAT_CD, STRAT, DEPTH_STRAT, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED,
                    SPECIES_CD, SPECIES_NAME, N,
                    MAX_DIAMETER, PERP_DIAMETER, HEIGHT,
                    OLD_MORT, RECENT_MORT, BLEACH_CONDITION, DISEASE)  %>%
      # Remove undefined STRAT
      dplyr::filter(!is.na(MAPGRID_NR))






    # select Tortugas demo
    SCREAM_Tort   <- SCREAM_demo2 %>%
      # keep only Tortugas
      dplyr::filter(REGION != "FLK") %>%
      # remove columns that get replaced by grid
      dplyr::select(-HABITAT_CD)

    # select Tort sites
    SCREAM_Tort_sites  <- SCREAM_demo_sites %>%
      dplyr::filter(REGION != "FLK") %>%
      # remove columns that get replaced by grid
      dplyr::select(-HABITAT_CD)

    # merge FLK with FL NCRMP 100m grid
    data_grid <- extract_grid_data(field_sites = SCREAM_Tort_sites,
                                   grid = tort_grid,
                                   region = "Tortugas")



    demo_data_Tort <- dplyr::full_join(SCREAM_Tort, data_grid) %>%

      dplyr::mutate(PROT = dplyr::case_when(SUB_REGION_NR == 3 & MPA_NR > 0 ~ 1,
                                            SUB_REGION_NR == 4 ~ 2, TRUE ~ 0))  %>%

      dplyr::mutate(STRAT = dplyr::case_when(SUB_REGION_NR == 3 & habclass_grid == "CONT_MR" ~ "CONT_HR",
                                             SUB_REGION_NR == 3 & habclass_grid == 'SPGR_LR'~ 'ISOL_MR',
                                             SUB_REGION_NR == 3 & habclass_grid == 'ISOL_HR' & MPA_NR == 0 ~ 'ISOL_MR',
                                             habclass_grid == 'RUBB_LR' ~ 'ISOL_LR', TRUE ~  as.character(habclass_grid))) %>%

      dplyr::mutate(STRAT = as.factor(STRAT)) %>%
      setNames(toupper(names(.))) %>%
      dplyr::mutate(RUGOSITY_CD = NA_character_,
                    WTD_RUG = NA_character_,
                    ADMIN = NA_character_,
                    SURVEY = "SCREAM",
                    MONTH = NA_character_,
                    DAY = NA_character_,
                    SUB_REGION_NAME = SUB_REGION,
                    HABITAT_CD = HABCLASS_GRID,
                    SPECIES_CD = CORAL_CD,
                    DEPTH_STRAT = NA_character_,
                    MIN_DEPTH = (MIN_DEPTH/3.28084),
                    MAX_DEPTH = (MAX_DEPTH/3.28084),
                    REGION = "Tortugas") %>%
      dplyr::select(SURVEY, REGION, YEAR, MONTH, DAY, PRIMARY_SAMPLE_UNIT, STATION_NR,
                    LAT_DEGREES, LON_DEGREES, MAPGRID_NR, RUGOSITY_CD, WTD_RUG,
                    SUB_REGION_NAME, SUB_REGION_NR, ZONE_NAME, ZONE_NR, PROT, ADMIN,  MPA_NAME, MPA_NR,
                    HABITAT_CD, STRAT, DEPTH_STRAT, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED,
                    SPECIES_CD, SPECIES_NAME, N,
                    MAX_DIAMETER, PERP_DIAMETER, HEIGHT,
                    OLD_MORT, RECENT_MORT, BLEACH_CONDITION, DISEASE) %>%
      # Remove undefined STRAT
      dplyr::filter(!is.na(MAPGRID_NR))


  coral_demo <- dplyr::bind_rows(demo_data_FLK, demo_data_Tort)


  # QAQC check for species name mismatches
  demo_species_name_mismatch <- coral_demo %>%
    dplyr::filter(is.na(SPECIES_NAME))

  # QAQC check for sites with no corals
  sites_no_corals <- coral_demo %>%
    dplyr::filter(SPECIES_CD == "NO CORAL")

  # QAQC size queries prior to converting corals < 4cm to NA
  max_diameter_zero <- coral_demo %>%
    dplyr::filter(MAX_DIAMETER == 0)

  perp_diameter_zero <- coral_demo %>%
    dplyr::filter(PERP_DIAMETER == 0)

  height_zero <- coral_demo %>%
    dplyr::filter(HEIGHT == 0)


  ## Label and clean corals > 4cm
  small_coralsA <- as.data.frame(coral_demo %>%
                                   # select rows with corals <4 cm in diameter or height
                                   dplyr::filter(MAX_DIAMETER < 4 & PERP_DIAMETER < 4 & HEIGHT < 4))

  small_coralsB <- as.data.frame(coral_demo %>%
                                   # select rows with corals <4 cm in diameter or height
                                   dplyr::filter(MAX_DIAMETER < 4 & is.na(PERP_DIAMETER) & HEIGHT < 4))

  small_coralsC <- as.data.frame(coral_demo %>%
                                   # select rows with corals <4 cm in diameter or height
                                   dplyr::filter(MAX_DIAMETER < 4 & PERP_DIAMETER < 4 & is.na(HEIGHT)))

  small_coralsD <- as.data.frame(coral_demo %>%
                                   # select rows with corals <4 cm in diameter or height
                                   dplyr::filter(MAX_DIAMETER < 4 & is.na(PERP_DIAMETER) & is.na(HEIGHT)))


  small_corals <- dplyr::bind_rows(small_coralsA, small_coralsB, small_coralsC, small_coralsD) %>%
    # if there are duplicate species per site, keep only the first row incidence per site
    dplyr::group_by(PRIMARY_SAMPLE_UNIT, SPECIES_CD) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    # add a 1 in a new col for JUV
    dplyr::mutate(JUV = 1) %>%

    # Convert placeholder 1s for corals < 4cm to NAs from 1 for size & mortality columns
    dplyr::mutate(MAX_DIAMETER = NA_integer_,
                  PERP_DIAMETER = NA_integer_,
                  HEIGHT = NA_integer_,
                  OLD_MORT = NA_integer_,
                  RECENT_MORT = NA_integer_,
                  BLEACH_CONDITION = as.factor(NA_character_),
                  DISEASE = as.factor(NA_character_))

  # Select the the corals =< 4cm
  not_small_corals <- coral_demo %>%
    # select rows with corals >4 cm in diameter or height or placeholder 0 spp occurrence
    dplyr::filter(MAX_DIAMETER >= 4 |
                    PERP_DIAMETER >=4 |
                    HEIGHT >= 4 |
                    N == 0) %>%
    # add a 0 in a new col for JUV
    dplyr::mutate(JUV = 0)

  # Select the corals with no size info that can still be used for density and richness
  no_size_corals <- coral_demo %>%
    dplyr::filter(is.na(MAX_DIAMETER) &
                    is.na(PERP_DIAMETER) &
                    is.na(HEIGHT) &
                    N == 1) %>%
    # add a 0 in a new col for JUV
    dplyr::mutate(JUV = 0)



  # Recombine corals > 4cm and < 4cm and not sized corals
  coral_demo <- as.data.frame(
    dplyr::bind_rows(small_corals, not_small_corals, no_size_corals) %>%
      dplyr::arrange(SPECIES_CD)
  )


  # arrange column order & drop unneeded ones:
  coral_demo <- coral_demo %>%
    dplyr::select(REGION, PRIMARY_SAMPLE_UNIT, STATION_NR,
                  YEAR, MONTH, DAY, HABITAT_CD, STRAT, RUGOSITY_CD, WTD_RUG,
                  LAT_DEGREES, LON_DEGREES, MAPGRID_NR,
                  SUB_REGION_NAME, SUB_REGION_NR, ZONE_NAME, ZONE_NR, MPA_NAME, MPA_NR, ADMIN, PROT,
                  DEPTH_STRAT, MIN_DEPTH, MAX_DEPTH, METERS_COMPLETED,
                  SPECIES_CD, SPECIES_NAME, N,JUV, MAX_DIAMETER, PERP_DIAMETER, HEIGHT,
                  OLD_MORT, RECENT_MORT, BLEACH_CONDITION, DISEASE)

  SCREAM_AR_demo <- coral_demo

  write.csv(SCREAM_AR_demo, "K:\\_BioGeoProjects\\NCRMP\\Data Analysis\\Analysis ready data\\FL\\SCREAM\\SCREAM_FL_1999_2015_2stage_coral_demographics.csv", row.names = F)

  output <- list(
    "SCREAM_AR_demo" =  SCREAM_AR_demo)

  return(output)


}
