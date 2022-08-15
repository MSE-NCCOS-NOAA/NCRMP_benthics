

## Function objective:
#  prepare analysis ready 2 stage FRRP and SCREAM data up to 2013 to be used in the FRRP / SCREAM comparison portion of the FL NCRMP report card.

# This function begins where make_FRRP_analysis_ready_data.R subsets the entire FRRP dataset to years >= 2014 (years that overlap with NCRMP Florida)

# This function is not currently called by another function. Analysis ready exports are made here manaully as you run the data through the code. For functions
# called by make_preNCRMP_FRRP_SCREAM_analysis_ready_data, see lines 53 - 57.

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Jun 2022



# -----------------------------------------------------------------------------------------------------------------

make_preNCRMP_FRRP_SCREAM_analysis_ready_data <- function(region, userlocation, surveyname){


  Kdrivepath <- "K:\\_BFTProjects\\ESA_Corals\\"
  Tdrivepath <- "T:\\NCRMP\\Data Analysis\\"



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
      SUBREGION_NR = SUBREG,
      lat = LATDD,
      lon = LONDD,
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
    # merge with species names
    # convert species codes to NCRMP codes and remove SCREAM codes
    dplyr::left_join(., ncrmp_frrp_sppcodes, by = c('SPECODE' = 'scream9909code')) %>%

    # rename and remove some columns
    dplyr::rename(CORAL_CD = fl_ncrmp_code,
                  SPECIES_NAME = species_name)


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
                  STATION_NR = Stn) %>%
    # convert species codes to NCRMP codes and remove SCREAM codes
    dplyr::left_join(., ncrmp_frrp_sppcodes, by = c('Species' = 'scream9909code')) %>%

    # rename and remove some columns
    dplyr::rename(CORAL_CD = fl_ncrmp_code,
                  SPECIES_NAME = species_name) %>%
    dplyr::select(-Species) %>%
    # Add in columns to make the 2012 data match the 2005, 2006, and 2009 dat so you can calculate the weights at the same time
    dplyr::mutate(MAX_DIAMETER = NA,
           PERP_DIAMETER = NA,
           HEIGHT = NA,
           OLD_MORT = NA,
           RECENT_MORT = NA,
           SUBREGION_NR = NA,
           DISEASE = NA) %>%
    # capitalize all columns
    setNames(toupper(names(.)))

  SCREAM_12_psu <- SCREAM_12_DEN$PRIMARY_SAMPLE_UNIT

  # Prep & tidy SCREAM 2012 site data
  SCREAM_12_sites <- read.csv(paste(Kdrivepath, "RawData\\SCREAM_updated\\Stations2012.csv", sep = ""), na.strings = NA) %>%
    # select columns to keep
    dplyr::select(Year, Site, Stn, Region, Habitat, LatDD, LonDD, MaxDep, MinDep) %>%
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
                  lat = LATDD,
                  lon = LONDD,
                  DEPTH_MAX = MAXDEP,
                  DEPTH_MIN = MINDEP) %>%
    dplyr::filter(PRIMARY_SAMPLE_UNIT %in% SCREAM_12_psu)

  # Merge 2012 demo and site information
  SCREAM_12 <- dplyr::left_join(SCREAM_12_sites, SCREAM_12_DEN) %>%
    # Change region column
    dplyr::mutate(REGION = ifelse(REGION == 1 |  # Biscayne
                             REGION == 2 | # Upper Keys
                             REGION == 3 | # Middle Keys
                             REGION == 4, "FLK",  # Lower Keys
                           "Tortugas")  #Dry Tortugas
    ) %>%
    dplyr::mutate(REGION = as.factor(REGION))


  ### SCREAM 2015

  SCREAM_15 <- read.csv(paste(Kdrivepath, "RawData\\SCREAM_updated\\Colony_level_data\\Demographics\\Raw_data\\SCREAM2015Coral_CoralDemo.csv", sep = ""), na.strings = NA) %>%
    dplyr::select(DAY, MONTH, YEAR, Field.ID, LatDD, LonDD, divhab, habname, Area, CORAL_CD, MAX_DIAMETER, PERP_DIAMETER, HEIGHT, OLD_MORT, RECENT_MORT, BLEACH_CONDITION, DISEASE) %>%
    dplyr::rename(PRIMARY_SAMPLE_UNIT = Field.ID,
                  METERS_COMPLETED = Area,
                  SPECIES_CD = CORAL_CD,
                  lat =LatDD,
                  lon = LonDD)

  ### Combine 1999-2009 with 2012 SCREAM data
  SCREAM_demo <- dplyr::bind_rows(SCREAM_9909, SCREAM_12, SCREAM_15) %>%
    dplyr::mutate(region = as.factor(REGION))

  # make df of unique sites
  SCREAM_demo_sites <- unique(SCREAM_demo[, c("YEAR", "PRIMARY_SAMPLE_UNIT", "STATION_NR", "lat", "lon", "DEPTH", "REGION", "region",
                                              "SUBREGION_NR", "HABITAT_CD", "METERS_COMPLETED", "RUGOSITY")])

  # There is no SEFCRI data in this SCREAM dataset so move right into FLK (!!!!!)



  ######################
  # FLK

  if(region == "FLK"){

    # select FLK demo
    SCREAM_FLK <- SCREAM_demo %>% filter(region == "FLK")

    # select FLK sites
    SCREAM_FLK_sites  <- SCREAM_demo_sites %>%
      filter(REGION == "FLK") %>%
      # Create a unique field id for every PSU and station numnber
      mutate(fieldid = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "_" ))

    # merge FLK with FL NCRMP 100m grid
    SCREAM_FLK_sites_grid <- extract_grid_data(demo_sites = SCREAM_FLK_sites,
                                               grid = fkdt_reprj)
    # obsolete 200m grid (call function extract_grid_data_200m instead)
    # grid = FL_200m_grid)
    names(SCREAM_FLK_sites_grid) <- gsub("\\.x", "", names(SCREAM_FLK_sites_grid))


    # For 2005 sites with no MinDep or MaxDep (ergo, no rugosity)
    # extract rugosity data from the RVC fish data: no luck w/ 100m grid
    # tmp <- SCREAM_FLK_sites_grid %>%
    #        filter(is.na(RUGOSITY))
    # # tmp2 <- extract_RVC_rugosity_for_FRRP(sites_grid = tmp)
    #
    # # merge FLK with FL NCRMP grid
    # tmp <- extract_grid_data_200m(demo_sites = tmp, grid =  FL_200m_grid)
    #
    # #Remove the .x from lat and lon so you can merge them later
    # names(pre_NCRMP_FLK_sites_sites_grid) <- gsub("\\.x", "", names(pre_NCRMP_FLK_sites_sites_grid))
    #
    #
    # #Add hab_diver from habclass column
    # pre_NCRMP_FLK_sites_sites_grid$hab_diver <- pre_NCRMP_FLK_sites_sites_grid$habclass
    # pre_NCRMP_FLK_sites_sites_grid$habclass <- NULL
    #
    # # Extract rugosity data from the RVC fish data
    # pre_NCRMP_FLK_sites_sites_grid <- extract_RVC_rugosity_for_FRRP(sites_grid =  pre_NCRMP_FLK_sites_sites_grid)
    #
    # pre_NCRMP_FLK_sites_sites_grid$rugosity[is.na( pre_NCRMP_FLK_sites_sites_grid$rugosity)] <- "NA"


    ## Tidy output
    SCREAM_FLK_sites_grid <- SCREAM_FLK_sites_grid %>%
      # Remove the .x from lat and lon so you can merge them later
      setNames(gsub("\\.x", "", names(.))) %>%
      # Add hab_diver from habclass column
      dplyr::rename(hab_diver = habclass)

    # View how many sites were sampled per region
    # hist(SCREAM_FLK_sites_grid$YEAR)

    # calculate site weights
    # SCREAM_FLK_sites_grid$lpi_mean_depth <- SCREAM_FLK_sites_grid$DEPTH
    # SCREAM_FLK_sites_grid$rugosity <- SCREAM_FLK_sites_grid$RUGOSITY
    tmp2 <- make_preNCRMPFL_weighting(surveyname = "SCREAM",
                                      region = "FLK",
                                      cleaned_data = SCREAM_FLK,
                                      sites_grid =  SCREAM_FLK_sites_grid,
                                      strata_ntot = strata_ntot)

    tmp3 <- tmp2$data

    SCREAM_FLK <- tmp3 %>%
      # create a unique field id for every PSU and station number
      mutate(fieldid = PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "_" ) %>%
      # filter to the first stage occurrences only :(
      # filter(fieldid %in% flk_unique_sites_1stage[, "first"]) %>%
      # drop columns
      dplyr::select(-PRESENT, -DEPTH, -region, -rugosity,-SUBREGION_NR.x, -SUBREGION_NR.y, -HABITAT_CD.y, -HABITAT_CD.x) %>%  #, -DEPTH_STRAT
      # remove .X for later merge
      setNames(gsub("\\.x", "", names(.))) %>%
      # capitalize all columns
      setNames( toupper(names(.))) %>%
      # rename columns to match NCRMP
      rename(MAPGRID_NR = NUM_ID,
             SUBREGION_NR = SUBREG_NR,
             LAT_DEGREES = LAT,
             LON_DEGREES = LON,
             HABITAT_CD = HAB_DIVER) %>%
      # add placeholder column
      mutate(DEPTH_STRAT = NA)

    # Remove sites with no sampling weights due to STRAT = NA. 23 sites are removed because they have no rugosity
    SCREAM_FLK2 <- SCREAM_FLK %>% drop_na(DEMO_SAMPLING_WEIGHTS)


    FLK_scream2stg_data <- SCREAM_FLK2[, c("REGION",  "YEAR", "PRIMARY_SAMPLE_UNIT", "STATION_NR",
                                           "LAT_DEGREES", "LON_DEGREES", "MAPGRID_NR", "STRAT", 'SUBREGION_NR', "HABITAT_CD",
                                           "PROT", "DEPTH_STRAT", "DEPTH", "MPA_NR", "ZONE_NR", "NTOT",
                                           "N_DEMO_CELLS_SAMPLED", "DEMO_SAMPLING_WEIGHTS", "METERS_COMPLETED",
                                           "CORAL_CD", "SPECIES_NAME", "MAX_DIAMETER", "PERP_DIAMETER", "HEIGHT",
                                           "OLD_MORT", "RECENT_MORT", "BLEACH_CONDITION", "DISEASE")]
    # return output

    output <- list(#"FRRP_scl" = FRRP_scl_modelinput,
      "SCREAM_FLK_sites_grid" = SCREAM_FLK_sites_grid,
      "FLK_demodata" = FLK_scream1stg_data)

    return(output)

    write.csv(FLK_scream2stg_data, paste(userpath, "NCRMP\\analysis_ready_data\\SCREAM_FLK_preNCRMP_demo_data.csv", sep = ""), row.names=FALSE)
  }


  if(region == "Tortugas"){

    # select Tortugas demo
    SCREAM_Tort   <- SCREAM_demo %>%
      # keep only Tortugas
      filter(region == "Tortugas") %>%
      # new column with fieldid
      mutate(fieldid = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "_" )) %>%
      # clean colnames
      dplyr::select(-HABITAT_CD, -SUBREGION_NR, -region)

    # merge Tort with FL NCRMP 100m grid
    SCREAM_Tort_sites_grid <- extract_grid_data(demo_sites = SCREAM_Tort_sites,
                                                grid = fkdt_reprj)

    SCREAM_Tort_sites_grid <- SCREAM_Tort_sites_grid %>%
      # Remove the .x from lat and lon so you can merge them later
      setNames(gsub("\\.x", "", names(.))) %>%
      dplyr::select(-REGION)

    # # Extract rugosity data from the RVC fish data
    # SCREAM_Tort_sites_grid <-SCREAM_Tort_sites_grid %>%
    #   inner_join(Rugosity,  by = c("NUM_ID" = "MAPGRID_NR", "YEAR" = "YEAR"))

    # View how many sites were sampled per region
    #hist(SCREAM_Tort_sites_grid$YEAR)


    # calculate site weights
    tmp2 <- make_preNCRMPFL_weighting(surveyname = "SCREAM",
                                      region = "Tortugas",
                                      cleaned_data = SCREAM_Tort,
                                      sites_grid =  SCREAM_Tort_sites_grid,
                                      strata_ntot = strata_ntot)
    tmp3 <- tmp2$data


    # Create a unique field id for every PSU and station number
    SCREAM_Tort <- tmp3 %>%
      mutate(fieldid = paste(PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "_" )) %>%
      dplyr::select(-present, -DEPTH, -SUBREGION_NR, -HABITAT_CD) %>%
      setNames(toupper(names(.))) %>%
      rename(MAPGRID_NR = NUM_ID,
             SUBREGION_NR = SUBREG_NR,
             LAT_DEGREES = LAT,
             LON_DEGREES = LON,
             HABITAT_CD = HABCLASS) %>%
      mutate(DEPTH_STRAT = NA) %>%
      setNames(gsub("\\.X", "", names(.)))


    Tort_scream2stg_data <- unique(SCREAM_Tort[, c("REGION",  "YEAR", "PRIMARY_SAMPLE_UNIT", "STATION_NR",
                                                   "LAT_DEGREES", "LON_DEGREES", "MAPGRID_NR", "STRAT", 'SUBREGION_NR', "HABITAT_CD",
                                                   "PROT", "DEPTH_STRAT", "DEPTH", "MPA_NR", "ZONE_NR", "NTOT",
                                                   "N_DEMO_CELLS_SAMPLED", "DEMO_SAMPLING_WEIGHTS", "METERS_COMPLETED",
                                                   "CORAL_CD", "SPECIES_NAME", "MAX_DIAMETER", "PERP_DIAMETER", "HEIGHT",
                                                   "OLD_MORT", "RECENT_MORT", "BLEACH_CONDITION", "DISEASE")])

    # return

    output <- list(SCREAM_demo,
                   "SCREAM_Tort_sites_grid" = SCREAM_Tort_sites_grid,
                   "Tort_demodata" = Tort_scream1stg_data)

    return(output)

    # write.csv(Tort_scream2stg_data, paste(userpath, "NCRMP\\analysis_ready_data\\SCREAM_Tort_preNCRMP_demo_data.csv", sep = ""), row.names=FALSE)


  }




  if(surveyname == "FRRP"){

    ###############
    # Load and prep FRRP data
    ###############


    # Load data from make_FRRP_analysis_ready_data.R
    # This takes a few minutes....

    # You must specify region = SEFCRI to run the function, but the output is all regions (all years). It's created before make_FRRP_analysis_ready_data.R splits
    # the data into regions.

    FRRP_SEFCRI <- make_FRRP_analysis_ready_data(userlocation = "Beaufort", region  = "SEFCRI")

    FRRP_all <- FRRP_SEFCRI$FRRP_scl %>% dplyr::select(-species_name)

    # subset to before 2014

    # Select pre NCRMP years
    pre_NCRMP_FRRP <- FRRP_all[which(FRRP_all[, 'YEAR'] <= "2013" ),] %>%
      mutate(year = YEAR)


    # make df with unique sample sites
    pre_NCRMP_FRRP_sites <- unique(pre_NCRMP_FRRP[, c("region", "YEAR", "primary_sample_unit", "station_nr", "lat", "lon", "lpi_mean_depth", "divhab")])


    ###########################
    # calculate weighting of sites by FL region
    # region is specified in function call
    ###########################

    # combines with FL 2014 grid data (SEFCRI & FloridaKeys_DryTortugas, separately) for use in 'NCRMP_FL_reportcard.Rmd'

    #########################
    # SEFCRI


    if(region == "SEFCRI"){

      # select SEFCRI demo
      pre_NCRMP_SEFCRI <- pre_NCRMP_FRRP %>% filter(region == "SEFCRI")
      # select SEFCRI sites
      pre_NCRMP_SEFCRI_sites <- pre_NCRMP_FRRP_sites %>% filter(region == "SEFCRI")

      # merge SEFCRI with 100m grid
      pre_NCRMP_SEFCRI_sites_grid <- extract_grid_data(demo_sites = pre_NCRMP_SEFCRI_sites,
                                                       grid =  sefcri_reprj)

      # Add hab_diver from hab column
      pre_NCRMP_SEFCRI_sites_grid <-  pre_NCRMP_SEFCRI_sites_grid %>%
        rename(hab_diver = hab) %>%
        #Add 0 for no MPAs in this region in mpa_nr column #Add rugosity column to be consistent with NCRMP format
        mutate(mpa_nr = NA,
               rugosity = NA,
               prot = 0)


      # View how many sites were sampled per region
      #hist(pre_NCRMP_SEFCRI_sites_grid$year)

      # calculate site weights
      tmp2 <- make_preNCRMPFL_weighting(surveyname = "FRRP",
                                        region = "SEFCRI",
                                        cleaned_data = pre_NCRMP_SEFCRI,
                                        sites_grid =  pre_NCRMP_SEFCRI_sites_grid,
                                        strata_ntot = strata_ntot)


      tmp3 <- tmp2$data


      pre_NCRMP_SEFCRI <- tmp3 %>%
        # clean column names
        dplyr::select(-species_name) %>%
        # capitalize
        setNames(toupper(names(.))) %>%
        # set column names to match NCRMP
        rename(MAPGRID_NR = CELLID,
               # DEPTH = LPI_MEAN_DEPTH, ## NOTE: this is observed lpi depth
               SUBREGION_NR = SUBREG_NR,
               LAT_DEGREES = LAT,
               LON_DEGREES = LON,
               HABITAT_CD = HAB_DIVER) %>%
        # Add DEPTH_STRAT and ZONE_NR to be consistent with NCRMP
        mutate(DEPTH_STRAT = NA,
               ZONE_NR = NA,
               # add NA for prot and mpa_nr: no MPAs in SEFCRI
               PROT = NA) %>%
        # clean output format
        setNames(gsub("\\.X", "", names( .)))

      # Drop sites with no sampling weights due to unclassified strat which has no NTOT
      pre_NCRMP_SEFCRI2 <- pre_NCRMP_SEFCRI %>% drop_na(DEMO_SAMPLING_WEIGHTS)


      SEFCRI_frrp2stg_data <- pre_NCRMP_SEFCRI2[, c("REGION",  "YEAR", "PRIMARY_SAMPLE_UNIT", "STATION_NR",
                                                    "LAT_DEGREES", "LON_DEGREES", "MAPGRID_NR", "STRAT", 'SUBREGION_NR', "HABITAT_CD",
                                                    "PROT", "DEPTH_STRAT", "DEPTH", "MPA_NR", "ZONE_NR", "NTOT",
                                                    "N_DEMO_CELLS_SAMPLED", "DEMO_SAMPLING_WEIGHTS", "METERS_COMPLETED",
                                                    "CORAL_CD", "SPECIES_NAME", "MAX_DIAMETER", "PERP_DIAMETER", "HEIGHT",
                                                    "OLD_MORT", "RECENT_MORT", "BLEACH_CONDITION", "DISEASE")]

      output <- list("FRRP_scl_modelinput" = FRRP_scl,
                     "FRRP1415_SEFCRI_sites_grid" = FRRP1415_SEFCRI_sites_grid,
                     "SEFCRI_demodata" = SEFCRI_frrp1stg_data)

      return(output)
    }
    # write.csv(SEFCRI_frrp2stg_data, paste(userpath, "NCRMP\\analysis_ready_data\\FRRP_SEFCRI_preNCRMP_demo_data.csv", sep = ""), row.names=FALSE)

    ######################
    # FLK

    if(region == "FLK"){

      # select FLK demo
      pre_NCRMP_FLK   <- pre_NCRMP_FRRP %>% filter(region == "FLK")

      # select FLK sites
      pre_NCRMP_FLK_sites  <- pre_NCRMP_FRRP_sites %>% filter(region == "FLK")

      # merge FLK with FL NCRMP 100m grid
      pre_NCRMP_FLK_sites_grid <- extract_grid_data(demo_sites = pre_NCRMP_FLK_sites,
                                                    grid = fkdt_reprj)




      # Remove the .x from lat and lon so you can merge them later
      pre_NCRMP_FLK_sites_grid <- pre_NCRMP_FLK_sites_grid %>%
        setNames(gsub("\\.x", "", names(pre_NCRMP_FLK_sites_grid))) %>%
        # Add hab_diver from habclass column
        rename(hab_diver = habclass)

      # Extract rugosity data from the RVC fish data
      pre_NCRMP_FLK_sites_sites_grid <- extract_RVC_rugosity_for_FRRP(sites_grid =  pre_NCRMP_FLK_sites_grid)

      pre_NCRMP_FLK_sites_sites_grid$rugosity[is.na(pre_NCRMP_FLK_sites_sites_grid$rugosity)] <- "NA"

      # View how many sites were sampled per region
      #hist(pre_NCRMP_FLK_sites_sites_grid$year)

      # calculate site weights
      tmp2 <- make_preNCRMPFL_weighting(surveyname = "FRRP",
                                        region = "FLK",
                                        cleaned_data = pre_NCRMP_FLK,
                                        sites_grid =  pre_NCRMP_FLK_sites_sites_grid,
                                        strata_ntot = strata_ntot)

      tmp3 <- tmp2$data
      tmp3$Depth <- NULL


      pre_NCRMP_FLK <- tmp3 %>%
        # capitalize
        setNames(toupper(names(.))) %>%
        rename(MAPGRID_NR = NUM_ID,
               DEPTH = LPI_MEAN_DEPTH, ## NOTE: this is observed lpi depth
               SUBREGION_NR = SUBREGION,
               LAT_DEGREES = LAT,
               LON_DEGREES = LON,
               HABITAT_CD = HAB_DIVER) %>%
        mutate(DEPTH_STRAT = NA) %>%
        # clean output format
        setNames(gsub("\\.X", "", names(.)))

      # Remove sites with no sampling weights - Strata is na or we have no NTOT (seagrass/other)
      pre_NCRMP_FLK2 <- pre_NCRMP_FLK %>% drop_na(DEMO_SAMPLING_WEIGHTS)


      FLK_frrp2stg_data <-pre_NCRMP_FLK2[, c("REGION",  "YEAR", "MONTH", "DAY", "PRIMARY_SAMPLE_UNIT", "STATION_NR",
                                             "LAT_DEGREES", "LON_DEGREES", "MAPGRID_NR", "STRAT", 'SUBREGION_NR', "HABITAT_CD",
                                             "PROT", "DEPTH_STRAT", "DEPTH", "MPA_NR", "ZONE_NR", "NTOT",
                                             "N_DEMO_CELLS_SAMPLED", "DEMO_SAMPLING_WEIGHTS", "METERS_COMPLETED",
                                             "CORAL_CD", "SPECIES_NAME", "MAX_DIAMETER", "PERP_DIAMETER", "HEIGHT",
                                             "OLD_MORT", "RECENT_MORT", "BLEACH_CONDITION", "DISEASE")]
      # return output

      output <- list(#"FRRP_scl" = FRRP_scl_modelinput,
        "FRRP14_16_FLK_sites_grid" = FRRP14_16_FLK_sites_grid,
        "FLK_demodata" = FLK_frrp1stg_data)

      return(output)
    }

    #write.csv(FLK_frrp2stg_data, paste(userpath, "NCRMP\\analysis_ready_data\\FRRP_FLK_preNCRMP_demo_data.csv", sep = ""), row.names=FALSE)

    ########################
    # Tortugas

    if(region == "Tortugas"){

      # select Tortugas demo
      pre_NCRMP_Tort   <- pre_NCRMP_FRRP %>% filter( region == "Tortugas")
      # select Tortugas sites
      pre_NCRMP_Tort_sites   <- pre_NCRMP_FRRP_sites %>%  filter( region == "Tortugas")

      # merge FLK with FL NCRMP 100m grid
      pre_NCRMP_Tort_sites_grid <- extract_grid_data(demo_sites = pre_NCRMP_Tort_sites,
                                                     grid = fkdt_reprj)

      #Remove the .x from lat and lon so you can merge them later
      names(pre_NCRMP_Tort_sites_grid) <- gsub("\\.x", "", names( pre_NCRMP_Tort_sites_grid))


      # View how many sites were sampled per region
      #hist(pre_NCRMP_Tort_sites_grid$year)


      # calculate site weights
      tmp2 <- make_preNCRMPFL_weighting(surveyname = "FRRP",
                                        region = "Tortugas",
                                        cleaned_data = pre_NCRMP_Tort,
                                        sites_grid =  pre_NCRMP_Tort_sites_grid,
                                        strata_ntot = strata_ntot)
      tmp3 <- tmp2$data
      tmp3$Depth <- NULL

      pre_NCRMP_Tort <- tmp3 %>%
        setNames(toupper(names(.))) %>%
        rename(MAPGRID_NR = NUM_ID,
               DEPTH = LPI_MEAN_DEPTH, ## NOTE: this is observed lpi depth
               SUBREGION_NR = SUBREG_NR,
               LAT_DEGREES = LAT,
               LON_DEGREES = LON,
               HABITAT_CD = HABCLASS) %>%
        mutate(DEPTH_STRAT = NA) %>%
        setNames(gsub("\\.Y", "", names(.)))

      pre_NCRMP_Tort2 <- pre_NCRMP_Tort %>% drop_na(DEMO_SAMPLING_WEIGHTS)


      Tort_frrp2stg_data <- unique(pre_NCRMP_Tort2[, c("REGION",  "YEAR", "MONTH", "DAY", "PRIMARY_SAMPLE_UNIT", "STATION_NR",
                                                       "LAT_DEGREES", "LON_DEGREES", "MAPGRID_NR", "STRAT", 'SUBREGION_NR', "HABITAT_CD",
                                                       "PROT", "DEPTH_STRAT", "DEPTH", "MPA_NR", "ZONE_NR", "NTOT",
                                                       "N_DEMO_CELLS_SAMPLED", "DEMO_SAMPLING_WEIGHTS", "METERS_COMPLETED",
                                                       "CORAL_CD", "SPECIES_NAME", "MAX_DIAMETER", "PERP_DIAMETER", "HEIGHT",
                                                       "OLD_MORT", "RECENT_MORT", "BLEACH_CONDITION", "DISEASE")])

      # return

      output <- list(#"FRRP_scl" = FRRP_scl_modelinput,
        "FRRP14_16_Tort_sites_grid" = FRRP14_16_Tort_sites_grid,
        "Tort_demodata" = Tort_frrp1stg_data)

      return(output)
    }
    #write.csv(Tort_frrp2stg_data, paste(userpath, "NCRMP\\analysis_ready_data\\FRRP_Tortugas_preNCRMP_demo_data.csv", sep = ""), row.names=FALSE)
  }
}
