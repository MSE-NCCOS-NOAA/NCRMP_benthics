## Function to re-format LPI data into Darwin Core standards

# Purpose:
# Re-format analysis-ready LPI data into Darwin Core formats


## Tag: data analysis


# outputs created in this file --------------
# percent cover by site and species
# Darwin format cover data


# CallS:
# analysis ready data

# output gets called by:
#
#

# NCRMP Caribbean Benthic analytics team: Groves, viehman, Williams
# Last update: Dec 2023


##############################################################################################################################



#' Format analysis ready LPI data into Darwin Core
#'
#' Re-formats analysis ready benthic cover (LPI) data into Darwin Core standards.
#'
#' @param dat A dataframe of analysis ready LPI data to be converted into Darwin Core.
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "GOM".
#' @param year A numeric indicating the year.
#'
#' @return A list of multiple dataframes including re-formatted data and percent cover by species and site.
#' @importFrom magrittr "%>%"
#' @export
#'

make_cover_darwincore <- function(dat, region, year){

  ################
  # Calculate Percent Cover
  ################

  # modified from NCRMP_calculate_cover in analysis package
  # modified in that it only does % cover for species at site level
  # all other calculations are excluded

  if(region == "FLK" | region == "Tortugas" | region == "SEFCRI"){
    dat <- dat %>%
      dplyr::filter(SUB_REGION_NAME != "Marquesas",
                    SUB_REGION_NAME != "Marquesas-Tortugas Trans") %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " "))
  } else if(region == "GOM"){
    dat <- dat %>%
      dplyr::mutate(ANALYSIS_STRATUM = "FGBNMS")
  } else{
    dat <- dat %>% dplyr::mutate(ANALYSIS_STRATUM = STRAT)
  }



  # Load species list

  # simplify species_name for MAC FLES and MAC CALC (as were not used in NCRMP) more detailed species collected in scream survey
  ncrmp_frrp_sppcodes2 <- ncrmp_frrp_sppcodes %>%
    dplyr::select(fl_ncrmp_code, species_name, cover_group) %>%
    # remove duplicate codes
    dplyr::distinct(fl_ncrmp_code, species_name, cover_group) %>%
    dplyr::mutate(species_name = as.character(species_name)) %>%
    dplyr::mutate(cover_group = as.character(cover_group)) %>%
    dplyr::mutate(cover_group = dplyr::case_when(species_name == "Ramicrusta spp" ~ "RAMICRUSTA SPP.",
                                                 species_name == "Peysonnellia" ~ "PEYSONNELLIA",
                                                 species_name == "Turf Algae Free of Sediment" ~ "TURF ALGAE",
                                                 species_name == "Turf Algae with Sediment" ~ "TURF ALGAE",
                                                 cover_group == "ALGAE" ~ "MACROALGAE",
                                                 TRUE ~ cover_group)) %>%
    dplyr::filter(species_name != "Undaria spp")


  cover_group_key <-  ncrmp_frrp_sppcodes2

  # Create a dataframe with site level mean depths, we will add this back in later

  depth <- dat %>%
    dplyr::ungroup() %>%
    dplyr::group_by(REGION, YEAR, PRIMARY_SAMPLE_UNIT) %>%
    dplyr::summarise(MIN_DEPTH = mean(MIN_DEPTH),
                     MAX_DEPTH = mean(MAX_DEPTH))


  # Calculate percent cover of species by site

  if(region == "SEFCRI" ||
     region == "FLK" ||
     region == "Tortugas") {

    # below would need to change if we ever go back to 2 stage data

    percent_cover_species <- dat %>%
      dplyr::mutate(Percent_Cvr = HARDBOTTOM_P + SOFTBOTTOM_P + RUBBLE_P,
                    #Percent_Cvr = rowSums(.[28:30]),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES)) %>%
      dplyr::left_join(.,ncrmp_frrp_sppcodes2,  by = c( "COVER_CAT_CD" = "fl_ncrmp_code")) %>%
      dplyr::select(-HARDBOTTOM_P, -SOFTBOTTOM_P, -RUBBLE_P) %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, YEAR, MONTH, DAY, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                      ANALYSIS_STRATUM, STRAT, HABITAT_CD, MIN_DEPTH, MAX_DEPTH,
                      PROT, COVER_CAT_CD, COVER_CAT_NAME, cover_group) %>%
      dplyr::summarise(Percent_Cvr_site = mean(Percent_Cvr)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Percent_Cvr = Percent_Cvr_site) %>%
      dplyr::select(REGION, YEAR, MONTH, DAY, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                    MIN_DEPTH, MAX_DEPTH,
                    ANALYSIS_STRATUM, STRAT,
                    HABITAT_CD, PROT, COVER_CAT_CD, COVER_CAT_NAME, cover_group, Percent_Cvr)
    # add depth back in...(averaging it above doesn't work well - gives multiple depths per site)
    #percent_cover_species <- percent_cover_species %>%
    #  dplyr::left_join(., depth, by = c("REGION", "YEAR", "PRIMARY_SAMPLE_UNIT"))

  } else {

    percent_cover_species <- dat %>%
      dplyr::mutate(Percent_Cvr = HARDBOTTOM_P + SOFTBOTTOM_P + RUBBLE_P,
                    #Percent_Cvr = rowSums(.[29:31]),
                    LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                    LON_DEGREES = sprintf("%0.4f", LON_DEGREES)) %>%
      dplyr::left_join(.,ncrmp_frrp_sppcodes2,  by = c( "COVER_CAT_CD" = "fl_ncrmp_code")) %>%
      dplyr::select(-HARDBOTTOM_P, -SOFTBOTTOM_P, -RUBBLE_P) %>%
      dplyr::mutate(PROT = NA) %>%
      dplyr::select(REGION, YEAR, MONTH, DAY, SUB_REGION_NAME, ADMIN, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES,
                    MIN_DEPTH, MAX_DEPTH, ANALYSIS_STRATUM, STRAT, HABITAT_CD, PROT, COVER_CAT_CD, COVER_CAT_NAME, cover_group, Percent_Cvr)

  }



  ################
  # Format for Darwin Core
  ################


  ### First use only hard coral cover identified to species
  coral_cover_species <- percent_cover_species %>%
    # use only hard coral cover
    dplyr::filter(cover_group == "HARD CORALS") %>%
    # filter out corals not ID'd to species
    dplyr::filter(
      COVER_CAT_NAME != "Orbicella spp",
      COVER_CAT_NAME != "Agaricia spp",
      COVER_CAT_NAME != "Scleractinia spp",
      COVER_CAT_NAME != "Isophyllia spp",
      COVER_CAT_NAME != 'Madracis spp',
      COVER_CAT_NAME != 'Porites spp',
      COVER_CAT_NAME != 'Colpophyllia spp',
      COVER_CAT_NAME != "Undaria spp",
      COVER_CAT_NAME != "Caryophyllia spp",
      COVER_CAT_NAME != "Siderastrea spp",
      COVER_CAT_NAME != "Solenastrea spp",
      COVER_CAT_NAME != "Isophyllia spp",
      COVER_CAT_NAME != "Meandrina spp",
      COVER_CAT_NAME != "Mycetophyllia spp",
      COVER_CAT_NAME != "Pseudodiploria spp",
      COVER_CAT_NAME != "Oculina spp",
      COVER_CAT_NAME != "Pseudodiploria spp",
      COVER_CAT_NAME != "Diploria spp.",
      COVER_CAT_NAME != "Orbicella annularis species complex",
      COVER_CAT_NAME != "Other coral",
      COVER_CAT_NAME != "Scolymia spp") %>%
    # filter out "absences" (only comes up 3 times, pre-NCRMP QAQC)
    dplyr::filter(Percent_Cvr > 0)


  ### Update species names with worms species ID's
  # read in worms ID - ncrmp species match up list
  worms_codes <- read.csv("K:\\_BioGeoProjects\\NCRMP\\Data Analysis\\Analysis ready data\\Data Queries\\CRCP Reports & Requests\\Darwin_Core\\species_names_matched.csv")
  colnames(worms_codes)[1] = "species_name"
  worms_codes_short <- worms_codes %>%  dplyr::select(species_name, LSID)

  # merge with coral data
  dwc_dat <- coral_cover_species %>%
    # fix any spelling issues between us and worms
    dplyr::mutate(COVER_CAT_NAME = dplyr::case_when(COVER_CAT_NAME == "Helioceris cucullata" ~ "Helioseris cucullata", TRUE ~ as.character(COVER_CAT_NAME))) %>%
    dplyr::left_join(., worms_codes_short, by = c("COVER_CAT_NAME" = "species_name"))


  ### Add necessary columns for Darwin Core formatting
  dwc_dat_final <- dwc_dat %>%
    # format date first
    dplyr::mutate(Date = lubridate::ymd(paste(YEAR, MONTH, DAY, sep = "-")),
                  eventDate = lubridate::format_ISO8601(Date)) %>%
    # change species so occurence ID is separated by "_"
    tidyr::separate(COVER_CAT_NAME, into = c("genus1", "species2"), sep = " ", remove = FALSE) %>%
    dplyr::mutate(COVER_CAT_NAME2 = paste(genus1, species2, sep = "_")) %>%
    # set up locality
    dplyr::mutate(locality = dplyr::case_when(REGION == "SEFCRI" ~ "Southeast Florida",
                                              REGION == "FLK" ~ "Florida Keys, Florida",
                                              REGION == "Tortugas" ~ "Dry Tortugas, Florida",
                                              REGION == "STTSTJ" ~ "St. Thomas & St. John, US Virgin Islands",
                                              REGION == "STX" ~ "St. Croix, US Virgin Islands",
                                              REGION == "GOM" ~ "Flower Garden Banks, Gulf of Mexico",
                                              REGION == "PRICO" ~ "Puerto Rico")) %>%
    # add columns needed for archiving with OBIS
    dplyr::mutate(basisOfRecord = 'HumanObservation',
                  scientificName = COVER_CAT_NAME,
                  decimalLatitude = LAT_DEGREES,
                  decimalLongitude = LON_DEGREES,
                  countryCode = 'US',
                  kingdom = 'Animalia',
                  geodeticDatum = 'WGS84',
                  occurrenceStatus = "present",
                  scientificNameID = LSID,
                  minimumDepthInMeters = MIN_DEPTH,
                  maximumDepthInMeters = MAX_DEPTH,
                  coordinateUncertaintyInMeters = 25,
                  samplingProtocol = "Line Point-Intersect (LPI) Transect",
                  taxonRank = "Species",
                  organismQuantity = Percent_Cvr,
                  organismQuantityType = "% cover",
                  datasetName = "NOAA National Coral Reef Monitoring Program, Benthic Cover Data",
                  institutionCode = "NOAA CRCP") %>%
    # creat occurrencID last
    dplyr::mutate(occurrenceID = paste(REGION, PRIMARY_SAMPLE_UNIT, eventDate, COVER_CAT_NAME2, samplingProtocol, sep = "_"),
                  eventID = paste(REGION, PRIMARY_SAMPLE_UNIT, eventDate, samplingProtocol, sep = "_")) %>%
    # select only the columns DWC wants
    dplyr::select(occurrenceID, eventID, eventDate, basisOfRecord, countryCode,
                  locality, decimalLatitude, decimalLongitude, geodeticDatum,
                  coordinateUncertaintyInMeters, occurrenceStatus, minimumDepthInMeters,
                  maximumDepthInMeters, taxonRank, kingdom, scientificNameID,
                  scientificName, organismQuantity, organismQuantityType,
                  samplingProtocol, datasetName, institutionCode)





  ################
  # Export
  ################

  # Create list to export
  output <- list(
    "percent_cover_species" = percent_cover_species,
    "Darwin_format_cover" = dwc_dat_final
  )

  return(output)

}
