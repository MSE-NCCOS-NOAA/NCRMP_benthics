## Function to re-format coral demographic data into Darwin Core standards

# Purpose:
# Re-format analysis-ready coral demographic data into Darwin Core formats


## Tag: data analysis


# outputs created in this file --------------
# a table that checks for duplicate occurrence ID's
# Darwin format coral demographic data


# CallS:
# analysis ready data

# output gets called by:
#
#

# NCRMP Caribbean Benthic analytics team: Groves, viehman, Williams
# Last update: Dec 2023


##############################################################################################################################




#' Format analysis ready demo data into Darwin Core
#'
#' @param dat A dataframe of analysis ready coral demographic data to be converted into Darwin Core format.
#'
#' @return A list of multiple dataframes including re-formatted data and another that checks for duplicate occurrence ID's.
#' @importFrom magrittr "%>%"
#' @export
#'

make_demo_darwincore <- function(dat){

  # Load data

  dat <- dat %>%
    # filter out spp columns
    dplyr::filter(
      SPECIES_NAME != "Orbicella spp",
      SPECIES_NAME != "Agaricia spp",
      SPECIES_NAME != "Scleractinia spp",
      SPECIES_NAME != "Isophyllia spp",
      SPECIES_NAME != 'Madracis spp',
      SPECIES_NAME != 'Porites spp',
      SPECIES_NAME != 'Colpophyllia spp',
      SPECIES_NAME != "Undaria spp",
      SPECIES_NAME != "Caryophyllia spp",
      SPECIES_NAME != "Siderastrea spp",
      SPECIES_NAME != "Solenastrea spp",
      SPECIES_NAME != "Isophyllia spp",
      SPECIES_NAME != "Meandrina spp",
      SPECIES_NAME != "Mycetophyllia spp",
      SPECIES_NAME != "Pseudodiploria spp",
      SPECIES_NAME != "Oculina spp",
      SPECIES_NAME != "Pseudodiploria spp",
      SPECIES_NAME != "Orbicella annularis species complex",
      SPECIES_NAME != "Other coral",
      SPECIES_NAME !=	"Scolymia spp") %>%
    dplyr::mutate(SPECIES_NAME = dplyr::recode(SPECIES_NAME, 'Helioceris cucullata' = "Helioseris cucullata"))

  dat_spp <- dat %>%
    dplyr::ungroup() %>%
    dplyr::group_by(REGION, YEAR, PRIMARY_SAMPLE_UNIT, SPECIES_NAME) %>%
    dplyr::summarise(Abundance = sum(N),
                     MIN_DEPTH = mean(MIN_DEPTH),
                     MAX_DEPTH = mean(MAX_DEPTH), .groups = "keep") %>%
    dplyr::mutate(occurrenceStatus = dplyr::case_when(Abundance > 0 ~ "present", Abundance == 0 ~ "absent"))

  dat_sites <- dat  %>%
    dplyr::ungroup() %>%
    dplyr::select(REGION, MONTH, DAY, YEAR, PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES) %>%
    dplyr::mutate(siteID = paste(REGION, YEAR, PRIMARY_SAMPLE_UNIT)) %>%
    dplyr::distinct(siteID, .keep_all = TRUE)

  dat_1 <- dplyr::left_join(dat_spp, dat_sites)

  species <- read.csv("K:\\_BioGeoProjects\\NCRMP\\Data Analysis\\Analysis ready data\\Data Queries\\CRCP Reports & Requests\\Darwin_Core\\species_names_matched.csv") %>%
    dplyr::filter(ï..species_name != "Other coral")

  dat1 <- dplyr::left_join(dat_1, species, by = c("SPECIES_NAME"="ï..species_name")) %>%
    dplyr::ungroup() %>%
    tidyr::separate(ScientificName_accepted, into = c("genus1", "species2"), sep = " ", remove = FALSE) %>%
    dplyr::mutate(ScientificName_accepted2 = paste(genus1, species2, sep = "_"))




  ### Reformatting the required columns
  dat2 <- dat1 %>%
    dplyr::ungroup() %>%
    dplyr::mutate(basisOfRecord = 'HumanObservation',
                  scientificName = ScientificName_accepted2,
                  scientificNameID = LSID,
                  Date = lubridate::ymd(paste(YEAR, MONTH, DAY, sep = "-")),
                  eventDate = lubridate::format_ISO8601(Date),
                  decimalLatitude = LAT_DEGREES,
                  decimalLongitude = LON_DEGREES,
                  countryCode = 'US',
                  locality = REGION,
                  kingdom = 'Animalia',
                  geodeticDatum = 'WGS84') %>%
    dplyr::select(-SPECIES_NAME, -LAT_DEGREES, -LON_DEGREES, -Date, -LSID, -YEAR, -MONTH, -DAY, -X, -Match.type, -AphiaID)



  ### Reformatting the recommended columns




  dat3 <- dat2 %>%
    dplyr::mutate(minimumDepthInMeters = MIN_DEPTH,
                  maximumDepthInMeters = MAX_DEPTH,
                  coordinateUncertaintyInMeters = 20,
                  samplingProtocol = "Coral_demographics",
                  taxonRank = "Species",
                  organismQuantity = Abundance,
                  organismQuantityType = "Abundance",
                  datasetName = "NOAA National Coral Reef Monitoring Program, Coral Demographic Data",
                  institutionCode = "NOAA CRCP",
                  occurrenceID = paste(REGION, PRIMARY_SAMPLE_UNIT, eventDate, scientificName, samplingProtocol, sep = "_"),
                  eventID = paste(REGION, PRIMARY_SAMPLE_UNIT, eventDate, samplingProtocol, sep = "_")) %>%
    dplyr::select(-MIN_DEPTH, -MAX_DEPTH) %>%
    dplyr::select(occurrenceID, eventID, eventDate, basisOfRecord, countryCode, locality, decimalLatitude, decimalLongitude, geodeticDatum, coordinateUncertaintyInMeters, occurrenceStatus,  minimumDepthInMeters, maximumDepthInMeters, taxonRank, kingdom, scientificNameID, scientificName, organismQuantity, organismQuantityType, samplingProtocol, datasetName, institutionCode) %>%
    dplyr::mutate(locality = dplyr::recode(locality,
                                           'FLK' = "Florida Keys, Florida",
                                           "Tortugas" = "Dry Tortugas, Florida",
                                           "SEFCRI" = "Southeast Florida",
                                           "STTSTJ" = "St. Thomas and St. John, US Virgin Islands",
                                           "STX" = "St. Croix, US Virgin Islands",
                                           "PRICO" = "Puerto Rico",
                                           "GOM" = "Flower Garden Banks, Gulf of Mexico"))

  dups <- dat3$occurrenceID[duplicated(dat3$occurrenceID)]

  dups <- as.data.frame(dups)
  #length(unique(dat3$occurrenceID))

  #write.csv(dat3, "K:\\_BioGeoProjects\\NCRMP\\Data Analysis\\Analysis ready data\\Data Queries\\CRCP Reports & Requests\\Darwin_Core\\NCRMP_demo_DarwinCore_2013_2020.csv")

  # Create list to export
  output <- list(
    "duplicate_occurrenceID" = dups,
    #"percent_cover_site" = percent_cover_site,
    #"cover_check_site" = cover_check_site,
    #"cover_group_key" = cover_group_key,
    #"cover_strata" = cover_strata,
    #"Domain_est" = Domain_est,
    #"Domain_check" = Domain_check
    "Darwin_format_demo" = dat3
  )

  return(output)

}
