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

# NCRMP Caribbean Benthic analytics team: Groves, Viehman, Williams, Krampitz
# Last update: Jan 2025


##############################################################################################################################

#' Species domain estimates for disease prevalence & bleaching prevalence for NCRMP and NCRMP + DRM data
#'
#' Calculates regional domain estimates for disease and bleaching prevalence by species,
#' for a specified region. NCRMP utilizes a stratified random sampling design.
#' Regional estimates of prevalence are weighted by the number of
#' grid cells of a stratum in the sample frame.
#' Uses data summaries created by [NCRMP_DRM_calculate_disease_prevalence_colonies()] function
#' (disease and bleaching prevalence by species and site).
#'
#'
#'
#'
#' @param project A string indicating the project, NCRMP, MIR, or NCRMP and DRM combined ("NCRMP_DRM").
#' @param region A string indicating the region. Options are: "SEFCRI", "FLK", "Tortugas", "STX", "STTSTJ", "PRICO", and "FGB".
#' @return A list dataframes, 1) bleaching prevalence domain estimates and
#' 2) disease prevalence domain estimates, for all years of a given region.
#' @importFrom magrittr "%>%"
#' @export
#'
#'
#'
#'

NCRMP_DRM_calculate_dis_ble_prevalence_species_domain <- function(project, region){

  ####species recode function####
  species_recode <- function(data) {
    data <- data %>%
      dplyr::mutate(SPECIES_CD = dplyr::recode(SPECIES_CD,
                                               "DIP CLIV" = "PSE CLIV",
                                               "DIP STRI" = "PSE STRI",
                                               'CLA ARBU' = "CLA ABRU",
                                               "ORB ANCX"="ORB SPE."))
  }

  ####get datasets for each region and project ####
  dat <- switch(region,
                "FLK" = switch(project,
                               "NCRMP" = NCRMP_FLK_2014_24_dis_ble_prev_species,
                               "MIR" = MIR_2022_dis_ble_prev_species_DUMMY,
                               "NCRMP_DRM" = NCRMP_DRM_FLK_2014_24_dis_ble_prev_species,
                               stop("Unknown project for FLK")),
                "Tortugas" = switch(project,
                                    "NCRMP" = NCRMP_Tort_2014_24_dis_ble_prev_species,
                                    "NCRMP_DRM" = NCRMP_DRM_Tort_2014_24_dis_ble_prev_species,
                                    stop("Unknown project for Tortugas")),
                "SEFCRI" = switch(project,
                                  "NCRMP_DRM" = NCRMP_DRM_SEFCRI_2014_24_dis_ble_prev_species,
                                  "NCRMP" = NCRMP_SEFCRI_2014_24_dis_ble_prev_species,
                                  stop("Unknown project for SEFCRI")),
                "PRICO" = NCRMP_PRICO_2014_23_dis_ble_prev_species,
                "STTSTJ" = NCRMP_STTSTJ_2013_23_dis_ble_prev_species,
                "STX" = NCRMP_STX_2015_23_dis_ble_prev_species,
                "FGB" = NCRMP_FGBNMS_2013_24_dis_ble_prev_species,
                stop("Unknown region")
  )

  #call helper function to recode species
  dat <- species_recode(dat) %>%
    dplyr::mutate(SPECIES_NAME = SPECIES_CD) #change species name to = species code


  #call NTOT
  ntot <- load_NTOT_species(region = region, inputdata = dat,project = project)


  ####Function that filters out Spp Corals####
  filter_corals <- function(data){
    exclude <- c("ORB SPE.", "AGA SPE.", "MYC SPE.", "SCO SPE.", "MAD SPE.", "SID SPE.",
                 "SOL SPE.", "PSE SPE.", "OTH CORA", "POR SPE.", "SCL SPE.", "OCU SPE.", "ISO SPE.")
    data <- data %>% filter(!SPECIES_CD %in% exclude)
  }


  ## coral data processing

  dat_ble_wide <- dat %>%
    dplyr::select(-Total_ble, -Total_dis, -Total_col, -DIS_PREV) %>%
    filter_corals()  # filter out spp columns

  dat_dis_wide <- dat %>%
    dplyr::select(-Total_ble, -Total_dis, -Total_col, -BLE_PREV) %>%
    filter_corals() # filter out spp columns

  # Defire Florida Groups
  FL <- c("SEFCRI", "FLK", "Tortugas")

  ####summarize helper function####
  help_summarize <- function(data){
    data <- data %>%
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

  #### Process data ####
  if(region %in% FL) {

    dat1 <- dplyr::left_join(dat_dis_wide, dat_ble_wide) %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, SPECIES_CD) %>% # Modify this line to changes analysis stratum
      help_summarize()

  } else {

    dat1 <- dplyr::left_join(dat_dis_wide, dat_ble_wide) %>%
      dplyr::mutate(ANALYSIS_STRATUM = STRAT) %>%
      dplyr::group_by(YEAR, ANALYSIS_STRATUM, STRAT, SPECIES_CD) %>% # Modify this line to changes analysis stratum
      help_summarize()
  }

  # Merge ntot with coral_est_spp
  # use a left join because spp. data werwe filtered out but not from NTOT stratum estimates
  dat2 <- left_join(dat1, ntot) %>%
    dplyr::mutate(whavDprev = wh * avDprev,
                  whavBprev = wh * avBprev,
                  whsvarD = wh^2 * svarD,
                  whsvarB = wh^2 * svarB,
                  whstdD = wh * stdD,
                  whstdB = wh * stdB,
                  n_sites = tidyr::replace_na(n_sites, 0))  %>%
    dplyr::ungroup()


  #### Check NTOT weights ####
  # weights should add up to 1 for each species in each year
  ntot_check <- dat2 %>%
    dplyr::group_by(YEAR, SPECIES_CD) %>%
    dplyr::summarize(wh_total = sum(wh))


  # set up species codes -- remove duplicate AGA spp.
  ncrmp_frrp_sppcodes2 <- ncrmp_frrp_sppcodes %>% dplyr::filter(FRRP_name != "Undaria spp")


  #### Final Domain Estimates ####
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
    dplyr::left_join(., ncrmp_frrp_sppcodes2 %>%
                       dplyr::select(fl_ncrmp_code, species_name),
                     by = c('SPECIES_CD' = 'fl_ncrmp_code')) %>%
    dplyr::filter(!is.na(SPECIES_CD))

  DomainEst_bl <- DomainEst %>%
    dplyr::select(REGION, YEAR, species_name, avBlePrev, SE_B)%>%
    dplyr::filter(!is.na(species_name))


  DomainEst_dis <- DomainEst %>%
    dplyr::select(REGION, YEAR, species_name, avDisPrev, SE_D)%>%
    dplyr::filter(!is.na(species_name))


  #### Return output ####
  output <- list(
    'DomainEst_bl' = DomainEst_bl,
    "DomainEst_dis" = DomainEst_dis,
    "ntot_check" = ntot_check)

  return(output)
}
