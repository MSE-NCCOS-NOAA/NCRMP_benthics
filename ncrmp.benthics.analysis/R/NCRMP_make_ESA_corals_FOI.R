## Function to calculate FOI at the species and region level for the most recent sampling year.

# Purpose:
# creates csv files


## Tag: data analysis


# outputs created in this file --------------
# FOI
#
#

# CallS:
# Invert and ESA corals analysis ready data

# output gets called by:
# Tech memo Rmarkdown
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: Aug 2022


##############################################################################################################################

#' Calculate ESA FOI
#'
#'
#'
#'
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'


# function to Calculate weights based on the most recent sampling grid
NCRMP_make_ESA_corals_FOI <- function(){




esa_spp <- dplyr::bind_rows(USVI_2021_inverts_ESAcorals,
                        PRICO_2021_inverts_ESAcorals,
                        FLK_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020),
                        Tortugas_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020),
                        FGBNMS_2018_inverts_ESAcorals,
                        SEFCRI_2020_inverts_ESAcorals %>% dplyr::select(-RUGOSITY_CD) %>% dplyr::mutate(YEAR = 2020)) %>%
  # calculate totals by species by region
  dplyr::mutate(O_ANNULARIS = dplyr::case_when(O_ANNULARIS == "PS" ~ 1,
                                               O_ANNULARIS == "PT" ~ 1,
                                               O_ANNULARIS == "P" ~ 1,
                                               O_ANNULARIS == "A" ~ 0, TRUE ~0),
                O_FRANKSI = dplyr::case_when(O_FRANKSI == "PS" ~ 1,
                                             O_FRANKSI == "PT" ~ 1,
                                             O_FRANKSI == "P" ~ 1,
                                             O_FRANKSI == "A" ~ 0, TRUE ~0),
                O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == "PS" ~ 1,
                                               O_FAVEOLATA == "PT" ~ 1,
                                               O_FAVEOLATA == "P" ~ 1,
                                               O_FAVEOLATA == "A" ~ 0, TRUE ~0),
                A_PALMATA = dplyr::case_when(A_PALMATA == "PS" ~ 1,
                                             A_PALMATA == "PT" ~ 1,
                                             A_PALMATA == "P" ~ 1,
                                             A_PALMATA == "A" ~ 0, TRUE ~0),
                A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == "PS" ~ 1,
                                                 A_CERVICORNIS == "PT" ~ 1,
                                                 A_CERVICORNIS == "P" ~ 1,
                                                 A_CERVICORNIS == "A" ~ 0, TRUE ~0),
                D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == "PS" ~ 1,
                                               D_CYLINDRUS == "PT" ~ 1,
                                               D_CYLINDRUS == "P" ~ 1,
                                               D_CYLINDRUS == "A" ~ 0, TRUE ~0),
                M_FEROX = dplyr::case_when(M_FEROX == "PS" ~ 1,
                                           M_FEROX == "PT" ~ 1,
                                           M_FEROX == "P" ~ 1,
                                           M_FEROX == "A" ~ 0, TRUE ~0)) %>%
  dplyr::group_by(REGION, YEAR) %>%
  dplyr::summarise(OANN = sum(O_ANNULARIS),
                   OFRA = sum(O_FRANKSI),
                   OFAV = sum(O_FAVEOLATA),
                   APAL = sum(A_PALMATA),
                   ACER = sum(A_CERVICORNIS),
                   DCYL = sum(D_CYLINDRUS),
                   MFER = sum(M_FEROX),
                   .groups = "keep") %>%
  tidyr::pivot_longer(., cols = OANN:MFER,
                      names_to= "species",
                      values_to = "N")

# get total # of sites surveyed for each species
# calculate NAs by species (not counted)
esa_Nsites <-  dplyr::bind_rows(USVI_2021_inverts_ESAcorals,
                                PRICO_2021_inverts_ESAcorals,
                                FLK_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020),
                                Tortugas_2020_inverts_ESAcorals  %>% dplyr::mutate(YEAR = 2020),
                                FGBNMS_2018_inverts_ESAcorals,
                                SEFCRI_2020_inverts_ESAcorals %>% dplyr::select(-RUGOSITY_CD) %>% dplyr::mutate(YEAR = 2020)) %>%
  # calculate totals by species by region
  dplyr::mutate(O_ANNULARIS = dplyr::case_when(O_ANNULARIS == "PS" ~ 1,
                                               O_ANNULARIS == "PT" ~ 1,
                                               O_ANNULARIS == "P" ~ 1,
                                               O_ANNULARIS == "A" ~ 1,
                                               TRUE ~0),
                O_FRANKSI = dplyr::case_when(O_FRANKSI == "PS" ~ 1,
                                             O_FRANKSI == "PT" ~ 1,
                                             O_FRANKSI == "P" ~ 1,
                                             O_FRANKSI == "A" ~ 1,
                                             TRUE ~0),
                O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == "PS" ~ 1,
                                               O_FAVEOLATA == "PT" ~ 1,
                                               O_FAVEOLATA == "P" ~ 1,
                                               O_FAVEOLATA == "A" ~ 1,
                                               TRUE ~0),
                A_PALMATA = dplyr::case_when(A_PALMATA == "PS" ~ 1,
                                             A_PALMATA == "PT" ~ 1,
                                             A_PALMATA == "P" ~ 1,
                                             A_PALMATA == "A" ~ 1,
                                             TRUE ~0),
                A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == "PS" ~ 1,
                                                 A_CERVICORNIS == "PT" ~ 1,
                                                 A_CERVICORNIS == "P" ~ 1,
                                                 A_CERVICORNIS == "A" ~ 1,
                                                 TRUE ~0),
                D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == "PS" ~ 1,
                                               D_CYLINDRUS == "PT" ~ 1,
                                               D_CYLINDRUS == "P" ~ 1,
                                               D_CYLINDRUS == "A" ~ 1,
                                               TRUE ~0),
                M_FEROX = dplyr::case_when(M_FEROX == "PS" ~ 1,
                                           M_FEROX == "PT" ~ 1,
                                           M_FEROX == "P" ~ 1,
                                           M_FEROX == "A" ~ 1,
                                           TRUE ~0)) %>%
  dplyr::group_by(REGION, YEAR) %>%
  dplyr::summarise(OANN = sum(O_ANNULARIS),
                   OFRA = sum(O_FRANKSI),
                   OFAV = sum(O_FAVEOLATA),
                   APAL = sum(A_PALMATA),
                   ACER = sum(A_CERVICORNIS),
                   DCYL = sum(D_CYLINDRUS),
                   MFER = sum(M_FEROX),
                   .groups = "keep") %>%
  tidyr::pivot_longer(., cols = OANN:MFER,
                      names_to= "species",
                      values_to = "Nsites")


FOI_species <- esa_spp %>%
  dplyr::full_join(esa_Nsites) %>%
  dplyr::mutate(foi = N/Nsites)


esa_reg <- dplyr::bind_rows(USVI_2021_inverts_ESAcorals,
                        PRICO_2021_inverts_ESAcorals,
                        FLK_2020_inverts_ESAcorals %>% dplyr::mutate(YEAR = 2020),
                        Tortugas_2020_inverts_ESAcorals  %>% dplyr::mutate(YEAR = 2020),
                        FGBNMS_2018_inverts_ESAcorals,
                        SEFCRI_2020_inverts_ESAcorals %>% dplyr::select(-RUGOSITY_CD) %>% dplyr::mutate(YEAR = 2020)) %>%
  # calculate totals by species by region
  dplyr::mutate(O_ANNULARIS = dplyr::case_when(O_ANNULARIS == "PS" ~ 1,
                                               O_ANNULARIS == "PT" ~ 1,
                                               O_ANNULARIS == "P" ~ 1,
                                               O_ANNULARIS == "A" ~ 0, TRUE ~0),
                O_FRANKSI = dplyr::case_when(O_FRANKSI == "PS" ~ 1,
                                             O_FRANKSI == "PT" ~ 1,
                                             O_FRANKSI == "P" ~ 1,
                                             O_FRANKSI == "A" ~ 0, TRUE ~0),
                O_FAVEOLATA = dplyr::case_when(O_FAVEOLATA == "PS" ~ 1,
                                               O_FAVEOLATA == "PT" ~ 1,
                                               O_FAVEOLATA == "P" ~ 1,
                                               O_FAVEOLATA == "A" ~ 0, TRUE ~0),
                A_PALMATA = dplyr::case_when(A_PALMATA == "PS" ~ 1,
                                             A_PALMATA == "PT" ~ 1,
                                             A_PALMATA == "P" ~ 1,
                                             A_PALMATA == "A" ~ 0, TRUE ~0),
                A_CERVICORNIS = dplyr::case_when(A_CERVICORNIS == "PS" ~ 1,
                                                 A_CERVICORNIS == "PT" ~ 1,
                                                 A_CERVICORNIS == "P" ~ 1,
                                                 A_CERVICORNIS == "A" ~ 0, TRUE ~0),
                D_CYLINDRUS = dplyr::case_when(D_CYLINDRUS == "PS" ~ 1,
                                               D_CYLINDRUS == "PT" ~ 1,
                                               D_CYLINDRUS == "P" ~ 1,
                                               D_CYLINDRUS == "A" ~ 0, TRUE ~0),
                M_FEROX = dplyr::case_when(M_FEROX == "PS" ~ 1,
                                           M_FEROX == "PT" ~ 1,
                                           M_FEROX == "P" ~ 1,
                                           M_FEROX == "A" ~ 0, TRUE ~0)) %>%
  dplyr::mutate(N_esa = O_ANNULARIS+O_FRANKSI+O_FAVEOLATA+A_PALMATA+A_CERVICORNIS+D_CYLINDRUS+M_FEROX) %>%
  dplyr::mutate(esa_present = dplyr::case_when(N_esa > 0 ~ 1, TRUE ~ 0)) %>%
  dplyr::group_by(REGION, YEAR) %>%
  dplyr::summarise(N_esa = sum(esa_present), .groups = "keep")

Nsites<- esa_Nsites %>%
  dplyr::group_by(REGION, YEAR) %>%
  dplyr::arrange(desc(Nsites), .by_group = TRUE) %>%
  dplyr::summarise(Nsites=dplyr::first(Nsites))

FOI_region <- esa_reg %>%
  dplyr::full_join(Nsites) %>%
  dplyr::mutate(foi = N_esa/Nsites)



################
# Export
################

# Create list to export

output <- list("FOI_region" = FOI_region,
               "FOI_species" = FOI_species)

return(output)

}
