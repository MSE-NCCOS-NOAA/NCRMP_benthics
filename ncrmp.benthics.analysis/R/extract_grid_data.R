
## NCRMP-Benthic Caribbean
## Function to overlay site coordinates on a grid and extract grid info

# Purpose:
# imports .RData files with FL grids (reading actual grids is # out)
# subsets coordinates from input datafile
# projects coordinates to match grid
# extract grid data using subset coordinates
# merge grid data w/ coordinates back into original data frame

# outputs created in this file --------------
# field sampling sites with grid information for each NCRMP-Benthic Caribbean region



# output gets called by:
# make_analysis_ready_data.R
# make_FRRP_dataclean.R
# NCCOS NCRMP 2013-2015
# NCRMP_clean_boatlog.R

# contributors: NCRMP-Benthic Caribbean analyses team (Viehman, Bauer, Groves, Egan)

#' Overlay site coordinates on a grid and extract grid info
#'
#' This function imports .RData files with FL grids, reprojects the grids to WGS84, subsets coordinates from the data, projects those coordinates to WGS84, extracts grid data using the subset coordinates,
#' and merges grid data with the coordinates back to the original dataframe. The function outputs demo sites with grid information for  FLK, DT, and SEFCRI.
#'
#' @family grid extractions
#' @param demo_sites A dataframe
#' @param grid A spatialPolygonsDataFrame
#' @return A spatialPolygonsDataFrame
#' @importFrom sp "coordinates"
#' @importFrom sp "CRS"
#' @importFrom sp "proj4string"
#' @importFrom sp "over"
#' @export

extract_grid_data <- function(field_sites, grid, region)
{

  #library(sp)

  # subset coordinates from site df
  xy  <- field_sites[, c("LAT_DEGREES", "LON_DEGREES")]
  # specify coordinates
  sp::coordinates(xy) <- ~LON_DEGREES+LAT_DEGREES




  ## For each unique sample geography:

  ## SEFCRI
  if(region == "SEFCRI")
  {

      grid@data <- grid@data %>%
        dplyr::mutate(SUBREG_NR = as.numeric(SUBREG_NR),
                      MPA_NR = as.numeric(MPA_NR),
                      ZONE_NR = as.numeric(ZONE_NR))


    # project coordinates to WGS84
    sp::proj4string(xy) <- sp::CRS("+init=epsg:4326")

    # reproject the grid to WGS84 [instead, use reprojected grid as input. this step takes too long]
    #sefcri_reprj <- sp::spTransform(grid, sp::CRS("+init=epsg:4326"))

    # overlay sefcri points on the reprojected grid

    # I changed the grid projection for over() to work
    # projection(grid) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    overlay_points <- sp::over(xy, grid, fn = NULL) # change 'grid' to sefcri_reprj if the above reproject step is used

    # clarify column names
    # USE THIS CODE FOR 2020 and later -- we updated the Florida grids
    overlay_points <- overlay_points %>%
      # add the sub region names based on grid number (the subregion names were missing in 2020)
      dplyr::mutate(SUB_REGION = case_when(
        SUBREG_NR == 13 ~ "Broward-Miami",
        SUBREG_NR == 14 ~ "Deerfield",
        SUBREG_NR == 15 ~ "South Palm Beach",
        SUBREG_NR == 16 ~ "North Palm Beach",
        SUBREG_NR == 17 ~ "Martin",
        TRUE ~ NA_character_)) %>%
      # update column names to indicate that they are from the grid file
      dplyr::rename(.,
                    "SUB_REGION_NR" = SUBREG_NR,
                    "DEPTH_grid" = MAP_DEPTH,
                    "rugosity_cd" = RUG_CD,
                    "lat_grid" = LAT_DEGREE,
                    "lon_grid" = LON_DEGREE,
                    "hab_grid" = MAP_HAB_CD)  %>%
      # clean column names
      dplyr::mutate(
        # add rownames to the overlay df
        rownames = rownames(overlay_points),
        REGION = region,
        MPA_NAME = NA_character_,
        ZONE_NAME = NA_character_)

    # FOR PRE-2020 (In 2020 we updated the florida grids)
    #overlay_points <- overlay_points %>%
    # update column names to indicate that they are from the grid file
    #dplyr::rename(.,
    #              "SUB_REGION" = REGION,
    #              "SUB_REGION_NR" = subreg_nr,
    #              "MAPGRID_NR" = CELLID_1,
    #              "DEPTH_grid" = Depth,
    #              "rugosity_cd" = rug_cat,
    #              "lat_grid" = lat_deg,
    #              "lon_grid" = lon_deg,
    #              "hab_grid" = hab,
    #              "STRAT" = strat)  %>%
    # clean column names
    #dplyr::mutate(
    # add rownames to the overlay df
    #  rownames = rownames(overlay_points),
    #  REGION = region,
    #  MPA_NAME = NA_character_,
    #  MPA_NR = 0,
    #  ZONE_NAME = NA_character_,
    #  ZONE_NR = NA_integer_)
  }


  ## FLK
  if (region == "FLK") {
    # project coordinates to WGS84
    sp::proj4string(xy) <- sp::CRS("+init=epsg:4326")

    # reproject the grid to WGS84
     fkdt_reprj <- spTransform(grid, CRS("+init=epsg:4326"))

    # I changed the projection of FLKcoords below because over() didn't work with different projections for the two arguments. I hope that doesn't mess things up.
     #projection(xy) <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"



     fkdt_reprj@data <- fkdt_reprj@data %>%
        dplyr::mutate(SUBREG_NR = as.numeric(SUBREG_NR),
                      MPA_NR = as.numeric(MPA_NR),
                      ZONE_NR = as.numeric(ZONE_NR))


    # overlay FLK points on the reprojected grid
    #! change 'grid' to fkdt_reprj if the above step is used!
    overlay_points <- sp::over(xy, fkdt_reprj, fn = NULL)


    # clarify column names
    # in 2020 the FL grids were updated and didn't include quite as full information,
    # that info is added here after points are overlaid on grid

      # split habitat and rugosity for a habitat only code
      hab_info <- overlay_points %>%
        dplyr::select(MAP_HAB_CD) %>%
        tidyr::separate(MAP_HAB_CD, c("hab", "rug"))
      overlay_points <- overlay_points %>%
        # add info thati s missing from grid
        dplyr::mutate(SUB_REGION = dplyr::case_when(SUBREG_NR == 8 ~ "Lower Keys",
                                             SUBREG_NR == 9 ~ "Middle Keys",
                                             SUBREG_NR == 10 ~ "Mid-Upper Keys Transition",
                                             SUBREG_NR == 11 ~ "Upper Keys",
                                             SUBREG_NR == 12 ~ "Biscayne",
                                             TRUE ~ NA_character_),
                      MPA_NAME = dplyr::case_when(MPA_NR == 0 ~ "Unprotected",
                                           MPA_NR == 1 ~ "Carysfort",
                                           MPA_NR == 2 ~ "Elbow",
                                           MPA_NR == 3 ~ "Key_Largo_Dry_Rocks",
                                           MPA_NR == 4 ~ "Grecian_Rocks",
                                           MPA_NR == 5 ~ "French",
                                           MPA_NR == 6 ~ "Molasses",
                                           MPA_NR == 7 ~ "Conch_Reef",
                                           MPA_NR == 8 ~ "Conch_RO",
                                           MPA_NR == 9 ~ "Davis",
                                           MPA_NR == 10 ~ "Hen_Chickens",
                                           MPA_NR == 11 ~ "Cheeca_Rocks",
                                           MPA_NR == 12 ~ "Alligator",
                                           MPA_NR == 13 ~ "Tennessee",
                                           MPA_NR == 14 ~ "Coffins_Patch",
                                           MPA_NR == 15 ~ "Sombrero",
                                           MPA_NR == 16 ~ "Looe_Key",
                                           MPA_NR == 17 ~ "Looe_RO",
                                           MPA_NR == 18 ~ "Newfound_Harbor",
                                           MPA_NR == 19 ~ "East_Sambo",
                                           MPA_NR == 20 ~ "West_Sambo",
                                           MPA_NR == 21 ~ "East_Dry_Rocks",
                                           MPA_NR == 22 ~ "Rock_Key",
                                           MPA_NR == 23 ~ "Sand_Key",
                                           TRUE ~ NA_character_),
                      ZONE_NAME = dplyr::case_when(ZONE_NR == 1 ~ "Inshore",
                                            ZONE_NR == 2 ~ "Mid Channel",
                                            ZONE_NR == 3 ~ "Offshore Patch Reef",
                                            ZONE_NR == 4 ~ "Forereef",
                                            TRUE ~ NA_character_),
                      hab = hab_info$hab) %>%
        dplyr::mutate(
          # add rownames to the overlay df
          rownames = rownames(overlay_points),
          # add grid region
          REGION = region) %>%
        # update column names to indicate that they are from the grid file
        dplyr::rename(.,
                      "SUB_REGION_NR" = SUBREG_NR,
                      "DEPTH_grid" = MAP_DEPTH,
                      "lat_grid" = LAT_DEGREE,
                      "lon_grid" = LON_DEGREE,
                      "hab_grid" = hab,
                      "habclass_grid" = MAP_HAB_CD
        )

  }


  ## Tortugas
  if (region == "Tortugas")
  {

      grid@data <- grid@data %>%
        dplyr::mutate(SUBREG_NR = as.numeric(SUBREG_NR),
                      MPA_NR = as.numeric(MPA_NR),
                      ZONE_NR = as.numeric(ZONE_NR))

    # project coordinates to WGS84
    sp::proj4string(xy) <- sp::CRS("+init=epsg:4326")

    # If the grid needs to be reprojected to WGS84
     tort_reprj <- spTransform(grid, CRS("+init=epsg:4326"))

    # If the xy projection needs to be changed:
    # projection(xy) <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

    # overlay sample points on grid to extract grid info [SLOW]
    overlay_points <- sp::over(xy, tort_reprj, fn = NULL)

    # clarify column names
    # split habitat and rugosity for a habitat only code
    hab_info <- overlay_points %>%
      dplyr::select(MAP_HAB_CD) %>%
      tidyr::separate(MAP_HAB_CD, c("hab", "rug"))
    overlay_points <- overlay_points %>%
      # add info that is missing from grid
      dplyr::mutate(SUB_REGION = dplyr::case_when(SUBREG_NR == 2 ~ "Tortugas--Rileys Hump",
                                                  SUBREG_NR == 3 ~ "Tortugas--Tortugas Bank",
                                                  SUBREG_NR == 4 ~ "Tortugas--Dry Tortugas NP",
                                                  SUBREG_NR == 5 ~ "Tortugas--Unmapped",
                                                  SUBREG_NR == 6 ~ "Marquesas-Tortugas Trans",
                                                  TRUE ~ NA_character_),
                    MPA_NAME = dplyr::case_when(MPA_NR == 0 ~ "Unprotected",
                                                MPA_NR == 24 ~ "Tortugas_NER",
                                                MPA_NR == 25 ~ "Tortugas_SER",
                                                MPA_NR == 26 ~ "DTNP_RNA",
                                                TRUE ~ NA_character_),
                    ZONE_NAME = dplyr::case_when(ZONE_NR == 0 ~ "Undetermined",
                                                 ZONE_NR == 4 ~ "Forereef",
                                                 ZONE_NR == 5 ~ "Deepwater",
                                                 ZONE_NR == 6 ~ "Lagoon",
                                                 ZONE_NR == 7 ~ "Bank",
                                                 TRUE ~ NA_character_),
                    hab = hab_info$hab) %>%
      dplyr::mutate(
        # add rownames to the overlay df
        rownames = rownames(overlay_points),
        # add grid region
        REGION = region) %>%
      # update column names to indicate that they are from the grid file
      dplyr::rename(.,
                    "SUB_REGION_NR" = SUBREG_NR,
                    "DEPTH_grid" = MAP_DEPTH,
                    "lat_grid" = LAT_DEGREE,
                    "lon_grid" = LON_DEGREE,
                    "hab_grid" = hab,
                    "habclass_grid" = MAP_HAB_CD
      )

  }


  ## Puerto Rico
  if (region == "PRICO")
  {
    # project coordinates to WGS84
    sp::proj4string(xy) <- sp::CRS("+init=epsg:4326")

    # overlay PR points on the reprojected grid
    overlay_points <- sp::over(xy, grid, fn = NULL)

    # add rownames to the overlay df and the original df
    overlay_points$rownames <- rownames(overlay_points)
    overlay_points$REGION <- region
  }


  ## STTSTJ
  if (region == "STTSTJ")
  {
    # project coordinates to WGS84
    sp::proj4string(xy) <- sp::CRS("+init=epsg:4326")

    # overlay STTSTJ points on the reprojected grid
    overlay_points <- sp::over(xy, grid, fn = NULL)

    # add rownames to the overlay df and the original df
    overlay_points$rownames <- rownames(overlay_points)

    overlay_points <-  overlay_points%>%
      dplyr::mutate(REGION = "STTSTJ")

  }


  ## STX
  if (region == "STX")
  {
    # project coordinates to WGS84
    sp::proj4string(xy) <- sp::CRS("+init=epsg:4326")

    # overlay STX points on the reprojected grid
    overlay_points <- sp::over(xy, grid, fn = NULL)

    # add rownames to the overlay df and the original df
    overlay_points$rownames <- rownames(overlay_points)
  }


  ## FGBNMS
  if (region == "FGBNMS")
  {
    # project coordinates to WGS84
    sp::proj4string(xy) <- sp::CRS("+init=epsg:4326")

    # overlay USVI points on the reprojected grid
    overlay_points <- sp::over(xy, grid, fn = NULL)

    # add rownames to the overlay df and the original df
    overlay_points$rownames <- rownames(overlay_points)
  }


  ## For ALL sampline geographies:

  # add rownames to the original df for subsequent combining
  field_sites$rownames <- rownames(field_sites)

  # merge input sample file with grid overly df by rownames (same order going in)
  samplesites_grid <- dplyr::left_join(field_sites, overlay_points,
                                       by = c("REGION", "rownames"))


  # return grid demo data frame
  return(samplesites_grid)

}
