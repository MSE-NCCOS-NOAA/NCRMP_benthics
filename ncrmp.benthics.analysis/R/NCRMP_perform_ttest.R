
## Function to perform pair-wise, two-tailed t-tests on NCRMP domain estimates by year

# Purpose:
# create output with t-test results

## Tag: data analysis


# outputs created in this file --------------
#


# CallS:
# Subjurisdiction domain estimates

# output gets called by:
# Analysis Rmarkdown, etc.
#

# NCRMP Caribbean Benthic analytics team: Groves, Viehman
# Last update: July 2022


##############################################################################################################################

#' Creates percent cover dataframe
#'
#'
#'
#'
#' @param dataframe A dataframe of yearly domain estimates for a single subjurisdiction
#' @param metric1 The metric you are testing: density, hard coral cover, macroalgae cover, disease prevalence, bleaching prevalence
#' @param metric2 The column name of the metric you are testing: avCvr, avDen, etc.
#' @param alpha THe alpha level you want to test, ex. 0.05
#' @param n_years The number of sampling years completed in the subjurisdiction, currently 4 for St. Thomas & St. John and 3 for all other regions
#' @return A dataframe
#' @importFrom magrittr "%>%"
#' @export
#'
#'





NCRMP_perform_ttest <- function(dataframe, metric1, metric2, alpha, n_years, return_dataframe){


  in_range <- function(x, lower, upper){
    range = c(lower, upper)
    stopifnot(length(range) == 2L)
    range[1] < x & x < range[2]
  }



  # Function is expecting the following columns in the dataframe
  must_have <- c(metric2, "Var", "n_sites", "n_strat")



  if(metric1 == "coral_cover"){

    d <- dataframe %>% dplyr::filter(cover_group == "HARD CORALS")

  }

  if(metric1 == "macroalgae_cover"){

    d <- dataframe %>% dplyr::filter(cover_group == "MACROALGAE")

  }

  if(metric1 == "density"){

    d <- dataframe

  }

  if(metric1 == "old_mortality"){

    d <- dataframe

  }

  if(metric1 == "colony_size"){

    d <- dataframe %>% dplyr::select(-avCm2, -Var_cm2, -SE_cm2) %>%
      dplyr::mutate(Var = Var_cm3)

  }

  # Check that dataframe includes must_have columns, stop with error if not
  for (i in must_have) {
    if (!i %in% colnames(d)) {
      stop(paste("Column", i, "not in dataframe"))
    }
  }


  # Add degrees of freedom (df), t-value, lower confidence interval (LCI) and upper confidence interval (UCI) to dataframe (d)
  d$df       <- d$n_sites - d$n_strat
  d$t_value  <- abs(qt(alpha/2, d$df))
  d$LCI      <- unlist(d[metric2]) - (sqrt(d$Var) * d$t_value)
  d$UCI      <- unlist(d[metric2]) + (sqrt(d$Var) * d$t_value)


if(n_years > 4) {

  # Check if metric_1 is within the confidence interval of metric_2 (return True/False) and
  #       if metric_2 is within the confidence interval of metric_1 (return True/False)
  t1 <- c(in_range(x = d[1,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI), in_range(x = d[2,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
  t2 <- c(in_range(x = d[1,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI), in_range(x = d[3,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
  t3 <- c(in_range(x = d[1,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
  t4 <- c(in_range(x = d[1,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI), in_range(x = d[5,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))

  t5 <- c(in_range(x = d[2,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI), in_range(x = d[3,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
  t6 <- c(in_range(x = d[2,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
  t7 <- c(in_range(x = d[2,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI), in_range(x = d[5,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))

  t8 <- c(in_range(x = d[3,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI))
  t9 <- c(in_range(x = d[3,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI), in_range(x = d[5,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI))

  t10 <- c(in_range(x = d[4,][metric2], lower = d[5,]$LCI, upper = d[5,]$UCI), in_range(x = d[5,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI))

  return_test <- function(){
    if(any(t1==T)) {
      print(paste(metric2, d[1,2], d[2,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[1,2], d[2,2], "Significantly different p <", alpha))
    }
    if(any(t2==T)) {
      print(paste(metric2, d[1,2], d[3,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[1,2], d[3,2], "Significantly different p <", alpha))
    }
    if(any(t3==T)) {
      print(paste(metric2, d[1,2], d[4,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[1,2], d[4,2], "Significantly different p <", alpha))
    }
    if(any(t4==T)) {
      print(paste(metric2, d[1,2], d[5,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[1,2], d[5,2], "Significantly different p <", alpha))
    }

    if(any(t5==T)) {
      print(paste(metric2, d[2,2], d[3,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[2,2], d[3,2], "Significantly different p <", alpha))
    }
    if(any(t6==T)) {
      print(paste(metric2, d[2,2], d[4,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[2,2], d[4,2], "Significantly different p <", alpha))
    }
    if(any(t7==T)) {
      print(paste(metric2, d[2,2], d[5,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[2,2], d[5,2], "Significantly different p <", alpha))
    }

    if(any(t8==T)) {
      print(paste(metric2, d[3,2], d[4,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[3,2], d[4,2], "Significantly different p <", alpha))
    }
    if(any(t9==T)) {
      print(paste(metric2, d[3,2], d[5,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[3,2], d[5,2], "Significantly different p <", alpha))
    }

    if(any(t10==T)) {
      print(paste(metric2, d[4,2], d[5,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[4,2], d[5,2], "Significantly different p <", alpha))
    }

  }

}else {

  # Check if metric_1 is within the confidence interval of metric_2 (return True/False) and
  #       if metric_2 is within the confidence interval of metric_1 (return True/False)
  t1 <- c(in_range(x = d[1,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI), in_range(x = d[2,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
  t2 <- c(in_range(x = d[1,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI), in_range(x = d[3,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))
  t3 <- c(in_range(x = d[1,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[1,]$LCI, upper = d[1,]$UCI))

  t4 <- c(in_range(x = d[2,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI), in_range(x = d[3,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))
  t5 <- c(in_range(x = d[2,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[2,]$LCI, upper = d[2,]$UCI))

  t6 <- c(in_range(x = d[3,][metric2], lower = d[4,]$LCI, upper = d[4,]$UCI), in_range(x = d[4,][metric2], lower = d[3,]$LCI, upper = d[3,]$UCI))

  return_test <- function(){
    if(any(t1==T)) {
      print(paste(metric2, d[1,2], d[2,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[1,2], d[2,2], "Significantly different p <", alpha))
    }
    if(any(t2==T)) {
      print(paste(metric2, d[1,2], d[3,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[1,2], d[3,2], "Significantly different p <", alpha))
    }
    if(any(t3==T)) {
      print(paste(metric2, d[1,2], d[4,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[1,2], d[4,2], "Significantly different p <", alpha))
    }
    if(any(t4==T)) {
      print(paste(metric2, d[2,2], d[3,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[2,2], d[3,2], "Significantly different p <", alpha))
    }
    if(any(t5==T)) {
      print(paste(metric2, d[2,2], d[4,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[2,2], d[4,2], "Significantly different p <", alpha))
    }
    if(any(t6==T)) {
      print(paste(metric2, d[3,2], d[4,2],  "NOT significant at alpha = ", alpha))
    } else {
      print(paste(metric2, d[3,2], d[4,2], "Significantly different p <", alpha))
    }


  }

}


  # Return dataframe (d) if return_datafram = T, otherwise return test output
  ifelse(return_dataframe == FALSE ||
        return_dataframe == "NULL", return_test(), return(d))
}
