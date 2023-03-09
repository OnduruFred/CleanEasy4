#' Stratified Systematic Sample for sampling AIM 3 subjects from AIM 2 in REMAPP
#'
#' Function allows you to sample AIM3 cohort from AIM 2 using Trimester as strata. for "Trimester 1","Trimester 2" & "Trimester 3"
#'
#'If you have an existing Trimester stratification variable, you must mutate it to lowercase i.e. trimester.
#'
#'
#' @examples
#' #Sampling n= 10 for "Trimester 2"
#' set.seed(123)
#' Remapp_data <- data.frame(MOMID = paste0(rep("KEARC000",50),1:50),
#' US_GA_WKS_AGE_FTS1 = rnorm( mean = 14,sd = 4,50),
#' US_GA_DAYS_AGE_FTS1 = rep(c(1:5),10),
#' TYPE_VISIT = c(rep(1,42),rep(6,3),rep(2:5),2),
#' CON_DSSTDAT = sample(seq(Sys.Date()-60,Sys.Date()-5,1),50,replace = FALSE)
#' )
#'df = strat_syst_sampling (data = Remapp_data ,n = 10,trimester = "Trimester 2",trim_var_exist = FALSE)
#'df
#'
#' @export
#' @param  data - The data set to be operated on
#' @param  n - sample size
#' @param trimester - Specify trimester stratum for for sampling i.e. "Trimester 1","Trimester 2" or "Trimester 3".
#' @param trim_var_exist - Is TRUE If trimester Variable exist else FALSE for the algorithm to help create one.


# ---  Stratified Systematic Sample

strat_syst_sampling = function(data ,n,trimester = "Trimester 1",trim_var_exist = FALSE){
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  if(!require(lubridate)){install.packages(lubridate); library(lubridate)}
  if(!require(janitor)){install.packages(janitor); library(janitor)}
  syst_sampling = function(N,n){
    set.seed(123)
    k = floor(N/n)
    r = sample(1:k, 1)
    seq(r, r + k*(n-1), k)
  }

  if(trim_var_exist == FALSE){
    #   Mutate trimester

    Excl = data %>% mutate(TYPE_VISIT1 = as.numeric(TYPE_VISIT)) %>%
      filter(TYPE_VISIT1 == 6) %>%
      select(MOMID)


    data = data %>% mutate(TYPE_VISIT1 = as.numeric(TYPE_VISIT)) %>%
      drop_na(MOMID) %>% filter(TYPE_VISIT1 == 1) %>%
      anti_join(Excl) %>%
      mutate(GA = as.numeric(US_GA_WKS_AGE_FTS1*7 + US_GA_DAYS_AGE_FTS1)/7) %>%
      mutate(today_ga = round(GA + as.numeric(today() - CON_DSSTDAT)/7,1)) %>%
      mutate(trimester = case_when(today_ga < 13 ~ "Trimester 1",
                                   today_ga >= 13 & today_ga < 27 ~ "Trimester 2",
                                   today_ga >= 27 ~ "Trimester 3"))

  }else if(trim_var_exist == TRUE){
    data = data %>% mutate(TYPE_VISIT1 = as.numeric(TYPE_VISIT))
  }



  if(trimester == "Trimester 1"){
    data = data %>% filter(trimester == "Trimester 1") %>% arrange(MOMID)
    data = data %>% mutate(id = row_number()) %>% select(id,everything())
    data[syst_sampling(N = nrow(data),n = n), ] %>% select(-TYPE_VISIT1)

  } else if(trimester == "Trimester 2"){
    data = data %>% filter(trimester == "Trimester 2") %>% arrange(MOMID)
    data = data %>% mutate(id = row_number()) %>% select(id,everything())
    data[syst_sampling(N = nrow(data),n = n), ] %>% select(-TYPE_VISIT1)

  }  else if(trimester == "Trimester 3"){
    data = data %>% filter(trimester == "Trimester 3") %>% arrange(MOMID)
    data = data %>% mutate(id = row_number()) %>% select(id,everything())
    data[syst_sampling(N = nrow(data),n = n), ] %>% select(-TYPE_VISIT1)
  }
}



