#' Replace specified variables to NA where a certain condition is met
#'
#' Function allows you to replace for multiple elements in the data set to NA
#'
#' @examples
#' df <- tibble::tribble(~x,  ~y,    ~z,
#'                           1,   "A",   -100,
#'                           3,   "N/A", -99,
#'                           NA,  NA,    -98,
#'                           -99, "E",   -101,
#'                           -98, "F",   -1)
#'
#' df
#'
#' #replace all instances of -99 with NA
#' replace_to_na_at(data = df,
#'                    .vars = "x",
#'                    condition = ~.x == -99)
#' #replace all instances of -99 & N/A with NA
#' replace_to_na_at(data = df,
#'                    .vars = c("x","z"),
#'                    condition = c(-99,"N/A"))
#'
#'
#' @export
#' @param  data - The data set to be operated on
#' @param  .vars - A character string of variables to replace with NA values
#' @param  condition - A condition required to be TRUE to set NA, all elements to be replace to NA



#---- Replace multiple elements to Missing

replace_to_na_at <- function(data,.vars,condition){
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  if(!require(tibble)){install.packages(tibble); library(tibble)}
  i = 1
  for (i in 1:length(condition)) {
    data = data %>% mutate_at(.vars,list(~na_if(.,condition[i])))
    i = i + 1
  }
  return(data)
}


#'
