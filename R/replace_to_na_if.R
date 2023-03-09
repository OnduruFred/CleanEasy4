#' Replace values with Missing(NA) based on some condition, for variables that meet some predicate
#'
#' Function allows you to replace for multiple elements in the data set to NA if condition required to be TRUE
#'
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
#' #replace all instances of "N/A","E","F" with NA
#' replace_to_na_if(data = df,
#'                    .predicate = is.character,
#'                    condition = c("N/A","E","F"))
#' #replace all instances of -99,-98,-99,-1 with NA
#' replace_to_na_if(data = df,
#'                    .predicate = is.double,
#'                  condition = c(-99,-98,-99,-1))
#'
#'
#' @export
#' @param  data - The data set to be operated on
#' @param  .predicate - A predicate function to be applied to the columns or a logical vector.
#' @param  condition - A condition required to be TRUE to set NA, all elements to be replace to NA



#---- Replace multiple elements to Missing

replace_to_na_if <- function(data,.predicate,condition){
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  i = 1
  for (i in 1:length(condition)) {
    data = data %>% mutate_if(.predicate,list(~na_if(.,condition[i])))
    i = i + 1
  }
  return(data)
}

