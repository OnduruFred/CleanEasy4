#' Replace multiple elements to Missing.
#'
#' Function allows you to replace for multiple elements in the data set to NA
#'
#'
#' @export
#' @param  data - The data set to be operated on
#' @param  val_to_repl - elements to be replace to NA
#' @param datatype - to help replace only a given data type
#' @param var - logical condition, if set to TRUE allow you select the variables of interest.
#' @param variable - allow you to insert the variable of interest or a vector of variable if var = TRUE.



#---- Replace multiple elements to Missing

mutate_to_na = function(data,val_to_repl,datatype ="is.double",var = FALSE,variable){
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}

  if(var == FALSE){
    i = 1
    for (i in 1:length(val_to_repl)) {
      if(datatype == "is.double"){
        data = data %>% mutate_if(is.double,list(~na_if(.,val_to_repl[i])))
      }else if(datatype == "is.character"){
        data = data %>% mutate_if(is.character,list(~na_if(.,val_to_repl[i])))
      }else if(datatype == "is.numeric"){
        data = data %>% mutate_if(is.numeric,list(~na_if(.,val_to_repl[i])))
      }
      i = i + 1
    }
    return(data)
  } else if(var ==TRUE){
    i = 1
    for (i in 1:length(val_to_repl)) {
      data = data %>% mutate_at(vars(variable),list(~na_if(.,val_to_repl[i])))
      i = i + 1
    }
    return(data)
  }
}
