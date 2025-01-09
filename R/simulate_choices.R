#' Simulate choices based on a data.frame with a design and respondents
#'
#' @param data a dataframe that includes a design repeated for the number of observations
#' @param utility a list with the utility functions, one utility function for each alternatives
#' @param setspp an integer, the number of choice sets per person
#' @param bcoeff List of initial coefficients for the utility function. List content/length can vary based on application. I ideally begins (but does not have to) with b and need be the same as those entered in the utility functions
#' @param decisiongroups A vector showing how decision groups are numerically distributed
#' @param manipulations A variable to alter terms of the utility functions examples may be applying a factor or applying changes to terms selectively for different groups
#' @param estimate If TRUE models will be estimated. If false only a dataset will be simulated. Default is true
#' @param preprocess_function = NULL You can supply a function that reads in external data (e.g. GIS coordinates) that will be merged with the simulated dataset. Make sure the the function outputs a data.frame that has a variable called ID which is used for matching.
#' @return a data.frame that includes simulated choices and a design
#' @export
#'
#' @examples \dontrun{simulate_choices(datadet, ut,setspp)}
simulate_choices <- function(data, utility, setspp, bcoeff, decisiongroups = c(0,1), manipulations = list(), estimate, preprocess_function = NULL) {  #the part in dataset that needs to be repeated in each run

  if (!is.null(preprocess_function)) {
    if (!is.function(preprocess_function)) {
      stop("`preprocess_function` must be a function.")
    } else {
      # Execute the user-supplied `preprocess_function`
      prepro_data <- preprocess_function()
      cat("\n Preprocess function has been executed.\n")
    }
  } else {
    cat("\n No preprocess function provided. Proceeding without additional preprocessing.\n")
  }


### unpack the bcoeff list so variables are accessible
  for (key in names(bcoeff)) {
    assign(key, bcoeff[[key]])
  }




  by_formula <- function(equation){ #used to take formulas as inputs in simulation utility function
      dplyr::pick(dplyr::everything()) |>
       dplyr::transmute(!!formula.tools::lhs(equation) := !!formula.tools::rhs(equation) )
  }

#  Here one can add additional case-specific data
  cat(" \n does sou_gis exist: ", exists("sou_gis"), "\n")

  if (exists("sou_gis") && is.function(sou_gis)) {
    sou_gis()

    cat("\n source of gis has been done \n")
  }


  if(!exists("manipulations")) manipulations=list() ## If no user input on further data manipulations

  n=seq_along(1:length(utility[[1]]))    # number of utility functions


  cat("\n dataset preprossed_data exists: ",exists("prepro_data"), "\n")

  if(exists("prepro_data")) data = dplyr::left_join(data,prepro_data, by = "ID")

  cat("\n decisiongroups exists: " ,length(decisiongroups)>2)

  if(length(decisiongroups)>2)  {     ### create a new variable to classify decision groups.

    data = dplyr::mutate(data,group = as.numeric(cut(dplyr::row_number(),
                                              breaks = decisiongroups * dplyr::n(),
                                              labels = seq_along(decisiongroups[-length(decisiongroups)]),
                                              include.lowest = TRUE)))

    print(table(data$group))
  } else {

    data$group=1
  }

## Do user entered manipulations to choice set
  data<- data %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(!!! manipulations)


## split dataframe into groups
  subsets<- split(data,data$group)


  ## for each group calculate utility
  subsets <-  purrr::map2(.x = seq_along(utility),.y = subsets,
                   ~ dplyr::mutate(.y,purrr::map_dfc(utility[[.x]],by_formula)))

  ##put data from eachgroup together again
  data <-dplyr::bind_rows(subsets)


## add random component and calculate total utility
  data<- data %>%
    dplyr::rename_with(~ stringr::str_replace_all(.,pattern = "\\.","_"), tidyr::everything()) %>%
    dplyr::mutate(dplyr::across(.cols=dplyr::all_of(n),.fns = ~ evd::rgumbel(setspp,loc=0, scale=1), .names = "{'e'}_{n}" ),
           dplyr::across(dplyr::starts_with("V_"), .names = "{'U'}_{n}") + dplyr::across(dplyr::starts_with("e_")) ) %>% dplyr::ungroup() %>%
    dplyr::mutate(CHOICE=max.col(.[,grep("U_",names(.))])
    )   %>%
    as.data.frame()




  cat("\n data has been created \n")

  cat("\n First few observations of the dataset \n ")
  print(utils::head(data))
  cat("\n \n ")
  return(data)

}
