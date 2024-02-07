#' Simulate choices based on a dataframe with a design
#'
#' @param data a dataframe that includes a design repeated for the number of observations
#' @param utility a list with the utility functions, one utility function for each alternatives
#' @param setspp an integer, the number of choice sets per person
#' @param destype Is it a design created with ngene or with spdesign. Ngene desings should be stored as the standard .ngd output. spdesign should be the spdesign object design$design
#' @return a dataframe that includes simulated choices and a design
#' @export
#'
#' @examples \dontrun{simulate_choices(datadet, ut,setspp)}
simulate_choices <- function(data, utility, setspp, destype, bcoefficients) {  #the part in dataset that needs to be repeated in each run
<<<<<<< HEAD

### unpack the bcoeff list
  bsq <- bcoefficients$bsq
  bredkite <- bcoefficients$bredkite
  bdistance <- bcoefficients$bdistance
  bcost <- bcoefficients$bcost
  bfarm2 <- bcoefficients$bfarm2
  bfarm3 <- bcoefficients$bfarm3
  bheight2 <- bcoefficients$bheight2
  bheight3 <- bcoefficients$bheight3
=======
  ### unpack the bcoeff list so variables are accessible
  for (key in names(bcoefficients)) {
    assign(key, bcoefficients[[key]])
  }
>>>>>>> 0c1de67f1d4f2236d97e706a4b99a89bdba8a0b3



  by_formula <- function(equation){ #used to take formulas as inputs in simulation utility function
    # //! cur_data_all may get deprecated in favor of pick
    dplyr::pick(dplyr::everything()) |>
    #cur_data_all() |>
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


  cat("\n dataset final_set exists: ",exists("final_set"), "\n")

  if(exists("final_set")) data = dplyr::left_join(data,final_set, by = "ID")

  cat("\n decisiongroups exists: " ,exists("decisiongroups"))

  if(exists("decisiongroups"))  {     ### create a new variable to classify decision groups.

    data = dplyr::mutate(data,group = as.numeric(cut(dplyr::row_number(),
                                              breaks = decisiongroups * dplyr::n(),
                                              labels = seq_along(decisiongroups[-length(decisiongroups)]),
                                              include.lowest = TRUE)))

    print(table(data$group))
  } else {

    data$group=1
  }


  data<- data %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(!!! manipulations)



  subsets<- split(data,data$group)

  subsets <-  purrr::map2(.x = seq_along(utility),.y = subsets,
                   ~ dplyr::mutate(.y,purrr::map_dfc(utility[[.x]],by_formula)))

  data <-dplyr::bind_rows(subsets)

  data<- data %>%
    dplyr::rename_with(~ stringr::str_replace(.,pattern = "\\.","_"), tidyr::everything()) %>%
    dplyr::mutate(dplyr::across(.cols=n,.fns = ~ evd::rgumbel(setspp,loc=0, scale=1), .names = "{'e'}_{n}" ),
           dplyr::across(dplyr::starts_with("V_"), .names = "{'U'}_{n}") + dplyr::across(dplyr::starts_with("e_")) ) %>% dplyr::ungroup() %>%
    dplyr::mutate(CHOICE=max.col(.[,grep("U_",names(.))])
    )   %>%
    as.data.frame()




  cat("\n data has been made \n")

  cat("\n First few observations \n ")
  print(utils::head(data))
  cat("\n \n ")
  return(data)

}
