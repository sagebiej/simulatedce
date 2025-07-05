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
#' @import data.table
#' @examples
#' example_df <- data.frame(
#'   ID = rep(1:100, each = 4),
#'   price = rep(c(10, 10, 20, 20), 100),
#'   quality = rep(c(1, 2, 1, 2), 100)
#' )
#'
#' beta <- list(
#'   bprice   = -0.2,
#'   bquality =  0.8
#' )
#'
#' ut <- list(
#'   u1 = list(
#'     v1 = V.1 ~ bprice * price + bquality * quality,
#'     v2 = V.2 ~ 0
#'   )
#' )
#' simulate_choices(example_df, ut, setspp = 4, bcoeff = beta, estimate = FALSE)
#'
simulate_choices <- function(data, utility, setspp, bcoeff, decisiongroups = c(0, 1), manipulations = list(), estimate, preprocess_function = NULL) { # the part in dataset that needs to be repeated in each run

  if (!is.null(preprocess_function)) {
    if (!is.function(preprocess_function)) {
      stop("`preprocess_function` must be a function.")
    } else {
      # Execute the user-supplied `preprocess_function`
      prepro_data <- preprocess_function()
      if (!is.null(prepro_data) && (!is.data.frame(prepro_data) || !"ID" %in% names(prepro_data))) {
        stop("The output of `preprocess_function` must be a data.frame with a column named 'ID'.")
      }
      message("\n Preprocess function has been executed.\n")
    }
  } else {
    message("\n No preprocess function provided. Proceeding without additional preprocessing.\n")
  }

  tictoc::tic("whole simulate choices")

  tictoc::tic("assign keys for bcoeff)")
  ### unpack the bcoeff list so variables are accessible
  for (key in names(bcoeff)) {
    assign(key, bcoeff[[key]])
  }

  tictoc::toc()


  by_formula <- function(equation) { # used to take formulas as inputs in simulation utility function
    dplyr::pick(dplyr::everything()) |>
      dplyr::transmute(!!formula.tools::lhs(equation) := !!formula.tools::rhs(equation))
  }



  if (!exists("manipulations")) manipulations <- list() ## If no user input on further data manipulations

  n <- seq_along(1:length(utility[[1]])) # number of utility functions


  message("\n dataset preprossed_data exists: ", exists("prepro_data"), "\n")

  if (exists("prepro_data")) data <- dplyr::left_join(data, prepro_data, by = "ID")

  message("\n decisiongroups exists: ", length(decisiongroups) > 2)

  if (length(decisiongroups) > 2) { ### create a new variable to classify decision groups.

    data <- dplyr::mutate(data, group = as.numeric(cut(dplyr::row_number(),
      breaks = decisiongroups * dplyr::n(),
      labels = seq_along(decisiongroups[-length(decisiongroups)]),
      include.lowest = TRUE
    )))

    message(
      "\nGroup counts:\n",
      paste(utils::capture.output(print(table(data$group))), collapse = "\n")
    )
  } else {
    data$group <- 1
  }

  tictoc::tic("user entered manipulations")

  ## Do user entered manipulations to choice set
  data <- data %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(!!!manipulations)
  tictoc::toc()

  #   browser()
  #
  # d2 <- as.data.table(data)
  #
  # lhs <- as.character(formula.tools::lhs(utility$u1$v1))
  # rhs <- as.character(formula.tools::rhs(utility$u1$v1))
  # d2<- d2[, (lhs) := eval(parse(text = rhs))]

  tictoc::tic("split dataframe into groups")
  ## split dataframe into groups
  subsets <- split(data, data$group)

  tictoc::toc()

  tictoc::tic("for each group calculate utility")
  ## for each group calculate utility
  subsets <- purrr::map2(
    .x = seq_along(utility), .y = subsets,
    ~ dplyr::mutate(.y, purrr::map_dfc(utility[[.x]], by_formula))
  )

  ## put data from eachgroup together again
  data <- dplyr::bind_rows(subsets)
  tictoc::toc()

  tictoc::tic("add random component")
  ## add random component and calculate total utility
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(~ stringr::str_replace_all(., pattern = "\\.", "_"), tidyr::everything()) %>%
    dplyr::mutate(
      dplyr::across(.cols = dplyr::all_of(n), .fns = ~ evd::rgumbel(dplyr::n(), loc = 0, scale = 1), .names = "{'e'}_{n}"),
      dplyr::across(dplyr::starts_with("V_"), .names = "{'U'}_{n}") + dplyr::across(dplyr::starts_with("e_"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(CHOICE = max.col(.[, grep("U_", names(.))])) %>%
    as.data.frame()


  tictoc::toc()

  tictoc::toc()

  message("\n data has been created \n")

  message(
    "\nFirst few observations of the dataset\n",
    paste(utils::capture.output(utils::head(data)), collapse = "\n"),
    "\n\n"
  )

  return(data)
}
