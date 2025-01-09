#' Simulate and estimate choices
#'
#' @param designfile path to a file containing a design.
#' @param no_sim Number of runs i.e. how often do you want the simulation to be repeated
#' @param respondents Number of respondents. How many respondents do you want to simulate in each run.
#' @param u A list with utility functions. The list can incorporate as many decision rule groups as you want. However, each group must be in a list in this list. If you just use one group (the normal),  this  group still  has to be in a list in  the u list. As a convention name beta coefficients starting with a lower case "b"
#' @inheritParams readdesign
#' @inheritParams simulate_choices
#' @param chunks The number of chunks determines how often results should be stored on disk as a safety measure to not loose simulations if models have already been estimated. For example, if no_sim is 100 and chunks = 2, the data will be saved on disk after 50 and after 100 runs.
#' @param utility_transform_type How the utility function you entered is transformed to the utility function required for mixl. You can use the classic way (simple) where parameters have to start with "b" and variables with "alt" or the more flexible (but potentially error prone) way (exact) where parameters and variables are matched exactly what how the are called in the dataset and in the bcoeff list. Default is "simple". In the long run, simple will be deleted, as exact should be downwards compatible.
#' @return a list with all information on the run
#' @export
#'
#' @examples \dontrun{  simchoice(designfile="somefile", no_sim=10, respondents=330,
#'  mnl_U,u=u[[1]] ,designtype="ngene")}
#'
sim_choice <- function(designfile, no_sim = 10, respondents = 330, u ,designtype = NULL, destype = NULL, bcoeff, decisiongroups=c(0,1), manipulations = list() , estimate, chunks=1, utility_transform_type = "simple") {


  #################################################
  ########## Input Validation Test ###############
  #################################################

  # Stop condition to check if chunks is a positive integer
  if (!is.numeric(chunks) || chunks <= 0 || chunks != as.integer(chunks)) {
    stop("`chunks` must be a positive integer.")
  }

  if (utility_transform_type == "simple") {
    message("'simple' is deprecated and will be removed in the future. Use 'exact' instead.")
  }



## make bcoeff clean
  bcoeff_result <- modify_bcoeff_names(bcoeff)
  bcoeff <- bcoeff_result$bcoeff
  bcoeff_lookup <- bcoeff_result$bcoeff_lookup

### make utility function clean
  u <- purrr::map(u, function(utility_group) {
    purrr::map(utility_group, function(utility) {
      # Convert the RHS of the formula to a single string
      rhs_string <- paste(deparse(formula.tools::rhs(utility)), collapse = " ")

      # Replace coefficient names in the RHS string
      modified_rhs <- stringr::str_replace_all(
        rhs_string,
        stats::setNames(
          bcoeff_lookup$modified,
          paste0("(?<![a-zA-Z0-9._])", bcoeff_lookup$original, "(?![a-zA-Z0-9._])")
        )
      )

      # Recreate the formula with the modified RHS
      stats::formula(paste(as.character(formula.tools::lhs(utility)), "~", modified_rhs))
    })
  })


#### Function to simulate ####

  estimate_sim <- function(run = 1,
                           data,  # Data for simulation
                           utility,  # Utility functions
                           setspp,   # Sets per respondent
                           bcoeff,   # Coefficients
                           decisiongroups, # Decision-making groups
                           manipulations,  # Manipulations
                           model_spec,     # Model specification for mixl
                           start_values,   # Starting values for estimation
                           availabilities  # Availability matrix
  ) {
    # Log the run number
    cat("This is Run number ", run, "\n")

    # Simulate choices
    database <- simulate_choices(data = datadet,
                                 utility = utility,
                                 setspp = setspp,
                                 bcoeff = bcoeff,
                                 decisiongroups = decisiongroups,
                                 manipulations = manipulations)

  }





# Empty list where to store all designs later on
designs_all <- list()

#### Print some messages ####

 cat("Utility function used in simulation, ie the true utility: \n\n")

     print(u)


     # future::plan("multisession")
     #
     #
     # tictoc::tic("start simulation")
     #
     # sets <- 1:no_sim %>%
     #   furrr::future_map(
     #     ~ {
     #
     #       estimate_sim(
     #         run = .x,
     #         data = datadet,
     #         utility = u,
     #         setspp = setpp,
     #         bcoeff = bcoeff,
     #         decisiongroups = decisiongroups,
     #         manipulations = manipulations,
     #         model_spec = model_spec,
     #         start_values = est,
     #         availabilities = availabilities
     #       )
     #     },
     #     .options = furrr::furrr_options(seed = TRUE)
     #   )
     # tictoc::toc()





#### Read in the design file and set core variables ####



  design<- readdesign(design = designfile, designtype = designtype, destype = destype)

  if (!("Block" %in% colnames(design))) design$Block=1  # If no Blocks exist, create a variable Blocks to indicate it is only one block

  nsets<-nrow(design)
  nblocks<-max(design$Block)
  setpp <- nsets/nblocks      # Choice Sets per respondent

  replications <- respondents/nblocks

  ## if replications is non int, assign unevenly

  datadet<- design %>%
    dplyr::arrange(Block,Choice.situation) %>%
    dplyr::slice(rep(dplyr::row_number(), replications)) %>%    ## replicate design according to number of replications

    dplyr::mutate(ID = rep(1:respondents, each=setpp)) %>%  # create Respondent ID.
    dplyr::relocate(ID,`Choice.situation`) %>%
    as.data.frame()


  switchmap <- function(.x, .f, mode = c("parallel", "sequential"), workers = NULL, ..., .progress = TRUE) {
    mode <- match.arg(mode)  # Ensure valid mode input

    switch(
      mode,
      "parallel" = {
        # Set up parallel backend
        if (!is.null(workers)) {
          future::plan("multisession", workers = workers)
        } else {
          future::plan("multisession")
        }
        on.exit(future::plan("sequential"), add = TRUE)  # Ensure plan is reset after execution
        furrr::future_map(.x, .f, ..., .options = furrr::furrr_options(seed = TRUE))
      },
      "sequential" = {

        purrr::map(.x, .f, ..., .progress = .progress)
      }
    )
  }


  sim_data<- 1:no_sim %>% switchmap(~ simulate_choices(datadet, utility = u, setspp=setpp, bcoeff = bcoeff, decisiongroups = decisiongroups, manipulations = manipulations), mode = "parallel")


### start estimation

  if(estimate==TRUE) {
database <- sim_data[[1]]


    ####  Function that transforms user written utility for simulation into utility function for mixl.
transform_util <- function() {

      mnl_U <-paste(purrr::map_chr(u[[1]],as.character,keep.source.attr = TRUE),collapse = "",";") %>%
        stringr::str_replace_all( c( "priors\\[\"" = "" , "\"\\]" = "" ,  "~" = "=", "\\." = "_" , " b" = " @b"  , "V_"="U_", " alt"=" $alt"))

    }


transform_util2 <- function() {
  # Exclude columns that match "V_<any integer>" or "U_<any integer>" pattern
  relevant_database_vars <- setdiff(
    names(database),
    c(grep("^(V_|U_|e_)\\d+$", names(database), value = TRUE), "CHOICE")
  )

  mnl_U <- paste(
    purrr::map_chr(u[[1]], as.character, keep.source.attr = TRUE),
    collapse = "",
    ";"
  ) %>%
    # Replace coefficients with exact matches
    stringr::str_replace_all(stats::setNames(
      paste0("@", names(bcoeff)),
      paste0("(?<![._a-zA-Z0-9])", names(bcoeff), "(?![._a-zA-Z0-9-])")
    )) %>%
    # General transformations
    stringr::str_replace_all(c(
      `priors\\["` = "",
      `"\\]` = "",
      `~` = "=",
    `\\.` = "_",    ## can be deleted when everything works
      `V_` = "U_"    ## was originally V_
    )) %>%
    # Replace only relevant database variables
    stringr::str_replace_all(stats::setNames(
      paste0("$", relevant_database_vars),
      paste0("(?<![._a-zA-Z0-9])", relevant_database_vars, "(?![._a-zA-Z0-9-])")
    )) %>%
    # Clean up duplicate symbols
    stringr::str_replace_all(c(`@@` = "@", "\\$\\$" = "$"))

  return(mnl_U)
}




# transform utility function to mixl format

    # transform utility function to mixl format
    mnl_U <- switch(
      utility_transform_type,
      "simple" = transform_util(),
      "exact" = transform_util2(),
      stop("Invalid utility_transform_type. Use 'simple' or 'exact'.")
    )

    ####  Print selected utility function
    cat("Transformed utility function (type:", utility_transform_type, "):\n")
    print(mnl_U)



# specify model for mixl estimation

  model_spec <- mixl::specify_model(utility_script = mnl_U, dataset = database, disable_multicore=T)

  est=stats::setNames(rep(0,length(model_spec$beta_names)), model_spec$beta_names)


  availabilities <- mixl::generate_default_availabilities(
    database, model_spec$num_utility_functions)

  if (chunks >1) {

  # Calculate the size of each chunk
  chunk_size <- ceiling(no_sim / chunks)

  # Initialize the starting point for the first chunk
  start_point <- 1

  for (i in 1:chunks) {
    # Calculate the end point for the current chunk
    end_point <- start_point + chunk_size - 1

    # Ensure we do not go beyond the total number of simulations
    if (end_point > no_sim) {
      end_point <- no_sim
    }

    # Run simulations for the current chunk


    output <- start_point:end_point %>% purrr::map(estimate_sim)



      saveRDS(output,paste0("tmp_",i,".RDS"))
      rm(output)

    gc()

    # Print or save the output as required
    print(paste("Results for chunk", i, "from", start_point, "to", end_point))

    # Update the start point for the next chunk
    start_point <- end_point + 1

    # Break the loop if the end point reaches or exceeds no_sim
    if (start_point > no_sim) break

}

  output <- list()  # Initialize the list to store all outputs

  # Assuming the files are named in sequence as 'tmp_1.RDS', 'tmp_2.RDS', ..., 'tmp_n.RDS'
  for (i in 1:chunks) {
    # Load each RDS file
    file_content <- readRDS(paste0("tmp_", i, ".RDS"))
    file.remove(paste0("tmp_", i, ".RDS"))

    # Append the contents of each file to the all_outputs list
    output <- c(output, file_content)
  }


  } else {





tictoc::tic("start_estimation")
    output <- purrr::map(
      sim_data,
      ~ mixl::estimate(
        model_spec = model_spec,
        start_values = est,
        availabilities = availabilities,
        data = .x
      )
    )

  }



  coefs<-purrr::map(1:length(output),~summary(output[[.]])[["coefTable"]][c(1,8)]  %>%
               tibble::rownames_to_column() %>%
               tidyr::pivot_wider(names_from = rowname, values_from = c(est, rob_pval0)) ) %>%
    dplyr::bind_rows(.id = "run")

  output[["summary"]] <-psych::describe(coefs[,-1], fast = TRUE)

  output[["coefs"]] <-coefs

  pvals <- output[["coefs"]] %>% dplyr::select(dplyr::starts_with("rob_pval0"))

  output[["power"]] <- 100*table(apply(pvals,1,  function(x) all(x<0.05)))/nrow(pvals)


  output[["metainfo"]] <- c(Path = designfile, NoSim = no_sim, NoResp =respondents)


  print(kableExtra::kable(output[["summary"]],digits = 2, format = "rst"))


  print(output[["power"]])


  return(output)
} else {

  return(sim_data)

}
}
