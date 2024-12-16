#' Simulate and estimate choices
#'
#' @param designfile path to a file containing a design.
#' @param no_sim Number of runs i.e. how often do you want the simulation to be repeated
#' @param respondents Number of respondents. How many respondents do you want to simulate in each run.
#' @param ut The first element of the utility function list
#' @inheritParams readdesign
#' @param estimate If TRUE models will be estimated. If false only a dataset will be simulated. Default is true
#' @inheritParams simulate_choices
#' @param chunks The number of chunks determines how often results should be stored on disk as a safety measure to not loose simulations if models have already been estimated. For example, if no_sim is 100 and chunks = 2, the data will be saved on disk after 50 and after 100 runs.
#' @return a list with all information on the run
#' @export
#'
#' @examples \dontrun{  simchoice(designfile="somefile", no_sim=10, respondents=330,
#'  mnl_U,ut=u[[1]] ,destype="ngene")}
#'
sim_choice <- function(designfile, no_sim=10, respondents=330,ut ,destype=destype, bcoefficients, decisiongroups=c(0,1), manipulations = list() , estimate, chunks=1, utility_transform_type = "simple") {



####  Function that transforms user written utility for simulation into utility function for mixl.
  transform_util <- function() {

    mnl_U <-paste(purrr::map_chr(ut[[1]],as.character,keep.source.attr = TRUE),collapse = "",";") %>%
      stringr::str_replace_all( c( "priors\\[\"" = "" , "\"\\]" = "" ,  "~" = "=", "\\." = "_" , " b" = " @b"  , "V_"="U_", " alt"="$alt"))

  }

  transform_util2 <- function() {

    mnl_U <-paste(purrr::map_chr(ut[[1]],as.character,keep.source.attr = TRUE),collapse = "",";") %>%
      stringr::str_replace_all( c( "priors\\[\"" = "" , "\"\\]" = "" ,  "~" = "=", "\\." = "_" ,    "V_"="U_")) %>%
      stringr::str_replace_all(setNames(paste0("@", names(bcoefficients)), names(bcoefficients)))

  }


  mnl_U <- switch(
    utility_transform_type,
    "simple" = transform_util(),
    "exact" = transform_util2(),
    stop("Invalid utility_transform_type. Use 'simple' or 'exact'.")
  )

  ####  Print selected utility function
  cat("Transformed utility function (type:", utility_transform_type, "):\n")
  print(mnl_U)

#### Function to simulate and estimate ####

  estimate_sim <- function(run=1) {         #start loop

    cat("This is Run number ", run)

    database <- simulate_choices(datadet, utility = ut, setspp=setpp, bcoefficients = bcoefficients, decisiongroups = decisiongroups, manipulations = manipulations)


    cat("This is the utility functions \n" , mnl_U)

    model<-mixl::estimate(model_spec,start_values = est, availabilities = availabilities, data= database)

    return(model)

  }


# transform utility function to mixl format
mnl_U <- transform_util()

# Empty list where to store all designs later on
designs_all <- list()

#### Print some messages ####

 cat("Utility function used in simulation, ie the true utility: \n\n")

     print(ut)


  cat("Utility function used for Logit estimation with mixl: \n\n")
  print(mnl_U)






#### Read in the design file and set core variables ####



  design<- readdesign(design = designfile, designtype = destype)

  if (!("Block" %in% colnames(design))) design$Block=1  # If no Blocks exist, create a variable Blocks to indicate it is only one block

  nsets<-nrow(design)
  nblocks<-max(design$Block)
  setpp <- nsets/nblocks      # Choice Sets per respondent

  replications <- respondents/nblocks

  ## if replications is non int, assign unevenly

  ##browser()
  datadet<- design %>%
    dplyr::arrange(Block,Choice.situation) %>%
    dplyr::slice(rep(dplyr::row_number(), replications)) %>%    ## replicate design according to number of replications

    dplyr::mutate(ID = rep(1:respondents, each=setpp)) %>%  # create Respondent ID.
    dplyr::relocate(ID,`Choice.situation`) %>%
    as.data.frame()


  database <- simulate_choices(data=datadet, utility = ut, setspp = setpp, bcoefficients = bcoefficients, decisiongroups = decisiongroups, manipulations = manipulations)


### start estimation

  if(estimate==TRUE) {



# specify model for mixl estimation

  model_spec <- mixl::specify_model(mnl_U, database, disable_multicore=F)

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

    output <- 1:no_sim %>% purrr::map(estimate_sim)

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

  output<- 1:no_sim %>% purrr::map(~ simulate_choices(datadet, utility = ut, setspp=setpp, bcoefficients = bcoefficients, decisiongroups = decisiongroups, manipulations = manipulations))
  return(output)

}
}
