#' Simulate and estimate choices
#'
#' @param designfile path to a file containing a design.
#' @param no_sim Number of runs i.e. how often do you want the simulation to be repeated
#' @param respondents Number of respondents. How many respondents do you want to simulate in each run.
#' @param ut The first element of the utility function list
#' @inheritParams readdesign
#' @param estimate If TRUE models will be estimated. If false only a dataset will be simulated. Default is true
#' @inheritParams simulate_choices
#' @return a list with all information on the run
#' @export
#'
#' @examples \dontrun{  simchoice(designfile="somefile", no_sim=10, respondents=330,
#'  mnl_U,ut=u[[1]] ,destype="ngene")}
#'
sim_choice <- function(designfile, no_sim=10, respondents=330,ut ,destype=destype, bcoefficients, decisiongroups=c(0,1), manipulations = list() , estimate) {



####  Function that transforms user written utility for simulation into utility function for mixl.
  transform_util <- function() {

    mnl_U <-paste(purrr::map_chr(ut[[1]],as.character,keep.source.attr = TRUE),collapse = "",";") %>%
      stringr::str_replace_all( c( "priors\\[\"" = "" , "\"\\]" = "" ,  "~" = "=", "\\." = "_" , " b" = " @b"  , "V_"="U_", " alt"="$alt"))

  }

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


  output<- 1:no_sim %>% purrr::map(estimate_sim)




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
} else return(database)


}
