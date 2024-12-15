#' Is a wrapper for sim_choice executing the simulation over all designs stored in a specific folder
#' update
#' @param nosim Number of runs or simulations. For testing use 2 but once you go serious, use at least 200, for better results use 2000.
#' @param resps Number of respondents you want to simulate
#' @inheritParams readdesign
#' @param designpath The path to the folder where the designs are stored. For example "c:/myfancydec/Designs"
#' @param u A list with utility functions. The list can incorporate as many decision rule groups as you want. However, each group must be in a list in this list. If you just use one group (the normal),  this  group still  has to be in a list in  the u list. As a convention name beta coefficients starting with a lower case "b"
#' @param bcoeff List of initial coefficients for the utility function. List content/length can vary based on application, but should all begin with b and be the same as those entered in the utility functions
#' @inheritParams sim_choice
#' @inheritParams simulate_choices
#' @return A list, with all information on the simulation. This list an be easily processed by the user and in the rmarkdown template.
#' @export
#'
#' @examples
#'
#'  designpath<- system.file("extdata","Rbook" ,package = "simulateDCE")
#'  resps =240  # number of respondents
#'  nosim=2 # number of simulations to run (about 500 is minimum)
#'
#'
#'  bcoeff <-list(bsq=0.00,
#'      bredkite=-0.05,
#'      bdistance=0.50,
#'      bcost=-0.05,
#'      bfarm2=0.25,
#'      bfarm3=0.50,
#'      bheight2=0.25,
#'      bheight3=0.50)
#'
sim_all <- function(nosim=2, resps, destype=NULL, designpath, u, bcoeff, decisiongroups = c(0,1), manipulations = list(), estimate = TRUE, chunks=1){

  #################################################
  ########## Input Validation Test ###############
  #################################################

  ########### validate the utility function ########
  if (missing(u) || !(is.list(u) && any(sapply(u, is.list)))){
    stop(" 'u' must be provided and must be a list containing at least one list element (list of lists).")
  }

  ########## validate the bcoeff list ################
  # Check if bcoeff is provided
  if (missing(bcoeff)) {
    stop("Argument 'bcoeff' is required.")
  }


  if (nosim < chunks) {
    stop("You cannot have more chunks than runs. The number of chunks tells us how often we save the simulation results on disk. Maximum one per run.")
  }

  # Check if bcoeff is a list
  if (!is.list(bcoeff)) {
    stop("Argument 'bcoeff' must be a list.")
  }

  if (length(u) != length(decisiongroups) -1){
    stop("Number of decision groups must equal number of utility functions!")
  }
  if (!is.vector(decisiongroups)) {
    stop("Decision groups must be a vector.")
  }

  # Check if decisiongroups starts with 0
  if (decisiongroups[1] != 0) {
    stop("Decision groups must start with 0.")
  }

  # Check if decisiongroups ends with 1
  if (tail(decisiongroups, 1) != 1) {
    stop("Decision groups must end with 1.")
  }


  # Check if values in bcoeff are numeric
  if (!all(sapply(bcoeff, is.numeric))) {
    stop("Values in 'bcoeff' must be numeric.")
  }

  #### check that all the coefficients in utility function have a cooresponding value in bcoeff ####
    # Extract coefficients from utility function starting with "b"
  coeff_names_ul <- unique(unlist(lapply(u, function(u) {
    formula_strings <- unlist(u)
    coef_names <- unique(unlist(lapply(formula_strings, function(f) {
      # Parse the formula to extract coefficient names
      all_vars <- all.vars(as.formula(f))
      coef_vars <- all_vars[grep("^b", all_vars)]
      return(coef_vars)
    })))
    return(coef_names)
  })))

  # Check if all utility function coefficients starting with "b" are covered in bcoeff list
  missing_coeffs <- coeff_names_ul[!(coeff_names_ul %in% names(bcoeff))]
  if (length(missing_coeffs) > 0) {
    stop(paste("Missing coefficients in 'bcoeff':", paste(missing_coeffs, collapse = ", "), ". Perhaps there is a typo?"))
  }
  ########## validate resps #####################
  if (missing(resps) ||  !(is.integer(resps) || (is.numeric(resps) && identical(trunc(resps), resps)))) {
    stop(" 'resps' must be provided and must be an integer indicating  the number of respondents per run.")
  }
  ########## validate designpath ################
  if (!dir.exists(designpath)) {
    stop(" The folder where your designs are stored does not exist. \n Check if designpath is correctly specified")
  }

  #################################################
  ########## End Validation Tests #################
  #################################################

  designfile<-list.files(designpath,full.names = T)
  designname <- stringr::str_remove_all(list.files(designpath,full.names = F),
                               "(.ngd|_|.RDS)")  ## Make sure designnames to not contain file ending and "_", as the may cause issues when replace


  tictoc::tic()

  all_designs<- purrr::map(designfile, sim_choice,
                           no_sim= nosim,respondents = resps,  destype=destype, ut=u, bcoefficients = bcoeff, decisiongroups = decisiongroups, manipulations = manipulations, estimate = estimate, chunks =chunks) %>%  ## iterate simulation over all designs
    stats::setNames(designname)


  time <- tictoc::toc()

  print(time)

if (estimate==TRUE) {



  powa <- purrr::map(all_designs, ~ .x$power)




  summaryall <- as.data.frame(purrr::map(all_designs, ~.x$summary)) %>%
    dplyr::select(!dplyr::ends_with("vars")) %>%
    dplyr::relocate(dplyr::ends_with(c(".n", "mean","sd", "min" ,"max", "range" , "se" )))

  coefall <- purrr::map(all_designs, ~ .x$coefs)

  pat<-paste0("(",paste(designname,collapse = "|"),").") # needed to identify pattern to be replaced

  s<-as.data.frame(coefall) %>%
    dplyr::select(!dplyr::matches("pval|run")) %>%
    dplyr::rename_with(~ sub("est_b", "", .x), dplyr::everything()) %>%
    dplyr::rename_with( ~ paste0(.,"_",stringr::str_extract(.,pat )), dplyr::everything() ) %>%   # rename attributes for reshape part 1
    dplyr::rename_with( ~ stringr::str_replace(.,pattern = pat,replacement=""), dplyr::everything() )  %>%
    stats::reshape(varying =1:ncol(.), sep = "_"  , direction = "long" ,timevar = "design", idvar = "run" )


  p=list()

  for (att in names(dplyr::select(s,-c("design","run")))) {

    p[[att]] <- plot_multi_histogram(s,att,"design")

    print(p[[att]])

  }

  all_designs[["summaryall"]] = summaryall
  all_designs[["graphs"]]=p
  all_designs[["powa"]]=powa

}
  all_designs[["time"]]=time
  all_designs[["arguements"]] = list( "Beta values" = bcoeff, "Utility functions" = u , "Decision groups" =decisiongroups , "Manipulation of vars" = manipulations,
                                      "Number Simulations" = nosim, "Respondents" = resps, "Designpath" = designpath)



  return(all_designs)
}
