#' Is a wrapper for sim_choice executing the simulation over all designs stored in a specific folder
#' update
#' @param nosim Number of runs or simulations. For testing use 2 but once you go serious, use at least 200, for better results use 2000.
#' @param resps Number of respondents you want to simulate
#' @param destype Is it a design created with ngene or with spdesign. Ngene desings should be stored as the standard .ngd output. spdesign should be the spdesign object design$design
#' @param designpath The path to the folder where the designs are stored. For example "c:/myfancydec/Designs"
#' @param u A list with utility functions. The list can incorporate as many decision rule groups as you want. However, each group must be in a list in this list. If you just use one group (the normal),  this  group still  has to be in a list in  the u list. As a convention name betacoefficients starting with a lower case "b"
#' @param bcoefficients List of coefficients for the utility function. List content/length can vary based on application, but should all begin with b and be the same as those entered in the utility functions
#'
#' @return A list, with all information on the simulation. This list an be easily processed by the user and in the rmarkdown template.
#' @export
#'
#' @examples
#'
#'  designpath<- system.file("extdata","Rbook" ,package = "simulateDCE")
#'  resps =240  # number of respondents
#'  nosim=2 # number of simulations to run (about 500 is minimum)
#'
#'  bcoeff <-list(bsq=0.00, # hypothesized beta coefficients for individual terms of the utility function
#'      bredkite=-0.05,
#'      bdistance=0.50,
#'      bcost=-0.05,
#'      bfarm2=0.25,
#'      bfarm3=0.50,
#'      bheight2=0.25,
#'      bheight3=0.50)
#'
sim_all <- function(nosim=2, resps, destype="ngene", designpath, u, bcoeff){


  ########### validate the utility function ########
  if (missing(u) || !(is.list(u) && any(sapply(u, is.list)))){
    stop(" 'u' must be provided and must be a list containing at least one list element.")
  }

  if (missing(resps) ||  !(is.integer(resps) || (is.numeric(resps) && identical(trunc(resps), resps)))) {
    stop(" 'resps' must be provided and must be an integer indicating  the number of respondents per run.")
  }

  if (!dir.exists(designpath)) {
    stop(" The folder where your designs are stored does not exist. \n Check if designpath is correctly specified")
  }
  
  ########## validate the bcoeff list ################
  # Check if bcoeff is provided
  if (missing(bcoeff)) {
    stop("Argument 'bcoeff' is required.")
  }
  
  # Check if bcoeff is a list
  if (!is.list(bcoeff)) {
    stop("Argument 'bcoeff' must be a list.")
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
  
  ### end input validation tests ##
  
  designfile<-list.files(designpath,full.names = T)
  designname <- stringr::str_remove_all(list.files(designpath,full.names = F),
                               "(.ngd|_|.RDS)")  ## Make sure designnames to not contain file ending and "_", as the may cause issues when replace


  tictoc::tic()

  all_designs<- purrr::map(designfile, sim_choice,
                           no_sim= nosim,respondents = resps,  destype=destype, ut=u, bcoefficients = bcoeff) %>%  ## iterate simulation over all designs
    stats::setNames(designname)


  time <- tictoc::toc()

  print(time)



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
  all_designs[["time"]]=time



  return(all_designs)
}
