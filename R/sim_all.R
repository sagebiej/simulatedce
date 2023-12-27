#' Title Is a wrapper for sim_choice executing the simulation over all designs stored in a specific folder
#'
#' @return a list, with all information on the simulation. This list an be easily processed by the user and in the rmarkdown template.
#' @export
#'
#' @examples
#'
#'  designpath<- system.file("extdata","Rbook" ,package = "simulateDCE")
#'  resps =240  # number of respondents
#'  nosim=2 # number of simulations to run (about 500 is minimum)
#'
sim_all <- function(){



  designfile<-list.files(designpath,full.names = T)
  designname <- stringr::str_remove_all(list.files(designpath,full.names = F),
                               "(.ngd|_|.RDS)")  ## Make sure designnames to not contain file ending and "_", as the may cause issues when replace

  if (!exists("destype")) destype="ngene"

  tictoc::tic()

  all_designs<- purrr::map(designfile, sim_choice,
                           no_sim= nosim,respondents = resps, mnl_U = mnl_U, destype=destype) %>%  ## iterate simulation over all designs
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
