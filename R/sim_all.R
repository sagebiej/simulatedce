sim_all <- function(){

  require("stringr")

  designfile<-list.files(designpath,full.names = T)
  designname <- str_remove_all(list.files(designpath,full.names = F),
                               "(.ngd|_|.RDS)")  ## Make sure designnames to not contain file ending and "_", as the may cause issues when replace

  if (!exists("destype")) destype="ngene"

  tictoc::tic()

  all_designs<- purrr::map(designfile, sim_choice,
                           no_sim= nosim,respondents = resps, mnl_U = mnl_U, destype=destype) %>%  ## iterate simulation over all designs
    setNames(designname)


  time <- tictoc::toc()

  print(time)



  powa <- map(all_designs, ~ .x$power)




  summaryall <- as.data.frame(purrr::map(all_designs, ~.x$summary)) %>%
    dplyr::select(!ends_with("vars")) %>%
    relocate(ends_with(c(".n", "mean","sd", "min" ,"max", "range" , "se" )))

  coefall <- map(all_designs, ~ .x$coefs)

  pat<-paste0("(",paste(designname,collapse = "|"),").") # needed to identify pattern to be replaced

  s<-as.data.frame(coefall) %>%
    dplyr::select(!matches("pval|run")) %>%
    rename_with(~ sub("est_b", "", .x), everything()) %>%
    #  rename_with(~ sub("est_asc_", "asc", .x), everything()) %>%
    rename_with( ~ paste0(.,"_",stringr::str_extract(.,pat )), everything() ) %>%   # rename attributes for reshape part 1
    rename_with( ~ stringr::str_replace(.,pattern = pat,replacement=""), everything() )  %>%
    reshape(varying =1:ncol(.), sep = "_"  , direction = "long" ,timevar = "design", idvar = "run" )


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
