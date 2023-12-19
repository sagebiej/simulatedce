#' Title
#'
#' @param designfile
#' @param no_sim
#' @param respondents
#' @param mnl_U
#' @param utils
#' @param destype
#'
#' @return
#' @export
#'
#' @examples
sim_choice <- function(designfile, no_sim=10, respondents=330, mnl_U,utils=u[[1]] ,destype) {


  require("tictoc")
  require("readr")
  require("psych")
  require("dplyr")
  require("evd")
  require("tidyr")
  require("kableExtra")
  require("gridExtra")
  require("stringr")
  require("mixl")
  require("furrr")
  require("purrr")
  require("ggplot2")
  require("formula.tools")
  require("rlang")


  estimate_sim <- function(run=1) {         #start loop

    cat("This is Run number ", run)

    database <- simulate_choices(datadet, utility = utils, setspp=setpp )


    cat("This is the utility functions \n" , mnl_U)

    model<-mixl::estimate(model_spec,start_values = est, availabilities = availabilities, data= database,)

    return(model)

  }

  mnl_U <-paste(map_chr(utils,as.character,keep.source.attr = TRUE),collapse = "",";") %>%
    str_replace_all( c( "priors\\[\"" = "" , "\"\\]" = "" ,  "~" = "=", "\\." = "_" , " b" = " @b"  , "V_"="U_", " alt"="$alt"))

  cat("mixl \n")
  cat(mnl_U)

  cat("\n Simulation \n")

  print(u)


  designs_all <- list()


  design<- readdesign(design = designfile)

  if (!exists("design$Block")) design$Block=1

  nsets<-nrow(design)
  nblocks<-max(design$Block)
  setpp <- nsets/nblocks      # Choice Sets per respondent; in this 'no blocks' design everyone sees all 24 sets

  replications <- respondents/nblocks

  datadet<- design %>%
    arrange(Block,Choice.situation) %>%
    slice(rep(row_number(), replications)) %>%    ## replicate design according to number of replications
    mutate(ID = rep(1:respondents, each=setpp)) %>%  # create Respondent ID.
    relocate(ID,`Choice.situation`) %>%
    as.data.frame()

  database <- simulate_choices(data=datadet, utility = utils, setspp = setpp)

  model_spec <- mixl::specify_model(mnl_U, database, disable_multicore=F)

  est=setNames(rep(0,length(model_spec$beta_names)), model_spec$beta_names)


  availabilities <- mixl::generate_default_availabilities(
    database, model_spec$num_utility_functions)


  output<- 1:no_sim %>% map(estimate_sim)




  coefs<-map(1:length(output),~summary(output[[.]])[["coefTable"]][c(1,8)]  %>%
               tibble::rownames_to_column() %>%
               pivot_wider(names_from = rowname, values_from = c(est, rob_pval0)) ) %>%
    bind_rows(.id = "run")

  output[["summary"]] <-psych::describe(coefs[,-1], fast = TRUE)

  output[["coefs"]] <-coefs

  pvals <- output[["coefs"]] %>% dplyr::select(starts_with("rob_pval0"))

  output[["power"]] <- 100*table(apply(pvals,1,  function(x) all(x<0.05)))/nrow(pvals)


  output[["metainfo"]] <- c(Path = designfile, NoSim = no_sim, NoResp =respondents)


  print(kable(output[["summary"]],digits = 2, format = "rst"))


  print(output[["power"]])


  return(output)


}
