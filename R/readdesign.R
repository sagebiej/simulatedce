



#'  Creates a dataframe with the design.
#'
#' @param design The path to a design file
#' @param designtype Is it a design created with ngene or with spdesign. Ngene desings should be stored as the standard .ngd output. spdesign should be the spdesign object design$design
#'
#' @return a dataframe
#' @export
#'
#' @examples library(simulateDCE)
#'           mydesign <-readdesign(
#'           system.file("extdata","agora", "altscf_eff.ngd" ,package = "simulateDCE"),
#'            "ngene")
#'
#'            print(mydesign)
#'



readdesign <- function(design = designfile, designtype = destype) {
  design <- switch(designtype,
                   "ngene" = suppressWarnings(readr::read_delim(design,
                                                                delim = "\t",
                                                                escape_double = FALSE,
                                                                trim_ws = TRUE,
                                                                col_select = c(-Design, -tidyr::starts_with("...")),
                                                                name_repair = "universal", show_col_types = FALSE ,guess_max = Inf
                   )) %>%
                     dplyr::filter(!is.na(Choice.situation)),
                   "spdesign" = {
                         designf <- readRDS(design)
                     if (is.list(designf) & !is.data.frame(designf)){
                       if (!"design" %in% names(designf)) {
                         stop("The 'design' list element is missing. Make sure to provide a proper spdesign object.")
                       }
                       designf<-designf[["design"]]
                     }
                     as.data.frame(designf) %>%
                       dplyr::mutate(Choice.situation = 1:dplyr::n()) %>%
                       dplyr::rename_with(~ stringr::str_replace(., pattern = "_", "\\."), tidyr::everything()) %>%
                       dplyr::rename_with(~ dplyr::case_when(
                         . == "block" ~ "Block",
                         TRUE ~ .
                       ), tidyr::everything())

                   }
                   ,
                   stop("Invalid value for design. Please provide either 'ngene' or 'spdesign'.")
  )

}


