plot_multi_histogram <- function(df, feature, label_column, hist=FALSE) { #function to create nice multi histograms, taken somewhere from the web
  plt <- ggplot2::ggplot(df, ggplot2::aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    ggplot2::geom_density(alpha=0.5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", linewidth=1) +  ## this makes a vertical line of the mean
    ggplot2::labs(x=feature, y = "Density") +
    ggplot2::guides(fill=ggplot2::guide_legend(title=label_column))

  if (hist==TRUE) plt + ggplot2::geom_histogram(alpha=0.7, position="identity", ggplot2::aes(y = ..density..), color="black")

  return(plt)

}


## The function below is intended to find a dataframe called $coef in the output
## regardless of the output data structure, which can vary
find_dataframe <- function(list_object, dataframe_name) {
  # Check if the current object is a list
  if (is.list(list_object)) {
    # Check if the dataframe_name exists in the names of the current list object
    if (dataframe_name %in% names(list_object)) {
      # Check if the object corresponding to dataframe_name is a data frame
      if (is.data.frame(list_object[[dataframe_name]])) {
        return(list_object[[dataframe_name]])  # Return the data frame if found
      }
    }

    # Recursively search through each element of the current list object
    for (element in list_object) {
      result <- find_dataframe(element, dataframe_name)
      if (!is.null(result)) {
        return(result)  # Return the data frame if found in any nested list
      }
    }
  }

  return(NULL)  # Return NULL if dataframe_name not found
}



#' Title Downloads and extracts external data to be used in the simulation
#'
#' @param url the URL from where the external data can be downloaded. The external data has be wrapped in a zip file
#' @param dest_folder The folder where the data should be stored
#' @param zip_name Only needs to be changed if the zip file is differently named as the last part of the URL
#'
#' @return nothing, stores the data on the local system
#' @export
#'
#' @examples \dontrun{ download_and_extract_zip(url="www.nextcloud.de/mysuperfile")}
download_and_extract_zip <- function(url, dest_folder = ".", zip_name = NULL) {
  # If zip_name is not provided, extract it from the URL
  if (is.null(zip_name)) {
    zip_name <- basename(url)
  }
  folder <- paste0(dest_folder,"/data")


  # Check if the folder is empty
  if (length(list.files(folder)) > 0) {
    warning("Destination folder is not empty. Nothing copied.")
    return(invisible(NULL))
  }


  # Download the zip file
  utils::download.file(url, zip_name, method = "auto", quiet = FALSE, mode = "w", cacheOK = TRUE)

  # Extract the contents
  utils::unzip(zip_name, exdir = dest_folder)


  # Return the path to the extracted folder
  return(file.path(dest_folder, tools::file_path_sans_ext(zip_name)))
}






make_md <- function(f=file){




  rmarkdown::render("simulation_output.rmd",
                    output_file = paste0(
                      stringr::str_remove_all(
                        file,"parameters_|.R$"),".html"),
                    params = list(file=file)
  )

}

