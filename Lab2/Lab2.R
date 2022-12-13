setwd("C:/Users/USER/Desktop/DataProcessingLabs/Lab2")

load_filtered_files_to_df <- function(directory, ids) {
  wd <- getwd()
  files <- dir(directory, pattern = "\\.csv", full.names = FALSE)
  filtered_files <- files[as.numeric(gsub(".csv", "", files)) %in% ids]
  setwd(directory)
  data <- do.call(rbind, lapply(filtered_files, read.csv))
  data <- na.omit(data)
  setwd(wd)
  return (data)
}

pollutantmean <- function(directory, pollutant, ids) {
  data <- load_filtered_files_to_df(directory, ids)
  return (mean(data[,pollutant]))
}

pollutantmean("specdata", "nitrate", 70:72)

complete <- function(directory, ids) {
  data <- load_filtered_files_to_df(directory, ids)
  return (aggregate(data$ID, list(data$ID), length))
}

complete("specdata", c(2, 4, 8, 10, 12))

corr <- function(directory, threshold) {
  complete_data <- complete(directory, 1:1e6)
  complete_above_threshold <- subset(complete_data, complete_data[2] >= threshold)
  data <- load_filtered_files_to_df(directory, complete_above_threshold$Group.1)
  if (is.null(data)) return (vector(mode="numeric", length=0))
  data <- do.call(rbind, lapply(
    split(data, data$ID), 
    function(x) cor(x$nitrate, x$sulfate)
  ))
  return (data)
}

cr <- corr("specdata", 400)
head(cr)
summary(cr)