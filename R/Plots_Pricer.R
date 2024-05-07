source("R/MonopolyProfitabilityLibrary.R")

scatterPlot <- function(data, xColumn, yColumn){
  # Check if columns exist in the data
  if (!(xColumn %in% names(data) && yColumn %in% names(data))) {
    stop("Specified columns do not exist in the data.")
  }

  title <- paste0(xColumn, " vs. ", yColumn)
  # Create scatter plot
  ggplot(data, aes(x = !!sym(xColumn), y = !!sym(yColumn))) +
    labs(title = title)+
    geom_point(color = "darkorchid") +
    theme_minimal()+
    labs(x = xColumn, y = yColumn)
}
