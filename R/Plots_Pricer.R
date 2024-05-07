source("R/MonopolyProfitabilityLibrary.R")

scatterPlot2 <- function(data, xColumn, yColumn){
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

modelPlot <- function(data, type, x, y){
  check_packages()
  sPlot <- scatterPlot2(data, x, y)
  title <- paste0(x, " vs. ", y, ": ", type)
  rSq <- round(rSquared(data, type, x, y), 3)
  modelFun <- modelFun(data, type, x, y)
  if(class(modelFun) == class(NA)) return()

  newPlot <- sPlot +
    geom_function(fun = modelFun, color = "orchid", lwd = 1.5, alpha =.4) +
    labs(title = title)+
    annotate("label", x = Inf, y = Inf,
             label = paste("R squared:", rSq),
             vjust = 1, hjust = 1,
             color = "darkorchid", alpha = .8,
             fontface = "bold")

  return((newPlot))
}

modelCompare <- function(data, x, y, n = 4) {
  check_packages()
  top_types <- nBestModels(data, x, y, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- modelPlot(data, type, x, y)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <- do.call(grid.arrange, plot_list)
  return(final_plot)
}

demandScatterPlot <- function(data){
  check_packages()
  sPlot<- scatterPlot2(data, 'wtp', 'quantity')

  plot <- sPlot +
    ylim(0, NA)+
    geom_point(color = "darkorange", size = 1.5) +
    labs(x = "Willingness to Pay ($'s)", y = "Quantity", title = "Demand") +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))

  return(plot)
}

revenueScatterPlot <- function(data){
  check_packages()
  sPlot <- scatterPlot2(data, 'wtp', 'revenue')

  plot <- sPlot +
    ylim(0, NA)+
    geom_point(color = "deepskyblue", size = 1.5)+
    labs(x = "Price ($'s)", y = "Revenue ($'s)", title = "Revenue") +
    theme_minimal()+
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))
  return(plot)
}
