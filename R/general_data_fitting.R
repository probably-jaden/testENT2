source("R/monopoly.R")

scatterPlot <- function(data, xColumn, yColumn) {
  # Check if columns exist in the data
  if (!(xColumn %in% names(data) && yColumn %in% names(data))) {
    stop("Specified columns do not exist in the data.")
  }

  title <- paste0(xColumn, " vs. ", yColumn)
  # Create scatter plot
  ggplot(data, aes(x = !!sym(xColumn), y = !!sym(yColumn))) +
    labs(title = title) +
    geom_point(color = "darkorchid") +
    theme_minimal() +
    labs(x = xColumn, y = yColumn)
}

modelFunction <- function(input, data, type, x, y) {
  title <- paste0(x, " vs. ", y, ": ", type)

  mf <- modelFun(data, type, x, y)
  output <- mf(input)

  mPlot <- ggplot(data = data) +
    xlim(0, max(data[x])) +
    geom_function(fun = mf, color = "orchid", lwd = 1.8, alpha = .8) +
    labs(title = title, x = x, y = y) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale()),
      breaks = scales::extended_breaks(),
      limits = c(0, NA)
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme_minimal() +
    annotate("label",
      x = Inf, y = Inf,
      label = paste("Input:", round(input, 2)),
      vjust = 1, hjust = 1, size = 4,
      color = "darkorchid", alpha = .8,
      fontface = "bold"
    ) +
    annotate("label",
      x = Inf, y = Inf,
      label = paste("Output:", conNum_short(round(output, 2))),
      vjust = 2.5, hjust = 1, size = 4,
      color = "darkorchid", alpha = .8,
      fontface = "bold"
    ) +
    theme(
      axis.text = element_text(size = 6),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8)
    ) +
    theme(plot.title = element_text(face = "bold")) +
    geom_segment(
      x = input, y = 0, xend = input, yend = output,
      linetype = "dashed", color = "darkorchid", lwd = .6, alpha = .5
    ) +
    geom_segment(
      x = 0, y = output, xend = input, yend = output,
      linetype = "dashed", color = "darkorchid", lwd = .4, alpha = .5
    )

  suppressWarnings(print(mPlot))
  return(output)
}

modelPlot <- function(data, type, x, y) {
  sPlot <- scatterPlot(data, x, y)
  title <- paste0(x, " vs. ", y, ": ", type)
  rSq <- round(rSquared(data, type, x, y), 3)
  modelFun <- modelFun(data, type, x, y)
  if (class(modelFun) == class(NA)) {
    return()
  }

  newPlot <- sPlot +
    geom_function(fun = modelFun, color = "orchid", lwd = 1.5, alpha = .4) +
    labs(title = title) +
    annotate("label",
      x = Inf, y = Inf,
      label = paste("R squared:", rSq),
      vjust = 1, hjust = 1,
      color = "darkorchid", alpha = .8,
      fontface = "bold"
    )

  return((newPlot))
}

modelCompare <- function(data, x, y, n = 4) {
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

modelOptimize <- function(data, type, x, y) {
  mf <- modelFun(data, type, x, y)
  opt_output <- optimize(mf, lower = 0, upper = max(data[x]), maximum = TRUE)[[2]]
  opt_input <- round(optimize(mf, lower = 0, upper = max(data[x]), maximum = TRUE)[[1]], 2)

  modelFunction(opt_input, data, type, x, y)

  return(list(opt_output, opt_input))
}
