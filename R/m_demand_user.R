source("R/monopoly.R")

###########################################
# Demand / Quantity
###########################################


#' Title
#'
#' @param data
#' @param col1
#'
#' @return
#' @export
#'
#' @examples
demandDurable <- function(data = NULL, wtp_col = NULL) { # , col2 = NULL){
  # Data scrubbing Willingness to Pay variable
  # if vector is labelled
  # for length of cols do the thing

  # if(!is.null(col1) & !is.null(col2)){
  #  data <- quantityCreation_multi(data, col1, col2)

  #  return(data)
  # }
  if (is.null(data)) {
    return("No data provided")
  }

  if(!is.null(wtp_col)){
    data <- data %>%
      rename(wtp = wtp_col)

  } else if (!any(colnames(data) %in% "wtp")) {
    if ("WTP" %in% colnames(data)) {
      data <- data %>%
        rename(wtp = WTP)
      print("Changed WTP column to wtp")
    } else {
      names(data)[1] <- "wtp"
      print("Changed 1st column to wtp")
    }
  }

  data <- data %>%
    select(wtp)

  # if vector is numeric / $ signs
  if (any(grepl("\\$", data))) {
    data <- removeDollarSigns(data)
    print("removed Dollar signs")
  }

  # Remove any NA's
  # Make sure wtp is numeric
  data <- data %>%
    filter(!is.na(wtp)) %>%
    mutate(wtp = as.numeric(wtp))

  # Make the quantity variable
  data <- quantityCreation(data)
  data <- revenueCreation(data)
  return(data)
}



#' Title
#'
#' @param data
#' @param price
#' @param quantityPerPerson
#'
#' @return
#' @export
#'
#' @examples
demandNonDurable <- function(data, price, quantityPerPerson) {
  data <- groupByPrice_ThenSum(data, {{ price }}, {{ quantityPerPerson }}, "quantity") %>%
    mutate(revenue = {{ price }} * quantity) %>%
    rename(wtp = {{ price }}) %>%
    select(wtp, quantity, revenue)

  return(data)
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
demandScatterPlot <- function(data) {
  sPlot <- scatterPlot(data, "wtp", "quantity")
  plot <- sPlot +
    ylim(0, NA) +
    geom_point(color = "darkorange", size = 1.5) +
    labs(x = "Willingness to Pay ($'s)", y = "Quantity", title = "Demand") +
    theme(
      axis.text = element_text(size = 6),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8)
    ) +
    theme(plot.title = element_text(face = "bold"))

  return(plot)
}

#' Title
#'
#' @param data
#' @param type
#'
#' @return
#' @export
#'
#' @examples
demandSummary <- function(data, type) {
  modelSummary(data, type, "wtp", "quantity")
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
demandSummaryCompare <- function(data) {
  lin_summary <- demandSummary(data, "Linear")
  exp_summary <- demandSummary(data, "Exponential")
  log_summary <- demandSummary(data, "Log")
  pow_summary <- demandSummary(data, "Power")

  return(list(lin_summary, exp_summary, log_summary, pow_summary))
}

#' Title
#'
#' @param data
#' @param type
#' @param population
#' @param sample
#'
#' @return
#' @export
#'
#' @examples
demandInterpret <- function(data, type, population, sample = NA) {
  switch(type,
    Linear      = do.call("linInterpret", list(data, population, sample)),
    Exponential = do.call("expInterpret", list(data, population, sample)),
    Log         = do.call("logInterpret", list(data, population, sample)),
    Power       = do.call("powInterpret", list(data, population, sample)),
    Sigmoid     = do.call("sigInterpret", list(data, population, sample)),
    stop("Invalid type")
  )
}

#' Title
#'
#' @param data
#' @param type
#' @param population
#' @param sample
#'
#' @return
#' @export
#'
#' @examples
demandFormula <- function(data, type, population, sample = NA) {
  switch(type,
    Linear      = do.call("linFormula", list(data, population, sample)),
    Exponential = do.call("expFormula", list(data, population, sample)),
    Log         = do.call("logFormula", list(data, population, sample)),
    Power       = do.call("powFormula", list(data, population, sample)),
    Sigmoid     = do.call("sigFormula", list(data, population, sample)),
    stop("Invalid type")
  )
}

#' Title
#'
#' @param data
#' @param type
#' @param population
#' @param sample
#'
#' @return
#' @export
#'
#' @examples
demandFormula2 <- function(data, type, population, sample = NA) {
  switch(type,
    Linear      = do.call("linFormulaFancy", list(data, population, sample)),
    Exponential = do.call("expFormulaFancy", list(data, population, sample)),
    Log         = do.call("logFormulaFancy", list(data, population, sample)),
    Power       = do.call("powFormulaFancy", list(data, population, sample)),
    Sigmoid     = do.call("sigFormulaFancy", list(data, population, sample)),
    stop("Invalid type")
  )
}



#' Title
#'
#' @param data
#' @param type
#' @param population
#' @param sample
#'
#' @return
#' @export
#'
#' @examples
demandPlot <- function(price = NULL, data = NULL, type = NULL, population = NULL, sample = NULL) {
  if (is.null(price) | is.null(data) | is.null(type) | is.null(population) | is.null(sample)) {
    return("Nice!")
  }
  #title <- paste("Demand:", type)
  title <- "Demand"
  rSq <- round(rSquaredDemand(data, type), 3)
  this_fQ <- fQ(data, type, population, sample)
  if (class(this_fQ) == class(NA)) {
    return()
  }

  yLowest <- min(0, floor(this_fQ(max(data$wtp))))

  quantityPrefix <- ifelse(this_fQ(price) < 0, "-", "")
  showQuantity <- paste0(quantityPrefix, conNum_short(floor(this_fQ(price))))

  if (is.null(sample)) sample <- nrow(data)
  scalar <- population / sample

  newTibble <- data %>%
    mutate(scaled_quantity = quantity * scalar)

  newPlot <- ggplot(data = newTibble) +
    geom_function(
      fun = this_fQ,
      color = "orange", lwd = 1.5, alpha = .4
    ) +
    geom_point(mapping = aes(x = wtp, y = scaled_quantity), color = "darkorange", size = 2, alpha = .8) +
    labs(x = "Price ($'s)", y = "Quantity Sold ") +
    annotate("label",
      x = Inf, y = Inf,
      label = paste("Quantity Sold:", showQuantity),
      #label = paste("R squared:", rSq),
      vjust = 1, hjust = 1, size = 5,
      color = "darkorange2", alpha = .8,
      fontface = "bold"
    ) +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      breaks = waiver()#,
      #limits = c(0, NA)
    ) +
    geom_segment(
      x = price, y = yLowest, xend = price, yend = this_fQ(price),
      linetype = "dashed", color = "darkorange2", lwd = .6
    ) +
    geom_segment(
      x = 0, y = this_fQ(price), xend = price, yend = this_fQ(price),
      linetype = "dashed", color = "orange", lwd = .4
    ) +
    geom_point(shape = 21, x = price, y = this_fQ(price), color = "darkorange3", fill = "white", size = 3) +
    theme(plot.title = element_text(face = "bold")) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    ) +
    theme(plot.title = element_text(face = "bold"))
  return(newPlot)
}

#round(rSquaredDemand(cpC, "Log"), 3)
#cpC


demandFunctionPlot <- function(price, data, type, population, sample = NA){
  fQ <- scaleFunction(data, type, "wtp", "quantity", population, sample)
  if (class(fQ) == class(NA)) {
    return(NA)
  }
  price <- round(price, 2)

  show_price <- paste0("$", format(round(price, 2), big.mark = ","))
  show_quantity <- conNum_short(round(fQ(price), 2))


  title <- paste0("Quantity when Price is ", show_price)

  newPlot <- ggplot(data = data) +
    xlim(0, max(data$wtp)) +
    geom_function(fun = fQ, color = "orange", lwd = 1.8, alpha = .8) +
    geom_point(x = price, y = fQ(price), color = "darkorange", size = 3) +
    geom_segment(
      x = price, y = 0, xend = price, yend = fQ(price),
      linetype = "dashed", color = "darkorange", lwd = .6
    ) +
    geom_segment(
      x = 0, y = fQ(price), xend = price, yend = fQ(price),
      linetype = "dashed", color = "orange", lwd = .4
    ) +
    labs(title = title, x = "Price ($'s)", y = "Quantity Sold ") +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      breaks = scales::extended_breaks(),
      limits = c(0, NA)
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme_minimal() +
    annotate("label",
             x = Inf, y = Inf,
             label = paste("Price:", show_price),
             vjust = 1, hjust = 1, size = 5,
             color = "darkorange2", alpha = .8,
             fontface = "bold"
    ) +
    annotate("label",
             x = Inf, y = Inf,
             label = (paste("Quantity:", show_quantity)),
             vjust = 2.5, hjust = 1, size = 5,
             color = "darkorange2", alpha = .8,
             fontface = "bold"
    ) +
    theme(
      axis.text = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    ) +
    theme(plot.title = element_text(face = "bold"))
  return(newPlot)
}

#' Title
#'
#' @param price
#' @param data
#' @param type
#' @param population
#' @param sample
#'
#' @return
#' @export
#'
#' @examples
demandFunction <- function(price, data, type, population, sample = NA) {
  fQ <- scaleFunction(data, type, "wtp", "quantity", population, sample)
  if (class(fQ) == class(NA)) {
    return(NA)
  }

  price <- round(price, 2)
  newPlot <- demandFunctionPlot(price, data, type, population, sample)

  suppressWarnings(print(newPlot))
  return(fQ(price))
}


#' Title
#'
#' @param data
#' @param population
#' @param sample
#' @param n
#'
#' @return
#' @export
#'
#' @examples
demandCompare <- function(data, population, sample = NA, n = 3) {
  top_types <- nBestDemandModels(data, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- demandPlot(data, type, population, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <- suppressWarnings(do.call(grid.arrange, c(plot_list, ncol = 2)))
  return(final_plot)
}
