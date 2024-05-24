source("R/monopoly.R")

###########################################
# Profit
###########################################

profitPlot <- function(data, type, variable, fixed, population, sampleSize = NA, yCap = 0) {
  fQ <- fQ(data, type, population, sampleSize)
  if (class(fQ) == class(NA)) {
    return(NA)
  }
  fR <- fR(data, type, population, sampleSize)
  fC <- fC(variable, fixed, fQ)
  fPi <- fPi(fR, fC)

  title <- paste("Optimized Profit")

  opt_Profit <- optimize(fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]
  opt_Price <- optimize(fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[1]]
  opt_Revenue <- optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]

  show_Profit <- paste0("$", conNum_short(round(opt_Profit, 2)))
  show_Price <- paste0("$", conNum_short(round(opt_Price, 2)))

  if (yCap == 0) {
    yCap <- opt_Revenue
  }

  plot <- ggplot(data = data) +
    xlim(0, max(data$wtp)) +
    geom_segment(
      x = opt_Price, y = 1, xend = opt_Price, yend = opt_Profit,
      linetype = "dashed", color = "lightgreen", lwd = .3
    ) +
    geom_function(fun = fR, color = "deepskyblue2", lwd = 1.2, alpha = .7) +
    geom_function(fun = fC, color = "brown1", lwd = 1.2, alpha = .7) +
    geom_function(fun = fPi, color = "green3", lwd = 1.2, alpha = .7) +
    labs(x = "Price ($'s)", y = "Profit ($'s)") +
    annotate("label",
      x = Inf, y = Inf,
      label = paste("Price:", show_Price),
      vjust = 1, hjust = 1, size = 4,
      color = "darkgreen", alpha = .8,
      fontface = "bold"
    ) +
    annotate("label",
      x = Inf, y = Inf,
      label = (paste("Profit:", show_Profit)),
      vjust = 2.5, hjust = 1, size = 4,
      color = "darkgreen", alpha = .8,
      fontface = "bold"
    ) +
    theme(
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size =12)
    ) +
    theme_minimal() +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      breaks = scales::extended_breaks(),
      limits = c(0, yCap)
    ) +
    labs(title = title) +
    theme(plot.title = element_text(face = "bold"))

  return(suppressWarnings(plot))
}


profitFunctionPlot <- function(price, data, type, variable, fixed, population, sample = NA){
    this_fQ <- fQ(data, type, population, sample)
    if (class(this_fQ) == class(NA)) {
      return(NA)
    }
    this_fR <- fR(data, type, population, sample)
    this_fC <- fC(variable, fixed, this_fQ)
    this_fPi <- function(p) this_fR(p) - this_fC(p)

    data <- data %>%
      mutate(profit = (wtp * quantity * (population/sample)) - (fixed + (variable * quantity * (population/sample))),
             netColor = ifelse(profit < 0, "red4", "green4"))

    lineDF <- data.frame(price = seq(min(data$wtp), max(data$wtp), length.out = 1000))
    lineDF <- lineDF %>%
      mutate(profit = this_fPi(price),
             netColorLine = ifelse(profit < 0, "red", "green3"),
             profit_next = lead(profit),    # Next y-value for segment
             price_next = lead(price)     # Next x-value for segment
             ) %>%
      filter(!is.na(profit_next))

    price <- round(price, 2)
    show_price <- paste0("$", format(round(price, 2), big.mark = ","))

    profitPrefix <- ifelse(this_fPi(price) < 0, "-$", "$")
    show_profit <- paste0(profitPrefix, conNum_short(round(this_fPi(price), 2)))
    title <- paste0("Profit when Price is ", show_price)

    #opt_Profit <- optimize(this_fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]
    #yHighest <- max(opt_Profit, max(data$profit))
    yLowest <- min(floor(this_fPi(0)), floor(min(data$profit)), floor(this_fPi(max(data$wtp))))


    newPlot <- ggplot() +
      xlim(0, max(data$wtp)) +
      #geom_function(data = data, fun = this_fPi, color = "green3", lwd = 1.8, alpha = .7) +
      geom_segment(data = lineDF, aes(x = price, y = profit, xend = price_next, yend = profit_next, color = netColorLine), size = 1.8, alpha = .6) +
      scale_color_identity() +
      geom_point(aes(x = price, y = this_fPi(price)), color = "green4", size = 2) +
      geom_segment(data = data, x = price, y = yLowest, xend = price, yend = this_fPi(price), linetype = "dashed", color = "green4", lwd = .6) +
      geom_segment(data = data, x = 0, y = this_fPi(price), xend = price, yend = this_fPi(price), linetype = "dashed", color = "green3", lwd = .4) +
      geom_point(data = data, aes(x = wtp, y = profit, color = netColor), size = 2, alpha = .8)+ #color = "green4")+
      labs(title = title, x = "Price ($'s)", y = "Profit ($'s) ") +
      scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        breaks = scales::extended_breaks()#,
        #limits = c(0, yHighest)
      ) +
      theme(plot.title = element_text(face = "bold")) +
      theme_minimal() +
      annotate("label",
               x = Inf, y = Inf,
               label = (paste("Profit:", show_profit)),
               vjust = 1, hjust = 1, size = 5,
               color = "darkgreen", alpha = .8,
               fontface = "bold"
      ) +
      theme(
        axis.text = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      ) +
      theme(plot.title = element_text(face = "bold"))

  return(suppressWarnings(newPlot))
}

#cp <- read_csv("CupcakesTest.csv")
#cpC <- demandDurable(cp, "cupcakes")

#profitFunctionPlot(1.27, cpC, "Exponential", 1, 1, 1000, 1)



profitFunction <- function(price, data, type, variable, fixed, population, sample = NA) {
  fQ <- fQ(data, type, population, sample)
  if (class(fQ) == class(NA)) {
    return(NA)
  }
  fR <- fR(data, type, population, sample)
  fC <- fC(variable, fixed, fQ)
  fPi <- function(p) fR(p) - fC(p)

  newPlot <- profitFunctionPlot(price, data, type, variable, fixed, population, sample)

  suppressWarnings(print(newPlot))
  return(fPi(price))
}

profitOptimize <- function(data, type, variable, fixed, population, sample = NA) {
  this_fQ <- fQ(data, type, population, sample)
  if (class(this_fQ) == class(NA)) {
    return(NA)
  }
  this_fR <- fR(data, type, population, sample)
  this_fC <- fC(variable, fixed, this_fQ)
  this_fPi <- fPi(this_fR, this_fC)

  opt_Profit <- (optimize(this_fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]])
  opt_Price <- round(optimize(this_fPi, lower = 0, upper = max(data$wtp), maximum = TRUE)[[1]], 2)

  profitFunction(opt_Price, data, type, variable, fixed, population, sample)

  return(list(opt_Profit, opt_Price))
}


profitRevFunction <- function(price, data, type, variable, fixed, population, sample = NA, yCap = 0) {
  fQ <- fQ(data, type, population, sample)
  if (class(fQ) == class(NA)) {
    return(NA)
  }
  fR <- fR(data, type, population, sample)
  fC <- fC(variable, fixed, fQ)
  fPi <- function(p) fR(p) - fC(p)

  opt_Revenue <- optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE)[[2]]

  if (yCap == 0) {
    yCap <- opt_Revenue
  }

  show_price <- paste0("$", format(round(price, 2), big.mark = ","))
  show_revenue <- paste0("$", conNum_short(round(fR(price), 2)))
  show_profit <- paste0("$", conNum_short(round(fPi(price), 2)))

  title <- paste0("Profit and Revenue when Price is ", show_price)

  newPlot <- ggplot(data = data) +
    xlim(0, max(data$wtp)) +
    geom_function(fun = fR, color = "deepskyblue2", lwd = 1.2, alpha = .7) +
    geom_function(fun = fPi, color = "green3", lwd = 1.8, alpha = .8) +
    geom_segment(
      x = price, y = 0, xend = price, yend = fPi(price),
      linetype = "dashed", color = "green4", lwd = .6
    ) +
    geom_segment(
      x = 0, y = fPi(price), xend = price, yend = fPi(price),
      linetype = "dashed", color = "green3", lwd = .2
    ) +
    geom_segment(
      x = price, y = fPi(price), xend = price, yend = fR(price),
      linetype = "dashed", color = "deepskyblue3", lwd = .6
    ) +
    geom_segment(
      x = 0, y = fR(price), xend = price, yend = fR(price),
      linetype = "dashed", color = "skyblue2", lwd = .2
    ) +
    labs(title = title, x = "Price ($'s)", y = "Profit ($'s) ") +
    geom_point(x = price, y = fPi(price), color = "green4", size = 3) +
    geom_point(x = price, y = fR(price), color = "deepskyblue3", size = 3) +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      breaks = scales::extended_breaks(),
      limits = c(0, yCap)
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme_minimal() +
    annotate("label",
      x = Inf, y = Inf,
      label = paste("Price:", show_price),
      vjust = 1, hjust = 1, size = 4,
      color = "darkorange2",
      fontface = "bold"
    ) +
    annotate("label",
      x = Inf, y = Inf,
      label = (paste("Rev:", show_revenue)),
      vjust = 2.5, hjust = 1, size = 4,
      color = "deepskyblue3",
      fontface = "bold"
    ) +
    annotate("label",
      x = Inf, y = Inf,
      label = (paste("Profit:", show_profit)),
      vjust = 4, hjust = 1, size = 4,
      color = "green3",
      fontface = "bold"
    ) +
    theme(
      axis.text = element_text(size = 6),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8)
    ) +
    theme(plot.title = element_text(face = "bold"))

  suppressWarnings(print(newPlot))
  return(fPi(price))
}

profitCompare <- function(data, variable, fixed, population, sample = NA) nBestProfitPlots(data, 3, variable, fixed, population, sample)
