source("R/monopoly.R")

###########################################
# Revenue
###########################################

revenueScatterPlot <- function(data){
  check_packages()
  sPlot <- scatterPlot(data, 'wtp', 'revenue')
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

revenuePlot <- function(data, type, population, sample = NA){
  check_packages()
  title <- paste("Revenue:", type)

  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return()
  fR <- fR(data, type, population, sample)

  opt_Rev <- optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE )[[2]]
  opt_Price <- optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE )[[1]]

  show_Rev <- paste0('$', conNum_short(round(opt_Rev, 2)))
  show_Price <- paste0('$', conNum_short(round(opt_Price,2)))

  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  newTibble <- data %>%
    mutate(scaled_revenue = quantity * scalar * wtp)

  newPlot <- ggplot(data = newTibble)+
    geom_segment(x = opt_Price, y = 1, xend = opt_Price, yend = opt_Rev,
                 linetype = "dashed", color = "deepskyblue2", lwd = .3)+
    geom_function(fun = fR, color = "deepskyblue3", lwd = 1.3, alpha = .5)+
    geom_point(mapping = aes(x = wtp, y = scaled_revenue), color = "deepskyblue3", alpha = .7)+
    labs(title = title, x = "Price ($'s)", y = "Revenue ($'s)") +
    annotate("label", x = Inf, y = Inf,
             label = paste("Price:", show_Price),
             vjust = 1, hjust = 1, size = 3,
             color = "deepskyblue4", alpha = .8,
             fontface = "bold") +
    annotate("label", x = Inf, y = Inf,
             label =(paste("Revenue:", show_Rev)),
             vjust = 2.5, hjust = 1, size = 3,
             color = "deepskyblue4", alpha = .8,
             fontface = "bold") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme_minimal()+
    theme(plot.title = element_text(face = "bold"))

  return(newPlot)
}


revenueFunction <- function(price, data, type, population, sample = NA){
  check_packages()

  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return(NA)
  fR <- function(price) fQ(price) * price

  price <- round(price, 2)

  show_price <- paste0('$', format(round(price,2), big.mark = ","))
  show_revenue <- paste0('$', format(conNum_short(round(fR(price),2)), big.mark = ","))

  title = paste0("Revenue when Price is ", show_price)

  newPlot <- ggplot(data = data) +
    xlim(0, max(data$wtp))+
    geom_function(fun = fR, color = "skyblue", lwd = 1.8, alpha = .8)+
    geom_point(x = price, y = fR(price), color = 'skyblue3', size = 3) +
    geom_segment(x = price, y = 0, xend = price, yend = fR(price),
                 linetype = "dashed", color = "skyblue3", lwd = .6)+
    geom_segment(x = 0, y = fR(price), xend = price, yend = fR(price),
                 linetype = "dashed", color = "skyblue", lwd = .4)+
    labs(title = title, x = "Price ($'s)", y = "Revenue ($'s) ") +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       breaks = scales::extended_breaks(),
                       limits = c(0, NA))+
    theme(plot.title = element_text(face = "bold"))+
    theme_minimal()+
    annotate("label", x = Inf, y = Inf,
             label = paste("Price:", show_price),
             vjust = 1, hjust = 1, size = 4,
             color = "skyblue4", alpha = .8,
             fontface = "bold") +
    annotate("label", x = Inf, y = Inf,
             label =(paste("Revenue:", show_revenue)),
             vjust = 2.5, hjust = 1, size = 4,
             color = "skyblue4", alpha = .8,
             fontface = "bold") +
    theme(axis.text = element_text(size = 6),
          axis.title.x =element_text(size = 8),
          axis.title.y =element_text(size = 8))+
    theme(plot.title = element_text(face = "bold"))

  suppressWarnings(print(newPlot))
  return(fR(price))
}

revenueCompare <- function(data, population, sample = NA, n = 3) {
  check_packages()
  top_types <- nBestDemandModels(data, n)
  plot_list <- list()

  for (type in top_types) {
    nextPlot <- revenuePlot(data, type, population, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  # Combine all the plots using grid.arrange()
  final_plot <-  suppressWarnings(do.call(grid.arrange, c(plot_list, ncol = 2)))
  return(final_plot)
}

revenueOptimize <- function(data, type, population, sample = NA){
  check_packages()
  fR <- fR(data, type, population, sample)
  opt_Rev <- optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE )[[2]]
  opt_Price <- round(optimize(fR, lower = 0, upper = max(data$wtp), maximum = TRUE )[[1]],2)

  revenueFunction(opt_Price, data, type, population, sample)

  return(list(opt_Rev, opt_Price))
}

###########################################
# Cost
###########################################

costFunction <- function(price, data, type, variable, fixed, population, sample = NA){
  check_packages()
  fQ <- fQ(data, type, population, sample)
  if(class(fQ) == class(NA)) return(NA)
  return(fC(variable, fixed, fQ)(price))
}

