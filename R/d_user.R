source("R/duopoly.r")


#' Title
#'
#' @param price1
#' @param price2
#' @param data
#' @param type
#' @param first_or_second
#' @param population
#' @param sample
#'
#' @return
#' @export
#'
#' @examples
competitionQuantity <- function(price1, price2, data, type, first_or_second, population, sample = NA) {
  cols <- whichColumns(first_or_second, data)
  quantity <- fQm(data, type, cols[[1]], cols[[2]], cols[[3]], population, sample)(price1, price2)
  cat(paste0("Quantity Sold: ", format(round(quantity, 2), big.mark = ","), "\n"))
}

demandSummaryDuo <- function(data, type, first_or_second) {
  cols <- whichColumns(first_or_second, data)
  model_summary <- summary(anyModel_duo(data, type, cols[[1]], cols[[2]], cols[[3]]))
  return(model_summary)
}


demandPlot3D <- function(data, type, first_or_second, population, sample) {
  cols <- whichColumns(first_or_second, data)
  model_summary <- summary(anyModel_duo(data, type, cols[[1]], cols[[2]], cols[[3]]))
  #r2 <- round(model_summary$adj.r.squared, 2)
  sigma <- round(model_summary$sigma, 2)
  #title_str <- paste0(type, ", r2: ", r2, ", sigma: ", sigma)
  title_str <- paste0("Demand: ", cols[[1]])

  mat_obj <- matrix_3D("Quantity", data, type, cols[[1]], cols[[2]], cols[[3]], population, sample)

  plot3D <- plotly::plot_ly() %>%
    plotly::add_markers(
      data = mat_obj[[4]], x = ~x1Data,
      y = ~x2Data, z = ~ yData * (population / sample), opacity = .95,
      size = 7, type = "scatter3d", mode = "markers", name = "data"
    ) %>%
    plotly::add_surface(
      x = mat_obj[[2]],
      y = mat_obj[[3]],
      z = mat_obj[[1]],
      opacity = 0.5, showscale = FALSE
    ) %>%
    plotly::layout(
      title = title_str,
      margin = list(t = 100),
      scene = list(
        xaxis = list(title = cols[[1]]),
        yaxis = list(title = cols[[2]]),
        zaxis = list(title = cols[[3]])
      ),
      showlegend = FALSE
    )
  return(plot3D) #list(plot3D, model_summary))
}


#competitor_price <- 2
#price <- 1
#data <- cpM
#type <- "Linear"
#first_or_second <- 1
#population <- 100000
#sample <- 1000

demandPlotDuo <- function(competitor_price, price, data, type, first_or_second, population, sample = NA) {
  cols <- whichColumns(first_or_second, data)
  title <- paste("Demand:", type)
  fQm_this <- function(price1) fQm(data, type, cols[[1]], cols[[2]], cols[[3]], population, sample)(price1, competitor_price)
  if (class(fQm_this) == class(NA)) {
    return()
  }

  model_summary <- summary(anyModel_duo(data, type, cols[[1]], cols[[2]], cols[[3]]))
  rSq <- round(model_summary$adj.r.squared, 3)

  qSold_prefix <- ifelse(fQm_this(price) > 0, "", "-")
  qSold_str <- paste0(qSold_prefix, conNum_short(floor(fQm_this(price))))

  if (class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population / sample

  data$scaled_y <- data[[cols[[3]]]] * scalar
  new_df_1 <- data.frame(c0 = rep(1, nrow(data)))
  new_df_1$x1 <- data[[cols[[1]]]]
  new_df_1$x2 <- data[[cols[[2]]]]
  new_df_1$y <- data$scaled_y
  new_df_1$distCPrice <- sqrt((data[[cols[[2]]]] - competitor_price)^2)
  new_df_1$distCPriceN <- 1/(new_df_1$distCPrice + 1)


  newPlot <- ggplot(data = new_df_1) +
    geom_function(
      fun = fQm_this,
      color = "orange", lwd = 1, alpha = .8
    ) +
    geom_point(mapping = aes(x = x1, y = y, alpha = distCPriceN), color = "darkorange", size = 2) +
    guides(alpha = FALSE)+
    labs(x = "Price ($'s)", y = "Quantity Sold ") +
    annotate("label",
      x = Inf, y = Inf,
      label = paste("Quantity Sold:", qSold_str),
      vjust = 1, hjust = 1.1, size = 5,
      color = "darkorange", alpha = .8,
      fontface = "bold"
    ) +
    geom_segment(
      x = price, y = 0, xend = price, yend = fQm_this(price),
      linetype = "dashed", color = "orange", lwd = .2, alpha =.5
    ) +
    geom_segment(
      x = 0, y = fQm_this(price), xend = price, yend = fQm_this(price),
      linetype = "dashed", color = "orange", lwd = .2, alpha = .5
    ) +
    geom_point(shape = 21, x = price, y = fQm_this(price), color = "darkorange3", fill = "white", size = 3) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale()),
      breaks = scales::extended_breaks(),
      limits = c(0, NA)
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    ) +
    theme(plot.title = element_text(face = "bold"))
  return(suppressWarnings(newPlot))
}



#library(scales)

#cp
#cpM <- quantityCreation_duo(cp, "cupcakes", "donuts")

demandPlotDuo(.03, 3, cpM, "Exponential", 1, 1, 1)



competitionRevenue <- function(price1, price2, data, type, first_or_second, population, sample = NA) {
  cols <- whichColumns(first_or_second, data)
  rev <- fRm(data, type, cols[[1]], cols[[2]], cols[[3]], population, sample)(price1, price2)
  cat(paste0("Revenue is $", format(round(rev, 2), big.mark = ","), "\n"))
}

revenuePlot3D <- function(data, type, first_or_second, population, sample) {
  cols <- whichColumns(first_or_second, data)
  data$Revenue_Obs <- data[[cols[[1]]]] * (data[[cols[[3]]]] * (population / sample))
  points_df <- data.frame(x = data[[cols[[1]]]], y = data[[cols[[2]]]], z = data$Revenue_Obs)

  mat_obj <- matrix_3D("Revenue", data, type, cols[[1]], cols[[2]], cols[[3]], population, sample)

  plot3D <- plotly::plot_ly() %>%
    plotly::add_markers(
      data = points_df, x = ~x, y = ~y, z = ~z,
      type = "scatter3d", mode = "markers", name = "data", marker = list(size = 4)
    ) %>%
    plotly::add_surface(
      x = mat_obj[[2]],
      y = mat_obj[[3]],
      z = mat_obj[[1]],
      opacity = 0.5, showscale = FALSE
    ) %>%
    plotly::layout(
      title = "Revenue",
      margin = list(t = 100),
      scene = list(
        xaxis = list(title = cols[[1]]),
        yaxis = list(title = cols[[2]]),
        zaxis = list(title = "Revenue ($'s)")
      ),
      showlegend = FALSE
    )
  return(plot3D)
}


competitionCost <- function(price1, price2, data, type, first_or_second, variable, fixed, population, sample = NA) {
  cols <- whichColumns(first_or_second, data)
  cost <- fCm(data, type, cols[[1]], cols[[2]], cols[[3]], variable, fixed, population, sample)(price1, price2)
  cat(paste0("Cost is $", format(round(cost, 2), big.mark = ","), "\n"))
}


costPlot3D <- function(data, type, first_or_second, var, fix, population, sample) {
  cols <- whichColumns(first_or_second, data)
  mat_obj <- matrix_3D("Cost", data, type, cols[[1]], cols[[2]], cols[[3]], population, sample, var, fix)
  plot3D <- plotly::plot_ly() %>%
    plotly::add_surface(
      x = mat_obj[[2]],
      y = mat_obj[[3]],
      z = mat_obj[[1]],
      opacity = 0.5, showscale = FALSE
    ) %>%
    plotly::layout(
      title = "Cost",
      margin = list(t = 100),
      scene = list(
        xaxis = list(title = cols[[1]]),
        yaxis = list(title = cols[[2]]),
        zaxis = list(title = "Cost ($'s)")
      ),
      showlegend = FALSE
    )
  return(plot3D)
}

competitionProfit <- function(price1, price2, data, type, first_or_second, variable, fixed, population, sample = NA) {
  cols <- whichColumns(first_or_second, data)
  profit <- fPi_m(data, type, cols[[1]], cols[[2]], cols[[3]], variable, fixed, population, sample)(price1, price2)
  cat(paste0("Profit is $", format(round(profit, 2), big.mark = ","), "\n"))
}

profitPlot3D <- function(data, type, first_or_second, var, fix, population, sample) {
  cols <- whichColumns(first_or_second, data)
  mat_obj <- matrix_3D("Profit", data, type, cols[[1]], cols[[2]], cols[[3]], population, sample, var, fix)
  plot3D <- plotly::plot_ly() %>%
    plotly::add_surface(
      x = mat_obj[[2]],
      y = mat_obj[[3]],
      z = mat_obj[[1]],
      opacity = 0.5, showscale = FALSE
    ) %>%
    plotly::layout(
      title = "Profit",
      margin = list(t = 100),
      scene = list(
        xaxis = list(title = cols[[1]]),
        yaxis = list(title = cols[[2]]),
        zaxis = list(title = "Profit ($'s)")
      ),
      showlegend = FALSE
    )
  return(plot3D)
}



profitPlotDuo <- function(price1, price2, data, type, first_or_second, var, fix, population, sample = NA){
  cols <- whichColumns(first_or_second, data)
  fPim_this <- function(p) fPi_m(data, type, cols[[1]], cols[[2]], cols[[3]], var, fix, population, sample)(p, price2)

  if (class(fPim_this) == class(NA)) {
    return()
  }

  show_price <- paste0(price2)
  show_profit <- paste0("$", conNum_short(round(fPim_this(price1), 2)))

  title <- paste0("Profit when Competitor's price is $", show_price)

  data$wtp <- data[[cols[[1]]]]
  data$profit <- (data[[cols[[1]]]] * (data[[cols[[3]]]] * (population / sample))) - (((data[[cols[[3]]]] * (population / sample)) * var) + fix)

  data$netColor <- ifelse(data$profit > 0, "green4", "red4")
  data$distCPrice <- sqrt((data[[cols[[2]]]] - price2)^2)
  data$distCPriceN <- 1/(data$distCPrice + 1)

  newPlot <- ggplot(data = data) +
    geom_function(
      fun = fPim_this,
      color = "green3", lwd = 1.5, alpha = .8
    ) +
    geom_point(aes(x = wtp, y = profit, color = netColor, alpha = distCPriceN), size = 2)+
    guides(alpha = FALSE)+
    scale_color_identity() +
    geom_point(shape = 21, x = price1, y = fPim_this(price1), color = "darkgreen", fill = "white", size = 1.5) +
    geom_segment( x = price1, y = 0, xend = price1, yend = fPim_this(price1), linetype = "dashed", color = "green3", lwd = .2) +
    geom_segment(x = 0, y = fPim_this(price1), xend = price1, yend = fPim_this(price1), linetype = "dashed", color = "green3", lwd = .2) +
    labs(x = "Price ($'s)", y = "Profit ($'s)") +
    annotate("label",
             x = Inf, y = Inf,
             label = paste("Profit:", show_profit),
             vjust = 1, hjust = 1, size = 5,
             color = "darkgreen", alpha = .8,
             fontface = "bold"
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale()),
      breaks = scales::extended_breaks(),
      limits = c(0, NA)
    ) +
    xlim(c(min(data[[cols[[1]]]]), max(data[[cols[[1]]]])))+
    theme(plot.title = element_text(face = "bold")) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    ) +
    theme(plot.title = element_text(face = "bold"))
  return(suppressWarnings(newPlot))
}


competitionSolve <- function(data, type, first_or_second, variable1, fixed1, variable2, fixed2, population, sample = NA) {
  cols <- whichColumns(first_or_second, data)
  opt_prices <- binary_Optim(data, type, cols[[1]], cols[[2]],
    cols[[3]], cols[[4]],
    variable1, fixed1, variable2, fixed2,
    population,
    sample = NA
  )

  opt_price1 <- opt_prices[[1]]
  opt_price2 <- opt_prices[[2]]

  cat(paste("Optimal Price for Product 1: $", format(round(opt_price1, 2), big.mark = ","), "\n"))
  cat(paste("Optimal Price for Product 2: $", format(round(opt_price2, 2), big.mark = ","), "\n"))

  cat("\nProduct 1\n")
  competitionQuantity(opt_price1, opt_price2, data, type, 1, population, sample = NA)
  competitionRevenue(opt_price1, opt_price2, data, type, 1, population, sample = NA)
  competitionCost(opt_price1, opt_price2, data, type, 1, variable1, fixed1, population, sample = NA)
  competitionProfit(opt_price1, opt_price2, data, type, 1, variable1, fixed1, population, sample = NA)

  cat("\nProduct 2\n")
  competitionQuantity(opt_price2, opt_price1, data, type, 2, population, sample = NA)
  competitionRevenue(opt_price2, opt_price1, data, type, 2, population, sample = NA)
  competitionCost(opt_price2, opt_price1, data, type, 2, variable2, fixed2, population, sample = NA)
  competitionProfit(opt_price2, opt_price1, data, type, 2, variable2, fixed2, population, sample = NA)
}


profitLine <- function(data, type, first_or_second, var, fix, population, sample){
  cols <- whichColumns(first_or_second, data)
  optLineFun <- function(data, type, first_or_second, var, fix, population, sample) {
    profit1_P2 <- function(x2_price) {
      profit1_P1 <- function(x1_price) {
        profitFun <- fPi_m(data, type, cols[[1]], cols[[2]], cols[[3]], var, fix, population, sample)(x1_price, x2_price)
        return(profitFun)
      }
      opt <- optimize(profit1_P1, lower = 0, upper = max(data[[cols[[1]]]]), maximum = TRUE)
      return(list(opt[[1]], opt[[2]]))
    }
    return(profit1_P2)
  }

  this_optLine <- optLineFun(data, type, first_or_second, var, fix, population, sample)
  y_points <- seq(min(data[[cols[[2]]]]), max(data[[cols[[2]]]]), length.out = nrow(data))

  lineData <- lapply(y_points, function(value) {
    output <- this_optLine(value)
    list(y = value, x = output[[1]], profit_line= output[[2]])
  })

  lineData_df <- data.frame(do.call(rbind, lineData))
  return(lineData_df)
}

profitOptLine <- function(data, type, first_or_second, var, fix, population, sample) {
  cols <- whichColumns(first_or_second, data)
  lineDF <- profitLine(data, type, first_or_second, var, fix, population, sample) %>%
    mutate(line_color = ifelse(profit_line > 0, "green", "red"))

  title_str <- paste("Profit Optimization:", cols[[1]])

  data$Profit_Obs <- (data[[cols[[1]]]] * (data[[cols[[3]]]] * (population / sample))) - (((data[[cols[[3]]]] * (population / sample)) * var) + fix)
  points_df <- data.frame(x = data[[cols[[1]]]], y = data[[cols[[2]]]], z = data$Profit_Obs)

  surface_mat <- matrix_3D("Profit", data, type, cols[[1]], cols[[2]], cols[[3]], population, sample, var, fix)

  plot3D <- plot_ly() %>%
    plotly::add_markers(
      data = points_df, x = ~x, y = ~y, z = ~z,
      name = "data", marker = list(size = 3, opacity = 0.5)
    ) %>%
    plotly::add_trace(
      x = c(0), y = c(0), z = c(0), mode = "lines", size = .001,
      name = "optimal profit", showlegend = TRUE, line = list(color = "green")
    ) %>%
    plotly::add_trace(
      data = lineDF, x = ~x, y = ~y, z = ~profit_line, type = "scatter3d", mode = "lines",
      showlegend = FALSE, opacity = 1, line = list(width = 8, color = ~line_color, reverscale = FALSE)
    ) %>%
    plotly::add_surface(
      x = surface_mat[[2]],
      y = surface_mat[[3]],
      z = surface_mat[[1]],
      opacity = 0.5, showscale = FALSE
    ) %>%
    plotly::layout(
      title = title_str,
      margin = list(t = 100),
      scene = list(
        xaxis = list(title = cols[[1]]),
        yaxis = list(title = cols[[2]]),
        zaxis = list(title = "Profit ($'s)")
      ),
      showlegend = FALSE
    )
  return(suppressWarnings(plot3D))
}


nash3D <- function(data, type, var1, fix1, var2, fix2, population, sample) {
  cols1 <- whichColumns(1, data)
  cols2 <- whichColumns(2, data)

  lineData1 <- profitLine(data, type, 1, var1, fix1, population, sample) %>%
    rename(profit_line1 = profit_line,
           x1 = x,
           y1 = y)

  lineData2 <- profitLine(data, type, 2, var2, fix2, population, sample) %>%
    rename(profit_line2 = profit_line,
           y2 = x,
           x2 = y)

  lineData <- cbind(lineData1, lineData2) %>%
    mutate(
      line1_color = ifelse(profit_line1 > 0, "lightsalmon", "red"),
      line2_color = ifelse(profit_line2 > 0, "lightblue", "darkblue")
    )

  nash_point <- binary_Optim(data, type, cols1[[1]], cols1[[2]], cols1[[3]], cols1[[4]], var1, fix1, var2, fix2, population, sample)
  points_df <- data.frame(x = rep(nash_point[[1]], 2), y = rep(nash_point[[2]], 2), profit = rep(NA, 2), point_color = rep(NA, 2))
  points_df[1, 3] <- fPi_m(data, type, cols1[[1]], cols1[[2]], cols1[[3]], var1, fix1, population, sample)(points_df[[1, 1]], points_df[[1, 2]])
  points_df[2, 3] <- fPi_m(data, type, cols2[[1]], cols2[[2]], cols2[[3]], var1, fix1, population, sample)(points_df[[2, 2]], points_df[[2, 1]])
  points_df[1, 4] <- ifelse(points_df[1, 3] > 0, "lightsalmon", "red")
  points_df[2, 4] <- ifelse(points_df[2, 3] > 0, "lightblue", "darkblue")

  surface_mat1 <- matrix_3D("Profit", data, type, cols1[[1]], cols1[[2]], cols1[[3]], population, sample, var1, fix1)
  surface_mat2 <- matrix_3D("Profit", data, type, cols2[[1]], cols2[[2]], cols2[[3]], population, sample, var2, fix2)

  #title_str <- paste0("Equilibrium firm 1: $", round(nash_point[[1]], 2), ", firm 2: $", round(nash_point[[2]], 2))
  title_str <- "Reaction Functions & Nash Equilibrium"

  plot3D <- plotly::plot_ly() %>%
    plotly::add_markers(
      data = points_df, x = ~x, y = ~y, z = ~profit,
      type = "scatter3d", mode = "markers", name = "equilibrium", opacity = 1,
      marker = list(size = 4.5, color = ~point_color)
    ) %>%
    plotly::add_trace(
      data = lineData, x = ~x1, y = ~y1, z = ~profit_line1,
      type = "scatter3d", mode = "lines", showlegend = FALSE, opacity = .8,
      line = list(width = 8, color = ~line1_color, reverscale = FALSE)
    ) %>%
    plotly::add_trace(
      data = lineData, x = ~x2, y = ~y2, z = ~profit_line2,
      type = "scatter3d", mode = "lines", opacity = .8, showlegend = FALSE,
      line = list(width = 8, color = ~line2_color, reverscale = FALSE)
    ) %>%
    plotly::add_surface(
      x = surface_mat1[[2]],
      y = surface_mat1[[3]],
      z = surface_mat1[[1]],
      opacity = 0.3, showscale = FALSE, color = ~ surface_mat2[[1]], colorscale = "YlOrRd"
    ) %>%
    plotly::add_surface(
      x = surface_mat2[[3]],
      y = surface_mat2[[2]],
      z = t(surface_mat2[[1]]),
      opacity = 0.2, showscale = FALSE, color = ~ surface_mat2[[1]], colorscale = "Blues"
    ) %>%
    plotly::layout(
      title = title_str,
      margin = list(t = 100),
      scene = list(
        xaxis = list(title = cols1[[1]]),
        yaxis = list(title = cols1[[2]]),
        zaxis = list(title = "Profit ($'s)")
      ),
      showlegend = FALSE
    )
  return(suppressWarnings(plot3D))
}



nash2D <- function(data, type, var1, fix1, var2, fix2, population, sample){
  cols <- whichColumns(1, data)
  lineDF1 <- profitLine(data, type, 1, var1, fix1, population, sample) %>%
    rename(profit_line1 = profit_line,
           x1 = x,
           y1 = y)

  lineDF2 <- profitLine(data, type, 2, var2, fix2, population, sample) %>%
    rename(profit_line2 = profit_line,
           y2 = x,
           x2 = y)

  lineData <- cbind(lineDF1, lineDF2) %>%
    mutate(
      line1_color = ifelse(profit_line1 > 0, "lightsalmon", "red"),
      line2_color = ifelse(profit_line2 > 0, "lightblue", "darkblue"),
      x1 = unlist(x1),
      y1 = unlist(y1),
      x2 = unlist(x2),
      y2 = unlist(y2),
      profit_line1 = unlist(profit_line1),
      profit_line2 = unlist(profit_line2)
    )
  coord <- binary_Optim(data, type, cols[[1]], cols[[2]], cols[[3]], cols[[4]], var1, fix1, var2, fix2, population, sample)
  coordDF <- data.frame( x = coord[[1]], y = coord[[2]])

  # Plot the data
  plot <- ggplot() +
    geom_line(data = lineData, aes(x = x1, y = y1, color = line1_color), size = 2.6, alpha = .3) +
    geom_line(data = lineData, aes(x = x2, y = y2, color = line2_color), size = 2.6, alpha = .3) +
    geom_point(data = coordDF, aes(x = x, y = y), color = "palevioletred3", size = 2.6, alpha = .5) +
    scale_color_identity() +
    labs(title = "Reaction Functions & Nash Equilibrium",
         x = paste(cols[[1]], "Price ($'s)"),
         y = paste(cols[[2]], "Price ($'s)")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11)) +
    annotate("text", x = max(lineData$x1), y = max(lineData$y1), label = paste(cols[[1]], "reaction"), color = "lightsalmon2", hjust = 1, vjust = 1, size = 5, fontface = "bold") +
    annotate("text", x = max(lineData$x2), y = max(lineData$y2), label = paste(cols[[2]], "reaction"), color = "lightblue3", hjust = 1, vjust = 1, size = 5, fontface = "bold")

  return(plot)
}

