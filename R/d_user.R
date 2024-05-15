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


demandPlot3D <- function(data, type, first_or_second, population, sample) {
  cols <- whichColumns(first_or_second, data)
  model_summary <- summary(anyModel_duo(data, type, cols[[1]], cols[[2]], cols[[3]]))
  r2 <- round(model_summary$adj.r.squared, 2)
  sigma <- round(model_summary$sigma, 2)
  title_str <- paste0(type, ", r2: ", r2, ", sigma: ", sigma)

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
      )
    )
  return(plot3D) #list(plot3D, model_summary))
}

demandPlotDuo <- function(competitor_price, data, type, first_or_second, population, sample = NA) {
  cols <- whichColumns(first_or_second, data)
  title <- paste("Demand:", type)
  fQm_this <- function(price1) fQm(data, type, cols[[1]], cols[[2]], cols[[3]], population, sample)(price1, competitor_price)
  if (class(fQm_this) == class(NA)) {
    return()
  }

  model_summary <- summary(anyModel_duo(data, type, cols[[1]], cols[[2]], cols[[3]]))
  rSq <- round(model_summary$adj.r.squared, 3)

  if (class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population / sample

  data$scaled_y <- data[[cols[[3]]]] * scalar
  new_df_1 <- data.frame(c0 = rep(1, nrow(data)))
  new_df_1$x1 <- data[[cols[[1]]]]
  new_df_1$x2 <- data[[cols[[2]]]]
  new_df_1$y <- data$scaled_y

  newPlot <- ggplot(data = new_df_1) +
    geom_function(
      fun = fQm_this,
      color = "orange", lwd = 1.5, alpha = .8
    ) +
    geom_point(mapping = aes(x = x1, y = y), color = "darkorange", size = 2) +
    labs(title = title, x = "Price ($'s)", y = "Quantity Sold ") +
    annotate("label",
      x = Inf, y = Inf,
      label = paste("R squared:", rSq),
      vjust = 1, hjust = 1,
      color = "darkorange", alpha = .8,
      fontface = "bold"
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale()),
      breaks = scales::extended_breaks(),
      limits = c(0, NA)
    ) +
    theme(plot.title = element_text(face = "bold")) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 6),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8)
    ) +
    theme(plot.title = element_text(face = "bold"))
  return(suppressWarnings(newPlot))
}



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
      )
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
      )
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
      )
    )
  return(plot3D)
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

profitOptLine <- function(data, type, first_or_second, var, fix, population, sample) {
  cols <- whichColumns(first_or_second, data)

  profitOptimizeDuo <- function(data, type, first_or_second, var, fix, population, sample) {
    cols <- whichColumns(first_or_second, data)
    returnFunction <- function(x2_price) {
      optFunction <- function(x1_price) {
        OptFun <- fPi_m(data, type, cols[[1]], cols[[2]], cols[[3]], var, fix, population, sample)(x1_price, x2_price)
        return(OptFun)
      }
      opt <- optimize(optFunction, lower = 0, upper = max(data[[cols[[1]]]]), maximum = TRUE)
      return(list(opt[[1]], opt[[2]]))
    }
    return(returnFunction)
  }
  lineGenFun <- profitOptimizeDuo(data, type, first_or_second, var, fix, population, sample)

  x2_points <- seq(min(data[[cols[[2]]]]), max(data[[cols[[2]]]]), length.out = nrow(data))
  lineData <- lapply(x2_points, function(value) {
    output <- lineGenFun(value)
    list(x2_line = value, x1_line = output[[1]], profit_line = output[[2]])
  })

  lineData_df <- data.frame(do.call(rbind, lineData)) %>%
    mutate(line_color = ifelse(profit_line > 0, "green", "red"))

  title_str <- paste("Profit Optimization Line for", cols[[1]])

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
      data = lineData_df, x = ~x1_line, y = ~x2_line, z = ~profit_line, type = "scatter3d", mode = "lines",
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
      )
    )
  return(suppressWarnings(plot3D))
}

nash <- function(data, type, var1, fix1, var2, fix2, population, sample) {
  cols1 <- whichColumns(1, data)
  cols2 <- whichColumns(2, data)

  profitOptimizeMulti <- function(data, type, first_or_second, var, fix, population, sample) {
    cols <- whichColumns(first_or_second, data)

    returnFunction <- function(x2_price) {
      optFunction <- function(x1_price) {
        OptFun <- fPi_m(data, type, cols[[1]], cols[[2]], cols[[3]], var, fix, population, sample)(x1_price, x2_price)
        return(OptFun)
      }
      opt <- optimize(optFunction, lower = 0, upper = max(data[[cols[[1]]]]), maximum = TRUE)
      return(list(opt[[1]], opt[[2]]))
    }
    return(returnFunction)
  }
  lineGenFun1 <- profitOptimizeMulti(data, type, 1, var1, fix1, population, sample)
  lineGenFun2 <- profitOptimizeMulti(data, type, 2, var2, fix2, population, sample)

  x2_points <- seq(min(data[[cols1[[2]]]]), max(data[[cols1[[2]]]]), length.out = nrow(data))
  lineData1 <- lapply(x2_points, function(value) {
    output <- lineGenFun1(value)
    list(x2_line1 = value, x1_line1 = output[[1]], profit_line1 = output[[2]])
  })

  x1_points <- seq(min(data[[cols2[[2]]]]), max(data[[cols2[[2]]]]), length.out = nrow(data))
  lineData2 <- lapply(x1_points, function(value) {
    output <- lineGenFun2(value)
    list(x1_line2 = value, x2_line2 = output[[1]], profit_line2 = output[[2]])
  })

  lineData_df1 <- data.frame(do.call(rbind, lineData1))
  lineData_df2 <- data.frame(do.call(rbind, lineData2))

  lineData_df <- cbind(lineData_df1, lineData_df2) %>%
    mutate(
      line1_color = ifelse(profit_line1 > 0, "lightsalmon", "red"),
      line2_color = ifelse(profit_line2 > 0, "lightblue", "darkblue")
    )

  nash_point <- binary_Optim(data, type, cols1[[1]], cols1[[2]], cols1[[3]], cols1[[4]], var1, fix1, var2, fix2, population, sample)
  points_df <- data.frame(x = rep(nash_point[[1]], 2), y = rep(nash_point[[2]], 2), profit = rep(NA, 2), point_color = rep(NA, 2))
  points_df[1, 3] <- lineGenFun1(nash_point[[2]])[[2]]
  points_df[2, 3] <- lineGenFun2(nash_point[[1]])[[2]]
  points_df[1, 4] <- ifelse(points_df[1, 3] > 0, "lightsalmon", "red")
  points_df[2, 4] <- ifelse(points_df[2, 3] > 0, "lightblue", "darkblue")

  surface_mat1 <- matrix_3D("Profit", data, type, cols1[[1]], cols1[[2]], cols1[[3]], population, sample, var1, fix1)
  surface_mat2 <- matrix_3D("Profit", data, type, cols2[[1]], cols2[[2]], cols2[[3]], population, sample, var2, fix2)

  title_str <- paste0("Equilibrium firm 1: $", round(nash_point[[1]], 2), ", firm 2: $", round(nash_point[[2]], 2))

  plot3D <- plotly::plot_ly() %>%
    plotly::add_markers(
      data = points_df, x = ~x, y = ~y, z = ~profit,
      type = "scatter3d", mode = "markers", name = "equilibrium", opacity = 1,
      marker = list(size = 4.5, color = ~point_color)
    ) %>%
    plotly::add_trace(
      data = lineData_df, x = ~x1_line1, y = ~x2_line1, z = ~profit_line1,
      type = "scatter3d", mode = "lines", showlegend = FALSE, opacity = .8,
      line = list(width = 8, color = ~line1_color, reverscale = FALSE)
    ) %>%
    plotly::add_trace(
      data = lineData_df, x = ~x1_line2, y = ~x2_line2, z = ~profit_line2,
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
      )
    )
  return(suppressWarnings(plot3D))
}
