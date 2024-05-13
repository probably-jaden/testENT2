source("R/monopoly.R")


whichColumns <- function(which, data) {
  if (which == 1) {
    x1 <- colnames(data)[1]
    x2 <- colnames(data)[2]
    y1 <- colnames(data)[4]
    y2 <- colnames(data)[5]
  } else if (which == 2) {
    x1 <- colnames(data)[2]
    x2 <- colnames(data)[1]
    y1 <- colnames(data)[5]
    y2 <- colnames(data)[4]
  } else {
    stop("Invalid which")
  }
  return(list(x1, x2, y1, y2))
}

quantityCreation_duo <- function(data, col1 = NULL, col2 = NULL) {
  col1_str <- deparse(substitute(col1))
  col2_str <- deparse(substitute(col2))

  Q1 <- paste0("Q_", col1_str)
  Q2 <- paste0("Q_", col2_str)
  P1 <- paste0("wtp_", col1_str)
  P2 <- paste0("wtp_", col2_str)

  data <- data %>%
    group_by({{ col1 }}, {{ col2 }}) %>%
    summarize(count = n()) %>%
    ungroup()

  data <- data %>%
    arrange(desc({{ col1 }}), {{ col2 }})

  data[Q1] <- cumsum(data["count"])

  data <- data %>%
    arrange(desc({{ col2 }}), {{ col1 }})
  data[Q2] <- cumsum(data["count"])

  return(data)
}

linModel_duo <- function(data, x1, x2, y) lm(as.formula(paste(y, "~", x1, "+", x2)), data = data)
expModel_duo <- function(data, x1, x2, y) lm(as.formula(paste("log(", y, " + ", logErr, ") ~", x1, "+", x2)), data = data)
logModel_duo <- function(data, x1, x2, y) lm(as.formula(paste(y, "~ log(", x1, " + ", logErr, ")", "+", "log(", x2, " + ", logErr, ")")), data = data)
powModel_duo <- function(data, x1, x2, y) lm(as.formula(paste("log(", y, " + ", logErr, ") ~ log(", x1, " + ", logErr, ")", "+", "log(", x2, " + ", logErr, ")")), data = data)

sigModel_duo <- function(data, x1, x2, y) {
  output <- NA
  tryCatch(
    {
      model <- nls(as.formula(paste(y, " ~ SSlogis(", x1, ", Asym1, xmid1, scal1) + SSlogis(", x2, ", Asym2, xmid2, scal2)")), data = data)
    },
    error = function(e) {
      output <- NA
      return(output)
    }
  ) -> output
  return(output)
}

anyModel_duo <- function(data, type, x1, x2, y) {
  switch(type,
    Linear      = do.call("linModel_multi", list(data, x1, x2, y)),
    Exponential = do.call("expModel_multi", list(data, x1, x2, y)),
    Log         = do.call("logModel_multi", list(data, x1, x2, y)),
    Power       = do.call("powModel_multi", list(data, x1, x2, y)),
    Sigmoid     = do.call("sigModel_multi", list(data, x1, x2, y)),
    stop("Invalid type")
  )
}


demandModel_duo <- function(data, type) {
  return(anyModel_multi(data, type, "wtp", "quantity"))
}

linFun_duo <- function(data, x1, x2, y) {
  model <- linModel_duo(data, x1, x2, y)
  fun <- function(param1, param2) {
    coef(model)[[1]] + (coef(model)[[2]] * param1) + (coef(model)[[3]] * param2)
  }
  return(fun)
}

expFun_duo <- function(data, x1, x2, y) {
  model <- expModel_duo(data, x1, x2, y)
  fun <- function(param1, param2) {
    exp(coef(model)[[1]] + (coef(model)[[2]] * param1) + (coef(model)[[3]] * param2))
  }
  return(fun)
}

logFun_duo <- function(data, x1, x2, y) {
  model <- logModel_duo(data, x1, x2, y)
  fun <- function(param1, param2) {
    coef(model)[[1]] + (coef(model)[[2]] * log(param1)) + (coef(model)[[3]] * log(param2))
  }
  return(fun)
}

powFun_multi <- function(data, x1, x2, y) {
  model <- powModel_duo(data, x1, x2, y)
  fun <- function(param1, param2) {
    exp(coef(model)[[1]] + (coef(model)[[2]] * log(param1)) + (coef(model)[[3]] * log(param2)))
  }
  return(fun)
}

modelFun_duo <- function(data, type, x1, x2, y) {
  switch(type,
    Linear      = do.call("linFun_duo", list(data, x1, x2, y)),
    Exponential = do.call("expFun_duo", list(data, x1, x2, y)),
    Log         = do.call("logFun_duo", list(data, x1, x2, y)),
    Power       = do.call("powFun_duo", list(data, x1, x2, y)),
    Sigmoid     = do.call("sigFun_duo", list(data, x1, x2, y)),
    stop("Invalid type")
  )
}


scaleFunction_duo <- function(data, type, x1, x2, y, pop, sample = NA, fun = NA) {
  if (class(fun) == class(NA)) fun <- modelFun_duo(data, type, x1, x2, y)
  if (class(fun) == class(NA)) {
    return(NA)
  }
  if (class(sample) == class(NA)) sample <- nrow(data)
  scaler <- (pop / sample)
  newFun <- function(x1, x2) fun(x1, x2) * scaler
  return(newFun)
}

fQm <- function(data, type, x1, x2, y, population, sample = NA) {
  fQ <- scaleFunction_duo(data, type, x1, x2, y, population, sample)
  if (class(fQ) == class(NA)) {
    return(NA)
  }
  return(fQ)
}

fRm <- function(data, type, x1, x2, y, population, sample = NA) {
  fQ_this <- fQm(data, type, x1, x2, y, population, sample)
  fR_this <- function(x1, x2) {
    fQ_this(x1, x2) * x1
  }
  return(fR_this)
}

fCm <- function(data, type, x1, x2, y, var, fix, population, sample = NA) {
  fQ <- fQm(data, type, x1, x2, y, population, sample)

  fC <- function(x1, x2) {
    (fQ(x1, x2) * var) + fix
  }
  return(fC)
}

fPi_m <- function(data, type, x1, x2, y, var, fix, population, sample = NA) {
  fQ <- fQm(data, type, x1, x2, y, population, sample)

  fPi <- function(x1, x2) {
    (fQ(x1, x2) * x1) - ((fQ(x1, x2) * var) + fix)
  }
  return(fPi)
}

binary_Optim <- function(data, type, x1, x2, y1, y2, var1, fix1, var2, fix2, population, sample = NA) {
  fPi_1 <- fPi_m(data, type, x1, x2, y1, var1, fix1, population, sample)
  fPi_2 <- fPi_m(data, type, x2, x1, y2, var2, fix2, population, sample)

  tolerance <- 0.01
  diff_p1 <- 10
  diff_p2 <- 10
  maxiter <- 100
  i <- 0

  p1_old <- sum(data[x1]) / nrow(data)
  p2_old <- sum(data[x2]) / nrow(data)

  x1_max <- max(data[x1])
  x1_min <- min(data[x1])

  x2_min <- max(data[x2])
  x2_max <- min(data[x2])

  while ((diff_p1 > tolerance || diff_p2 > tolerance) && i <= maxiter) {
    p1 <- optimize(fPi_1, interval = c(x1_min, x1_max), maximum = T, x2 = p2_old)[[1]]
    p2 <- optimize(fPi_2, interval = c(x2_min, x2_max), maximum = T, x2 = p1)[[1]]

    diff_p1 <- abs(p1_old - p1)
    diff_p2 <- abs(p2_old - p2)

    p1_old <- p1
    p2_old <- p2

    i <- i + 1
  }
  return(list(p1, p2))
}

matrix_3D <- function(stage, data, type, col1, col2, y, population, sample, var = 0, fix = 0) {
  stage_function <- switch(stage,
    Quantity  = fQm(data, type, col1, col2, y, population, sample),
    Revenue   = fRm(data, type, col1, col2, y, population, sample),
    Cost      = fCm(data, type, col1, col2, y, var, fix, population, sample),
    Profit    = fPi_m(data, type, col1, col2, y, var, fix, population, sample),
    stop("Invalid type")
  )

  x_intervals <- seq(min(data[col1]), max(data[col1]), length.out = 100) + 1e-10
  y_intervals <- seq(min(data[col2]), max(data[col2]), length.out = 100) + 1e-10

  grid <- expand.grid(x_cross = x_intervals, y_cross = y_intervals)

  grid$z_vals <- with(grid, stage_function(x_cross, y_cross))
  Q_matrix <- matrix(grid$z_vals, nrow = length(x_intervals), ncol = length(y_intervals), byrow = TRUE)

  data$x1Data <- data[[col1]]
  data$x2Data <- data[[col2]]
  data$yData <- data[[y]]

  return(list(Q_matrix, x_intervals, y_intervals, data))
}

# data_ex <- cd_clean
# col1 = "cupcakes"
# col2 = "donuts"

# x_intervals <- seq(min(data_ex[col1]), max(data_ex[col1]), length.out = 100) + 1e-10
# y_intervals <- seq(min(data_ex[col2]), max(data_ex[col2]), length.out = 100) + 1e-10
# grid <- expand.grid(x_cross = x_intervals, y_cross = y_intervals)

# new_data <- data.frame(cupcakes = grid$x_cross,
#                   donuts = grid$y_cross)

# preds <- predict(object = test_mlsr, newdata = new_data)

# Q_matrix <- matrix(preds, nrow = length(x_intervals), ncol = length(y_intervals), byrow = TRUE)

# title_str <- "test"
# plotly::plot_ly() %>%
#  plotly::add_markers(data =cd_clean, x = ~cupcakes,
#                      y = ~donuts , z = ~Q_donuts, type = 'scatter3d', mode = 'markers', name = "data", marker = list(size = 4)) %>%
#  plotly::add_surface(x = x_intervals,
#                      y = y_intervals,
#                      z = Q_matrix,
#                      opacity = 0.5, showscale = FALSE) %>%
#  plotly::layout(
#    title = title_str,
#    margin = list(t = 100)
#  )


# cd_clean

# nls.control(minFactor = 1/4096)
# test_mlsr <- nls(Q_donuts ~ Asym / (1 + exp((xmid - donuts - cupcakes)/scal)),
#                 start = list(Asym = 20, xmid = 2, scal = -1),
#                 control = nls.control(minFactor = 1/1042, maxiter = 50),
#                 data = cd_clean)

# pred <- data.frame(cupcakes = cd_clean$cupcakes,
#                   donuts = cd_clean$donuts,
#                   Q_donuts = predict(test_mlsr))


# getInitial(Q_donuts ~ SSlogis(cupcakes, Asym, xmid, scal), data = cd_clean)
