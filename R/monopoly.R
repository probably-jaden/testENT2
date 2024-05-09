conNum_short <- function(number){
  rounded_Num <- abs(round(number, 3))
  formatted_Num <- if(rounded_Num >= 1e12) {
    paste0(format(rounded_Num / 1e12, digits = 2), "t")
  } else if(rounded_Num >= 1e9) {
    paste0(format(rounded_Num / 1e9, digits = 2), "b")
  } else if(rounded_Num >= 1e6) {
    paste0(format(rounded_Num / 1e6, digits = 2), "m")
  } else if(rounded_Num >= 1e3) {
    paste0(format(rounded_Num / 1e3, digits = 2), "k")
  } else {
    as.character(rounded_Num)  # Keep the value unchanged for var below 1K
  }
  return(formatted_Num)
}

conNum_long <- function(number) {
  rounded_Num <- round(number, 3)

  formatted_Num <- if (abs(rounded_Num) >= 1e9) {
    paste0(ifelse(rounded_Num < 0, "-", ""), format(abs(rounded_Num) / 1e9, digits = 3), " \ billion")
  } else if (abs(rounded_Num) >= 1e6) {
    paste0(ifelse(rounded_Num < 0, "-", ""), format(abs(rounded_Num) / 1e6, digits = 3), " \ million")
  } else if (abs(rounded_Num) >= 1e3) {
    paste0(ifelse(rounded_Num < 0, "-", ""), format(abs(rounded_Num) / 1e3, digits = 3), " \ thousand")
  } else {
    as.character(rounded_Num)  # Keep the value unchanged for values below 1K
  }
  return(formatted_Num)
}

quantityCreation <- function(data){
  data <- data %>%
    group_by(wtp) %>%
    summarize(count = n()) %>%
    arrange(desc(wtp)) %>%
    mutate(quantity = cumsum(count))
  return(data)
}

revenueCreation <- function(data){
  data <- data %>%
    mutate(revenue = wtp * quantity)
  return(data)
}

removeDollarSigns <- function(vector){
  vector <- vector %>%
    mutate(wtp = as.numeric(gsub("\\$", "", wtp)))
  return(vector)
}

pivotData <- function(data, columns, valueName, columnName){
  data <- data %>%
    pivot_longer(cols = {{columns}}) %>%
    rename(!!columnName := 'name',
           !!valueName := 'value')
  return(data)
}

groupByPrice_ThenSum <- function(data, price, varToSum, newName){
  data <- data %>%
    group_by( {{ price }} ) %>%
    summarize( {{ newName }} := sum( {{ varToSum }} ))
  return(data)
}

logErr <- .0000000000000000001

# returns the model for your data
linModel <- function(data, x, y) lm(as.formula(paste(y, "~", x)), data = data)
expModel <- function(data, x, y) lm(as.formula(paste("log(", y, " + ", logErr, ") ~", x)), data = data)
logModel <- function(data, x, y) lm(as.formula(paste(y, "~ log(", x, " + ", logErr, ")")), data = data)
powModel <- function(data, x, y) lm(as.formula(paste("log(", y, " + ", logErr, ") ~ log(", x, " + ", logErr, ")")), data = data)

sigModel <- function(data, x, y){
  check_packages()
  output <- NA
  tryCatch({
    model <- nls(as.formula(paste(y, " ~ SSlogis(", x, ",Asym, xmid, scal)")), data = data)
  }, error = function(e) {
    output <- NA
    return(output)
  }) -> output
  return (output)
}

anyModel <- function(data, type, x, y){
  switch(type,
         Linear      = do.call("linModel", list(data, x, y)),
         Exponential = do.call("expModel", list(data, x, y)),
         Log         = do.call("logModel", list(data, x, y)),
         Power       = do.call("powModel", list(data, x, y)),
         Sigmoid     = do.call("sigModel", list(data, x, y)),
         stop("Invalid type"))
}

demandModel <- function(data, type) return(anyModel(data, type, "wtp", "quantity"))

modelSummary <- function(data, type, x, y){
  if(type == "Sigmoid") return(NA)
  return(summary(anyModel(data, type, x, y)))
}

rSquared <- function(data, type, x, y){
  check_packages()
  if(type == "Sigmoid"){
    model <- sigModel(data, x, y)

    if(class(model) != class(NA)){
      predicted <- predict(model)
      residuals <- data[[y]] - predicted

      # Calculate residual sum of squares (RSS) and total sum of squares (TSS)
      RSS <- sum(residuals^2)
      mean_y <- mean(data[[y]])
      TSS <- sum((data[[y]] - mean_y)^2)

      # Calculate pseudo-R-squared (PRE)
      pseudo_R_squared <- 1 - (RSS / TSS)
      rSq <- pseudo_R_squared

    } else {
      rSq <- 0
    }
  } else {
    model <- modelSummary(data, type, x, y)
    rSq <- model$adj.r.squared
  }
  return(rSq)
}

rSquaredDemand <- function(data, type) rSquared(data, type, "wtp", "quantity")

nBestModels <- function(data, x, y, n){
  r_squared_values <- c(
    Linear = rSquared(data, "Linear", x, y),
    Exponential = rSquared(data, "Exponential", x, y),
    Log = rSquared(data, "Log", x, y),
    Power = rSquared(data, "Power", x, y),
    Sigmoid = rSquared(data, "Sigmoid", x, y)
  )

  # Sort models by R-squared and select top 4
  sorted_models <- sort(r_squared_values, decreasing = TRUE)
  top_models <- names(sorted_models)[1:n]

  return(top_models)
}

nBestDemandModels <- function(data, n) nBestModels(data, "wtp", "quantity", n)

linFormulaFancy <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_short((coef(demandModel(data, "Linear"))[[1]] * scalar))
  slope <- conNum_short(coef(demandModel(data, "Linear"))[[2]] * scalar)

  latex_string <- sprintf("%s \\ - \\left( \\ Price \\ \\times \\ %s \\right)", int, slope)
  return(latex_string)
}

bestModel <- function(data, x, y) nBestModels(data, x, y, 1)
bestDemand <- function(data) nBestDemandModels(data, 1)

expFormulaFancy <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_short(exp(coef(demandModel(data, "Exponential"))[[1]]) * scalar)
  slope <- conNum_short(exp(coef(demandModel(data, "Exponential"))[[2]]))

  latex_string <- sprintf("%s \\ \\times -%s^{Price}", int, slope)
  return(latex_string)
}

logFormulaFancy <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_short(coef(demandModel(data, "Log"))[[1]] * scalar)
  slope <- conNum_short(coef(demandModel(data, "Log"))[[2]] * scalar)

  latex_string <- sprintf("%s - %s \\times \\log(Price)", int, slope)
  return(latex_string)
}

powFormulaFancy <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_short(coef(demandModel(data, "Linear"))[[1]] * scalar)
  slope <- conNum_short(exp(coef(demandModel(data, "Linear"))[[2]]))

  latex_string <- sprintf("%s \\ \\times  %s^{ \\ log(Price)} ", int, slope)
  return(latex_string)
}

sigFormulaFancy <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  model <- demandModel(data, "Sigmoid")
  if(class(model) == class(NA)) return(NA)

  A_Int <- conNum_short(coef(model)[[1]] * scalar)
  B_Mid <- conNum_short(coef(model)[[2]])
  C_scale <- conNum_short(coef(model)[[3]])

  latex_string <- sprintf("\\frac{%s}{1 + \\mbox{e}^{\\frac{%s - \\text{Price}}{%s}}}", A_Int, B_Mid, C_scale)
  return(latex_string)
}

linInterpret <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- conNum_long(coef(demandModel(data, "Linear"))[[1]] * scalar)
  slope <- conNum_long(coef(demandModel(data, "Linear"))[[2]] * scalar)

  intInterpret<-paste0("* **Intercept** : If the price was $0 we expect to sell **", int, "** unit(s)\n")
  slopeInterpret <-paste0("* **Slope** : For every $1 dollar increase in price, we loose **", slope, "** sale(s) on average\n")

  return(list(intInterpret, slopeInterpret))
}

expInterpret <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample,2)

  intercept <- conNum_long(exp(coef(demandModel(data, "Exponential"))[[1]]) * scalar)
  slope <- coef(demandModel(data, "Exponential"))[[2]]
  slopePercent <- conNum_long((exp(slope)-1) * 100)

  intInterpret <- paste0(" * **Intercept**: If the price was $0 we expect to sell **", intercept, "** unit(s)\n")
  slopeInterpret <- paste0(" * **Slope**: For every $1 dollar increase in price, our sales will drop by **", slopePercent, "%**\n")

  return(list(intInterpret, slopeInterpret))
}

logInterpret <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  intercept <- conNum_long(coef(demandModel(data, "Log"))[[1]] * scalar)
  slope <- coef(demandModel(data, "Log"))[[2]] * scalar
  slopeRate <- conNum_short(round(slope/100, 2))

  intInterpret <- paste0(" * **Intercept** : If the price was $0 we expect to sell **", intercept, "** unit(s)\n")
  slopeInterpret <- paste0(" * **Slope** : For every 1% increase in price, we loose **", slopeRate, "** sales on average\n")

  return(list(intInterpret, slopeInterpret))
}

powInterpret <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  intercept <- conNum_short(exp(coef(demandModel(data, "Power"))[[1]]) * scalar)
  slope <- coef(demandModel(data, "Power"))[[2]]
  slopePercent <- conNum_short(slope * 100)

  intInterpret <- paste0(" * **Intercept**: If the price was $0 we expect to sell **", intercept, "** unit(s)\n")
  slopeInterpret <- paste0(" * **Slope**: For every 1% increase in price, our sales will drop by **", slopePercent,"%** \n")

  return(list(intInterpret, slopeInterpret))
}

sigInterpret <- function(data, population, sample = NA){
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  model <- demandModel(data, "Sigmoid")
  if(class(model) == class(NA)) return(NA)

  A_Int <- conNum_short(coef(model)[[1]] * scalar)
  B_Mid <- conNum_short(round(coef(model)[[2]], 2))
  #C_scale <- coef(model)[[3]]

  intInterpret <- paste0(" * **Intercept**: If the price was $0 we expect to sell **", A_Int, "** unit(s)\n")
  midPointInterpret <- paste0(" * **Mid-Point**: The curve will change from decreasing at an increasing rate to a decreasing rate when the price equals **$", B_Mid, "**\n")

  return(list(intInterpret, midPointInterpret))
}

linFormula <- function(data, population, sample = NA) {
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- population/sample

  int <- round(coef(demandModel(data, "Linear"))[[1]] * scalar, 3)
  slope <- round(coef(demandModel(data, "Linear"))[[2]] * scalar,3)

  cat("Linear Demand: \n \n  Quantity = Intercept + (Slope * Price)  \n")
  cat(paste0("  Quantity = ", int, " + (", slope, " * Price)\n"))

  cat(paste0("\nIntercept: If the price was $0 we expect to sell ", int, " unit(s)\n"))
  cat(paste0("Slope: For every $1 dollar increase in price, we loose ", slope, " sale(s) on average \n\n"))
}

expFormula <- function(data, population, sample = NA) {
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample,2)

  lin_intercept <- round(coef(demandModel(data, "Exponential"))[[1]], 2)
  lin_slope <- round(coef(demandModel(data, "Exponential"))[[2]], 2)

  intercept <- round(exp(coef(demandModel(data, "Exponential"))[[1]]) * scalar, 2)
  slope <- round(exp(coef(demandModel(data, "Exponential"))[[2]]),2)

  slopePercent <- round((slope - 1) * 100, 3)

  cat("Exponential Demand: \n\n")
  cat("  Linearized (transformed): \n    log(Quantity) = Scalar * (intercept + (Slope * Price)) \n")
  cat(paste0("    log(Quantity) = ", scalar, " * (", lin_intercept, " + (", lin_slope, " * Price))"))
  cat("\n   \n  Actual (untransformed): \n    Quantity = intercept * exp{Slope * Price} \n")
  cat(paste0("    Quantity = ", intercept, " * (", slope, " ^ Price)"))

  cat(paste0("\n \nIntercept: If the price was $0 we expect to sell ", intercept, " unit(s)\n"))
  cat(paste0("Slope: For every $1 dollar increase in price, our sales will drop by ", slopePercent, "% \n\n"))
}

logFormula <- function(data, population, sample = NA) {
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  intercept <- round(coef(demandModel(data, "Log"))[[1]] * scalar, 2)
  slope <- round(coef(demandModel(data, "Log"))[[2]] * scalar,2)
  slopeRate <- -round(slope/100, 3)

  lin_intercept <- round(coef(demandModel(data, "Exponential"))[[1]], 2)

  cat("Log Demand: \n\n")
  cat("    Quantity = intercept * (Slope * log(Price))\n")
  cat(paste0("    Quantity = ", intercept, " * (", slope, " * log(Price))"))

  cat(paste0("\n \nIntercept: If the price was $0 we expect to sell ", round(intercept), " unit(s)\n"))
  cat(paste0("Slope: For every 1% increase in price, we loose ", slopeRate, " sales on average \n\n"))
}

powFormula <- function(data, population, sample = NA) {
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  intercept <- round(exp(coef(demandModel(data, "Power"))[[1]]) * scalar, 2)
  slope <- round(coef(demandModel(data, "Power"))[[2]],2)
  slopePercent <- round(slope * 100, 3)

  lin_intercept <- round(coef(demandModel(data, "Power"))[[1]], 2)

  cat("Power Demand: \n\n")
  cat("  Linearized (transformed): \n    log(Quantity) = Scalar * (intercept + (Slope * log(Price))) \n")
  cat(paste0("    log(Quantity) = ", scalar, " * (", lin_intercept, " + (", slope, " * log(Price)))"))
  cat("\n\n  Actual (untransformed): \n    Quantity = intercept * exp{Slope * log(Price)} \n")
  cat(paste0("    Quantity = ", intercept, " * exp{", slope, " * log(Price)}"))

  cat(paste0("\n \nIntercept: If the price was $0 we expect to sell ", intercept, " unit(s)\n"))
  cat(paste0("Slope: For every 1% increase in price, our sales will drop by ", -slopePercent,"% \n\n"))
}

sigFormula <- function(data, population, sample = NA){
  check_packages()
  if(class(sample) == class(NA)) sample <- nrow(data)
  scalar <- round(population/sample, 2)

  model <- demandModel(data, "Sigmoid")
  if(class(model) == class(NA)) return(NA)

  A_Int <- round(coef(model)[[1]] * scalar, 2)
  B_Mid <- round(coef(model)[[2]], 2)
  C_scale <- round(coef(model)[[3]], 2)

  cat("Sigmoid Demand: \n\n")
  cat("    Quantity = Asymptote / (1 + exp{(Mid-Point - Price) / Scale})\n")
  cat(paste0("    Quantity = ", A_Int, " / (1 + exp{(", B_Mid, " - Price) / ", C_scale, " } \n"))

  cat(paste0("\nAsymptotet: The demand/quantity does not exceed ", A_Int, " unit(s) got any price\n"))
  cat(paste0("Mid-Point: The curve will change from decreasing at an increasing \n           rate to a decreasing rate when the price equals $", B_Mid, "\n\n"))
}

linFun <- function(data, x, y) function(p) coef(linModel(data, x, y))[[1]] + coef(linModel(data, x, y))[[2]] * p
expFun <- function(data, x, y) function(p) exp(coef(expModel(data, x, y))[[1]] + coef(expModel(data, x, y))[[2]] * p)
logFun <- function(data, x, y) function(p) coef(logModel(data, x, y))[[1]] + coef(logModel(data, x, y))[[2]] * log(p)
powFun <- function(data, x, y) function(p) exp(coef(powModel(data, x, y))[[1]] + coef(powModel(data, x, y))[[2]] * log(p))

sigFun <- function(data, x, y) {
  check_packages()
  sModel <- sigModel(data, x, y)
  fun <- NA
  if(class(sModel) != class(NA)) fun <- function(p) coef(sModel)[[1]] / (1 + exp(-(p - coef(sModel)[[2]]) / coef(sModel)[[3]]))
  return(fun)
}

modelFun <- function(data, type, x, y){
  check_packages()
  switch(type,
         Linear      = do.call("linFun", list(data, x, y)),
         Exponential = do.call("expFun", list(data, x, y)),
         Log         = do.call("logFun", list(data, x, y)),
         Power       = do.call("powFun", list(data, x, y)),
         Sigmoid     = do.call("sigFun", list(data, x, y)),
         stop("Invalid type"))
}

scaleFunction <- function(data, type, x, y, pop, sample = NA, fun = NA){
  check_packages()
  if(class(fun) == class(NA)) fun <- modelFun(data, type, x, y)
  if(class(fun) == class(NA)) return(NA)
  if(class(sample) == class(NA)) sample <- nrow(data)
  scaler <- (pop/sample)
  newFun <-  function(input) fun(input) * scaler
  return(newFun)
}

fQ <- function(data, type, population, sample = NA){
  fQ <- scaleFunction(data, type, "wtp", "quantity", population, sample)
  if(class(fQ) == class(NA)) return(NA)
  return(fQ)
}

fR <- function(data, type, pop, sample = NA) function(p) fQ(data, type, pop, sample)(p) * p
fC <- function(variable, fixed, fQ) fC <- function(p) fixed + variable * fQ(p)
fPi <- function(fR, fC) fPi <- function(p) fR(p) - fC(p)

nBestProfitPlots <- function(data, n, variable, fixed, pop, sample = NA){
  check_packages()
  top_models<-nBestDemandModels(data, n)
  plot_list <- list()

  for (model in top_models) {
    nextPlot <- profitPlot(data, model, variable, fixed, pop, sample)
    plot_list <- c(plot_list, list(nextPlot))
  }

  final_plot <- do.call(grid.arrange, c(plot_list, ncol = 2))
  return(suppressWarnings(final_plot))
}



















