create_data = function(fname, ngroups = 5) {
  if (file.exists(fname)) {
    data = readRDS(fname)
    # data = data %>% filter(group %in% seq(ngroups))
  } else {
    ntimes = 15
    n = ntimes * ngroups
    data <- data_frame(
      x = rep(1:ntimes, ngroups),
      group = factor(rep(1:ngroups, each = ntimes)))
    data = data %>% mutate(
      # y = rpois(n, lambda = 99),
      y = rnorm(n, mean = 1.1029, sd = 0.5),
      y = 10^(y) - 1,
      group_name = as.character(group)
    )
    data$y[ data$y < 0] = 0
  }
  ngroups = length(unique(data$group))
  y_max = max(data$y)
  x_min = min(data$x)
  y_min = 0
  x_max = max(data$x)
  
  number_of_add_weeks = 12
  
  if (is.Date(x_max)) {
    by = "week"
  } else {
    by = 1
  }
  lower_limit = NA_real_
  eg = expand.grid(
    x = seq(x_max + 1, 
            x_max + 1 + number_of_add_weeks, by = by),
    group = unique(data$group),
    y = lower_limit)
  gg = unique(data[, c("group", "group_name")])
  eg = left_join(eg, gg, by = "group")
  data = full_join(data, eg)
  data = data %>% arrange(group, x, y)
  data = data %>% mutate(y = na.locf(y))
  L = list(
    ngroups = ngroups,
    y_max = y_max,
    x_min = x_min,
    y_min = y_min,
    x_max = x_max,
    data = data
  )
  return(L)
}
