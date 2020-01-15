
geom_rope <- function(rope, color = "purple") {
  if (!is.null(rope)) { 
    list(
      geom_vline(xintercept = rope, size = .5, color = color, alpha = 0.8, linetype = "dashed"), 
      geom_vline(xintercept = mean(rope), size = .5, color = color, alpha = 0.5, linetype = "solid"), 
      annotate("text", x = rope, y = 0, label = rope, size = 4, vjust = -6)
    )
  }
}

geom_hdi_with_annotation <- function() {
  list(
    geom_hdi(color = "black", size = 2, credible_mass = ci), 
    annotate("text", x = hdi_rng, y = 0, label = hdi_texts, size = 4, hjust = "center", vjust = -1), 
    annotate("text", x = mean(hdi_rng), y = 0, label = hdi_title, size = 5, vjust = -2)
  )
}

plot_post <- function(data, param_name, rope = NULL, cent_type = "median", title = NULL, 
                      ci = 0.95, dec = 2, xlim = NULL, mle = NULL) {
  param <- data[[param_name]]
  
  central <- if (cent_type == "median") quantile(param, 0.5) else if (cent_type == "mean") mean(param) else NULL
  central_text <- paste0(cent_type, " = ", round(central, dec))
  
  hdi_rng <- hdi(param, ci = ci);  hdi_rng <- c(hdi_rng$CI_low, hdi_rng$CI_high)
  hdi_title <- paste0(ci * 100, "% HDI")
  hdi_texts <- round(hdi_rng, dec)
  
  p <- ggplot(data, aes(param)) +
    geom_histogram(aes(y = ..density..), bins = 40, fill = "skyblue", color = "white", alpha = 0.6) + 
    geom_vline(xintercept = central, color = "grey60", size = 1, alpha = 0.5) +
    annotate("text", x = central, y = Inf, size = 5, label = central_text, vjust = 4) + 
    
    labs(x = param_name, title = title, y = NULL) + 
    geom_rope(rope) + 
    {if (!is.null(xlim)) lims(x = xlim)} + 
    {if (!is.null(mle)) 
      geom_point(data = tibble(x = mle, y = 0), aes(x, y), pch = "+", size = 7, color = "grey50")
    } + 
    theme_post()
  
  return(p)
}
plot_post(res, "omega", rope = ROPE_OMEGA, xlim = c(0.3, 0.6), title = "Posteria of Omega")
plot_post(res, "omega",  xlim = c(0.3, 0.6), title = "Posteria of Omega")




piechart <- function(data, mapping) {
  ggplot(data, mapping) + 
    geom_bar(width = 1) + 
    coord_polar(theta = "y") + 
    labs(x = NULL, y = NULL)
}
piechart(mpg, aes(factor(1), fill = class))

pcp_data <- function(df) {
  is_numeric <- vapply(df, is.numeric, logical(1))
  rescale01 <- function(x) {
    rng <- range(x, na.rm = T)
    (x - rng[1]) / (rng[2] - rng[1])
  }
  df[is_numeric] <- lapply(df[is_numeric], rescale01)
  
  df$.row <- rownames(df)
  
  gather(df, "variable", "value", names(df)[is_numeric])
}

pcp <- function(df, ...) {
  df <- pcp_data(df)
  ggplot(df, aes(variable, value, group = .row)) + geom_line(...)
}

pcp(mpg)
pcp(mpg, aes(color = drv))

vapply(mpg, is.numeric, logical(1))
c <- vapply(mpg, is.numeric, logical(1))
lapply(mpg[c], rescale01)

mpg[is_numeric]


x_var <- "displ"

aes(displ)
aes_(quote(x_var))
aes(displ)
aes_(as.name(x_var))
as.name(x_var)

aes_(parse(text = x_var)[[1]])

parse(text = x_var)[[1]]
as.name(x_var)

f <- function(x_var) {
  #aes(substitute(x_var))
  aes(x_var)
}
f(hh)

aes_(substitute(x_var))

displ

ff <- function(data, mapping) {
  
}

aes_(~displ)

m <- aes(displ)

piechart1 <- function(data, mapping, ...) {
  
  var <- enquo(var)
  data %>% select(!!var)
  #head(data[[parse(text = var)[[1]]]])
  #piechart(data, aes_(factor(1), fill = substitute(var)))
}
piechart1(mpg, hwy)

m <- aes(hwy)
substitute(m$x)

f <- function() {
  n <- 10
  geom_line(aes(x / n))
}
df <- data.frame(x = 1:3, y = 1:3)
ggplot(df, aes(x, y)) + f()





