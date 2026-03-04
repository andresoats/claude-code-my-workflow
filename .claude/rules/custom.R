##### custom functions to make life easier ##### 


## load packages ##
pacman::p_load(tidyverse, ggplot2, showtext, paletteer, rmarkdown)

showtext_auto()
font_add("Palatino", regular = "/System/Library/Fonts/Palatino.ttc")

pprint <- function(x, ...) {
  rmarkdown::paged_table(x)
}

#### custom useful functions ####

#= dataset summariser =#
na_summary <- function(df) {
  df %>%
    summarise(across(everything(),
                     list(
                       n_missing = ~sum(is.na(.)),
                       pct_missing = ~mean(is.na(.))*100
                     ),
                     .names = "{.col}_{.fn}"
    ))
}



summarise_df <- function(df) {
  stats <- tibble(
    variable    = names(df),
    data_type   = sapply(df, function(x) class(x)[1]),
    n_missing   = sapply(df, function(x) sum(is.na(x))),
    n  = count(df)$n,
    n_unique    = sapply(df, function(x) length(unique(x))),
    mean        = sapply(df, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA_real_),
    median      = sapply(df, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA_real_),
    p10         = sapply(df, function(x) if(is.numeric(x)) quantile(x, 0.1, na.rm = TRUE, names = FALSE) else NA_real_),
    p90         = sapply(df, function(x) if(is.numeric(x)) quantile(x, 0.9, na.rm = TRUE, names = FALSE) else NA_real_),
    min         = sapply(df, function(x) if(is.numeric(x)) min(x, na.rm = TRUE) else NA_real_),
    max         = sapply(df, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA_real_),
    top_category= sapply(df, function(x) {
      if (is.character(x) || is.factor(x)) {
        tab <- sort(table(x), decreasing = TRUE)
        names(tab)[1]
      } else {
        NA_character_
      }
    })
  )
  print(stats)
  invisible(stats)
}


### quick scatter function ###
quick_scatter <- function(
    df, x, y, 
    colour = NULL, colour_palette = "viridis::viridis", 
    size = 2, size_var = NULL,
    shape = 16, alpha = 0.8,
    fit_line = FALSE, 
    fit_family = "Palatino", 
    forty_line = FALSE
) {
  # Build aes mapping
  aes_args <- list(x = as.name(x), y = as.name(y))
  if (!is.null(colour)) aes_args$colour <- as.name(colour)
  if (!is.null(size_var)) aes_args$size <- as.name(size_var)
  
  p <- ggplot(df, do.call(aes, aes_args)) +
    geom_point(
      shape = shape, 
      alpha = alpha,
      # Only set fixed colour/size if not mapping variables
      colour = if(is.null(colour)) "#026795" else NULL,
      size = if(is.null(size_var)) size else NULL
    ) +
    
    # Colour scale for mapped variable
    {if (!is.null(colour)) scale_colour_paletteer_c(colour_palette)} +
    # Size scale for mapped variable
    {if (!is.null(size_var)) scale_size_continuous()} +
    
    labs(
      x = x, y = y,
      colour = if(!is.null(colour)) colour,
      size   = if(!is.null(size_var)) size_var
    ) +
    theme_standard
  
  # Fit line and statistics
  if (fit_line) {
    formula <- as.formula(paste(y, "~", x))
    fit <- lm(formula, data = df)
    coeff <- coef(fit)[2]
    r2 <- summary(fit)$r.squared
    se <- summary(fit)$coefficients[2,2]
    stats_str <- sprintf("Coef: %.3f\nR²: %.3f\nSE: %.3f", coeff, r2, se)
    
    p <- p +
      geom_smooth(method = "lm", se = TRUE, colour = "#C3AE78", fill = "#C3AE78", alpha = 0.3) +
      ggplot2::annotate("text",
               x = max(df[[x]], na.rm = TRUE), 
               y = max(df[[y]], na.rm = TRUE),
               label = stats_str, 
               hjust = 1, 
               vjust = 1, 
               size = 4, 
               family = fit_family)
  }
  
  if (forty_line) {
    p <- p +
      geom_abline(slope = 1, intercept = 0, colour = "#E63946", linetype = "dashed")
  }
  
  print(p)
  invisible(p)
}

quick_binscatter <- function(
    df, x, y,
    bins = 20, bin_by = c("x", "quantile"),  # "x" = equal-width; "quantile" = equal-count
    colour = NULL, colour_palette = "viridis::viridis",
    size = 2, size_var = NULL,
    shape = 16, alpha = 0.9,
    fit_line = FALSE,
    fit_family = "Palatino"
) {
  stopifnot(is.data.frame(df))
  bin_by <- match.arg(bin_by)
  
  # Defensive: drop missing x/y
  df0 <- df[!is.na(df[[x]]) & !is.na(df[[y]]), , drop = FALSE]
  
  # Create bins
  if (bin_by == "x") {
    # equal-width bins
    brks <- pretty(df0[[x]], n = bins)
    df0$.bin <- cut(df0[[x]], breaks = brks, include.lowest = TRUE, right = TRUE)
  } else {
    # equal-count bins (quantiles)
    probs <- seq(0, 1, length.out = bins + 1)
    brks <- unique(as.numeric(stats::quantile(df0[[x]], probs = probs, na.rm = TRUE, type = 7)))
    if (length(brks) < 2) stop("Not enough unique x values to create quantile bins.")
    df0$.bin <- cut(df0[[x]], breaks = brks, include.lowest = TRUE, right = TRUE)
  }
  
  # Aggregate within bins
  agg_fun <- function(v) mean(v, na.rm = TRUE)
  dfb <- dplyr::group_by(df0, .bin) |>
    dplyr::summarise(
      x_bin = agg_fun(.data[[x]]),
      y_bin = agg_fun(.data[[y]]),
      colour_bin = if (!is.null(colour)) agg_fun(.data[[colour]]) else NA_real_,
      size_bin   = if (!is.null(size_var)) agg_fun(.data[[size_var]]) else NA_real_,
      n = dplyr::n(),
      .groups = "drop"
    )
  
  # Build aes mapping
  aes_args <- list(x = as.name("x_bin"), y = as.name("y_bin"))
  if (!is.null(colour))   aes_args$colour <- as.name("colour_bin")
  if (!is.null(size_var)) aes_args$size   <- as.name("size_bin")
  
  p <- ggplot2::ggplot(dfb, do.call(ggplot2::aes, aes_args)) +
    ggplot2::geom_point(shape = shape, alpha = alpha, size = if (is.null(size_var)) size else NULL) +
    {if (!is.null(colour)) paletteer::scale_colour_paletteer_c(colour_palette) else NULL} +
    {if (!is.null(size_var)) ggplot2::scale_size_continuous() else NULL} +
    ggplot2::labs(
      x = x, y = y,
      colour = if (!is.null(colour)) colour,
      size   = if (!is.null(size_var)) size_var
    ) +
    theme_standard
  
  # Fit line and statistics (on binned points)
  if (fit_line) {
    fit <- stats::lm(y_bin ~ x_bin, data = dfb)
    coeff <- stats::coef(fit)[2]
    r2 <- summary(fit)$r.squared
    se <- summary(fit)$coefficients[2, 2]
    stats_str <- sprintf("Coef: %.3f\nR²: %.3f\nSE: %.3f", coeff, r2, se)
    
    p <- p +
      ggplot2::geom_smooth(method = "lm", se = TRUE,
                           colour = "#C3AE78", fill = "#C3AE78", alpha = 0.3) +
      ggplot2::annotate(
        "text",
        x = max(dfb$x_bin, na.rm = TRUE),
        y = max(dfb$y_bin, na.rm = TRUE),
        hjust = 1, vjust = 1,
        label = stats_str, size = 4, family = fit_family
      )
  }
  
  print(p)
  invisible(p)
}

quick_hist <- function(df, var, bins = 30, fill = "#026795", add_density = TRUE) {
  p <- ggplot(df, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bins = bins, fill = fill, alpha = 0.6) +
    geom_vline(aes(xintercept = mean(get(var), na.rm = TRUE)),
               color = "#F77F00", linetype = "dashed") +
    theme_standard +
    labs(title = paste("Histogram of", var))
  
  if (add_density) {
    p <- p + geom_density(color = "#2E5266", alpha = 0.8, linetype = "dashed")
  }
  
  p
}

quick_bar <- function(df, cat_var, num_var) {
  ggplot(df, aes(x = reorder(.data[[cat_var]], .data[[num_var]]), y = .data[[num_var]])) +
    geom_col(fill = "#026795") +
    coord_flip() +
    theme_standard +
    labs(x = cat_var, y = num_var)
}

quick_lm <- function(df, y, x) {
  formula <- as.formula(paste(y, "~", x))
  fit <- lm(formula, df)
  coefs <- summary(fit)$coefficients
  summ <- summary(fit)
  
  tibble(
    term = rownames(coefs), 
    estimate = coefs[,1], 
    std.error = coefs[,2], 
    p.value = coefs[,4],
    r.squared = summ$r.squared,
    f.statistic = summ$fstatistic[1]
  )
}

cor_heatmap <- function(df, method = "pearson", palette = "YlOrRd", lab_size = 3) {
  require(ggplot2)
  require(reshape2)
  num_df <- df %>% dplyr::select(where(is.numeric))
  cormat <- cor(num_df, use = "pairwise.complete.obs", method = method)
  
  melt_cormat <- melt(cormat)
  
  ggplot(melt_cormat, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_distiller(palette = palette, direction = 1,
                         limits = c(-1, 1), name = "Correlation") +
    geom_text(aes(label = sprintf("%.2f", value)), size = lab_size, color = "black") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, family = "Palatino"),
      axis.text.y = element_text(family = "Palatino"),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = paste0("Correlation Heatmap (", method, ")"),
      x = "", y = ""
    )
}

peek <- function(df, n = 5) {
  print(head(df, n))
  invisible(df)
}

#### custom colours ####
colours_custom_1_len6 <- c("#C3AE78","#026795","#25980D", "#F77F00","#344D33" , "#616161")
colours_custom_2_len8 <- c("#2E5266","#E63946", "#F77F00", "#6B9BD1", "#665191", "#457B9D","#828282", "#FCBF49")

get_palette_fun <- function(palette_name) {
  function(n) {
    colorRampPalette(paletteer_d(palette_name))(n)
  }
}

# discrete colours #
pd_okeeffe  <- get_palette_fun('MoMAColors::OKeeffe')
pd_hokusai1 <- get_palette_fun('MetBrewer::Hokusai1')
pd_hokusai2 <- get_palette_fun('MetBrewer::Hokusai2')
pd_hokusai3 <- get_palette_fun('MetBrewer::Hokusai3')
pd_exter <- get_palette_fun('MoMAColors::Exter')
pd_blues <- get_palette_fun('grDevices::blues9')
pd_reds <- get_palette_fun('grDevices::reds9')
pd_bluegreen <- get_palette_fun('rcartocolor::BluGrn')


#### custom ggplot functions ####
theme_custom <-  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Palatino", size = 10),
    plot.subtitle = element_text(hjust = 0.5, family = "Palatino", size = 8),
    axis.title.x = element_text(family = "Palatino", size = 12),
    axis.title.y = element_text(family = "Palatino", size = 12),
    axis.text = element_text(family = "Palatino"),
    legend.title = element_text(family = "Palatino", size = 10),
    legend.text = element_text(family = "Palatino", size = 10),
    legend.position = "top",
    strip.text = element_text(family = "Palatino", face = "bold"),
    #aspect.ratio = 3/4
  )

theme_custom_bw <-  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Palatino", size = 10),
    plot.subtitle = element_text(hjust = 0.5, family = "Palatino", size = 8),
    axis.title.x = element_text(family = "Palatino", size = 12),
    axis.title.y = element_text(family = "Palatino", size = 12),
    axis.text = element_text(family = "Palatino"),
    legend.title = element_text(family = "Palatino", size = 10),
    legend.text = element_text(family = "Palatino", size = 10),
    legend.position = "top",
    strip.text = element_text(family = "Palatino", face = "bold"),
    #aspect.ratio = 3/4
  )

theme_custom_r <-  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Palatino", size = 10),
    plot.subtitle = element_text(hjust = 0.5, family = "Palatino", size = 8),
    axis.title.x = element_text(family = "Palatino", size = 12),
    axis.title.y = element_text(family = "Palatino", size = 12),
    axis.text = element_text(family = "Palatino"),
    legend.title = element_text(family = "Palatino", size = 10),
    legend.text = element_text(family = "Palatino", size = 10),
    legend.position = "right",
    strip.text = element_text(family = "Palatino", face = "bold"),
    #aspect.ratio = 3/4
  )

theme_custom_n <-  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Palatino", size = 10),
    plot.subtitle = element_text(hjust = 0.5, family = "Palatino", size = 8),
    axis.title.x = element_text(family = "Palatino", size = 12),
    axis.title.y = element_text(family = "Palatino", size = 12),
    axis.text = element_text(family = "Palatino"),
    legend.title = element_text(family = "Palatino", size = 10),
    legend.text = element_text(family = "Palatino", size = 10),
    legend.position = "none",
    strip.text = element_text(family = "Palatino", face = "bold"),
    #aspect.ratio = 3/4
  )

theme_standard <-  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Palatino", size = 10),
    plot.subtitle = element_text(hjust = 0.5, family = "Palatino", size = 8),
    axis.title.x = element_text(family = "Palatino", size = 12),
    axis.title.y = element_text(family = "Palatino", size = 12),
    axis.text = element_text(family = "Palatino"),
    legend.title = element_text(family = "Palatino", size = 10),
    legend.text = element_text(family = "Palatino", size = 10),
    legend.position = "top",
    strip.text = element_text(family = "Palatino", face = "bold"),
    #aspect.ratio = 3/4
  )