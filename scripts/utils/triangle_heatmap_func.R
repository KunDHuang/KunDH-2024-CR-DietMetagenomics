

est_pcor <- function(df, x, y, control_vars = NULL) {
  # Function to estimate correlation coefficients between two variables, by correlation of partial correlation.
  # Args:
  #   df: Dataframe containing the data.
  #   x: Name of the first variable (string).
  #   y: Name of the second variable (string).
  #   control_vars: A vector of variable names to control for in the case of particial correlation. default: [NULL]
  # Returns:
  #   The result of correlation test.
    if (is.null(control_vars)) {
        cor_res <- cor.test(df[, x], df[, y], method = "spearman")
    } else {
        control_vec <- c() # create a vector to store values from to-be-controlled variables
        for (c_var in control_vars) {
            control_vec <- c(control_vec, df[, c_var])
        }
        cor_res <- ppcor::pcor.test(df[, x], df[, y], control_vec, method = "spearman")
    }
    cor_res # assign correlation result to variable 'cor_res'
}

make_est_df <- function(df, parameters, control_vars = NULL) {
  # Function to compute correlation coefficients between pairs of parameters, and store them in a dataframe.
  # Args:
  #   df: Dataframe containing the data.
  #   parameters: A vector of parameter names.
  #   control_vars: A vector of variable names for controlling in partial correlation analysis. default: [NULL]
  # Returns:
  #   A dataframe of correlation coefficients between pairs of parameters.
  parameters_1 <- parameters
  parameters_2 <- parameters
  column_headers <- c("var1", "var2", "coef") # create headers for three columns
  res_df <- tibble::tibble() # create an empty tibble for storing results
  colnames(res_df) <- column_headers
  for (par1_i in 1:length(parameters_1)) {
    for (par2_i in 1:length(parameters_2)) {
      par1 <- parameters_1[par1_i]
      par2 <- parameters_2[par2_i]

      res <- est_pcor(df, par1, par2, control_vars)
      cor_est <- res$estimate
      res_df <- rbind(res_df,
                      tibble::tibble(var1 = par1, var2 = par2, coef = cor_est))
    }
  }
  res_df$coef <- as.numeric(res_df$coef)
  res_df$coef[res_df$var1 == res_df$var2] <- 1.00
  res_df$coef <- round(res_df$coef, digits = 2)
  ## Convert to matrix
  cor_matrix <- xtabs(coef ~ var1 + var2, res_df)
  cor_matrix
}

get_lower_tri <- function(cormat) {
  # Function to get the lower triangle of a correlation matrix.
  # Args:
  #   cormat: A square correlation matrix.
  # Returns:
  #   A matrix with the upper triangle set to NA.
  
  cormat[upper.tri(cormat)] <- NA
  cormat
}



plot_correlation_heatmap <- function(cor_matrix,
                                     grad_colors = c(low = "#3C486B", high = "#FF8400", "mid" = "#FFFFFF"),
                                     x_text_angle = 45,
                                     y_text_angle = 0,
                                     x_text_size = 11,
                                     y_text_size = 11,
                                     x_vjust = 1,
                                     y_vjust = 1,
                                     x_hjust = 1,
                                     y_hjust = 1,
                                     font_style = "Arial",
                                     insquare_text_size = 2,
                                     insquare_text_color = "black") {
  # Function to plot a correlation heatmap.
  # Args:
  #   cor_matrix: A correlation matrix to plot.
  # Returns:
  #   A ggplot object representing the heatmap.
  
  cor_matrix_lower <- get_lower_tri(cor_matrix)
  cor_melt <- reshape2::melt(cor_matrix_lower, na.rm = TRUE)
  ggheatmap <- ggplot2::ggplot(data = cor_melt, ggplot2::aes(var1, var2, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = grad_colors["low"], high = grad_colors["high"], mid = grad_colors["mid"],
                                  midpoint = 0, limit = c(-1,1), space = "Lab",
                                  name = "Spearman\nCorrelation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x_text_angle, vjust = x_vjust,
                                                       size = x_text_size, hjust = x_hjust, family = font_style),
                   axis.text.y = ggplot2::element_text(angle = y_text_angle, vjust = y_vjust,
                                                       size = y_text_size, hjust = y_hjust, family = font_style) ) +
    ggplot2::coord_fixed()
  ggheatmap +
    ggplot2::geom_text(ggplot2::aes(var1, var2, label = value), color = insquare_text_color, size = insquare_text_size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.justification = c(1, 0),
      legend.position.inside = c(0.6, 0.7),
      legend.direction = "horizontal") +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 7, barheight = 1,
                                                   title.position = "top", title.hjust = 0.5))
}



