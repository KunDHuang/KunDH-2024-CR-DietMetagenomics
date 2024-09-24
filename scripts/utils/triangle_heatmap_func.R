# Load necessary libraries
library(ggpubr)
library(ppcor)
library(ggplot2)
library(reshape2)
library(tidyverse)


est_pcor <- function(df, x, y, control_vars, method = "spearman") {
  # Function to estimate partial correlation between two variables, controlling for others.
  # Args:
  #   df: Data frame containing the data.
  #   x: Name of the first variable (string).
  #   y: Name of the second variable (string).
  #   control_vars: Vector of variable names to control for.
  #   method: Correlation method ("spearman" or "pearson"). Default is "spearman".
  # Returns:
  #   The result of the partial correlation test.
  
  control_vec <- c()
  for (i in control_vars) {
    control_vec <- c(control_vec,md_df[[i]])
  }
  
  res <- pcor.test(df[, x], df[, y], control_vec, method = method)
  res
}

make_est_df <- function(df, parameters, control_vars) {
  # Function to compute partial correlations between all pairs of parameters.
  # Args:
  #   df: Data frame containing the data.
  #   parameters: Vector of parameter names to compute correlations between.
  #   control_vars: Variables to control for in partial correlations.
  # Returns:
  #   A partial correlation coefficients matrix.
  
  columns <- c("var1", "var2", "coef")
  res_df <- tibble()
  colnames(res_df) <- columns
  colnames(res_df) <- columns
  for (par1 in parameters) {
    for (par2 in parameters) {
      res <- est_pcor(df, par1, par2, control_vars)
      cor_est <- res$estimate
      res_df <- rbind(res_df,
                      tibble(var1 = par1, var2 = par2, coef = cor_est))
    }
  }
  res_df$coef <- as.numeric(res_df$coef)
  res_df$coef[res_df$coef == -1.00] <- 1.00
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

reorder_cormat <- function(cormat, new_order) {
  # Function to reorder a correlation matrix.
  # Args:
  #   cormat: A square correlation matrix.
  #   new_order: A vector specifying the new order of the rows and columns.
  # Returns:
  #   The reordered correlation matrix.
  
  cormat <- cormat[new_order, new_order]
  cormat
}

plot_density <- function(df, x_var, color_var, fill_var, xlab, ylab, palette) {
  # Function to plot a density plot.
  # Args:
  #   df: Data frame containing the data.
  #   x_var: Variable for x-axis.
  #   color_var: Variable for color.
  #   fill_var: Variable for fill.
  #   xlab: Label for x-axis.
  #   ylab: Label for y-axis.
  #   palette: Color palette.
  # Returns:
  #   A ggplot object representing the density plot.
  
  ggpubr::ggdensity(df, x = x_var,
                    add = "mean", rug = TRUE,
                    color = color_var, fill = fill_var,
                    xlab = xlab,
                    ylab = ylab,
                    palette = palette)
}

plot_correlation_heatmap <- function(cor_matrix) {
  # Function to plot a correlation heatmap.
  # Args:
  #   cor_matrix: A correlation matrix to plot.
  # Returns:
  #   A ggplot object representing the heatmap.
  
  cor_matrix_lower <- get_lower_tri(cor_matrix)
  cor_melt <- reshape2::melt(cor_matrix_lower, na.rm = TRUE)
  
  ggheatmap <- ggplot2::ggplot(data = cor_melt, ggplot2::aes(var1, var2, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                  midpoint = 0, limit = c(-1,1), space = "Lab",
                                  name = "Spearman\nCorrelation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       size = 12, hjust = 1)) +
    ggplot2::coord_fixed()
  ggheatmap +
    ggplot2::geom_text(ggplot2::aes(var1, var2, label = value), color = "black", size = 2) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal") +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 7, barheight = 1,
                                                   title.position = "top", title.hjust = 0.5))
}


## ---------- ##
## ---------- ##
## ---------- ##

# Main script
filename <- "Mappingfile_dietquality_02122022.tsv"
md_df <- read.csv(filename, sep = "\t", header = TRUE)
md_df <- head(md_df, -1)

# Map 'Suppl_Vit' and 'Sex' to numeric values
lookup_vit_suppl <- c("user" = 1, "non-user" = 0)
lookup_sex <- c("Male" = 1, "Female" = 0)
md_df$Suppl_Vit <- lookup_vit_suppl[md_df$Suppl_Vit]
md_df$Sex <- lookup_sex[md_df$Sex]

md_df

# Plot density plot
palette <- c("#0072B2", "#D55E00", "#009E73")
density_plot <- plot_density(md_df, x_var = "dietquality_score",
                             color_var = "Diet", fill_var = "Diet",
                             xlab = "HEI-FLEX scoring",
                             ylab = "Subjects (density)",
                             palette = palette)
print(density_plot)

# Define parameters and control variables
parameters <- c("dietquality_score", "CRP", "IL_6", "creatinin", "glucose",
                "iron", "transferrin", "ferritin", "folic_acid", "vit_B12",
                "cholesterol", "HDL_chol", "LDL_chol", "triglycerides", "homocystein",
                "parathormon", "vit_D3", "HbA1c", "insulin")

control_vars <- c("Diet_duration", "Suppl_Vit", "Age", "Sex", "BMI")

# Compute partial correlations
cor_df <- make_est_df(md_df, parameters, control_vars)


# Reorder correlation matrix
new_order <- c(10, 12, 17, 14, 1, 3, 5, 2, 7, 16, 8, 11, 13, 15, 9, 19, 18, 6, 4)
cor_matrix_reordered <- reorder_cormat(cor_df, new_order)

# Plot heatmap
heatmap_plot <- plot_correlation_heatmap(cor_matrix_reordered)
print(heatmap_plot)
