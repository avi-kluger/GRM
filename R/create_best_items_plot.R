#' Create Best Items Information Curves Plot
#'
#' Creates a single plot showing information curves for the best items
#' at theta = -2, 0, and 2, with vertical lines indicating these target levels.
#'
#' @param item_analysis Data frame containing item analysis results
#' @param item_labels Named vector of item labels (optional)
#' @param fit The fitted mirt model object
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_vline labs theme_minimal theme element_text scale_color_manual scale_linetype_manual
create_best_items_plot <- function(item_analysis, item_labels = NULL, fit) {
  
  # Find best item at each theta level
  best_at_m2 <- item_analysis$Item[which.max(item_analysis$Info_theta_m2)]
  best_at_0 <- item_analysis$Item[which.max(item_analysis$Info_theta_0)]
  best_at_p2 <- item_analysis$Item[which.max(item_analysis$Info_theta_p2)]
  
  # Get their labels
  best_at_m2_label <- paste0(best_at_m2, ": ", item_analysis$Label[item_analysis$Item == best_at_m2])
  best_at_0_label <- paste0(best_at_0, ": ", item_analysis$Label[item_analysis$Item == best_at_0])
  best_at_p2_label <- paste0(best_at_p2, ": ", item_analysis$Label[item_analysis$Item == best_at_p2])
  
  # Get unique items (in case same item is best at multiple levels)
  unique_items <- unique(c(best_at_m2, best_at_0, best_at_p2))
  all_items <- item_analysis$Item
  
  # Calculate information curves across theta range
  theta_range <- seq(-4, 4, length.out = 200)
  
  plot_data_list <- list()
  for (item in unique_items) {
    idx <- which(all_items == item)
    item_label <- paste0(item, ": ", item_analysis$Label[item_analysis$Item == item])
    
    # Determine which theta level(s) this item is best at
    best_at_levels <- c()
    if (item == best_at_m2) best_at_levels <- c(best_at_levels, "Best at θ=-2")
    if (item == best_at_0) best_at_levels <- c(best_at_levels, "Best at θ=0")
    if (item == best_at_p2) best_at_levels <- c(best_at_levels, "Best at θ=2")
    
    info_values <- sapply(theta_range, function(theta) {
      mirt::iteminfo(mirt::extract.item(fit, idx), Theta = matrix(theta))
    })
    
    plot_data_list[[item]] <- data.frame(
      Theta = theta_range,
      Information = as.numeric(info_values),
      Item = item_label,
      Best_At = paste(best_at_levels, collapse = " & "),
      stringsAsFactors = FALSE
    )
  }
  
  plot_data <- do.call(rbind, plot_data_list)
  
  # Create color palette based on number of unique items
  n_items <- length(unique_items)
  colors <- c("#2E86AB", "#A23B72", "#F18F01")[1:n_items]
  
  # Create the plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Theta, y = Information, 
                                                color = Item, linetype = Best_At)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = -2, linetype = "dashed", color = "gray40", alpha = 0.6) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.6) +
    ggplot2::geom_vline(xintercept = 2, linetype = "dashed", color = "gray40", alpha = 0.6) +
    ggplot2::annotate("text", x = -2, y = max(plot_data$Information) * 0.95, 
                     label = "θ = -2", hjust = -0.1, size = 3.5, color = "gray30") +
    ggplot2::annotate("text", x = 0, y = max(plot_data$Information) * 0.95, 
                     label = "θ = 0", hjust = -0.1, size = 3.5, color = "gray30") +
    ggplot2::annotate("text", x = 2, y = max(plot_data$Information) * 0.95, 
                     label = "θ = 2", hjust = -0.1, size = 3.5, color = "gray30") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = "Best Items at Different Ability Levels",
      subtitle = "Information curves for items providing maximum information at θ = -2, 0, and 2",
      x = expression(theta~"(Ability Level)"),
      y = "Information",
      color = "Item",
      linetype = "Best At"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.title = ggplot2::element_text(size = 11),
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 9),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  return(p)
}