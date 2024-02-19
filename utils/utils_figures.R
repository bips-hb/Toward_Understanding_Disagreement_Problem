################################################################################
#                 Utility functions for final FIGURES
################################################################################

# Section 3 --------------------------------------------------------------------
create_distribution_fig <- function(result_all, result_local) {
  # Crate plot for the distribution of attribution values
  p1 <- ggplot(result_all) +
    geom_jitter(aes(x = feature, y = attribution, color = attribution), size = 0.2) +
    geom_violin(aes(x = feature, y = attribution), scale = "width", alpha = 0) +
    facet_grid(cols = vars(method_name), scales = "free") +
    scale_color_gradient2(low = "#3A3A98", mid = "white", high = "#832424", limits = c(-2, 2),
                          oob = scales::squish) +
    geom_hline(yintercept = 0, color = "gray40") + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    xlab(NULL) + ylab("Attribution") + guides(color = "none")
  
  # Create plot for a local explanation
  p2 <- ggplot(result_local) +
    geom_bar(aes(x = feature, y = attribution, fill = attribution), stat = "identity") +
    facet_grid(cols = vars(method_name), scales = "free") +
    geom_hline(yintercept = 0, color = "gray40") +
    scale_fill_gradient2(low = "#3A3A98", mid = "white", high = "#832424", limits = c(-2, 2),
                         oob = scales::squish) +
    theme(strip.background.x = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill = "none") + xlab("Feature") + ylab("Attribution")
  
  # Combine both plots
  suppressWarnings({
    plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(3, 2))
  })
}
