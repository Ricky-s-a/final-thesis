errorbar_lm <- function(data) {
  data$model <- data$model %>% set_names(c(df$country %>% unique())) 
  data$summary <- lapply(data$model, summary)
  
  # load library
  library(ggplot2)
  
  # create data frame for plotting error bars
  coefficient <- c()
  p_value <- c()
  sd_error <- c()
  for (i in 1:length(data$country)) {
    coefficient[i] <- data$summary[i][[1]]$coefficients[2, 1]
    p_value[i] <- data$summary[i][[1]]$coefficients[2, 4]
    sd_error[i] <- data$summary[i][[1]]$coefficients[2, 2]
  }
  
  df_graphics <- tibble(
    country = data$country,
    coefficient = coefficient,
    p_value = p_value,
    sd_error = sd_error,
    ci_lower = coefficient - 1.96*sd_error,
    ci_upper = coefficient + 1.96*sd_error,
    p_logical = ifelse(p_value > 0.05, "p>0.05", "p<=0.05"),
    labor_flexibility = data$labor_flexibility_ranking
  )
  
  # plot with ggplot2
  ggplot(df_graphics, 
         aes(reorder(country, labor_flexibility), 
             coefficient, 
             colour = p_logical)) +
    geom_hline(yintercept = 0, alpha = 0.25, linetype = "dashed") +
    geom_point() +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
    scale_color_manual(values = c( "#33ccff", "#ff9980")) +
    # facet_wrap(~p_logical, scales = "free_x") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0),
          legend.position = "none")
}
