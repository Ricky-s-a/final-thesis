
# load library ------------------------------------------------------------

library(tidyverse)
library(plotly)


# source funcitons --------------------------------------------------------

source("~/R/Musashi_University/2021_second_semester/final-thesis/R/utilis.R")

# import data -------------------------------------------------------------

path <- "~/R/Musashi_University/2021_second_semester/final-thesis/Data/df_imf"

df <- read_rds(path)


# tidy data ---------------------------------------------------------------

# add a "continent" variable

# get the all countries' name data
world_country <- countrycode::codelist[,c(3,6)] %>% 
  setNames(c("continent", "country"))

df <- left_join(df, world_country) %>% 
  mutate(continent = case_when(
    country == "Czech Republic" ~ "Europe",
    country == "Korea, Rep." ~ "Asia",
    country == "Slovak Republic" ~ "Europe",
    TRUE ~ as.character(continent)
  ))

# visualization -------------------------------------------------------------------------

g1 <- 
  df %>% 
  ggplot(aes(gdp_rate, uem_rate, colour = country)) +
  geom_point(size = 0.25, alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(aes(label = country),
                           data = df %>% filter(year == 2015),
                           position = "identity") +
  # facet_wrap(~continent) +
  labs(title = "A Scatter Plot of GDP and Unemployment Rate for All OECD Countires",
       subtitle = str_wrap("Some countries has a positive relation between unemployment rate and GDP growth rate based on data avilable on world bank data base", 80),
       x = "Real GDP Growth Rate %",
       y = "Unemployment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0),
        legend.position = "none")

ggplotly(g1)


# Modeling ----------------------------------------------------------------

df_lm <-  df %>% 
  group_by(country) %>%
  nest() %>%
  mutate(model = map(data, ~lm(gdp_rate ~ uem_rate, data = .)))

# visualization

# for OECD countries
m <- NULL
result <- NULL
rob_st <- list()
for (i in 1:length(df_lm$country)) {
  m <- df_lm$model[[i]]
  result <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type="HC1"))
  rob_st[[i]] <- result
}
rob_st <- setNames(rob_st, df_lm$country)

# visualization

# create data frame for plotting error bars
coefficient <- c()
p_value <- c()
sd_error <- c()
for (i in 1:length(rob_st)) {
  coefficient[i] <- rob_st[[i]][2, 1]
  p_value[i] <- rob_st[[i]][2, 4]
  sd_error[i] <- rob_st[[i]][2, 2]
}

df_graphics <- tibble(
  country = names(rob_st),
  coefficient = coefficient,
  p_value = p_value,
  sd_error = sd_error,
  ci_lower = coefficient - 1.96*sd_error,
  ci_upper = coefficient + 1.96*sd_error,
  p_logical = ifelse(p_value > 0.05, "p>0.05", "p<=0.05"), 
  # labor_flexibility_ranking = labor_flexibility$labor_flexibility_ranking,
  # labor_flexibility_value = labor_flexibility$employ_protect
)

# plot with ggplot2
# ggplot(df_graphics, aes(reorder(country, labor_flexibility_ranking), 
#                         coefficient,
#                         colour = p_logical)) +
#   geom_hline(yintercept = 0, alpha = 0.25, linetype = "dashed") +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
#   scale_color_manual(values = c( "#33ccff", "#ff9980")) +
#   # facet_wrap(~p_logical, scales = "free_x") +
#   labs(
#     title = "The Difference Model with World Bank Data among OECD Countires",
#     subtitle = str_wrap("16 countries, which are colored blue, has a negative Beta1 with the difference model.", 100),
#     x = "OECD countries (ordered by labour flexibility, the highest from the left)",
#     y = "Beta 1") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0),
#         legend.position = "none")

ggplot(df_graphics, aes(x = coutnry, # labor_flexibility_value, 
                        y = coefficient,
                        colour = p_logical)) +
  geom_hline(yintercept = 0, alpha = 0.25, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  ggrepel::geom_text_repel(aes(label = country), position = "identity") +
  # ggforce::geom_mark_ellipse(aes(label = p_logical, group = p_logical)) +
  scale_color_manual(values = c( "#33ccff", "#ff9980")) +
  labs(
    title = "The Difference Model with World Bank Data among OECD Countires",
    subtitle = str_wrap("8 countries, which are colored blue, has a negative Beta1 with the difference model.", 100),
    x = "OECD countries (ordered by labour flexibility, the highest from the left)",
    y = "Beta 1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0),
        legend.position = "none")
