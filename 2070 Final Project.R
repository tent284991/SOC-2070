
# Data Visualization  -----------------------------------------------------

wine_data <- read.csv ('/Users/evancheng/Downloads/Wine 2070 Data/winemag-data_first150k.csv')
colnames(wine_data)


# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(maps)

# Figure 1: Box Plot of Wine Scores by Country
ggplot(wine_data %>% 
         group_by(country) %>% 
         filter(n() > 100), # Only countries with >100 wines
       aes(x = reorder(country, points, median), y = points)) +
  geom_boxplot(aes(fill = country), alpha = 0.7) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Figure 1. Distribution of Wine Scores by Country",
       x = "Country",
       y = "Points (80-100 scale)")

# Figure 2: Price vs Points Scatter with Variety Overlay
# First get top 10 varieties
top_varieties <- wine_data %>%
  count(variety) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  pull(variety)

ggplot(wine_data %>% 
         filter(variety %in% top_varieties,
                !is.na(price),
                !is.na(points),
                price < quantile(price, 0.99, na.rm = TRUE)), 
       aes(x = points, y = price)) +
  # Use faceting to separate varieties
  facet_wrap(~variety, scales = "free_y", ncol = 2) +
  # Add density estimation with contours
  geom_density_2d_filled(alpha = 0.7) +
  # Add points with reduced alpha
  geom_point(alpha = 0.2, size = 0.5) +
  # Add trend line
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linewidth = 1) +
  scale_y_log10() +
  theme_minimal() +
  theme(panel.spacing = unit(1, "lines"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5)) +
  labs(title = "Figure 2. Wine Scores vs. Price Relationships by Variety",
       x = "Points",
       y = "Price (USD, log scale)")

# Table
library(knitr)

# Create a data frame with the sentiment analysis summary
sentiment_summary <- data.frame(
  Sentiment = c("Positive", "Neutral", "Negative"),
  Distribution = c(127617, 13202, 10111),
  `Average Price` = c(33.967436, 28.332459, 29.085314),
  `Average Points` = c(88.227893, 86.455537, 85.474632)
)

# Display the table using kable
kable(sentiment_summary, 
      caption = "Table 1. Sentiment Analysis Summary",
      format = "pipe", # Choose a suitable format (e.g., "pipe", "html", "latex")
      align = "c", # Center align the columns
      digits = 2 # Round numeric values to 2 decimal places
)
