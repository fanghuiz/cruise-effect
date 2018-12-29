library(readr)
library(ggplot2)
library(tidyverse)
library(lubridate)

theme_set(hrbrthemes::theme_ipsum_rc())

# Raw data retrieved is from Nov 28 to Dec 05
# This is the cleaned up version from the raw data downloaded from Google trend. 
# All time points with too few data (coded as <1 by Google) is changed to 0.5
compare <- read_csv("compare.csv")

# Subset data to Dec 01 - Dec 05
compare_subset <- compare[55:nrow(compare), ]

# Convert to tidy formate
compare_subset <- compare_subset %>%
  gather(key = search_on, value = interest, -Time)

compare_subset <- compare_subset %>%
  mutate(location = case_when(search_on == "us_web" ~ "United States",
                              search_on == "us_youtube" ~ "United States",
                              search_on == "ww_web" ~ "Worldwide",
                              search_on == "ww_youtube" ~ "Worldwide"),
         venue = case_when(search_on == "us_web" ~ "Web Search",
                           search_on == "us_youtube" ~ "YouTube Search",
                           search_on == "ww_web" ~ "Web Search",
                           search_on == "ww_youtube" ~ "YouTube Search"))

# Timestamp for Cruise tweet
tweet <- ymd_hms("2018-12-04 16:45:00")

# Base plot
trend_plot <- compare_subset %>%
  ggplot(aes(x = Time, y = interest, color = venue))

trend_plot +
  geom_line(alpha = 0.9, size = 0.8) +
  geom_vline(aes(xintercept = tweet),
             color = "grey40", linetype = "dashed") +
  annotate("text", x = tweet, y = 100, label = "Tom Cruise tweet\nDec 04, 16:45 (EST)", 
           hjust = 1.1, vjust = 1, size = 3, lineheight = 1,
           family = "Roboto Condensed") +
  facet_wrap(~ location, ncol = 1) +
  labs(title = "Interests in TV Motion Smoothing following Tom Cruise PSA",
       subtitle = "Google search trends for \"motion smoothing\" from Dec 01 to Dec 05 2018",
       caption = "'Interest over time' numbers represent search interest relative to the highest point on the chart for the given region and time.
       A value of 100 is the peak popularity for the term. A value of 50 means that the term is half as popular.
       \nData: Google Trends. Search term: \"motion smoothing\". Retrieved on Dec 05 2018, 17:00 (EST)",
       x = "Time",
       y = "Interest\nover time") +
  scale_color_manual(values = c("skyblue1", "salmon")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 0))

