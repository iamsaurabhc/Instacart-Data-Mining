library(readr)          # To read data
# library(tidyverse)    # includes readr
library(data.table)     # For data manipulation
library(ggplot2)        # For plotting
library(gridExtra)      # To arrange ggplots
library(scales)         # To adjust numeric scales in ggplots
library(dplyr)          # To use pipes?

orders <- read_csv('../input/orders.csv')
head(orders)
orders <- as.data.table(orders)

#Days since prior order
x11()
ggplot(orders, aes(x = days_since_prior_order)) +
  geom_bar(fill = c("pink", rep("grey25", 6), 
                    "pink", rep("grey25", 6), 
                    "pink", rep("grey25", 6),
                    "pink", rep("grey25", 6),
                    "pink", "grey50", "pink")) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Days Since Prior Order",
       y = "Count",
       title = "Distribution of Orders by Days Since Prior Order") +
  scale_y_continuous(labels = comma)

# Days since prior order and Day of Week
## Facet using grid.arrange for greater control of layout and colours
x11()
po0 <- ggplot(orders[order_dow == 0, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("grey25", 7), "orange", rep("grey25", 6), "orange",
                    rep("grey25", 6), "orange", rep("grey25", 6), "orange",
                    rep("grey25", 2))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 0")

po1 <- ggplot(orders[order_dow == 1, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("grey25", 7), "orange", rep("grey25", 6), "orange",
                    rep("grey25", 6), "orange", rep("grey25", 6), "orange",
                    rep("grey25", 2))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 1")

po2 <- ggplot(orders[order_dow == 2, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("grey25", 7), "orange",  rep("grey25", 6), "orange", "orange",
                    rep("grey25", 6), "orange", rep("grey25", 6), "orange",
                    rep("grey25", 1))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 2")

po3 <- ggplot(orders[order_dow == 3, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("grey25", 2), "orange",  rep("grey25", 28))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 3")

po4 <- ggplot(orders[order_dow == 4, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("grey25", 3), "orange",  rep("grey25", 3), "orange",
                    rep("grey25", 23))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 4")

po5 <- ggplot(orders[order_dow == 5, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("grey25", 4), "orange",  rep("grey25", 2), "orange",
                    rep("grey25", 23))) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 5")

po6 <- ggplot(orders[order_dow == 6, ], aes(x = days_since_prior_order)) +
  geom_bar(fill = c(rep("grey25", 6), "orange", "orange",  rep("grey25", 5), "orange",
                    rep("grey25", 6), "orange", rep("grey25", 6), "orange",
                    rep("grey25", 3))) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Day 6",
       x = "Day Since Last Order")

grid.arrange(po0, po1, po2, po3, po4, po5, po6,
             ncol = 1)

#Order hour of the day when orders on same day

##When is second order?
# Select orders where days_since_prior_order == 0

x11()
ggplot(orders[as.numeric(orders$days_since_prior_order) == 0, ], 
       aes(x = order_hour_of_day)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Hour of Day",
       y = "",
       title = "Hour of day when days since prior order = 0")

## How far apart the ordering is?
## Calculate hours since prior order, only if days since prior order is 0

orders <- data.table(orders)
temp <- orders$order_hour_of_day
temp <- c("24", temp)
temp <- temp[1:length(temp)-1]
orders$temp <- temp
orders[, hours_since_last_order := as.numeric(order_hour_of_day) - as.numeric(temp)]
orders[days_since_prior_order != 0 | is.na(days_since_prior_order), 
       hours_since_last_order := NA]
temp <- orders[days_since_prior_order == 0, ]

x11()
ggplot(temp, aes(x = hours_since_last_order)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Hours Since Last Order",
       y = "",
       title = "Hours Since Last Order if Ordered on the Same Day")

##Orders by Day of the Week

x11()
ggplot(orders, aes(x = order_dow)) + 
  geom_bar(fill = c(rep("gold", 2), rep("grey25", 5))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Day of Week (unknown)",
       y = "Count",
       title = "Distribution of Orders by Day of Week") +
  scale_y_continuous(labels = comma)

##Orders by hour of the day

x11()
ggplot(orders, aes(x = order_hour_of_day)) + 
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 4), "cyan3",
                    rep("grey25", 8))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Hour of the Day",
       y = "",
       title = "Distribution of Orders by Hour of the Day") +
  scale_y_continuous(labels = comma)

## Orders by Hour and Day of Week

x11()
p0 <- ggplot(orders[order_dow == 0, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 14), "gold", rep("grey25", 9))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 0")

p1 <- ggplot(orders[order_dow == 1, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 1")

p2 <- ggplot(orders[order_dow == 2, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 2")

p3 <- ggplot(orders[order_dow == 3, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 3")

p4 <- ggplot(orders[order_dow == 4, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 4")

p5 <- ggplot(orders[order_dow == 5, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 5")

p6 <- ggplot(orders[order_dow == 6, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 14), "gold", rep("grey25", 9))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 6",
       x = "Hour of the Day")



grid.arrange(p0, p1, p2, p3, p4, p5, p6, ncol = 1)