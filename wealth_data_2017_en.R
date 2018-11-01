library("tidyverse")
library("ggthemes")
library("svglite")
library("scales")

min_wealth_decile <- c(90, 466, 1083, 2152, 3582, 5855, 9941, 20536, 76754)

decile_str <- c("10", "20", "30", "40", "50", "60", "70", "80", "90")

min_wealth_df <- data.frame(decile_str, min_wealth_decile)
names(min_wealth_df) <- c("decile", "USD")

g1 <- ggplot(min_wealth_df, aes(decile, USD)) +
  geom_col(width = 0.8) + 
  scale_y_continuous(name="Wealth (USD)"
                     , breaks = c(15000, 30000, 45000, 60000)
                     , labels = scales::dollar) +
  scale_x_discrete(name="Global Wealth PR Value") +
  coord_flip() +
  theme_wsj(base_size = 12) +
  scale_colour_wsj("colors6") + 
  theme(text = element_text(size=12), 
        title = element_text(size=15),
        legend.title = element_text(size=13), 
        axis.title = element_text(size=14),
        axis.line.x = element_line(size=0.5, color = "black"), 
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10)) + 
  ggtitle("The Minimum Wealth\nof Global Wealth Decile")
g1

decile_str2 <- c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90")
decile_pop_by_region <- read_csv("wd_decile_2017_en.csv") %>%
  mutate(decile=as.character(decile))
decile_pop_by_region_long <- gather(decile_pop_by_region, key="region", value="percentage", -decile)

g2 <- ggplot(decile_pop_by_region_long, aes(decile, percentage)) +
  geom_col(width = 0.8) + 
  facet_wrap(~ region, nrow = 4) +
  scale_y_continuous(name="Percentage") +
  scale_x_discrete(name="Global Wealth PR Value") +
  coord_flip() +
  theme_wsj(base_size = 12) +
  scale_colour_wsj("colors6") + 
  theme(text = element_text(size=12), 
        title = element_text(size=15),
        strip.text = element_text(size=14),
        legend.title = element_text(size=13), 
        axis.title = element_text(size=14),
        axis.line.x = element_line(size=0.5, color = "black"), 
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10)) + 
  ggtitle("Percentage of the Population in\nEach Global Wealth Decile in Regions")
g2

tw_decile_pop <- c(0, 0, 0, 0.01, 0.03, 0.06, 0.16, 0.5, 0.99, 2.07)
tw_decile <- data.frame(decile_str2, tw_decile_pop)
names(tw_decile) <- c("decile", "percentage")
tw_decile <- mutate(tw_decile, population = percentage*495/100)

g3 <- ggplot(tw_decile, aes(decile, population)) +
  geom_col(width = 0.8) + 
  scale_y_continuous(name="Population (million people)", breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(name="Global Wealth PR Value") +
  coord_flip() +
  theme_wsj(base_size = 12) +
  scale_colour_wsj("colors6") + 
  theme(text = element_text(size=12), 
        title = element_text(size=15),
        legend.title = element_text(size=13), 
        axis.title = element_text(size=14),
        axis.line.x = element_line(size=0.5, color = "black"), 
        axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10)) + 
  ggtitle("Population in Global Wealth Decile\nin Taiwan")
g3

