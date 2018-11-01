library("tidyverse")
library("ggthemes")
library("svglite")

min_wealth_decile <- c(90, 466, 1083, 2152, 3582, 5855, 9941, 20536, 76754)
min_wealth_decile <- round(min_wealth_decile*30.3)

decile_str <- c("10", "20", "30", "40", "50", "60", "70", "80", "90")

min_wealth_df <- data.frame(decile_str, min_wealth_decile)
names(min_wealth_df) <- c("decile", "NTD")

g1 <- ggplot(min_wealth_df, aes(decile, NTD)) +
  geom_col(width = 0.8) + 
  scale_y_continuous(name="財富金額（新台幣）"
                     , breaks = c(500000, 1000000, 1500000, 2000000)
                     , labels = c("50萬", "100萬", "150萬", "200萬")) +
  scale_x_discrete(name="全球財富 PR 值") +
  coord_flip() +
  theme_wsj(base_size = 12, base_family = "Noto Sans CJK TC", title_family = "Noto Sans CJK TC") +
  scale_colour_wsj("colors6") + 
  theme(text = element_text(size=12), #控制基本字體大小
        title = element_text(size=16),
        legend.title = element_text(size=13), #控制圖例標題字體大小
        axis.title = element_text(size=14),
        axis.line.x = element_line(size=0.5, color = "black"), #控制x軸粗細與顏色
        axis.text.x = element_text(size=10), #控制x軸內容字體大小
        axis.text.y = element_text(size=10)) + #控制y軸內容字體大小
  ggtitle("全球財富 PR 值各區間財富門檻")
g1

decile_str2 <- c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90")
decile_pop_by_region <- read_csv("wealth_data_decile_2017.csv") %>%
  mutate(decile=as.character(decile))
decile_pop_by_region_long <- gather(decile_pop_by_region, key="region", value="percentage", -decile)

g2 <- ggplot(decile_pop_by_region_long, aes(decile, percentage)) +
  geom_col(width = 0.8) + 
  facet_wrap(~ region, nrow = 4) +
  scale_y_continuous(name="百分比") +
  scale_x_discrete(name="全球財富 PR 值") +
  coord_flip() +
  theme_wsj(base_size = 12, base_family = "Noto Sans CJK TC", title_family = "Noto Sans CJK TC") +
  scale_colour_wsj("colors6") + 
  theme(text = element_text(size=12), #控制基本字體大小
        title = element_text(size=16),
        strip.text = element_text(size=14),
        legend.title = element_text(size=13), #控制圖例標題字體大小
        axis.title = element_text(size=14),
        axis.line.x = element_line(size=0.5, color = "black"), #控制x軸粗細與顏色
        axis.text.x = element_text(size=10), #控制x軸內容字體大小
        axis.text.y = element_text(size=10)) + #控制y軸內容字體大小
  ggtitle("各 PR 值區間區域人口組成比例")
g2

tw_decile_pop <- c(0, 0, 0, 0.01, 0.03, 0.06, 0.16, 0.5, 0.99, 2.07)
tw_decile <- data.frame(decile_str2, tw_decile_pop)
names(tw_decile) <- c("decile", "percentage")
tw_decile <- mutate(tw_decile, population = percentage*495/100)

g3 <- ggplot(tw_decile, aes(decile, population)) +
  geom_col(width = 0.8) + 
  scale_y_continuous(name="人數（百萬人）", breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(name="全球財富 PR 值") +
  coord_flip() +
  theme_wsj(base_size = 12, base_family = "Noto Sans CJK TC", title_family = "Noto Sans CJK TC") +
  scale_colour_wsj("colors6") + 
  theme(text = element_text(size=12), #控制基本字體大小
        title = element_text(size=16),
        legend.title = element_text(size=13), #控制圖例標題字體大小
        axis.title = element_text(size=14),
        axis.line.x = element_line(size=0.5, color = "black"), #控制x軸粗細與顏色
        axis.text.x = element_text(size=10), #控制x軸內容字體大小
        axis.text.y = element_text(size=10)) + #控制y軸內容字體大小
  ggtitle("財富十等分位人口組成（台灣）")
g3

