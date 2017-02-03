library(tidyverse)

thresholds <- c(10,100,1000)
blacklist.names <- c("500% BONUS ON TIX")

df.tix <- read.csv("tix.csv") %>% filter(!(Name %in% blacklist.names))
df.tix$Date <- as.Date(parse_datetime(as.character(df.tix$Time)))
df.tix$Full_Price <- df.tix$Price + df.tix$Bounty

df.thresholds <- tibble()

for (i in 1:length(unique(df.tix$Date))) {
  date <- unique(df.tix$Date)[i]
  print(date)
  df.date <- filter(df.tix, Date == date) %>% 
    group_by(UID) %>%
    summarise(Q_Max = min(max(Q_Max),sum(Q)), Full_Price = max(Full_Price)) %>%
    arrange(desc(Full_Price))
  df.date$Cumulative_Max <- cumsum(df.date$Q_Max)
  
  for (val in thresholds) {
    idx <- Position(function(x) x >= val, df.date$Cumulative_Max)
    P <- as.double(df.date[idx,"Full_Price"])
    df.thresholds <- rbind(df.thresholds,
                           tibble(Date=date, Price=P, Threshold=as.factor(val)))
  }
}

price_plot <- ggplot(df.thresholds, aes(x=Date, y=Price, color=Threshold)) +
  geom_line() +
  labs(y="Price",x="Date",color="Marginal Offer for X Tix") +
  ggtitle("MTGO Ticket Prices on PucaTrade") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        strip.text = element_text(size=14)) +
  ylim(100,400)

ggsave("tix.png", price_plot, height = 8, width = 12)
