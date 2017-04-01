library(tidyverse)

thresholds <- c(10,100)
blacklist.names <- c("500% BONUS ON TIX")
blacklist.dates <- c("2017-02-10","2017-02-11")

df.tix <- read.csv("output.csv", stringsAsFactors = FALSE)
df.tix$Date <- as.Date(parse_datetime(as.character(df.tix$time)))
df.tix$Full_Price <- ifelse(as.logical(df.tix$promoted), as.integer(df.tix$price), 100 + as.integer(df.tix$bounty))
df.tix$balance <- as.integer(gsub(",","",df.tix$balance))

df.tix <-  df.tix %>% filter(!(user %in% blacklist.names | as.character(Date) %in% blacklist.dates) & promoted == 'True')

df.thresholds <- tibble()

# No need to get fancy here with the combinatorics of what a balance can pay for. Not worth it.
for (i in 1:length(unique(df.tix$Date))) {
  date <- unique(df.tix$Date)[i]
  df.date <- filter(df.tix, Date == date & balance >= Full_Price) %>% 
    arrange(desc(Full_Price))
  
  for (val in thresholds) {
    P <- as.double(df.date[val,"Full_Price"])
    df.thresholds <- rbind(df.thresholds,
                           tibble(Date=date, Price=P, Threshold=as.factor(val)))
  }
}

price_plot <- ggplot(df.thresholds, aes(x=Date, y=Price, color=Threshold)) +
  geom_point() +
  geom_smooth() +
  labs(y="Price",x="Date",color="Marginal Offer for X Tix") +
  ggtitle("MTGO Ticket Prices on PucaTrade") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        strip.text = element_text(size=14)) +
  xlim(as.Date("2017-02-01"),as.Date("2017-03-31")) +
  ylim(200,300) +
  geom_vline(xintercept=as.numeric(as.Date("2017-02-02")))

ggsave("tix.png", price_plot, height = 8, width = 12)
