library(tidyverse)

thresholds <- c(10,100)
blacklist.names <- c("500% BONUS ON TIX")
blacklist.dates <- c("2017-02-10","2017-02-11")
date.min <- c(as.Date("2017-12-01"))

df.tix <- read.csv("output.csv", stringsAsFactors = FALSE)
df.tix$Date <- as.Date(parse_datetime(as.character(df.tix$time)))
df.tix$Full_Price <- ifelse(as.logical(df.tix$promoted), as.integer(df.tix$price), 100 + as.integer(df.tix$bounty))
df.tix$balance <- as.integer(gsub(",","",df.tix$balance))

df.tix <-  df.tix %>% 
  filter(!(user %in% blacklist.names | as.character(Date) %in% blacklist.dates) & promoted == 'True') %>% 
  filter(Date >= date.min)

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

df.th.flat <- spread(df.thresholds, Threshold, Price)
colnames(df.th.flat) <- c("Date","T10","T100")
df.th.flat <- mutate(df.th.flat, TDiff = T10-T100, PDiff10 = T10-lag(T10), PDiff100 = T100-lag(T100))
df.lagged <- gather(df.th.flat[,c("TDiff","PDiff10","PDiff100")], Q, PDiff, -TDiff)

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
  xlim(date.min,as.Date(max(df.date$Date))) +
  ylim(200, 500)

ggsave("tix.png", price_plot, height = 8, width = 12)

# Users analysis
date.min.senders <- c(as.Date("2018-04-01"))
date.max.senders <- c(as.Date("2018-05-05"))

df.traders <- read.csv("Site Comparison - Pucatrade.csv", stringsAsFactors = FALSE)

df.traders$Date <- as.Date(parse_datetime(as.character(df.traders$Date), format = "%m/%d/%Y"))

senders_plot <- ggplot(df.traders, aes(x=Date, y=PT.Senders)) +
  geom_point() +
  geom_smooth() +
  labs(y="Unique Senders",x="Date") +
  ggtitle("Unique PucaTrade Senders Per Day") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        strip.text = element_text(size=14)) +
  xlim(as.Date(date.min.senders),as.Date(date.max.senders)) +
  ylim(0, 150)

ggsave("senders.png", senders_plot, height = 8, width = 12)

'
movement_plot <- ggplot(df.lagged, aes(x=TDiff, y=PDiff, color=Q)) +
  geom_point() +
  #geom_smooth() +
  labs(x="T10-T100",y="Next Day Price Change",color="Type") +
  ggtitle("Effects of T10/T100 Spread on Price Movement") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),
        legend.title = element_text(size=20),
        legend.text = element_text(size=10),
        strip.text = element_text(size=14)) +
  xlim(0, 60) +
  ylim(-40,40)

ggsave("movement.png", movement_plot, height = 8, width = 12)
'
