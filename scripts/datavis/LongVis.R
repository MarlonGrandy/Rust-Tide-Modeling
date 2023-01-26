dat <- count_env 
dat$Season <- factor(quarters(dat$Date), levels = c("Q1", "Q2", "Q3", "Q4"), 
                     labels = c("winter", "spring", "summer", "fall"))


ggplot(dat,
       aes(x = Date, y = count)) +
  geom_line(color = "blue") +
  labs( y = "Abundance (Cells/L)") +
  scale_fill_binned(type = "viridis") + 
  theme(plot.title = element_text(size = 20)) +
  theme( plot.title = element_text(hjust = .5)) +
  theme(axis.title = element_text(size = 25), axis.text =element_text(size = 20))

