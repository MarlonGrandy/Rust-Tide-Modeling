impdata <- read_csv(get_path(DATA_DIR, "maxent", "varimpplot.csv"))

impplot <- ggplot(data = impdata) +
  geom_bar(aes(x = Model, y = `wind speed`))
