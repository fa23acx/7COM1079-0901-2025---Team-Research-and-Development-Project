 summary_stats <- clean %>%
  summarise(
    n = n(),
    price_mean = mean(price), price_median = median(price), price_sd = sd(price),
    mileage_mean = mean(mileage), mileage_median = median(mileage), mileage_sd = sd(mileage),
    min_price = min(price), max_price = max(price),
    min_mileage = min(mileage), max_mileage = max(mileage)
  )

print(summary_stats)
write_csv(summary_stats, file.path(output_dir, "summary_stats.csv"))
log_append("Summary statistics saved")
