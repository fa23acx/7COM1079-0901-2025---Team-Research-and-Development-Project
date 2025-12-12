library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)

input_csv <- "audi.csv"
output_dir <- "outputs"
if (!dir.exists(output_dir)) dir.create(output_dir)

log_file <- file.path(output_dir, "R_analysis_log.txt")

log_append <- function(text) {
  cat(paste0(text, "\n"), file = log_file, append = TRUE)
}

cat("R analysis log\n", file = log_file)
log_append(paste("Started at:", Sys.time()))

# --- Load data -------------------------------------------------------------
raw <- read_csv(input_csv, show_col_types = FALSE)
log_append(paste("Rows read:", nrow(raw)))

glimpse(raw)
summary(raw)

# --- Data cleaning ---------------------------------------------------------
raw <- raw %>% mutate(across(where(is.character), trimws))
names(raw) <- tolower(names(raw))

raw <- raw %>%
  mutate(
    year = as.integer(year),
    price = as.numeric(price),
    mileage = as.numeric(mileage),
    transmission = as.factor(transmission),
    fueltype = as.factor(fueltype),
    tax = as.numeric(tax),
    mpg = as.numeric(mpg),
    enginesize = as.numeric(enginesize)
  )

clean <- raw %>% filter(!is.na(price) & !is.na(mileage)) %>% distinct()

log_append(paste("Final cleaned rows:", nrow(clean)))
summary(clean)

# --- Summary statistics ----------------------------------------------------
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


# --- Scatter plot ----------------------------------------------------------
scatter_plot <- ggplot(clean, aes(x = mileage, y = price)) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Scatter plot: Mileage vs Price (Audi)",
    x = "Mileage (miles)", y = "Price (GBP)"
  ) +
  theme_minimal()

# Show in RStudio
print(scatter_plot)

# Save to file
ggsave(file.path(output_dir, "scatter_mileage_price.png"),
       plot = scatter_plot, width = 8, height = 6, dpi = 300)
log_append("Saved scatter plot")


# --- Histogram: Mileage ----------------------------------------------------
m_mean <- mean(clean$mileage)
m_sd <- sd(clean$mileage)

hist_mileage <- ggplot(clean, aes(x = mileage)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.6) +
  geom_density(size = 1) +
  stat_function(fun = dnorm, args = list(mean = m_mean, sd = m_sd),
                size = 1, linetype = "dashed") +
  labs(
    title = "Histogram of Mileage with Density + Normal Curve",
    x = "Mileage (miles)", y = "Density"
  ) +
  theme_minimal()

print(hist_mileage)

ggsave(file.path(output_dir, "hist_mileage.png"),
       plot = hist_mileage, width = 8, height = 6, dpi = 300)
log_append("Saved mileage histogram")


# --- Histogram: Price ------------------------------------------------------
p_mean <- mean(clean$price)
p_sd <- sd(clean$price)

hist_price <- ggplot(clean, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.6) +
  geom_density(size = 1) +
  stat_function(fun = dnorm, args = list(mean = p_mean, sd = p_sd),
                size = 1, linetype = "dashed") +
  labs(
    title = "Histogram of Price with Density + Normal Curve",
    x = "Price (GBP)", y = "Density"
  ) +
  theme_minimal()

print(hist_price)

ggsave(file.path(output_dir, "hist_price.png"),
       plot = hist_price, width = 8, height = 6, dpi = 300)
log_append("Saved price histogram")


# --- Normality tests --------------------------------------------------------
set.seed(123)
n_shapiro <- min(5000, nrow(clean))
samp <- clean %>% sample_n(n_shapiro)

shap_m <- shapiro.test(samp$mileage)
shap_p <- shapiro.test(samp$price)

print(shap_m)
print(shap_p)

log_append(paste("Shapiro mileage p:", shap_m$p.value))
log_append(paste("Shapiro price p:", shap_p$p.value))


# --- Correlation tests ------------------------------------------------------
pearson_res <- cor.test(clean$mileage, clean$price, method = "pearson")
spearman_res <- cor.test(clean$mileage, clean$price, method = "spearman")

print(pearson_res)
print(spearman_res)

writeLines(capture.output(pearson_res),
           file.path(output_dir, "pearson_results.txt"))
writeLines(capture.output(spearman_res),
           file.path(output_dir, "spearman_results.txt"))

results_df <- tibble(
  test = c("pearson", "spearman"),
  estimate = c(pearson_res$estimate, spearman_res$estimate),
  p_value = c(pearson_res$p.value, spearman_res$p.value),
  conf_low = c(pearson_res$conf.int[1], NA),
  conf_high = c(pearson_res$conf.int[2], NA)
)

write_csv(results_df, file.path(output_dir, "correlation_summary.csv"))
log_append("Saved correlation summary")

log_append(paste("Pearson r:", round(pearson_res$estimate, 4)))
log_append(paste("Spearman rho:", round(spearman_res$estimate, 4)))
log_append(paste("Finished at:", Sys.time()))
