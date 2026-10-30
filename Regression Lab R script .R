# label: setup
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(performance)
library(gt)

# Load the attitude dataset into  workspace
data(attitude)
dat <- attitude

#descriptive-table
head(dat)
n <- nrow(dat)

descriptives <- dat |>
  summarise(
    across(
      c(rating, complaints, learning,raises),
      list(mean = mean, sd = sd, min = min, max = max)
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "_"
  )

descriptives |>
  gt() |>
  fmt_number(columns = where(is.numeric), decimals = 2)


descriptives |>
  gt() |>
  fmt_number(columns = where(is.numeric), decimals = 2)|>
               tab_header(
                 title = "Descriptive Statistics for Selected Attitude Variables") 
                 
#histogram of rating
ggplot(dat, aes(x = rating)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 15,
    fill = "#669BBC",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(
    linewidth = 0.9,
    color = "#9A3038"
  ) +
  labs(
    title = "Distribution of rating scores",
    x = "Rating score",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)


# complaints-scatterplot
ggplot(dat, aes(x = complaints, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A"
  ) +
  labs(
    title = "Relationship between rating and complaints",
    x = "Complaints",
    y = "Rating Score"
  ) +
  theme_minimal(base_size = 12)

# raises-scatterplot
ggplot(dat, aes(x = raises, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A"
  ) +
  labs(
    title = "Relationship between rating and raises",
    x = "Raises",
    y = "Rating Score"
  ) +
  theme_minimal(base_size = 12)


#Specify and estimate competing models
# Intercept-only model
mod0 <- lm(rating ~ 1, data = dat)

# Complaints model
mod1 <- lm(rating ~ complaints, data = dat)

# Complaints + learning model
mod2 <- lm(rating ~ complaints + learning, data = dat)

# Complaints+ learning + raises model
mod3 <- lm(rating ~ complaints + learning + raises, data = dat)

summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)

confint(mod3)

#nested model comparisons
tests <- anova(mod0, mod1, mod2, mod3)

tests

#select and rename
tests <- tests |>
  select(Res.Df, Df, F, `Pr(>F)`) |>
  rename(
    Residual.df = Res.Df,
    Nested.Df = Df,
    Nested.F.stat = F,
    Nested.p.value = `Pr(>F)`
  )

#extract overall model performance
glance0 <- glance(mod0)
glance1 <- glance(mod1)
glance2 <- glance(mod2)
glance3 <- glance(mod3)

mods <- c(
  "M0: Null",
  "M1: Complaints",
  "M2: + Learning",
  "M3: + Raises"
)

glances <- rbind(
  glance0,
  glance1,
  glance2,
  glance3
)

#Combine nested tests and model performance
comparison_table <- cbind(
  Model = mods,
  glances,
  tests
)

comparison_table <- comparison_table |>
  mutate(
    Delta.R2 = r.squared - lag(r.squared),
    
    RMSE = c(
      sqrt(mean(residuals(mod0)^2)),
      sqrt(mean(residuals(mod1)^2)),
      sqrt(mean(residuals(mod2)^2)),
      sqrt(mean(residuals(mod3)^2))
    )
  ) |>
  select(
    Model,
    r.squared,
    Delta.R2,
    adj.r.squared,
    RMSE,
    Residual.df,
    Nested.Df,
    Nested.F.stat,
    Nested.p.value
  )


#Display the combined model-comparison table
comparison_table |>
  gt() |>
  cols_label(
    Model = "Model",
    r.squared = "R²",
    Delta.R2 = "ΔR²",
    adj.r.squared = "Adjusted R²",
    RMSE = "RMSE",
    Residual.df = "Residual df",
    Nested.Df = "Nested df",
    Nested.F.stat = "Nested F",
    Nested.p.value = "Nested p"
  ) |>
  fmt_number(
    columns = c(
      r.squared,
      Delta.R2,
      adj.r.squared,
      RMSE,
      Nested.F.stat
    ),
    decimals = 3
  ) |>
  fmt(
    columns = Nested.p.value,
    fns = function(x) {
      ifelse(
        is.na(x),
        "—",
        ifelse(x < .001, "< .001", sprintf("%.3f", x))
      )
    }
  ) |>
  sub_missing(
    columns = c(Delta.R2, Nested.Df, Nested.F.stat),
    missing_text = "—"
  )

performance_comparison <- compare_performance(
  mod0,
  mod1,
  mod2,
  mod3,
  rank = TRUE
)

performance_comparison
plot(performance_comparison)

#selecting a provisional model
selected_row <- comparison_table |>
  slice_max(
    order_by = adj.r.squared,
    n = 1,
    with_ties = FALSE
  )

selected_name <- selected_row$Model

selected_model <- switch(
  selected_name,
  "M0: Null" = mod0,
  "M1: Complaints" = mod1,
  "M2: + Learning" = mod2,
  "M3: + Raises" = mod3
)

#Examine the assumptions/ regression diagnoatics
op <- par(mfrow = c(2, 2))

plot(selected_model)

par(op)

check_model(selected_model)

check_collinearity(selected_model)

#Report the selected model
coefficient_results <- tidy(
  selected_model,
  conf.int = TRUE,
  conf.level = 0.95
)

coefficient_results |>
  select(
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  ) |>
  gt() |>
  cols_label(
    term = "Predictor",
    estimate = "Estimate",
    std.error = "SE",
    conf.low = "95% CI lower",
    conf.high = "95% CI upper",
    p.value = "p"
  ) |>
  fmt_number(
    columns = c(
      estimate,
      std.error,
      conf.low,
      conf.high
    ),
    decimals = 3
  ) |>
  fmt(
    columns = p.value,
    fns = function(x) {
      ifelse(
        x < .001,
        "< .001",
        sprintf("%.3f", x)
      )
    }
  )

#Report overall model performance
selected_row |>
  select(
    Model,
    r.squared,
    adj.r.squared,
    RMSE
  ) |>
  gt() |>
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adjusted R²",
    RMSE = "RMSE"
  ) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  )
