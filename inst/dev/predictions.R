pacman::p_load("tidyverse", "arrow", "R2jags")

# test wise for current date
load("data/jags_output/germany.RData")
mcmc <- as.mcmc(fit)

posteriors <- lapply(mcmc, function(chain) {
  as.data.frame(chain[, str_detect(colnames(chain), "att|def|home")]) %>%
    pivot_longer(everything()) %>% 
    mutate(id = str_remove_all(name, "[^0-9]") %>% as.numeric(), 
           name = str_remove_all(name, "[^a-z]")) %>% 
    left_join(ids, by = "id")
}) %>% bind_rows()

# plot to inspect how good teams are
posteriors %>% 
  select(name, team, value) %>% 
  group_by(name, team) %>% 
  summarise(mean = mean(value), 
            lower = quantile(value, 0.025), 
            upper = quantile(value, 0.975)) %>% 
  ggplot(aes(y = team, color = name)) +
  scale_color_manual(values = c("steelblue", "orange")) +
  geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1.5, alpha = 0.5) + 
  geom_point(aes(x = mean), size = 4) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        axis.text = element_text(size = 20))

# get last historic results date
last_match <- open_dataset("data/parquet/germany.parquet") %>% 
  collect() %>% 
  summarise(date = max(date, na.rm = T)) %>% 
  pull(date)

# get open matches
est_id <- readxl::read_excel("data/germany/mapping/historic_names.xlsx")
m_id <- readxl::read_excel("data/germany/mapping/names.xlsx")

open_matches <- open_dataset("data/germany/spieplan/matches.parquet") %>% 
  filter(date > last_match) %>% 
  collect() %>% 
  left_join(m_id %>% select(matches, home_id = id), by = c("home" = "matches")) %>% 
  left_join(m_id %>% select(matches, away_id = id), by = c("away" = "matches"))

get_results <- function(date, matchup, posteriors, n = 1000) {
  # get relevant posteriors
  post_home <- posteriors %>% 
    filter(id == matchup[1]) %>% 
    group_by(name) %>% 
    mutate(iteration = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = name, values_from = value)
  
  post_away <- posteriors %>% 
    filter(id == matchup[2]) %>% 
    group_by(name) %>% 
    mutate(iteration = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = name, values_from = value)
  
  home_adv <- posteriors %>% filter(name == "home") %>% pull(value)
  
  theta1 <- exp(sample(home_adv, size = n, replace = T) + sample(post_home$att, size = n, replace = T) + sample(post_away$def, size = n, replace = T))
  theta2 <- exp(sample(post_away$att, size = n, replace = T) + sample(post_home$def, size = n, replace = T))
  
  results <- data.frame(home = rpois(n, theta1), away = rpois(n, theta2))
  
  # get expected result
  expected <- results %>% summarise(e_home = median(home), e_away = median(away))
  
  # get odds and probabilities 
  results %>% 
    summarise(p_h = mean(home > away), 
              p_d = mean(home == away)) %>% 
    mutate(p_a = 1 - p_h - p_d, 
           o_h = 1 / p_h, 
           o_d = 1 / p_d, 
           o_a = 1 / p_a) %>% 
    cbind(expected, data.frame(home = distinct(post_home, team)$team, away = distinct(post_away, team)$team), date)
}

predictions <- open_matches %>% 
  rowwise() %>% 
  group_split() %>% 
  map(~ get_results(date = .$date, 
                    matchup = c(.$home_id, .$away_id), 
                    posteriors = posteriors)) %>% 
  bind_rows() %>% 
  arrange(date)

predictions %>% view









