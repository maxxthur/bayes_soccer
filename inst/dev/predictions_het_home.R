pacman::p_load("tidyverse", "arrow", "R2jags", "readxl", "writexl")

# countries
country <- c(#germany = "deutschland", 
             #england = "england", 
             spain = "spanien", 
             #france = "frankreich", 
             italy = "italien"
             #netherlands = "niederlande"
             )

divisions <- list(# germany = c("D1", "D2"), 
               # england = "E0", 
               spain = c("SP1", "SP2"), 
                #france = c("F1", "F2"), 
               italy = c("I1", "I2")
               # netherlands = "N1"
               )

# preliminary clean up
file.remove(list.files(paste0("data/", names(country), "/predictions"), recursive = T, full.names = T))

for(i in 1:length(country)) {
  #for(j in divisions[[i]]) {
    
    cat(names(country[i]))

# test wise for current date
load(paste0("data/jags_output/", names(country)[i], "_het_home.RData"))
mcmc <- as.mcmc(fit)

ids <- read_excel(paste0("data/" , names(country[i]), "/mapping/mapping_teams_all.xlsx")) %>% 
  filter(!is.na(value)) %>% 
  group_by(value) %>% 
  mutate(id = min(id)) %>% 
  ungroup() %>%
  select(team = value, id) %>% 
  distinct()

posteriors <- lapply(mcmc, function(chain) {
  as.data.frame(chain[, str_detect(colnames(chain), "att|def|home")]) %>%
    pivot_longer(everything()) %>% 
    mutate(id = str_remove_all(name, "[^0-9]") %>% as.numeric(), 
           name = str_remove_all(name, "[^a-z]")) %>% 
    left_join(ids, by = "id")
}) %>% bind_rows()

# plot to inspect how good teams are
posteriors %>% 
  filter(name %in% c("att", "def", "home") & !is.na(team)) %>% 
  select(name, team, value) %>% 
  group_by(name, team) %>% 
  summarise(mean = mean(value), 
            lower = quantile(value, 0.025), 
            upper = quantile(value, 0.975)) %>% 
  ungroup() %>% 
  ggplot(aes(y = team, color = name)) +
  scale_color_manual(values = c("steelblue", "orange", "darkred")) +
  geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 1.5, alpha = 0.5) + 
  geom_point(aes(x = mean), size = 4) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        axis.text = element_text(size = 20))

hist_ids <- posteriors %>% distinct(id) %>% pull()

tipico <- open_dataset(paste0("data/", names(country[i]), "/tipico/")) %>% 
  select(home, away, retrieved) %>% 
  collect() %>% 
  filter(retrieved == max(retrieved)) %>% 
  distinct() %>% 
  left_join(ids %>% rename(home_id = id), by = c("home" = "team")) %>% 
  left_join(ids %>% rename(away_id = id), by = c("away" = "team")) %>% 
  na.omit() %>% 
  filter(home_id %in% hist_ids & away_id %in% hist_ids)
  
get_results <- function(date, matchup, posteriors, n = 10000) {
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
  
  theta1 <- exp(sample(post_home$home, size = n, replace = T) + sample(post_home$att, size = n, replace = T) + sample(post_away$def, size = n, replace = T))
  theta2 <- exp(sample(post_away$att, size = n, replace = T) + sample(post_home$def, size = n, replace = T))
  
  results <- data.frame(home = rpois(n, theta1), away = rpois(n, theta2))
  
  teams <- c(home = unique(post_home$team), away = unique(post_away$team))
  
  # get expected result
  expected <- results %>% summarise(e_home = median(home), e_away = median(away))
  
  # get probabilities 
  tipp <- results %>% 
    summarise(p_h = mean(home > away), 
              p_d = mean(home == away)) %>% 
    mutate(p_a = 1 - p_h - p_d) %>% 
    cbind(expected) %>% 
    mutate(home = teams[1], 
           away = teams[2])
  
  dc <- tipp %>% 
    transmute(p_hd = p_h + p_d, 
           p_ha = p_h + p_a, 
           p_ad = p_a + p_d) %>% 
    mutate(home = teams[1], 
           away = teams[2])
  
  ou <- data.frame(goals = seq(0.5, 5.5, by = 0.5), 
             p_above= sapply(seq(0.5, 5.5, by = 0.5), function(c) mean((results$home + results$away) > c))) %>%
            mutate(p_below = 1 - p_above) %>% 
    mutate(home = teams[1], 
           away = teams[2])
  
  ou_home <- data.frame(goals = seq(0.5, 5.5, by = 0.5), 
                        p_above= sapply(seq(0.5, 5.5, by = 0.5), function(c) mean(results$home > c))) %>%
    mutate(p_below = 1 - p_above) %>% 
    mutate(home = teams[1], 
           away = teams[2])
  
  ou_away <- data.frame(goals = seq(0.5, 5.5, by = 0.5), 
                        p_above= sapply(seq(0.5, 5.5, by = 0.5), function(c) mean(results$away > c))) %>%
    mutate(p_below = 1 - p_above) %>% 
    mutate(home = teams[1], 
           away = teams[2])
  
  bs <- results %>% 
    summarise(p_yes = mean(home > 0 & away > 0)) %>% 
    mutate(p_no = 1 - p_yes) %>% 
    mutate(home = teams[1], 
           away = teams[2])
  
  list(tipp = tipp, 
       dc = dc, 
       ou = ou, 
       ou_home = ou_home, 
       ou_away = ou_away, 
       bs = bs)
}

predictions <- tipico %>% 
  rowwise() %>% 
  group_split() %>% 
  map(~ get_results(date = Sys.Date(), 
                    matchup = c(.$home_id, .$away_id), 
                    posteriors = posteriors))

folder <- c("tipp", "dc", "ou", "ou_home", "ou_away", "bs")

lapply(folder, function(f) {
  bind_rows(lapply(predictions, function(p) p[[f]])) %>% 
    write_parquet(sink = paste0("data/", names(country[i]), "/predictions/", f, "/", Sys.Date(), ".parquet"))
})

#}
}



