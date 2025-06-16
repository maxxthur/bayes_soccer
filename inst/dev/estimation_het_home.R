pacman::p_load("R2jags", "tidyverse", "arrow", "readxl")

# countries
country <- c( # germany = "deutschland", 
             # england = "england", 
             spain = "spanien", 
             # france = "frankreich", 
             italy = "italien"
             # netherlands = "niederlande"
             )

for(i in names(country)) {

# load data (last 34 matches)
ds <- open_dataset(paste0("data/parquet/", i, ".parquet")) %>% 
  collect() %>% 
  head(34 * 18)

div <- unique(ds$division) %>% str_subset("E1|E2|E3", negate = T)

#for(j in div) {

# load id mapping
ids <- read_excel(paste0("data/", i, "/mapping/mapping_teams_all.xlsx")) %>% 
  filter(!is.na(value)) %>% 
  group_by(value) %>% 
  mutate(id = min(id)) %>% 
  ungroup() %>%
  select(team = home, id) %>% 
  distinct()

data <- ds %>% 
  left_join(ids %>% select(team, home_id = id), by = c("home" = "team")) %>% 
  left_join(ids %>% select(team, away_id = id), by = c("away" = "team")) %>% 
  na.omit() # %>% 
  # group_by(home) %>% 
  # mutate(n_home = n()) %>% 
  # ungroup() %>% 
  # group_by(away) %>% 
  # mutate(n_away = n()) %>% 
  # ungroup() %>% 
  # mutate(n = n_home + n_away) %>% 
  # filter(n > 20) %>% 
  # distinct() %>% 
  # select(-contains("n"))

# specify model 
ngames <- nrow(data)
nteams <- length(unique(c(data$home, data$away)))

# set up the model
model <- function() {
  # LIKELIHOOD AND RANDOM EFFECT MODEL FOR THE SCORING PROPENSITY 
  for (g in 1:ngames) { 
    # Observed number of goals scored by each team 
    y1[g] ~ dpois(theta[g,1]) 
    y2[g] ~ dpois(theta[g,2]) 
    
    # Predictive distribution for the number of goals scored 
    ynew[g,1] ~ dpois(theta[g,1]) 
    ynew[g,2] ~ dpois(theta[g,2]) 
    
    # Average Scoring intensities (accounting for mixing components) 
    log(theta[g,1]) <- home[hometeam[g]] + att[hometeam[g]] + def[awayteam[g]] 
    log(theta[g,2]) <- att[awayteam[g]] + def[hometeam[g]] 
  } 
  
  # 1. BASIC MODEL FOR THE HYPERPARAMETERS 
  
  # prior on the home effect
  for(t in 1:nteams) {
    home[t] ~ dnorm(mu.home, tau.home)
  }
  # 
  # # Trick to code the ‘‘sum-to-zero’’ constraint 
  # for (t in 1:nteams){ 
  #   att.star[t] ~ dnorm(mu.att,tau.att) 
  #   def.star[t] ~ dnorm(mu.def,tau.def) 
  #   att[t] <- att.star[t] - mean(att.star[]) 
  #   def[t] <- def.star[t] - mean(def.star[]) 
  # } 
  
  # priors on the random effects 
  # mu.att ~ dnorm(0,0.0001) 
  # mu.def ~ dnorm(0,0.0001)
   mu.home ~ dnorm(0,0.0001) 
  # tau.att ~ dgamma(.01,.01) 
  # tau.def ~ dgamma(.01,.01)
   tau.home ~ dgamma(.01,.01)
  # 
  
  # 2. MIXTURE MODEL FOR THE HYPERPARAMETERS 
  
  # Mixture parameters & components (‘‘sum-to-zero’’ constraint) 
  for (t in 1:nteams) { 
    grp.att[t] ~ dcat(p.att[t,]) 
    grp.def[t] ~ dcat(p.def[t,]) 
    att[t] ~ dt(mu.att[grp.att[t]],tau.att[grp.att[t]],4) 
    def[t] ~ dt(mu.def[grp.def[t]],tau.def[grp.def[t]],4) 
    att.star[t] <- att[t] - mean(att[]) 
    def.star[t] <- def[t] - mean(def[]) 
    
    # Priors on the mixture parameter (team specific) 
    p.att[t,1:3] ~ ddirch(prior.att[t,]) 
    p.def[t,1:3] ~ ddirch(prior.def[t,]) 
    
  } 
  
  # Priors on the random effects 
  # group 1: bottom-table teams 
  mu.att[1] ~ dnorm(0,0.001);T(-3, 0)
  mu.def[1] ~ dnorm(0,0.001);T(0, 3)
  tau.att[1] ~ dgamma(0.01,0.01) 
  tau.def[1] ~ dgamma(0.01,0.01) 
  
  # group 2: mid-table teams 
  mu.att[2] <-0 
  mu.def[2] <-0 
  tau.att[2] ~ dgamma(0.01,0.01) 
  tau.def[2] ~ dgamma(0.01,0.01) 
  
  # group 3: top-table teams 
  mu.att[3] ~ dnorm(0,0.001);T(0, 3) 
  mu.def[3] ~ dnorm(0,0.001);T(-3, 0)
  tau.att[3] ~ dgamma(0.01,0.01)
  tau.def[3] ~ dgamma(0.01,0.01)
}

# create data structure for jags
jagsdata <- with(data, list(hometeam = home_id, awayteam = away_id, 
                            y1 = home_goals, y2 = away_goals, ngames = ngames, 
                            nteams = nteams, prior.def = matrix(rep(1/3, 3 * nteams), ncol = 3), 
                            prior.att = matrix(rep(1/3, 3 * nteams), ncol = 3)))
# specify which parameters are recorded
params <- c("att", "def", "home", "grp.att", "grp.def")

fit <- jags(data = jagsdata, parameters.to.save = params, model.file = model,
            n.chains = 3, n.iter = 20000, n.burnin = 5000, n.thin = 10, DIC = F)

# store results 
save(fit, file = paste0("data/jags_output/", i,  "_het_home.RData"))
}
#}

