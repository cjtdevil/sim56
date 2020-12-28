library(tidyverse);library(ggplot2);library(ggbeeswarm)

set.seed(1995)

# From Evolving-Hockey 2019-20 RAPMs
df <- data.frame(
  team = c('BOS','PHI','WSH','PIT','NYR','NYI','BUF','NJD'),
  gf_imp = c(-.031,.217,.435,.165,.23,-.26,-.252,-.393),
  ga_imp = c(-.695,-.137,.133,-.029,.106,-.236,-.079,.352)
)

# FUNCTION: Simulate Season
sim_season = function(n,df){
  
  lg_avg = 2.47
  
  # Generate schedule and simulate poisson scoring
  game_grid <- expand.grid(home=df$team,away=df$team) %>%
    filter(home != away) %>%
    slice(rep(1:n(), each = n)) %>%
    left_join(df %>% `colnames<-`(c('home','hgf_imp','hga_imp')),by='home') %>%
    left_join(df %>% `colnames<-`(c('away','agf_imp','aga_imp')),by='away') %>%
    mutate(hg_lam = lg_avg + hgf_imp + aga_imp,
           ag_lam = lg_avg + agf_imp + hgf_imp) %>% rowwise() %>%
    mutate(hgf = rpois(1,hg_lam),
           agf = rpois(1,ag_lam)) %>% ungroup 
  
  # Repeat for home/away and allocate points
  d1 = game_grid %>% select(home,away,hgf,agf) %>%
    mutate(points = ifelse(hgf>agf,2,ifelse(hgf==agf,1,0)))
  game_grid %>% select(home=away, away=home, hgf=agf, agf=hgf)%>%
    mutate(points = ifelse(hgf>agf,2,ifelse(hgf==agf,1,0))) %>%
    bind_rows(d1) %>% group_by(home) %>%
    summarise(gp = n(),
              pts = sum(points)) %>%
    select(team = home,gp,pts) %>%
    mutate(pts = pts/(2*gp))
  
}

# SIMULATION: Seasons of two lengths

# Simulate 56-game season (Each team/venue pairing 4 times)
res4 = list()
for(i in 1:100){
  res4[[i]] = sim_season(4,df) %>%
    mutate(season_no = i)
}
res4df = bind_rows(res4) %>%
  group_by(season_no) %>%
  mutate(rk = rank(-pts,ties.method = 'random'),
         playoffs = ifelse(rk<5,1,0)) 

# Simulate 84-game season (Each team/venue pairing 6 times)
res6 = list()
for(i in 1:100){
  res6[[i]] = sim_season(6,df) %>%
    mutate(season_no = i)
}
res6df = bind_rows(res6)%>%
  group_by(season_no) %>%
  mutate(rk = rank(-pts,ties.method = 'random'),
         playoffs = ifelse(rk<5,1,0)) 

# TABLE: Playoff results
res4df %>%
  ungroup %>% group_by(team) %>%
  summarise(playoffs = sum(playoffs))

res6df %>%
  ungroup %>% group_by(team) %>%
  summarise(playoffs = sum(playoffs))

# GRAPH: Point totals and playoff results
res4df %>%
  bind_rows(res6df) %>%
  mutate(gp = ifelse(gp==56,"56-game Season","84-game Season"),
         playoffs = ifelse(playoffs==1,"Made Playoffs","Miss Playoffs")) %>%
  ggplot(aes(x=team,y=pts,color=playoffs)) +
  facet_grid(~gp) +
  geom_beeswarm() +
  ylab("Points") + xlab("Team") +
  theme(legend.title = element_blank(),
        legend.position=c(.5,.1))




