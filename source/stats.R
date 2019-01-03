library(dplyr)
library(ggplot2)
library(ggthemes)
players = read.csv("data/players.csv")
players <- players %>% filter(teamid != 'TOT')
players_agg = read.csv("data/players_agg.csv")
players<-inner_join(players, players_agg)
# Raw BPM = a*ReMPG + b*ORB% + c*DRB% + d*STL% + e*BLK% + f*AST% - g*USG%*TO% + 
#   h*USG%*(1-TO%)*[2*(TS% - TmTS%) + i*AST% + j*(3PAr - Lg3PAr) - k] + l*sqrt(AST%*TRB%)
stats_players <- players %>% group_by(year, teamid) %>% mutate(
  Tm_ts_pct = (sum(fg2*2)+sum(fg3*3)+sum(ft))/(2*((sum(fg2a)+sum(fg3a)) + (0.44*sum(fta))))
) %>% ungroup() %>% group_by(year) %>% mutate(
  Lg3PAr = sum(fg3a)/(sum(fg3a)+sum(fg2a))
) %>% ungroup() %>% mutate(
  ReMPG = (mp/g)/48,
  raw_bpm = 0.123391*ReMPG + 0.119597*orb_pct + (-0.151287*drb_pct) + 1.255644*stl_pct + 0.531838*blk_pct + (-0.305868*ast_pct) + -1*0.921292*usg_pct*(tov_pct/100) + 0.711217*usg_pct*(1-(tov_pct/100))*(2*(ts_pct - Tm_ts_pct) + 0.017022*ast_pct + 0.297639*(fg3a_per_fga_pct - Lg3PAr) - 0.213485) + 0.725930*sqrt(ast_pct*trb_pct)
) %>% ungroup() 

load("data/replacement.RData")
stats_players <- inner_join(stats_players, replacment_level)
max_games = stats_players %>% group_by(year) %>% summarise(max_g = max(g, na.rm = T))
stats_players <- inner_join(stats_players, max_games)
stats_players<-stats_players %>% group_by(year, teamid) %>% mutate(
  bpm_tm_adj = ((weighted.mean(raw_bpm, mp, na.rm = T)*1.20) - raw_bpm)/5,
  mp_pct = (mp/(max_g*48)),
  adj_vorp = ((raw_bpm+bpm_tm_adj) - RL_bpm) * (mp_pct) * (g/82), 
  adj_war = round(adj_vorp *2.7, digits = 3),
  yearid = year
) %>% ungroup() %>% group_by(playerID) %>% mutate(
  N = row_number()
)
stats_players<-stats_players %>% select(playerName, playerID, teamid, year, age, N, g, pos, mp, mp_pct, bpm_tm_adj, bpm, vorp, adj_vorp, adj_war, RL_bpm, raw_bpm, ReMPG, orb_pct, drb_pct, trb_pct, stl_pct, blk_pct, ast_pct, usg_pct, tov_pct, ts_pct, fg3a_per_fga_pct)
