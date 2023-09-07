# install.packages("nflfastR")
# install.packages("nflreadr")
#install.packages("remotes")
#remotes::install_github("FantasyFootballAnalytics/ffanalytics")

library(ffanalytics)
library(dplyr)
library(nflfastR)
library(nflreadr)
library(tidyverse)

## Weekly Data
nfl_data = load_player_stats(seasons = c(2018:2022),
                             stat_type = c("offense")) %>%
  select(player_display_name, position, season_type, season, completions, attempts, passing_yards,
         passing_tds, interceptions, sack_fumbles_lost,passing_2pt_conversions, carries, rushing_yards, 
         rushing_tds, rushing_fumbles_lost,rushing_2pt_conversions, receptions, targets, receiving_yards,
         receiving_tds, receiving_fumbles_lost, receiving_2pt_conversions, target_share, special_teams_tds, recent_team) %>%
  filter(season_type == "REG") %>%
  select(-season_type) %>%
  group_by(player_display_name, position, season, recent_team) %>%
  mutate_at(vars(-c(player_display_name, position, season, recent_team)), function(x) sum(x)) %>%
  distinct() %>%
  mutate(FantasyPoints = passing_yards*0.04 + passing_tds*4 + interceptions*(-2) + rushing_yards*0.1 + 
           rushing_tds*6 + receptions*0.5 + receiving_yards*0.1 + receiving_tds*6 + sack_fumbles_lost*(-2) + 
           rushing_fumbles_lost*(-2) + receiving_fumbles_lost*(-2) +passing_2pt_conversions*2 + 
           rushing_2pt_conversions*2 + receiving_2pt_conversions*2 + special_teams_tds*6,
         yds_per_rec = receiving_yards/receptions,
         td_per_rec = receiving_tds/receptions,
         yds_per_carry = rushing_yards/carries,
         td_per_carry = rushing_tds/carries,
         rec_percentage = receptions/targets,
         pass_cmp_percentage = completions/attempts,
         touches = carries + receptions) %>%
  mutate_all(~ ifelse(is.na(.) | is.nan(.), 0, .))

## Snap Counts
snap_counts = load_snap_counts(seasons = c(2018:2022)) %>%
  select(player, season,  position, offense_snaps, offense_pct, game_type, team) %>%
  filter(game_type == "REG") %>%
  select(-game_type) %>%
  filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  group_by(player, season, position, team) %>%
  summarise(total_snaps = sum(offense_snaps),
            games_played = n())

exp_points = load_ff_opportunity(seasons = c(2018:2022)) %>%
  select(season, posteam, full_name, position, pass_yards_gained_exp, rec_yards_gained_exp,
         rush_yards_gained_exp, pass_touchdown_exp, receptions_exp, rec_touchdown_exp, rush_touchdown_exp,
         pass_two_point_conv_exp, rec_two_point_conv_exp, rush_two_point_conv_exp,pass_interception_exp) %>%
  subset(., full_name != "NA") %>%
  mutate(FantasyPoints_exp = pass_yards_gained_exp*0.04 + pass_touchdown_exp*4 + pass_interception_exp*(-2) + 
           rush_yards_gained_exp*0.1 + rush_touchdown_exp*6 + receptions_exp*0.5 + rec_yards_gained_exp*0.1 + 
           rec_touchdown_exp*6 + pass_two_point_conv_exp*2 + rec_two_point_conv_exp*2 + rush_two_point_conv_exp*2) %>%
  select(season, full_name, position, posteam, FantasyPoints_exp) %>%
  group_by(season, full_name, position, posteam) %>%
  summarise(FantasyPoints_exp = sum(FantasyPoints_exp))

rankings_curr_season = load_ff_rankings(type = c("draft")) %>%
  filter(ecr_type == "ro" & pos %in% c("QB", "RB", "WR", "TE")) %>%
  select(player, pos, team, ecr)

## Renaming the columns to maintain a certain pattern
nfl_data = nfl_data %>%
  rename(player = player_display_name,
         team = recent_team) %>%
  left_join(.,snap_counts, by = c("player","season","team","position"))


exp_points = exp_points %>%
  rename(player = full_name,
         team = posteam) %>%
  mutate(season = as.numeric(season))

rankings_curr_season = rankings_curr_season %>%
  rename(position = pos)


## Joining all the data
full_data = nfl_data %>%
  left_join(., exp_points, by = c("player","season","team","position")) #%>%
  #left_join(., rankings_curr_season, by = c("player","season","team","position"))


## Season projections
projections = ffanalytics::scrape_ecr(rank_period = c("draft"), position = c("QB"), rank_type = c("Half"))

project_data = scrape_data(src = c("CBS","ESPN","NFL","FantasyPros"), pos = c("QB","RB","WR","TE"), week = 0)

## QB 
qb = project_data[["QB"]] %>%
  mutate(player = (gsub("[.'\"]", "", player))) %>%
  mutate_all(~ ifelse(is.na(.) | is.nan(.), 0, .)) %>%
  mutate(player = substr(player,1,12)) %>%
  mutate(site_pts = pass_yds*0.04 + pass_tds*6 + pass_int*(-2) + rush_yds*0.1 + rush_tds*6 + fumbles_lost*(-2)) %>%
  select(player, pos, site_pts, data_src) %>%
  pivot_wider(id_cols = c(player, pos), names_from = data_src, values_from = site_pts) %>%
  na.omit()

rb = project_data[["RB"]] %>%
  mutate(player = (gsub("[.'\"]", "", player))) %>%
  mutate_all(~ ifelse(is.na(.) | is.nan(.), 0, .)) %>%
  mutate(player = substr(player,1,12)) %>%
  mutate(site_pts = rush_yds*0.1 + rush_tds*6 + fumbles_lost*(-2) + rec*0.5 + rec_yds*0.1 + rec_tds*6) %>%
  select(player, pos, site_pts, data_src) %>%
  pivot_wider(id_cols = c(player, pos), names_from = data_src, values_from = site_pts) %>%
  na.omit()


wr = project_data[["WR"]] %>%
  mutate(player = (gsub("[.'\"]", "", player))) %>%
  mutate_all(~ ifelse(is.na(.) | is.nan(.), 0, .)) %>%
  mutate(player = substr(player,1,12)) %>%
  mutate(site_pts = rush_yds*0.1 + rush_tds*6 + fumbles_lost*(-2) + rec*0.5 + rec_yds*0.1 + rec_tds*6) %>%
  select(player, pos, site_pts, data_src) %>%
  pivot_wider(id_cols = c(player, pos), names_from = data_src, values_from = site_pts) %>% na.omit()

te = project_data[["TE"]] %>%
  mutate_all(~ ifelse(is.na(.) | is.nan(.), 0, .)) %>%
  mutate(player = (gsub("[.'\"]", "", player))) %>%
  mutate(player = substr(player,1,12)) %>%
  mutate(site_pts = rush_yds*0.1 + rush_tds*6 + fumbles_lost*(-2) + rec*0.5 + rec_yds*0.1 + rec_tds*6) %>%
  select(player, pos, site_pts, data_src) %>%
  pivot_wider(id_cols = c(player, pos), names_from = data_src, values_from = site_pts) %>% na.omit()

all_projections = rbind(qb,rb,te,wr) %>% rename(position = pos)

# Add the diff between projected points and actual points in the last season (0 for rookies)
diff_exp_actual = full_data %>%
  filter(season == 2022) %>%
  mutate(proj_diff = FantasyPoints - FantasyPoints_exp) %>%
  select(player, position, proj_diff) %>%
  mutate(player = substr(player,1,12))

# Players ids
qb_id = project_data[["QB"]] %>% 
  filter(data_src == "CBS") %>%
  select(player, id)

rb_id = project_data[["RB"]] %>% 
  filter(data_src == "CBS") %>%
  select(player, id)

wr_id = project_data[["WR"]] %>% 
  filter(data_src == "CBS") %>%
  select(player, id)

te_id = project_data[["TE"]] %>% 
  filter(data_src == "CBS") %>%
  select(player, id)

players_ids = rbind(qb_id, rb_id, wr_id, te_id) %>%
  mutate(player = (gsub("[.'\"]", "", player))) %>%
  #mutate_all(~ ifelse(is.na(.) | is.nan(.), 0, .)) %>%
  mutate(player = substr(player,1,12))

## ADP
adp = get_adp(sources = c("CBS","NFL"), metric = "adp") %>%
  left_join(players_ids, by = "id") %>%
  select(player, adp_avg)

## Rankings 2023 season
rankings_curr_season = rankings_curr_season %>%
  mutate(player = (gsub("[.'\"]", "", player))) %>%
  mutate(player = substr(player,1,12)) %>% select(-team)

## Joining datasets
nfl_data_final = left_join(all_projections, diff_exp_actual, by = c("player","position")) %>%
  select(-c(season, team)) %>% mutate(proj_diff = replace_na(proj_diff,0)) %>% 
  group_by(player, position, CBS, ESPN, NFL, FantasyPros) %>% 
  summarise(proj_diff = sum(proj_diff)) %>%
  left_join(rankings_curr_season) %>%
  mutate(avg_projections = (CBS+ ESPN+NFL+FantasyPros)/4)


  
## Exporting
season_2023 = full_data %>%
  filter(player %in% all_projections$player)

write.csv2(season_2023, file = "C:/Users/gabri/OneDrive/Documentos/fantasy_data.csv", row.names = F)
write.csv2(nfl_data_final, file = "C:/Users/gabri/OneDrive/Documentos/fantasy_season_2023.csv", row.names = F)

