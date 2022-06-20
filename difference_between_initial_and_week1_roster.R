library(tidyverse)
library(nflverse)
library(gt)
library(gtExtras)

# dataset ---------------------------------------------------------------------

# initial_53 roster data ----------

init_data_raw <- read_csv("https://raw.githubusercontent.com/ajreinhard/NFL-public/main/misc-data/2013_to_2021_init53.csv")

init_data <- init_data_raw |> 
#  filter(team == my_team) |> 
  mutate(position_pff = case_when(position_pff == "HB" ~ "RB", 
                                  TRUE ~ position_pff)) |> 
  mutate(position_pff = factor(position_pff, c(
    "QB", "WR", "TE", "RB", "FB", "T", "G", "C", 
    "ED", "DI", "LB", "S", "CB", "K", "P", "LS")
  )
  ) |> 
  mutate(side = case_when(position_pff %in% c("QB", "WR", "TE", "RB", "FB", "T", "G", "C") ~ "Offense", 
                          position_pff %in% c("ED", "DI", "LB", "S", "CB") ~ "Defense", 
                          TRUE ~ "Specialists"))

# week_1 roster data ----------

wk_data <- load_depth_charts(seasons = 2013:2021)

wk_df <- wk_data |> 
  filter(week == 1, season_type == "REG") |> 
  select(season, week, team, position, full_name, jersey_number, gsis_id) |> 
  distinct() |> 
  identity()

counted <- wk_df |> 
  count(season)

# join datasets -----------

diff_df <- anti_join(wk_df, init_data, by = c("season", "gsis_id")) |> 
  mutate(team = clean_team_abbrs(as.character(team))) |> 
  count(team, season, name = "diff")

nfl_avg <- mean(diff_df$diff)

# ggplot -----------------------------------------------------------------------

max_diff <- max(diff_df$diff)

plt <- ggplot(diff_df, aes(x = season, y = diff)) +
  geom_hline(yintercept = nfl_avg, color = "orange", linetype = "dashed") +
  geom_point(aes(color = team)) +
  scale_color_nfl(type = "primary") + 
  facet_wrap(vars(team), nrow = 4, ncol = 8) +
  scale_y_continuous("diff", 
                     labels = 1:max_diff, 
                     breaks = 1:max_diff, 
                     expand = expansion(mult = c(.1, .1))) +
  scale_x_continuous("season", 
                     labels = as.character(diff_df$season), 
                     breaks = diff_df$season, 
                     expand = expansion(mult = c(.1, .1))) +
  #coord_fixed(ratio = 1, xlim = 9, ylim = 9) +
  labs(
    title = "Initial vs Week 1 Roster, 2013-2021", 
    subtitle = "Number of players on the Week 1 roster that weren't on the initial 53 man roster.   |   Orange Line = NFL average", 
    caption = "Chart: @josepjefe     |     Data: @nflverse & TheFootballDB via @reinhurdler"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 5),
    axis.text.y = element_text(size = 4.5, face = "bold"),
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_line(size = 0.25), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.25), 
    panel.border = element_rect(color = "grey", fill=NA, size = 0.5), 
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 7), 
    plot.caption = element_text(size = 6), 
    plot.background = element_rect(fill = "#f5f5f5"),
    panel.background = element_rect(fill = "white"), 
    strip.text = element_nfl_wordmark(size = 0.5)
  )

ggsave(plot = plt, width = 8, height = 5, dpi = 300,  filename = "Diff_init_week_roster.png")
