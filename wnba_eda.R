library(tidyverse)
library(wehoop)
library(rvest)
library(janitor)
library(hablar)
library(ggforce)
library(magick)
library(ggtext)
library(extrafont)



"""""
Create a theme and play around with the 
e63946
a8dadc
457b9d
1d3557
"""""

theme_ben <- function () { 
  theme_minimal(base_size=9, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = '#f1faee', color = '#f1faee')
    )
}

ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point() +
  theme_ben()

"""""
install the appropriate package WeHoop and then pull in data. It is important to know that there is no team box scores
for 2002 so do all analysis from 2003 and going forward. 
"""""
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
# Alternatively, using the devtools package:
devtools::install_github(repo = "saiemgilani/wehoop")


"""""
Import the data 
"""""

future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(2002:2021)
})
tictoc::toc()

#NO 2002 
future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(2003:2021)
})
tictoc::toc()


future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(2002:2021)
})
tictoc::toc()



"""""
subset player box scores for more than 500 minutes
first step is iterate through and group players
second step is to mutate 

"""""



sub_pbox <- wnba_player_box %>%
  select(athlete_display_name, season, min, ast, to, pts)

sub_pbox$ast = as.numeric(as.character(sub_pbox$ast))
sub_pbox$to = as.numeric(as.character(sub_pbox$to))
sub_pbox$pts = as.numeric(as.character(sub_pbox$pts))
sub_pbox$min = as.numeric(as.character(sub_pbox$min))

sub_pbox[is.na(sub_pbox)] = 0

summarise_pbox <- sub_pbox %>%
  group_by(athlete_display_name) %>%
  summarise(mins = mean(min), pts = mean(pts), ast = mean(ast), to = mean(to))

ggplot(summarise_pbox, aes(x = pts, y = ast)) +
  geom_point() + 
  theme_ben()



"""""
sub_pbox <- sub_pbox %>%
  group_by(athlete_display_name, season) %>%
  mutate(mins_avg = mean(min)) %>%
  mutate(pts_avg = mean(pts)) %>%
  mutate(ast_avg = mean(ast)) %>%
  mutate(to_avg = mean(to)) %>%
  mutate(sum_min = sum(min))

year_2018 <- sub_pbox %>%
  filter(season == 2018) %>%
  group_by(athlete_display_name) %>%
  summarise(mins = mean(min), pts = mean(pts), ast = mean(ast), to = mean(to)) %>%
  mutate()

attach(year_2018)
X <- data.frame(ast, to, pts)/mins
detach(year_2018)

THIS IS WRONG

ggplot(X, aes(x=ast, y=to, color = pts)) +
  geom_point()
  
"""""