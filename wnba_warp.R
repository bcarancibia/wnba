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
  theme_minimal(base_size=9) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = '#f1faee', color = '#f1faee')
    )
}


future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(2002:2021)
})
tictoc::toc()


i <- c(3:16)
wnba_player_box[ , i] <- apply(wnba_player_box[ , i], 2,          
                    function(x) as.numeric(as.character(x)))

wnba_player_box[is.na(wnba_player_box)] = 0
