#===============================================================================
# 2019-11-03 -- 30 days maps
# Analyze twitter users participating
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(rtweet)
library(httr)
library(tidyverse)
library(hrbrthemes)

options(scipen = 9999)


rt <- search_tweets(
    "#30DayMapChallenge", n = 18000, include_rts = FALSE
)


my_fds <- get_friends("ikashnitsky")


rt %>% lookup_coords()

rtg <- lat_lng(rt)


usr <- rt %>% users_data()

library(ggdark)

rt %>% 
    # as_tibble() %>% 
    group_by(user_id, followers_count, friends_count) %>% 
    summarise(likes = sum(favorite_count)) %>% 
    ungroup() %>% 
    ggplot(aes(followers_count, friends_count, color = likes))+
    geom_abline(slope = 1)+
    geom_point()+
    scale_x_continuous(trans = "log", breaks = 10^{0:4})+
    scale_y_continuous(trans = "log", breaks = 10^{0:4})+
    scale_color_viridis_c(trans = "log", breaks = 10^{0:3},
                          option = "B", begin = .2)+
    coord_fixed(
        # xlim = c(1, rt$followers_count %>% max()),
        # ylim = c(1, rt$friends_count %>% max())
    )+
    dark_theme_minimal()

# discrete
rt %>% 
    filter(media_type == "photo") %>% 
    group_by(user_id, followers_count, friends_count) %>% 
    summarise(likes = sum(favorite_count)) %>% 
    ungroup() %>% 
    mutate(likes = likes %>% cut(c(0, 10, 50, 100, 250, 500, 1000))) %>% 
    arrange(likes) %>% 
    ggplot(aes(followers_count, friends_count, color = likes))+
    geom_abline(slope = 1)+
    geom_point(size = 2)+
    scale_color_viridis_d(option = "B", begin = .15)+
    scale_x_continuous(trans = "log", breaks = 10^{0:4})+
    scale_y_continuous(trans = "log", breaks = 10^{0:4})+
    coord_fixed(ylim = c(10, rt$friends_count %>% max))+
    dark_theme_minimal(base_family = font_rc)+
    theme(legend.position = c(.15, .7),
          plot.title = element_text(family = "Roboto Slab"),
          axis.title = element_text(family = "Roboto Slab"))+
    labs(title = "Tweets of #30DayMapChallenge by 174 unique users",
         subtitle = "It seems, number of likes user's tweets receive does not strictly depend on popularity", 
         caption = "Data harvested with {rtweet} on 2 Nov 2019 at 5pm CET\nDataviz by @ikashnitsky",
         x = "Followers count",
         y = "Number of users the person follows",
         color = "Likes count")


ggsave(here::here("1911-map-challenge/day0-likes-vs-popularity.png"), width = 6, height = 5, type = "cairo")

