#===============================================================================
# 2019-11-03 -- 30 days maps
# Day 1 points
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# Idea -- US cities named after other cities

library(tidyverse)
library(magrittr)
library(hrbrthemes); import_roboto_condensed()

# remotes::install_github("hrbrmstr/albersusa")
library(albersusa)
library(sf)
library(maps)


# get US geodata
usa <- usa_sf("laea")

# borders between states
library(rmapshaper)
bord <- usa %>% ms_innerlines()

# explore world.cities
world.cities %>% View
world.cities %>% arrange(country.etc) %>% pull(country.etc) %>%  unique() 

# filter out US cities
cit_us <- world.cities %>% filter(country.etc=="USA") 
cit_rest <- world.cities %>% filter(!country.etc=="USA") 

# matching names
match <- cit_us %>% 
    inner_join(cit_rest %>% select(name), by = "name") %>% 
    group_by(name, pop) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    distinct()%>% 
    arrange(n %>% desc) %>% 
    st_as_sf(
        coords = c("long", "lat"),
        crs = 4326
    ) %>% 
    st_transform(usa %>% st_crs())

# add insormation on states
match <- match %>% st_intersection(usa %>% transmute(state = iso_3166_2)) 



# plot
library(ggdark)
library(ggthemes)
library(ggrepel)

theme_set(dark_mode(theme_map(base_family = font_rc, base_size = 14)))

usa %>% 
    ggplot()+
    geom_sf(color = NA, fill = "#3B454A")+
    geom_sf(data = bord, color = "#b2b2b277")+
    geom_sf(data = match, aes(color = n, size = n))+
    scale_color_viridis_c(option = "B", begin = .2, breaks = 1:23)+
    # dark_mode(theme_map(base_family = font_rc))+
    theme(legend.position = "bottom",
          plot.title = element_text(family = "Roboto Slab", size = 20))+
    guides(
        size = FALSE, 
        color = guide_colorbar(
            barwidth = 27, 
            barheight = .5, 
            title.position = "top",
            nbin = 23
        )
    )+
    labs(
        title = "US cities with names matching cities in other countries",
        subtitle = "Data: maps::world.cities (Gazetteer) | #30DayMapChallenge",
        color =  "Number of cities outside US with the same name",
        caption = "@ikashnitsky"
    )+
    geom_sf_text(
        data = match %>% 
            filter(state %>% is_in(c("CA", "TX")) %>%  not(),
                   n %>% is_greater_than(3)) %>% 
            arrange(n %>% desc), 
        aes(label = name, color = n, size = {3+.2*n} ), 
        check_overlap = TRUE,
        family = font_rc, 
        hjust = -.2
    )+
    # add Texas names with left alignment
    geom_sf_text(
        data = match %>% 
            filter(state %>% is_in(c("TX")),
                   n %>% is_greater_than(3)) %>% 
            arrange(n %>% desc), 
        aes(label = name, color = n, size = {3+.2*n} ), 
        family = font_rc, 
        hjust = 1.2
    )

a <- last_plot()


# insets for CA and TX
ca <- usa %>% filter(iso_3166_2 %in% c("CA")) 

match_ca <- match %>% ms_clip(ca)

ca %>% 
    ggplot()+
    geom_sf(color = NA, fill = "#3B454A")+
    geom_sf(data = match_ca, aes(color = n, size = n))+
    scale_color_viridis_c(option = "B", begin = .2, breaks = 1:23)+
    theme(legend.position = "none")+
    geom_sf_text(
        data = match_ca%>% slice(1:7), 
        aes(label = name, color = n, size = {3+.2*n} ), 
        family = font_rc, 
        hjust = -.2
    )
    

b <- last_plot()    




# align with patchwork
library(patchwork)

c <- a + b + plot_layout(ncol = 2, widths = c(3, 1))

ggsave(filename = here::here("1911-map-challenge/day1-points.png"), 
       plot = c,
       width = 10, height = 7, type = "cairo")



# facet CA and TX ---------------------------------------------------------
# didn't manage to make it work properly

ca_tx <- usa %>% filter(iso_3166_2 %in% c("CA", "TX")) 

match_catx <- match %>% 
    st_set_crs(ca_tx %>% st_crs) %>% 
    ms_clip(ca_tx) %>% 
    mutate(iso_3166_2 = if_else(
        geometry %>% st_coordinates() %>% as_tibble %>% pull(X) %>% is_less_than(-110), "CA", "TX"
    ))


ca_tx  %>% 
    ggplot()+
    geom_sf(color = NA, fill = "#3B454A")+
    geom_sf(data = match_catx, aes(color = n, size = n))+
    scale_color_viridis_c(option = "B", begin = .2, breaks = 1:23)+
    theme(legend.position = "none")+
    geom_sf_text(
        data = match_catx%>% slice(1:10), 
        aes(label = name), 
        family = font_rc, 
        size = 4,
        hjust = -.2
    )+
    facet_wrap(~iso_3166_2, ncol = 1,drop = F,  shrink = F)
