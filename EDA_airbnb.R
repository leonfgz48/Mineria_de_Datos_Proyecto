library(tidyverse)


## lista de airbnb's
raw_list <- read_csv("https://data.insideairbnb.com/mexico/df/mexico-city/2024-06-27/data/listings.csv.gz")
clean_list <- read_csv("https://data.insideairbnb.com/mexico/df/mexico-city/2024-06-27/visualisations/listings.csv")

## precios por ID y fecha
calendar_data = read_csv("https://data.insideairbnb.com/mexico/df/mexico-city/2024-06-27/data/calendar.csv.gz")

## reviews
reviews_data = read_csv("https://data.insideairbnb.com/mexico/df/mexico-city/2024-06-27/data/reviews.csv.gz")


#### revision clean_list ####
clean_list |> 
  summary()

## NAs en clean_list
clean_list |> 
  select(-c(neighbourhood_group, license)) |> 
  filter(if_any(.cols=everything(), .fns=is.na))


## Posibles variables útiles:
##    id, neighbourhood, latitude, longitude, room_type, price, minimum_nights,
##    calculated_host_listings_count



#### revision raw_list ####

raw_selection <- raw_list |> 
  select(id, neighbourhood_cleansed, latitude, longitude, property_type,
         room_type, accommodates, bathrooms, bedrooms, beds, amenities,
         price, minimum_nights, maximum_nights, ##aviabilitys?,
         instant_bookable) |> 
  mutate(price = str_remove_all(price, "\\$|,") |> as.numeric())

raw_selection |> 
  summary()


genera_box_plots<- function(df, var_categorica){
  df |> 
    ggplot(aes(x=factor(.data[[var_categorica]]), y=log(price)))+
    geom_boxplot()+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90))
}


genera_box_plots(raw_selection, "neighbourhood_cleansed")
genera_box_plots(raw_selection, "property_type")
genera_box_plots(raw_selection, "room_type")
genera_box_plots(raw_selection, "accommodates")
genera_box_plots(raw_selection, "bathrooms")
genera_box_plots(raw_selection, "bedrooms")
genera_box_plots(raw_selection, "instant_bookable")



#### calendario
calendar_data |> 
  group_by(listing_id) |> 
  filter(n_distinct(price)>1)
  
### todos los IDs tienen el mismo precio para todo el proximo año
