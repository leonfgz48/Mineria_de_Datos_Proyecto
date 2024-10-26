library(tidyverse)


## lista de airbnb's
raw_list <- read_csv("https://data.insideairbnb.com/mexico/df/mexico-city/2024-06-27/data/listings.csv.gz")
raw_list$id <- raw_list$id |> format(scientific = F) |> trimws()
writexl::write_xlsx(raw_list, "Respaldo Emanuel/ITAM/semestre_01/MineriaDatos/raw_list.xlsx")



### EDA variables categórcias

categ <- raw_list |> 
  select(id,
    listing_url, last_scraped, source, name, description, neighborhood_overview,
    picture_url, host_url, host_name, host_since, host_location,
    host_about, host_response_time, host_is_superhost, host_thumbnail_url, 
    host_picture_url, host_neighbourhood, host_verifications, host_has_profile_pic, 
    host_identity_verified, neighbourhood, neighbourhood_cleansed, neighbourhood_group_cleansed,
    property_type, room_type, bathrooms_text, amenities, calendar_updated, 
    has_availability, calendar_last_scraped, first_review, last_review, license, 
    instant_bookable
  )

# eliminamos por obj del modelo: listing_url, last_scraped, source, name, picture_url, host_url, host_name 
#   host_thumbnail_url, host_picture_url, has_availability, calendar_last_scraped, first_review, last_review
# eliminamos por variable limpia en base: neighbourhood, host_neighbourhood, property_type
# eliminamos por nulos: neighbourhood_group_cleansed, calendar_updated, license
# transforamos a binarias: description, neighborhood_overview, host_location, host_about,
#   host_has_profile_pic, host_identity_verified
# calculamos host_since como dias/semanas/meses a la fecha de extraccion
# dejamos nominales: host_response_time, host_is_superhost, neighbourhood_cleansed, room_type, instant_bookable
# modificamos a nominales: host_verifications, host_response_time
# usamos para completar otra y extraer info: bathrooms_text
# usamos para construir más variables: amenities


categ_utiles <- categ |> 
  select(-c(listing_url, source, name, picture_url, host_url, host_name,
            host_thumbnail_url, host_picture_url, has_availability, calendar_last_scraped, 
            first_review, last_review, neighbourhood, neighbourhood_group_cleansed,
            calendar_updated, license, host_neighbourhood, property_type)) |> 
  mutate(
    host_about=ifelse(host_about=="", NA, host_about),
    across(
      c(description, neighborhood_overview, host_location, host_about,
        host_has_profile_pic, host_identity_verified),
      \(x)case_when(is.na(x)~0, T~1)),
    host_since_days = as.numeric(last_scraped-host_since),
    host_since_weeks = as.numeric(difftime(last_scraped, host_since, units = "w")) |> round(2),
    host_since_months = interval(host_since, last_scraped) %/% months(1),
    host_verifications = str_remove_all(host_verifications, "\\[|\\]|'"),
    bathrooms_numeric = case_when(
      bathrooms_text %in% c("Half-bath", "Private half-bath", "Shared half-bath") ~ 0.5,
      T ~ as.numeric(str_extract(bathrooms_text, "^\\d{1,}\\.?\\d?"))
    ),
    bathrooms_type = tolower(coalesce(str_extract(bathrooms_text, "[Pp]rivate|[Ss]hared"),"unespecified")),
    amenities = str_remove_all(amenities, '\\[|\\]|"|\\\\u5b9a|\\\\u81ea|\\\\u60e0|\\\\u800c|\\\\u6d66') |> 
      str_replace_all("\\\\u2019|\\\\u00b4", "'") |>
      str_replace_all("\\\\u2013", "-") |>
      str_replace_all("\\\\u00a0", " ") |> 
      str_replace_all("\\\\u00e1", "á") |> 
      str_replace_all("\\\\u00e9", "é") |> 
      str_replace_all("\\\\u00ed", "í") |> 
      str_replace_all("\\\\u00f3", "ó") |> 
      str_replace_all("\\\\u00fa", "ú") |> 
      str_replace_all("\\\\u00f6", "ö") |> 
      str_replace_all("\\\\u00e8", "è") |> 
      str_replace_all("\\\\u00da", "Ú") |> 
      str_replace_all("\\\\u00f1", "ñ") |> 
      str_replace_all("\\\\u014d", "ō") |> 
      str_replace_all("\\\\u00c1", "Á") |> 
      str_replace_all("\\\\u00c9", "É") |> 
      str_replace_all("\\\\u00d3", "Ó") |> 
      str_replace_all("\\\\u00a8", "¨") |> 
      str_replace_all("\\\\u201c|\\\\u201d", '"') |> 
      str_replace_all("\\\\u00b7", "·") |> 
      str_replace_all("\\\\u00e3", "ã") |> 
      toupper() |> 
      str_split(","),
    amenities = map(amenities, trimws),
    amenities = map(amenities, sort),
    amenities_n = map_dbl(amenities, length),
    host_response_time = ifelse(host_response_time=="N/A", "no_response_time", host_response_time),
    across(c(host_is_superhost, instant_bookable), as.numeric),
    across(c(host_response_time, neighbourhood_cleansed, room_type),
           \(x)str_replace_all(x, pattern=" ", replacement="_"))
  ) |> 
  separate_wider_delim(host_verifications, delim = ", ", names_sep = "_", too_few = "align_start") |> 
  select(-c(bathrooms_text, last_scraped, host_since)) |> 
  mutate(
    across(contains("host_verifications"), \(x)ifelse(x=="", NA, x)),
    host_verifications_4 = case_when(
      is.na(host_verifications_1)&is.na(host_verifications_2)&is.na(host_verifications_3) ~ "no_verifications", T~NA),
    .after = host_verifications_3
  ) |> 
  pivot_longer(
    cols=contains("host_verifications"), values_to = "name", names_to = "value"
  ) |> 
  mutate(value=1) |> 
  filter(!is.na(name)) |> 
  pivot_wider(names_prefix = "host_verifications_", values_fill = 0)


## completar nulos para host_is_super_host
is_super_host <- categ_utiles |>
  filter(!is.na(host_is_superhost)) |> 
  group_by(host_about, host_location, host_has_profile_pic, host_identity_verified,host_response_time, host_is_superhost) |> 
  tally() |> 
  filter(n==max(n)) |> 
  select(-n) |> 
  ungroup()


categ_utiles <- categ_utiles |> 
  left_join(
    is_super_host, by = join_by(host_about, host_location, host_has_profile_pic, host_identity_verified, host_response_time),
    suffix = c("","_nas")
  ) |> 
  mutate(
    host_is_superhost = coalesce(host_is_superhost, host_is_superhost_nas)
  ) |> 
  select(-host_is_superhost_nas)


## completar nulos para bathrooms_numeric

categ_utiles <- categ_utiles |> 
  mutate(bathrooms_numeric=coalesce(bathrooms_numeric, 
                                    categ_utiles |> pull(bathrooms_numeric) |> mean(na.rm = T) |> round(1)))

## variables categoricas para entrenamiento
categ_modelo <- categ_utiles |> 
  mutate(pivot_response=1,
         pivot_room=1,
         pivot_neig=1,
         pivot_bath=1) |> 
  pivot_wider(names_from=host_response_time, values_from=pivot_response,
              names_prefix = "host_response_time_", values_fill = 0) |> 
  pivot_wider(names_from=room_type, values_from=pivot_room,
              names_prefix = "room_type_", values_fill = 0) |> 
  pivot_wider(names_from=neighbourhood_cleansed, values_from=pivot_neig,
              names_prefix = "alcaldia_", values_fill = 0) |>
  pivot_wider(names_from=bathrooms_type, values_from=pivot_bath,
              names_prefix = "bathrooms_type_", values_fill = 0) |> 
  select(
    id,
    #binarias
    description, neighborhood_overview, host_location, host_about, host_is_superhost,
    host_has_profile_pic, host_identity_verified, instant_bookable, 
    #categoricas
    contains("host_verifications_"), contains("host_response_time_"),
    contains("room_type_"), contains("bathrooms_type_"), contains("alcaldia_"),
    #numericas
    contains("host_since_"), bathrooms_numeric, amenities_n
  )

  
writexl::write_xlsx(categ_modelo, "Respaldo Emanuel/ITAM/semestre_01/MineriaDatos/variables_categoricas_airbnb.xlsx")



####

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