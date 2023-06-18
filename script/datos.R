
# paquetes ----------------------------------------------------------------

library(spotifyr)
library(tidyverse)
library(glue)
library(fontawesome)

# TWICE spotify
# browseURL("https://open.spotify.com/artist/7n2Ycct7Beij7Dj7meI4X0")

# audio features
# browseURL(glue("https://developer.spotify.com/documentation/web-api/reference/
#   get-several-audio-features"))

# paquete {spotifyr}
# browseURL("https://www.rcharlie.com/spotifyr/")
 
# función para generar mensajes en la consola, para separar las secciones
f_msj <- function(x) {
  a <- nchar(x)
  b <- str_flatten(rep("X", a + 8))
  c <- str_flatten(c("X", rep(" ", a + 6), "X"))
  d <- str_flatten(c("X   ", x, "   X"))
  e <- glue("\n\n{b}\n{c}\n{d}\n{c}\n{b}\n\n")
  
  return(e)
}

# credenciales ------------------------------------------------------------

f_msj("Credenciales SPOTIFY")

key <- read_csv("datos/key.csv")

Sys.setenv(SPOTIFY_CLIENT_ID = key$key[1])
Sys.setenv(SPOTIFY_CLIENT_SECRET = key$key[2])

access_token <- get_spotify_access_token()

# features ----------------------------------------------------------------

f_msj("Obtengo features de TWICE")

# álbum
ft_album <- get_artist_audio_features(
  artist = "TWICE",
  include_groups = "album",
  return_closest_artist = TRUE,
  dedupe_albums = TRUE,
  authorization = get_spotify_access_token()) |>
  as_tibble() |> 
  # expando la columna con los datos de las imágenes
  unnest(cols = album_images) |> 
  # elijo solamente la resolución más alta
  filter(width == 640)

# singles
ft_single <- get_artist_audio_features(
  artist = "TWICE",
  include_groups = "single",
  return_closest_artist = TRUE,
  dedupe_albums = TRUE,
  authorization = get_spotify_access_token()) |>
  as_tibble() |>
  # expando la columna con los datos de las imágenes
  unnest(cols = album_images) |>
  # elijo solamente la resolución más alta
  filter(width == 640)

# seleccionando albums/singles/tracks -------------------------------------

# manualmente ELIMINO:
eliminar <- c(
  "5mqmE56gLXHSS4IH2nSGsw", "4GWHz1Kxn8EirxXQgP7u7u", "1nJn5rN8Z8DAcXkakrTnQd", 
  "56w7UaxCFwsRr5VVk0agy0", "5o5cJ6yZmcyy4y4hK0J6de",
  # HOSPITAL PLAYLIST SEASON 2 (aparte)
  "38EuSpoG8AAKgFDkWQZCxx")

# vector con id de los álbums/singles individuales de interés
albums_id <-  bind_rows(
  ft_album,
  ft_single) |> 
  select(
    album_name, album_type, nro = track_number, 
    fecha = album_release_date, album_id) |> 
  mutate(album_name = str_to_upper(album_name)) |> 
  group_by(album_name, album_type) |> 
  slice_max(order_by = nro, n = 1) |> 
  ungroup() |> 
  arrange(album_name) |> 
  filter(nro > 1) |> 
  group_by(album_name, album_type, nro) |> 
  mutate(i = cur_group_id()) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  select(-i) |> 
  group_by(album_name, fecha) |> 
  mutate(i = cur_group_id()) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  select(-i) |>
  filter(!album_id %in% eliminar) |> 
  pull(album_id)

# features de los álbums/singles de interés
todos_album <- bind_rows(
  ft_album,
  ft_single) |> 
  filter(album_id %in% albums_id) |> 
  rename(url_img = url)

# todos_album # 298*41

# arreglo final de datos, 172*9
d <- todos_album |> 
  select(
    "Track" = track_name, 
    url_img,
    "Album" = album_name, 
    ends_with("ness"),
    "Valence" = "valence", 
    "Energy" = "energy",
    -instrumentalness, -loudness,
    "link" = external_urls.spotify) |> 
  rename_with(.fn = str_to_sentence) |> 
  # remplazo <’> -> <'>
  mutate(Track = str_replace(Track, "’", "'")) |> 
  # remuevo remixes, instrumental, versiones en japonés, inglés, coreano
  mutate(Track = str_to_lower(Track)) |> 
  filter(!str_detect(string = Track, pattern = "remix| japanese ver|eng|korean|instrumental")) |> 
  distinct(Track, .keep_all = TRUE) |> 
  # mayúscula para tracks, title para album
  mutate(Track = str_to_upper(Track)) |> 
  mutate(Album = str_to_title(Album)) |> 
  # ordeno por track
  arrange(Track) 

f_msj("Guardo los datos")

# guardo los datos
write_tsv(d, "datos/d.tsv")
