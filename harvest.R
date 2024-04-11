# Saca tabla de imdb

library(rvest)
library(stringr)
library(dplyr)

get_df <- function(path) {
    html <- read_html(path)
    ul <- html |> html_elements(".ipc-metadata-list") |> html_elements("li") |> html_text2()
    df <- data.frame()
    for (i in 1:length(ul)) {
        l <- ul[i]
        s <- str_split(l, '\n') |> 
            unlist() |> 
            matrix(nrow = 1, ncol = 5) |> 
            data.frame()
        df <- rbind(df, s)
    }

    colnames(df) <- c("titulo", "anios", "puntaje", "tipo", "descr")
    final <- df |> select(titulo, puntaje, descr) |>
        mutate(puntaje = as.numeric(str_sub(puntaje, 1, 3)))

    return(final)
}

# Grupos
ruta_crimen <- "https://www.imdb.com/search/title/?title_type=tv_series&release_date=2010-01-01,2023-12-31&num_votes=999,&genres=crime&country_of_origin=US&sort=user_rating,desc"
ruta_drama <- "https://www.imdb.com/search/title/?title_type=tv_series&release_date=2010-01-01,2023-12-31&num_votes=999,&genres=drama&country_of_origin=US&sort=user_rating,desc"

df_crimen <- get_df(ruta_crimen)
df_drama <- get_df(ruta_drama)

df_crimen$tipo = "Crimen"
df_drama$tipo = "Drama"

total <- rbind(df_crimen, df_drama)

# De una vez el test
t.test(df_crimen$puntaje, df_drama$puntaje)

