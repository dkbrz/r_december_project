install.packages("devtools")
devtools::install_github("richfitz/remoji")
library(remoji)
emj <- emoji(list_emoji(), TRUE)
xvect <- c('😂', 'no', '🍹', '😀', 'no', '😛') 
xvect[xvect %in% trimws(emj)]

library('RSQLite')
setwd('/home/dkbrz/github/r_december_project/')
db <- dbConnect(SQLite(), './a_plus_mama/test_db.db')
data <- dbGetQuery(db, "SELECT id, text FROM comments;")
dbDisconnect(db)

find_emojis <- function(x){
  xvect <- unlist(strsplit(x, ''))
  xvect[xvect %in% trimws(emj)]
}
find_emojis(xvect)
table(xvect[1:5]) + table(xvect)

data$emojis <- as.vector(lapply(data$text, find_emojis))
all_emojis <- unlist(data$emojis)
data <- as.data.frame(table(all_emojis))
colnames(data) <- c('name', 'freq')
dbWriteTable(db, "emojis", data)

library(wordcloud2)
get_data_emoji <- function(min_freq){
  db <- dbConnect(SQLite(), './a_plus_mama/test_db.db')
  df <- dbGetQuery(db, sprintf(
    "SELECT name, freq FROM emojis
    WHERE freq > %s
    ORDER BY freq DESC;", min_freq))
  dbDisconnect(db)
  return (df)
}
color_arr <- c("#149BED", "#EE0011", "#EC579A", "#FEC10B", "#15983D", "#FA6B09", "#5a5895")

plot_wordcloud_emojis <- function(min_freq = 10){
  df <- get_data_emoji(min_freq)
  df$freq <- unlist(lapply(df$freq, function(x){return (log(x, 24))}))
  df$color <- sample(color_arr, length(df$freq), replace = TRUE, prob = NULL)
  wordcloud2(data = df, fontFamily="Arial", rotateRatio=0, color = df$color)
}

plot_wordcloud_emojis(10)

