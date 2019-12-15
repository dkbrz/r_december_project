library(shiny)
library(plotly)
library(ggplot2)
library(RSQLite)
library(wordcloud2)
library(RColorBrewer)
# setwd('/home/dkbrz/github/r_december_project/a_plus_mama/')


# ============================== Likes ==============================
load_video_meta <- function() {
    db <- dbConnect(SQLite(), 'test_db.db')
    data <- dbGetQuery(db,
    "SELECT * FROM video_meta")
    dbDisconnect(db)
    return (data)
}

scatter_video <- function(data){
    data$likeCount <- sapply(data$likeCount, log)
    data$dislikeCount <- sapply(data$dislikeCount, log)
    plot_ly(
        data = data, type = 'scatter', mode = 'markers',
        text = paste(data$year, data$title), color = data$year,
        x = data$likeCount, y = data$dislikeCount
    ) %>% hide_legend() %>% config(displayModeBar = F)}

video_time <- function(data){
    data$year_month <- format(as.Date(data$publishedAt), "%Y-%m")
    plot_ly(
        data = data, type = 'histogram',
        x = ~year_month
    ) %>% hide_legend() %>% config(displayModeBar = F)}

video_duration <- function(data){
    data$year_month <- format(as.Date(data$publishedAt), "%Y-%m")
    plot_ly(
        data = data, type = 'box',
        x = ~year_month, y = ~duration_int
    ) %>% layout(
        yaxis = list(range = c(0, 2600))
    ) %>% hide_legend() %>% config(displayModeBar = F)}

# ============================== Tags ==============================

load_tags_list <- function() {
    db <- dbConnect(SQLite(), 'test_db.db')
    data <- dbGetQuery(db, "
    SELECT tags.id, tags.tag, count(video_to_tags.id_video) as cnt FROM video_to_tags
    	JOIN tags ON video_to_tags.id_tag = tags.id
    GROUP BY tags.id
    HAVING cnt > 20
    ORDER BY cnt DESC;")
    data <- split(data$id, data$tag)
    dbDisconnect(db)
    return (data)
}

scatter_tags_in_time_load <- function(tags){
    print(tags)
    db <- dbConnect(SQLite(), 'test_db.db')
    in_str <- paste(tags, collapse=", ")
    data <- dbGetQuery(db, sprintf("
    SELECT tags.tag as tag, video_meta.publishedAt as date, video_meta.viewCount as vcnt, video_meta.title as title
    FROM video_to_tags
    	JOIN video_meta ON video_to_tags.id_video = video_meta.id
    	JOIN tags ON tags.id = video_to_tags.id_tag
    WHERE tags.id in ( %s )", in_str))
    data$date <- as.Date(data$date)
    dbDisconnect(db)
    return (data)
}

scatter_tags_in_time_print <- function(tags){
    data <- scatter_tags_in_time_load(tags)
    data$date <- as.character(sapply(data$date, function(x){return (substr(x, 1, 10))}))
    plot_ly(
        data = data,
        type = 'scatter',
        x = ~date,
        y = ~tag,
        color = ~tag,
        text = data$title
    ) %>% layout(
        title = "Time Series with Custom Date-Time Format",
        xaxis = list(
            type = 'date',
            tickformat = "%d %B (%a)<br>%Y"
        )) %>% hide_legend() %>% config(displayModeBar = F)
}

load_wc <- function(min_freq){
    db <- dbConnect(SQLite(), 'test_db.db')
    data <- dbGetQuery(db, sprintf(
    "SELECT tags.tag as name, count(video_to_tags.id_video) as freq
    FROM video_to_tags
    	JOIN tags ON video_to_tags.id_tag = tags.id
    GROUP BY tags.id
    HAVING freq > %s
    ORDER BY freq DESC;", min_freq))
    dbDisconnect(db)
    return (data)
}

plot_wordcloud <- function(min_freq = 10){
    df <- load_wc(min_freq)
    wordcloud2(data = df, fontFamily="Arial", rotateRatio=0)
}

# ============================== Server ==============================

shinyServer(function(input, output) {
    
    video_meta_data <- load_video_meta()
    tag_list <- load_tags_list()

    output$scatter_video <- renderPlotly({scatter_video(video_meta_data)})
    output$video_time <- renderPlotly({video_time(video_meta_data)})
    output$video_duration <- renderPlotly({video_duration(video_meta_data)})
    output$wc <- renderWordcloud2({plot_wordcloud(10)})
    
    output$scatter_tags_in_time_print <- renderPlotly({
        input$run_tags
        tags <- isolate(input$tags)
        scatter_tags_in_time_print(tags)
    })
    
    output$main <- renderUI({
        tagList(
            tags$b('текст'),
            plotlyOutput('scatter_video'),
            plotlyOutput('video_time'),
            plotlyOutput('video_duration'),
            selectInput('tags', label = "Выберите теги", choices = tag_list, selected = c(237, 383, 75, 671, 69, 14), multiple = TRUE),
            actionButton('run_tags', 'Построить график'),
            plotlyOutput('scatter_tags_in_time_print'),
            wordcloud2Output('wc')
        )
    })

})
