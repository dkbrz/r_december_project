library(shiny)
library(plotly)
library(ggplot2)
library(RSQLite)
#setwd('/home/dkbrz/github/r_december_project/a_plus_mama/')


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

scatter_tags_in_time <- function(tags){
    db <- dbConnect(SQLite(), 'test_db.db')
    data <- dbGetQuery(db, "
    SELECT tags.id, tags.tag, count(video_to_tags.id_video) as cnt FROM video_to_tags
    	JOIN tags ON video_to_tags.id_tag = tags.id
    GROUP BY tags.id
    HAVING cnt > 20
    ORDER BY cnt DESC;")
    data <- split(data$id, data$tag)
    dbDisconnect(db)
}

# ============================== Server ==============================

shinyServer(function(input, output) {
    
    video_meta_data <- load_video_meta()
    tag_list <- load_tags_list()

    #output$scatter_video <- renderPlotly({scatter_video(video_meta_data)})
    #output$video_time <- renderPlotly({video_time(video_meta_data)})
    output$video_duration <- renderPlotly({video_duration(video_meta_data)})
    
    output$tags_in_time <- 4

    output$main <- renderUI({
        tagList(
            tags$b('текст'),
            #plotlyOutput('scatter_video'),
            #plotlyOutput('video_time'),
            #plotlyOutput('video_duration')
            selectInput('tags', label = "Выберите теги", choices = tag_list, multiple = TRUE)
        )
    })

})
