library(shiny)
library(plotly)
library(ggplot2)
library(RSQLite)
library(wordcloud2)
library(RColorBrewer)
library(DT)
# setwd('/home/dkbrz/github/r_december_project/a_plus_mama/')


# ============================== Likes ==============================
load_video_meta <- function() {
    db <- dbConnect(SQLite(), 'test_db.db')
    data <- dbGetQuery(db,
    "SELECT * FROM video_meta")
    dbDisconnect(db)
    return (data)
}

load_top_authors <- function() {
    db <- dbConnect(SQLite(), 'test_db.db')
    data <- dbGetQuery(db,
    "SELECT authors.name, count(comments.id) as cnt
    FROM comments
    	JOIN authors ON comments.author = authors.id
    GROUP BY comments.author
    ORDER BY cnt DESC
    LIMIT 250;")
    dbDisconnect(db)
    return (data)
}
scatter_video_ld <- function(data){
    data$likeCount <- sapply(data$likeCount, log)
    data$dislikeCount <- sapply(data$dislikeCount, log)
    data$year <- as.character(data$year)
    plot_ly(
        data = data, type = 'scatter', mode = 'markers',
        text = paste(data$year, data$title), color = data$year,
        x = data$likeCount, y = data$dislikeCount,
        colors=c('#15983D', '#EE0011')
    ) %>% layout(
        yaxis = list(title='Количество дизлайков (log)'),
        xaxis = list(title = "Количество лайков (log)")
    ) %>% config(displayModeBar = F)}

scatter_video_vl <- function(data){
    data$likeCount <- sapply(data$likeCount, log)
    data$viewCount <- sapply(data$viewCount, log)
    data$year <- as.character(data$year)
    plot_ly(
        data = data, type = 'scatter', mode = 'markers',
        text = paste(data$year, data$title), color = data$year,
        x = data$viewCount, y = data$likeCount,
        colors=c('#149BED', '#FA6B09')
    ) %>% layout(
        yaxis = list(title='Количество лайков (log)'),
        xaxis = list(title = "Количество просмотров (log)")
    ) %>% config(displayModeBar = F)}

video_time <- function(data){
    data$year_month <- format(as.Date(data$publishedAt), "%Y-%m")
    plot_ly(
        data = data, type = 'histogram',
        x = ~year_month,
        color = 'const', colors = c("#b91226")
    ) %>% layout(
        yaxis = list(title='Количество видео'),
        xaxis = list(title = "Месяц")
    ) %>% hide_legend() %>% config(displayModeBar = F)}

video_duration <- function(data){
    data$year_month <- format(as.Date(data$publishedAt), "%Y-%m")
    plot_ly(
        data = data, type = 'box',
        x = ~year_month, y = ~duration_int,
        colors = c("#EC579A"), color = 'const'
    ) %>% layout(
        yaxis = list(range = c(0, 2600), title='Длительность видео'),
        xaxis = list(title = "Дата")
    ) %>% hide_legend() %>% config(displayModeBar = F)}

video_views <- function(data){
    data$year_month <- format(as.Date(data$publishedAt), "%Y-%m-%d")
    data$likes <- data$likeCount / data$dislikeCount
    plot_ly(
        data = data, type = 'scatter', mode = 'markers',
        text = paste(data$year, data$title),
        x = ~year_month, y = ~viewCount, color = ~likes,
        colors=c('#EE0011', "#FEC10B", '#15983D')
    ) %>% layout(
        yaxis = list(range = c(0, 400000))
    ) %>% layout(
        xaxis=list(title = 'Дата'),
        yaxis=list(title = 'Количество просмотров')
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
        colors = c("#EE0011", "#FEC10B", "#EC579A", "#149BED", "#15983D"),
        text = data$title
    ) %>% layout(
        title = "Теги во времени",
        xaxis = list(
            type = 'date',
            tickformat = "%d %B (%a)<br>%Y",
            title = "Дата"),
        yaxis = list(
            title = ""
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

color_arr <- c("#149BED", "#EE0011", "#EC579A", "#FEC10B", "#15983D", "#FA6B09", "#5a5895")
plot_wordcloud <- function(min_freq = 10){
    df <- load_wc(min_freq)
    df$color <- sample(color_arr, length(df$freq), replace = TRUE, prob = NULL)
    wordcloud2(data = df, fontFamily="Arial", rotateRatio=0, color = df$color)
}

# ============================== Server ==============================

shinyServer(function(input, output) {
    
    video_meta_data <- load_video_meta()
    tag_list <- load_tags_list()

    output$video_views <- renderPlotly({video_views(video_meta_data)})
    output$scatter_video_ld <- renderPlotly({scatter_video_ld(video_meta_data)})
    output$scatter_video_vl <- renderPlotly({scatter_video_vl(video_meta_data)})
    output$video_time <- renderPlotly({video_time(video_meta_data)})
    output$video_duration <- renderPlotly({video_duration(video_meta_data)})
    
    output$top_authors <- renderDataTable({datatable(load_top_authors())})
    output$wc <- renderWordcloud2({plot_wordcloud(10)})
    
    output$scatter_tags_in_time_print <- renderPlotly({
        input$run_tags
        tags <- isolate(input$tags)
        scatter_tags_in_time_print(tags)
    })
    
    output$main <- renderUI({
        tagList(
            tags$div(
                class = 'one-block',
                includeMarkdown("./md/intro.md"),
                plotlyOutput('video_views'),
            ),
            tags$div(
                class = 'one-block',
                includeMarkdown("./md/number.md"),
                plotlyOutput('video_time'),
            ),
            tags$div(
                class = 'one-block',
                plotlyOutput('video_duration'),
            ),
            tags$div(
                class = 'one-block',
                includeMarkdown("./md/likes_views.md"),
                splitLayout(
                    plotlyOutput('scatter_video_vl'),
                    plotlyOutput('scatter_video_ld')
                )
            ),
            tags$div(
                class = 'one-block',
                includeMarkdown("./md/wordcloud.md"),
                tags$div(
                    class = 'sub-block',
                    selectInput('tags', label = "Выберите теги", choices = tag_list, selected = c(89, 237, 14, 76), multiple = TRUE),
                    actionButton('run_tags', 'Обновить график'),
                ),
                tags$div(
                    class = 'sub-block',
                    plotlyOutput('scatter_tags_in_time_print', height = 200),
                )
            ),
            tags$div(
                class = 'one-block',
                wordcloud2Output('wc', height = 600),
            ),
            tags$div(
                class = 'one-block',
                includeMarkdown("./md/comments.md"),
                dataTableOutput('top_authors')
            ),
            tags$div(
                class = 'one-block',
                
            ),
            tags$div(
                class = 'one-block',
                
            )
        )
    })

})
