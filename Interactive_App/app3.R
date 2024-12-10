library(shiny)
library(dplyr)
library(plotly)
library(lubridate)
library(shinydashboard)
library(DT)

# Load the necessary data
usage_data <- read.csv("usage.csv") # Replace with actual path
users_data <- read.csv("users.csv") # Replace with actual path
content_data <- read.csv("content.csv") # Replace with actual path

# Create a named list for the dropdown with unique content
content_choices <- c("All" = "all",
                     setNames(as.character(content_data$guid), content_data$title))

# Define the UI
ui <- fluidPage(
  titlePanel("Content Usage Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu to select content
      selectInput(
        inputId = "content_select",
        label = "Select Content",
        choices = content_choices,
        selected = "all"
      ),
      
      # Dropdown menu to select user
      selectInput(
        inputId = "user_select",
        label = "Select User",
        choices = NULL,
        selected = NULL
      )
    ),
    
    mainPanel(
      # Tab panels for navigation
      tabsetPanel(
        tabPanel(
          "Overview", value = "overview",
          
          # First row: Daily Usage Trends Over Time graph
          fluidRow(
            column(12, 
                   plotlyOutput("daily_trends_plot")
            )
          ),
          
          # Second row: 3 columns for the other graphs
          fluidRow(
            column(4, 
                   plotlyOutput("top_apps_plot")
            ),
            column(4, 
                   plotlyOutput("top_viewers_plot")
            ),
            column(4, 
                   plotlyOutput("top_owners_plot")
            )
          )
        ),
        
        tabPanel("General Stats", value = "general_stats", 
                 # 1. Value Boxes
                 fluidRow(
                   valueBoxOutput("unique_users", width = 2),
                   valueBoxOutput("anonymous_users", width = 2),
                   valueBoxOutput("opened_sessions", width = 2),
                   valueBoxOutput("avg_time_daily", width = 2),
                   valueBoxOutput("active_days", width = 2)
                 ),
                 
                 # 2. Unique Users vs Opened Sessions Over Time
                 fluidRow(
                   column(12,
                          plotlyOutput("unique_users_sessions_plot")
                   )
                 ),
                 
                 # 3. Average Session Time Over Time
                 fluidRow(
                   column(12,
                          plotlyOutput("avg_session_time_plot")
                   )
                 )
        ),
        
        tabPanel("User Specific Stats", value = "user_specific_stats", 
                 # 1. Bar chart for users logged each day
                 fluidRow(
                   column(12,
                          plotlyOutput("users_logged_daily_plot")
                   )
                 ),
                 
                 # 2. Value boxes
                 fluidRow(
                   valueBoxOutput("hours_spent", width = 3),
                   valueBoxOutput("days_logged", width = 3),
                   valueBoxOutput("sessions_opened", width = 3),
                   valueBoxOutput("first_login", width = 3)
                 )
        ),
        
        # New Menu: Content Insights
        tabPanel("Content Insights", value = "content_insights", 
                 # 1. Heatmap
                 fluidRow(
                   column(12,
                          plotlyOutput("heatmap_sessions")
                   )
                 ),
                 
                 # 2. Scatter Plot for Top User Engagement
                 fluidRow(
                   column(12,
                          plotlyOutput("top_user_engagement_plot")
                   )
                 ),
                 
                 # 3. Table for Content Performance Metrics
                 fluidRow(
                   column(12,
                          dataTableOutput("content_performance_table")
                   )
                 )
        ),
        
        id = "tabs"
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive data filtered by content selection
  filtered_usage <- reactive({
    if (input$content_select == "all") {
      usage_data
    } else {
      usage_data %>% filter(content_guid == input$content_select)
    }
  })
  
  # 1. Daily Usage Trends Over Time
  output$daily_trends_plot <- renderPlotly({
    daily_trends <- filtered_usage() %>%
      mutate(date = as.Date(started)) %>%
      group_by(date) %>%
      summarise(sessions = n())
    
    plot_ly(
      data = daily_trends,
      x = ~date,
      y = ~sessions,
      type = "scatter",
      mode = "lines+markers",
      name = "Daily Trends"
    ) %>%
      layout(title = "Daily Usage Trends Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "# of Sessions"))
  })
  
  # 2. Top 20 Most Used Apps by # of Sessions
  output$top_apps_plot <- renderPlotly({
    top_apps <- filtered_usage() %>%
      group_by(content_guid) %>%
      summarise(sessions = n()) %>%
      top_n(20, wt = sessions) %>%
      left_join(content_data, by = c("content_guid" = "guid")) %>%
      arrange(desc(sessions))
    
    plot_ly(
      data = top_apps,
      x = ~sessions,
      y = ~reorder(title, sessions),
      type = "bar",
      orientation = "h",
      name = "Top Apps"
    ) %>%
      layout(title = "Top 20 Most Used Apps by # of Sessions",
             xaxis = list(title = "# of Sessions"),
             yaxis = list(title = "App"))
  })
  
  # 3. Viewers (Top 20 by # of Sessions)
  output$top_viewers_plot <- renderPlotly({
    top_viewers <- filtered_usage() %>%
      group_by(user_guid) %>%
      summarise(sessions = n()) %>%
      top_n(20, wt = sessions) %>%
      left_join(users_data, by = c("user_guid" = "guid")) %>%
      arrange(desc(sessions))
    
    plot_ly(
      data = top_viewers,
      x = ~sessions,
      y = ~reorder(email, sessions),
      type = "bar",
      orientation = "h",
      name = "Top Viewers"
    ) %>%
      layout(title = "Viewers (Top 20 by # of Sessions)",
             xaxis = list(title = "# of Sessions"),
             yaxis = list(title = "Viewer"))
  })
  
  # 4. Owners (Top 20 by # of Sessions)
  output$top_owners_plot <- renderPlotly({
    # Aggregate sessions by owner_guid
    top_owners <- filtered_usage() %>%
      left_join(content_data, by = c("content_guid" = "guid")) %>%
      group_by(owner_guid) %>%
      summarise(sessions = n()) %>%
      arrange(desc(sessions)) %>%
      top_n(20, wt = sessions)
    
    # Join with users_data to get owner details
    top_owners <- top_owners %>%
      left_join(users_data, by = c("owner_guid" = "guid"))
    
    # Handle cases where owner emails might be missing
    top_owners <- top_owners %>%
      mutate(email = ifelse(is.na(email), "Unknown Owner", email))
    
    # Create the plot
    plot_ly(
      data = top_owners,
      x = ~sessions,
      y = ~reorder(email, sessions),
      type = "bar",
      orientation = "h",
      name = "Top Owners"
    ) %>%
      layout(
        title = "Owners (Top 20 by # of Sessions)",
        xaxis = list(title = "# of Sessions"),
        yaxis = list(title = "Owner")
      )
  })
  
  
  ####################################### General Stats ##########################################
  
  # 1. Value Boxes
  output$unique_users <- renderValueBox({
    unique_users_count <- filtered_usage() %>%
      distinct(user_guid) %>%
      nrow()
    
    valueBox(
      unique_users_count,
      "# of Unique Users (with Logins)",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$anonymous_users <- renderValueBox({
    anonymous_users_count <- filtered_usage() %>%
      filter(is.na(user_guid)) %>%
      nrow()
    
    valueBox(
      anonymous_users_count,
      "# of Anonymous Users",
      icon = icon("user-secret"),
      color = "red"
    )
  })
  
  output$opened_sessions <- renderValueBox({
    sessions_count <- filtered_usage() %>%
      nrow()
    
    valueBox(
      sessions_count,
      "# of Sessions Opened",
      icon = icon("folder-open"),
      color = "green"
    )
  })
  
  output$avg_time_daily <- renderValueBox({
    avg_time <- filtered_usage() %>%
      mutate(date = as.Date(started)) %>%
      group_by(date) %>%
      summarise(daily_time = sum(session_duration, na.rm = TRUE)) %>%
      summarise(avg_daily_time = mean(daily_time, na.rm = TRUE)) %>%
      pull(avg_daily_time) / 3600 # Convert to hours
    
    valueBox(
      round(avg_time, 2),
      "Average Time Spent Daily (hours)",
      icon = icon("clock"),
      color = "orange"
    )
  })
  
  output$active_days <- renderValueBox({
    active_days_count <- filtered_usage() %>%
      mutate(date = as.Date(started)) %>%
      distinct(date) %>%
      nrow()
    
    valueBox(
      active_days_count,
      "# of Days Active",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  # 2. Unique Users vs Opened Sessions Over Time
  output$unique_users_sessions_plot <- renderPlotly({
    data <- filtered_usage() %>%
      mutate(date = as.Date(started)) %>%
      group_by(date) %>%
      summarise(unique_users = n_distinct(user_guid),
                sessions = n())
    
    plot_ly(data, x = ~date) %>%
      add_bars(y = ~unique_users, name = "Unique Users", marker = list(color = 'blue')) %>%
      add_bars(y = ~sessions, name = "Sessions", marker = list(color = 'green')) %>%
      layout(
        title = "Unique Users vs Opened Sessions Over Time",
        barmode = "group",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count")
      )
  })
  
  # 3. Average Session Time Over Time
  output$avg_session_time_plot <- renderPlotly({
    data <- filtered_usage() %>%
      mutate(date = as.Date(started),
             session_duration_hours = session_duration / 3600) %>% # Convert seconds to hours
      group_by(date) %>%
      summarise(avg_session_time = mean(session_duration_hours, na.rm = TRUE))
    
    plot_ly(data, x = ~date, y = ~avg_session_time, type = "bar", name = "Avg Session Time") %>%
      layout(
        title = "Average Session Time (hours) Over Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Avg Session Time (hours)")
      )
  })
  
  ##################################### User Specific Stats ###########################################
  
  # Populate selectInput choices with users filtered by content
  observe({
    updateSelectInput(
      session,
      inputId = "user_select",
      choices = setNames(as.character(users_data$guid), users_data$username)
    )
  })
  
  # Reactive data filtered by user selection
  filtered_user_usage <- reactive({
    req(input$user_select)
    filtered_usage() %>% 
      filter(user_guid == input$user_select)
  })
  
  # Render outputs (e.g., value boxes and plots) based on `filtered_user_usage`
  
  # 1. Bar chart: Users Logged Each Day (Returning vs New)
  output$users_logged_daily_plot <- renderPlotly({
    data <- filtered_usage() %>%
      mutate(date = as.Date(started)) %>%
      group_by(date, user_guid) %>%
      summarise(sessions = n()) %>%
      mutate(user_type = if_else(user_guid %in% 
                                   unique(first(user_guid)), "New", "Returning")) %>%
      group_by(date, user_type) %>%
      summarise(user_count = n())
    
    plot_ly(data, x = ~date, y = ~user_count, color = ~user_type, type = "bar") %>%
      layout(
        title = "Users Logged Each Day (Returning vs New)",
        barmode = "stack",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count of Users")
      )
  })
  
  # 2. Value boxes
  output$hours_spent <- renderValueBox({
    total_hours <- filtered_user_usage() %>%
      summarise(total_duration = sum(session_duration, na.rm = TRUE)) %>%
      pull(total_duration) / 3600 # Convert seconds to hours
    
    valueBox(
      round(total_hours, 2),
      "# of Hours Spent",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$days_logged <- renderValueBox({
    days_logged <- filtered_user_usage() %>%
      mutate(date = as.Date(started)) %>%
      distinct(date) %>%
      nrow()
    
    valueBox(
      days_logged,
      "# of Days Logged",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$sessions_opened <- renderValueBox({
    sessions_count <- filtered_user_usage() %>%
      nrow()
    
    valueBox(
      sessions_count,
      "# of Sessions Opened",
      icon = icon("folder-open"),
      color = "orange"
    )
  })
  
  output$first_login <- renderValueBox({
    first_login <- filtered_user_usage() %>%
      summarise(first_date = min(as.Date(started), na.rm = TRUE)) %>%
      pull(first_date)
    
    # Handle empty or invalid dates
    first_login <- ifelse(is.infinite(first_login), "No Data", first_login)
    
    valueBox(
      first_login,
      "First Login",
      icon = icon("calendar-alt"),
      color = "purple"
    )
  })
  
  #############################################################################################
  
  # 1. Heatmap: Sessions by Hour of Day and Day of Week
  output$heatmap_sessions <- renderPlotly({
    data <- filtered_usage() %>%
      mutate(
        hour = hour(ymd_hms(started)),
        weekday = wday(ymd_hms(started), label = TRUE)
      ) %>%
      group_by(hour, weekday) %>%
      summarise(sessions = n())
    
    plot_ly(
      data = data,
      x = ~hour,
      y = ~weekday,
      z = ~sessions,
      type = "heatmap",
      colors = "Blues"
    ) %>%
      layout(
        title = "Sessions by Hour of Day and Day of Week",
        xaxis = list(title = "Hour of Day"),
        yaxis = list(title = "Day of Week")
      )
  })
  
  # 2. Scatter Plot: Top User Engagement (Sessions vs Time Spent)
  output$top_user_engagement_plot <- renderPlotly({
    top_users <- filtered_usage() %>%
      group_by(user_guid) %>%
      summarise(
        total_sessions = n(),
        total_time_spent = sum(session_duration, na.rm = TRUE) / 3600 # Convert seconds to hours
      ) %>%
      arrange(desc(total_sessions)) %>%
      top_n(20, wt = total_sessions) %>%
      left_join(users_data, by = c("user_guid" = "guid"))
    
    plot_ly(
      data = top_users,
      x = ~total_sessions,
      y = ~total_time_spent,
      text = ~email,
      type = "scatter",
      mode = "markers",
      marker = list(size = 10),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Top User Engagement: Sessions vs Time Spent",
        xaxis = list(title = "Total Sessions"),
        yaxis = list(title = "Total Time Spent (hours)")
      )
  })
  
  # 3. Table: Content Performance Metrics
  output$content_performance_table <- renderDataTable({
    content_metrics <- filtered_usage() %>%
      group_by(content_guid) %>%
      summarise(
        total_sessions = n(),
        avg_session_duration = mean(session_duration, na.rm = TRUE) / 60, # Convert seconds to minutes
        total_time_spent = sum(session_duration, na.rm = TRUE) / 3600 # Convert seconds to hours
      ) %>%
      left_join(content_data, by = c("content_guid" = "guid")) %>%
      select(title, total_sessions, avg_session_duration, total_time_spent)
    
    datatable(
      content_metrics,
      colnames = c("Content Title", "Total Sessions", "Avg Session Duration (mins)", "Total Time Spent (hours)"),
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
}

# Lancer l'application
shinyApp(ui = ui, server = server)