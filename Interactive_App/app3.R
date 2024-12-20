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
content_choices <- c(
  "All" = "all", 
  setNames(as.character(content_data$guid), content_data$title)
)


ui <- dashboardPage(
  
  dashboardHeader(
    title = glue::glue("Posit Usage at X Enterprise"), 
    titleWidth = "40%"
  ),
  
  dashboardSidebar(disable = TRUE),  # Désactiver la barre latérale
  
  dashboardBody(
    box(
      fluidRow(
        column(6,
               # Dropdown menu to select content
               selectInput(
                 inputId = "content_select",
                 label = "Select Content",
                 choices = content_choices,
                 selected = "all"
               )
        ),
        column(6,
               # Date Range Input
               dateRangeInput(
                 inputId = "date_range",
                 label = "Select Date Range",
                 start = NULL, # Default values will be set dynamically
                 end = NULL    # Default values will be set dynamically
               )
        )
      ),
      width = 12
    ),
    
    tabsetPanel(
      tabPanel(
        title = "Overview",
        icon = icon("home"),
        fluidRow(
          shinycssloaders::withSpinner(
            plotlyOutput("daily_trends_plot")
          )
        ),
        fluidRow(
          box(
            plotlyOutput("top_apps_plot"),
            width = 4
          ),
          box(
            plotlyOutput("top_viewers_plot"),
            width = 4
          ),
          box(
            plotlyOutput("top_owners_plot"),
            width = 4
          )
        )
      ),
      
      tabPanel(
        title = "General Stats",
        icon = icon("chart-bar"),
        fluidRow(
          valueBoxOutput("unique_users", width = 2),   # # of Unique users (with Logins)
          valueBoxOutput("anonymous_users", width = 2),  # # of Anonymous Users
          valueBoxOutput("opened_sessions", width = 2),  # # of Sessions Opened
          valueBoxOutput("avg_time_daily", width = 3),   # Average Time Spent Daily
          valueBoxOutput("active_days", width = 3)    # # of Days Active
        ),
        fluidRow(
          box(
            title = "Unique Users / Opened Sessions Over Time",
            plotlyOutput("unique_users_sessions_plot"),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Average Session Time (hours)",
            plotlyOutput("avg_session_time_plot"),
            width = 12
          )
        )
      ),
      
      
      tabPanel(
        title = "User specific stats",
        icon = icon("user"),
        # First output: Stacked bar chart for Users logged each day
        fluidRow(
          box(
            title = "Users logged each day (Returning vs New)",
            width = 12,
            plotlyOutput("users_logged_daily_plot") %>%
              shinycssloaders::withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "Select User",
            # Dropdown menu to select user
            selectInput(
              inputId = "user_select",
              label = "Select User",
              choices = NULL,
              selected = NULL
            ),
            width = 12
          )
        ),
        fluidRow(
          valueBoxOutput("hours_spent", width = 3),
          valueBoxOutput("days_logged", width = 3),
          valueBoxOutput("sessions_opened", width = 3),
          valueBoxOutput("first_login", width = 3)
        )
      ),
      
      # App Mode Analysis
      tabPanel(
        title = "App Mode Analysis",
        icon = icon("tasks"),
        fluidRow(
          # Pie Chart (Donut Chart) for Share of Shiny, Python, Other
          box(
            title = "Share of Content by App Mode",
            width = 6,
            plotlyOutput("app_mode_pie_chart") %>%
              shinycssloaders::withSpinner()
          ),
          
          # Bar Chart for Number of Sessions by App Mode
          box(
            title = "Number of Sessions by App Mode",
            width = 6,
            plotlyOutput("app_mode_bar_chart") %>%
              shinycssloaders::withSpinner()
          )
        ),
        
        fluidRow(
          # Table for Summary Statistics
          box(
            title = "App Mode Summary Statistics",
            width = 6,
            dataTableOutput("app_mode_summary_table") %>%
              shinycssloaders::withSpinner()
          ),
          
          # List of Users by App Mode
          box(
            title = "Users by App Mode",
            width = 6,
            fluidRow(
              column(
                12,
                selectInput(
                  inputId = "app_mode_select",
                  label = "Select App Mode",
                  choices = NULL,  # Choices will be dynamically populated in the server
                  selected = NULL
                )
              )
            ),
            fluidRow(
              column(
                12,
                dataTableOutput("users_by_app_mode_table") %>%
                  shinycssloaders::withSpinner()
              )
            )
          )
        
        )
      ),
      
      
      # New Menu: Content Insights
      tabPanel("Content Insights", icon = icon("cogs"), value = "content_insights",
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
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  # Set default values for date range based on the dataset
  observe({
    updateDateRangeInput(
      session,
      "date_range",
      start = min(as.Date(usage_data$started, na.rm = TRUE)),
      end = max(as.Date(usage_data$ended, na.rm = TRUE)),
      min = min(as.Date(usage_data$started, na.rm = TRUE)),
      max = max(as.Date(usage_data$ended, na.rm = TRUE))
    )
  })
  
  # Reactive data filtered by content and date range
  filtered_usage <- reactive({
    usage_data %>%
      filter(
        (input$content_select == "all" | content_guid == input$content_select) &
          as.Date(started) >= input$date_range[1] &
          as.Date(ended) <= input$date_range[2]
      )
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
  
  # Reactive expression to filter by selected user
  filtered_user_usage <- reactive({
    req(input$user_select)
    filtered_usage() %>%
      filter(user_guid == input$user_select)
  })
  
  # 1. Bar chart: Users Logged Each Day (Returning vs New)
  output$users_logged_daily_plot <- renderPlotly({
    data <- filtered_usage() %>%
      mutate(date = as.Date(started)) %>%
      group_by(date, user_guid) %>%
      summarise(sessions = n()) %>%
      mutate(user_type = if_else(user_guid %in% unique(first(user_guid)), 
                                 "New", "Returning")) %>%
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
      summarise(first_date = min(as.Date(started, na.rm = TRUE))) %>%
      pull(first_date)
    
    # Handle empty or invalid dates
    # first_login <- ifelse(is.infinite(first_login), "No Data", first_login)
    
    valueBox(
      first_login,
      "First Login",
      icon = icon("calendar-alt"),
      color = "purple"
    )
  })
  
  
  ################################# App Mode Analysis #############################################
  
  # Reactive data for app_mode filtered by content selection and date range
  filtered_content <- reactive({
    content_data %>%
      filter(
        (input$content_select == "all" | guid == input$content_select) &
          guid %in% filtered_usage()$content_guid
      )
  })
  
  # Reactive for app_mode and sessions
  app_mode_data <- reactive({
    # Merge content and usage data to calculate total sessions by app_mode
    filtered_usage() %>%
      left_join(content_data, by = c("content_guid" = "guid")) %>%
      group_by(app_mode) %>%
      summarise(
        total_content = n_distinct(content_guid),
        total_sessions = n(),  # Count sessions
        avg_session_duration = mean(session_duration, na.rm = TRUE) / 60  # Convert to minutes
      )
  })
  
  # Populate the Dropdown with App Modes
  observe({
    # Extract unique app modes from the content data
    app_modes <- filtered_content() %>%
      pull(app_mode) %>%
      unique()
    
    # Update the dropdown options dynamically
    updateSelectInput(
      session,
      "app_mode_select",
      choices = app_modes,
      selected = app_modes[1]  # Default to the first app mode
    )
  })
  
  
  # 1. Donut Chart: Share of Content by App Mode
  output$app_mode_pie_chart <- renderPlotly({
    pie_data <- filtered_content() %>%
      count(app_mode) %>%
      mutate(percentage = n / sum(n) * 100)
    
    plot_ly(
      data = pie_data,
      labels = ~app_mode,
      values = ~n,
      type = "pie",
      hole = 0.4,
      textinfo = "label+percent",
      insidetextorientation = "radial"
    ) %>%
      layout(title = "Share of Content by App Mode")
  })
  
  # 2. Bar Chart: Number of Sessions by App Mode
  output$app_mode_bar_chart <- renderPlotly({
    bar_data <- filtered_usage() %>%
      left_join(content_data, by = c("content_guid" = "guid")) %>%
      group_by(app_mode) %>%
      summarise(sessions = n())
    
    plot_ly(
      data = bar_data,
      x = ~app_mode,
      y = ~sessions,
      type = "bar",
      text = ~sessions,
      textposition = "auto",
      name = "Sessions"
    ) %>%
      layout(
        title = "Number of Sessions by App Mode",
        xaxis = list(title = "App Mode"),
        yaxis = list(title = "Number of Sessions")
      )
  })
  
  # 3. Summary Table: Statistics for App Mode
  output$app_mode_summary_table <- renderDataTable({
    app_mode_data() %>%
      mutate(avg_session_duration = round(avg_session_duration / 60, 2)) %>%  # Convert to minutes
      rename(
        "App Mode" = app_mode,
        "Total Content Items" = total_content,
        "Total Sessions" = total_sessions,
        "Avg Session Duration (mins)" = avg_session_duration
      )
  })
  
  # 4. Users by App Mode
  # Reactive expression to filter users based on the selected app mode
  filtered_users_by_app_mode <- reactive({
    req(input$app_mode_select)
    
    # Filter content and usage data for the selected app mode
    relevant_content <- filtered_content() %>%
      filter(app_mode == input$app_mode_select)
    
    relevant_usage <- filtered_usage() %>%
      filter(content_guid %in% relevant_content$guid)
    
    # Get user information
    relevant_usage %>%
      left_join(users_data, by = c("user_guid" = "guid")) %>%
      select(username, email) %>%
      distinct()
  })
  
  # Render the table of users by app mode
  output$users_by_app_mode_table <- renderDataTable({
    filtered_users_by_app_mode() %>%
      rename(
        "Username" = username,
        "Email" = email
      )
  }, options = list(pageLength = 10, autoWidth = TRUE))
  
  
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