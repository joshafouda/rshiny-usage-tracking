library(shiny)
library(shinydashboard)
library(apexcharter)
library(dplyr)
library(lubridate)
library(shinycssloaders)
library(plotly)
library(tidyr)

##################################################################

# Set seed for reproducibility
set.seed(42)

# Generate 336 rows of data for shiny_rsc
content_guids <- c("guid_1", "guid_2", "guid_3", "guid_4", "guid_5")
user_guids <- c(paste0("user_", 1:20), rep(NA, 10))  # Mix of identified and anonymous users
content_names <- c("No-Code Modeling App", "Sales Dashboard", "Marketing Report", "Data Analysis Tool", "Executive Report")

# Date range (last 30 days)
end_date <- as_datetime("2024-10-16")
start_date <- end_date - days(30)

# Generate session data
shiny_rsc <- tibble(
  content_guid = sample(content_guids, 336, replace = TRUE),
  user_guid = sample(user_guids, 336, replace = TRUE),
  started = start_date + days(sample(0:30, 336, replace = TRUE)) +
    hours(sample(0:23, 336, replace = TRUE)) +
    minutes(sample(0:59, 336, replace = TRUE)),
  data_version = rep(1, 336)
)

# Calculate ended time and session_duration
shiny_rsc <- shiny_rsc %>%
  mutate(
    session_duration = sample(30:7200, 336, replace = TRUE),
    ended = started + seconds(session_duration)
  )

#View(shiny_rsc)

# Generate shiny_rsc_titles (GUIDs and human-readable names)
shiny_rsc_titles <- tibble(
  content_guid = content_guids,
  content_name = content_names
)

#View(shiny_rsc_titles)

# Perform the join
merged_df <- shiny_rsc %>%
  inner_join(shiny_rsc_titles, by = "content_guid")

# Display the merged dataset (you can use View(merged_df) in RStudio to inspect)
#View(merged_df)

# Vous avez déjà shiny_rsc, donc on le conserve tel quel.

# Simulons le dataframe content, qui représente l'usage des contenus statiques
content <- tibble(
  content_guid = sample(content_guids, 100, replace = TRUE),  # Les mêmes GUIDs que dans shiny_rsc
  user_guid = sample(user_guids, 100, replace = TRUE),  # Les mêmes users que dans shiny_rsc
  started = start_date + days(sample(0:30, 100, replace = TRUE)) +
    hours(sample(0:23, 100, replace = TRUE)) +
    minutes(sample(0:59, 100, replace = TRUE)),  # Simule les dates de sessions
  data_version = rep(1, 100),  # Version des données
  session_duration = sample(60:3600, 100, replace = TRUE)  # Durée des sessions entre 1 min et 1h
)

# Calcul de l'heure de fin de session pour 'content'
content <- content %>%
  mutate(ended = started + seconds(session_duration))

# Créons la liste data_arr avec shiny_rsc et content
data_arr <- list(
  shiny = shiny_rsc,   # Les données d'utilisation des apps Shiny
  content = content    # Les données d'utilisation des contenus statiques
)

# Liste des user_guids (guid d'utilisateurs) et noms d'utilisateurs (username)
#user_guids <- paste0("user_", 1:20)  # Les mêmes identifiants que ceux dans shiny_rsc

# Simuler des noms d'utilisateur cohérents
usernames <- paste0("user", 1:20, "_name")

# Simuler la table all_users pour les 20 utilisateurs identifiés
all_users <- tibble(
  guid = paste0("user_", 1:20),  # Identifiants d'utilisateur (guids) pour les utilisateurs identifiés
  username = usernames           # Noms d'utilisateur correspondants
)

#################################################################

# Paramètres pour la période de rapport simulée
days_back <- 30
report_from <- lubridate::today() - lubridate::ddays(days_back)
report_to <- lubridate::today()

# Simulated Dataframes
shiny_rsc <- shiny_rsc  # Dataframe pour l'usage Shiny
shiny_rsc_titles <- shiny_rsc_titles  # Dataframe pour les titres des apps Shiny
content <- content  # Static content usage data
all_users <- all_users  # Dataframe des utilisateurs


ui <- dashboardPage(
  dashboardHeader(
    title = glue::glue("Shiny Usage - Last {days_back} Days"), titleWidth = "40%"
  ),
  dashboardSidebar(disable = TRUE),  # Désactiver la barre latérale
  dashboardBody(
    inputPanel(
      selectInput(
        inputId = "content_filter",
        label = "Choose a content:",
        choices = c("all", shiny_rsc_titles$content_name),
        selected = "all"
      )
    ),
    tabsetPanel(
      tabPanel(
        title = "Overview",
        fluidRow(
          shinycssloaders::withSpinner(
            apexchartOutput("shiny_time")
          )
        ),
        fluidRow(
          box(
            apexchartOutput("shiny_content"),
            width = 4
          ),
          box(
            apexchartOutput("shiny_viewer"),
            width = 4
          ),
          box(
            apexchartOutput("shiny_owner"),
            width = 4
          )
        ),
        fluidRow(
          verbatimTextOutput("verbatim")
        )
      ),
      tabPanel(
        title = "General Stats",
        fluidRow(
          valueBoxOutput("unique_users", width = 2),   # # of Unique users (with Logins)
          valueBoxOutput("anonymous_users", width = 2),  # # of Anonymous Users
          valueBoxOutput("sessions_opened", width = 2),  # # of Sessions Opened
          valueBoxOutput("avg_time_spent", width = 3),   # Average Time Spent Daily
          valueBoxOutput("days_active", width = 3)    # # of Days Active
        ),
        fluidRow(
          box(
            title = "Unique Users / Opened Sessions Over Time",
            plotlyOutput("unique_vs_sessions"),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Average Session Time (hours)",
            plotlyOutput("avg_session_time"),
            width = 12
          )
        )
        # fluidRow(
        #   box(
        #     title = "Total Navigations and Inputs",
        #     plotlyOutput("total_navigations_inputs"),
        #     width = 12
        #   )
        # )
      ),
      
      tabPanel(
        title = "User specific stats",
        
        # First output: Stacked bar chart for Users logged each day
        fluidRow(
          box(
            title = "Users logged each day (Returning vs New)", 
            width = 12,
            apexchartOutput("users_logged_each_day") %>%
              shinycssloaders::withSpinner()
          )
        ),
        
        # Second output: Total users logged each hour
        # fluidRow(
        #   box(
        #     title = "Total users logged each hour", 
        #     width = 12,
        #     apexchartOutput("total_users_logged_each_hour") %>%
        #       shinycssloaders::withSpinner()
        #   )
        # )
        
        fluidRow(
          box(
            title = "Select User",
            selectInput(
              inputId = "user_filter",
              label = "Choose a user:",
              choices = NULL,  # Populated by server dynamically
              selected = NULL
            ),
            width = 12
          )
        ),
        fluidRow(
          valueBoxOutput("user_hours_spent", width = 3),
          valueBoxOutput("user_days_logged", width = 3),
          valueBoxOutput("user_actions_executed", width = 3),
          valueBoxOutput("user_first_login", width = 3)
        )
      )
      
    )
  )
)



# Helper function for date filtering
safe_filter <- function(data, min_date = NULL, max_date = NULL) {
  data_prep <- data
  if (!is.null(min_date)) {
    data_prep <- data_prep %>% filter(started >= min_date)
  }
  if (!is.null(max_date)) {
    data_prep <- data_prep %>% filter(started <= max_date + 1)
  }
  return(data_prep)
}

server <- function(input, output, session) {
  
  # Reactive values for time selection
  minTime <- reactiveVal(report_from)
  maxTime <- reactiveVal(report_to)
  
  # Reactive expression to filter based on selected content
  filtered_shiny <- reactive({
    if (input$content_filter == "all") {
      shiny_rsc
    } else {
      shiny_rsc %>%
        inner_join(shiny_rsc_titles, by = "content_guid") %>%
        filter(content_name == input$content_filter)
    }
  })
  
  # Data prep with debounce for performance optimization
  delay_duration <- 500
  
  # Reactive data for shiny content
  shiny_content <- debounce(reactive(
    filtered_shiny() %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      group_by(content_guid) %>%
      tally() %>%
      left_join(
        shiny_rsc_titles %>% select(content_guid, content_name),
        by = c(content_guid = "content_guid")
      ) %>%
      filter(!is.na(content_name)) %>%
      arrange(desc(n))
  ), delay_duration)
  
  # Reactive data for shiny viewers
  shiny_viewers <- debounce(reactive(
    filtered_shiny() %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      group_by(user_guid) %>%
      tally() %>%
      left_join(
        all_users %>% select(guid, username),
        by = c(user_guid = "guid")
      ) %>%
      arrange(desc(n))
  ), delay_duration)
  
  # Reactive data for shiny owners
  # Reactive data for shiny owners
  shiny_owners <- debounce(reactive(
    filtered_shiny() %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      left_join(
        shiny_rsc_titles %>% select(content_guid),  # Select the correct content_guid from shiny_rsc_titles
        by = c(content_guid = "content_guid")
      ) %>%
      filter(!is.na(content_guid)) %>%  # Ensure content_guid is not NA
      group_by(content_guid) %>%
      tally() %>%
      left_join(
        all_users %>% select(guid, username),
        by = c(content_guid = "guid")  # Join on content_guid and user_guid instead of content_name
      ) %>%
      arrange(desc(n))
  ), delay_duration)
  
  
  # Reactive data for shiny sessions over time
  shiny_over_time <- debounce(reactive(
    filtered_shiny() %>%
      mutate(
        date = lubridate::as_date(lubridate::floor_date(started, "day"))
      ) %>%
      group_by(date) %>%
      tally() %>%
      mutate(date_disp = format(date, format="%a %b %d %Y")) %>%
      arrange(date)
  ), delay_duration)
  
  # Render shiny time graph
  output$shiny_time <- renderApexchart(
    apexchart(auto_update = FALSE) %>%
      ax_chart(type = "line") %>%
      ax_title("By Date") %>%
      ax_plotOptions() %>%
      ax_series(list(
        name = "Count",
        data = purrr::map2(shiny_over_time()$date_disp, shiny_over_time()$n, ~ list(.x, .y))
      )) %>%
      ax_xaxis(type = "datetime") %>%
      set_input_selection("time")
  )
  
  # Render shiny content graph
  output$shiny_content <- renderApexchart(
    apex(
      data = shiny_content() %>% head(20), 
      type = "bar", 
      mapping = aes(content_name, n)
    ) %>%
      ax_title("By App (Top 20)") %>%
      set_input_click("content")
  )
  
  # Render shiny viewer graph
  output$shiny_viewer <- renderApexchart(
    apex(
      data = shiny_viewers() %>% head(20), 
      type = "bar", 
      mapping = aes(username, n)
    ) %>%
      ax_title("By Viewer (Top 20)") %>%
      set_input_click("viewer")
  )
  
  # Render shiny owner graph
  output$shiny_owner <- renderApexchart(
    apex(
      data = shiny_owners() %>% head(20), 
      type = "bar", 
      mapping = aes(username, n)
    ) %>%
      ax_title("By Owner (Top 20)") %>%
      set_input_click("owner")
  )
  
  output$verbatim <- renderText(capture.output(
    str(input$time), str(minTime()), str(maxTime()), str(input$content), str(input$viewer), str(input$owner)
  ))
  
  ####################### Sorties de General Stats #####################################
  # Server logic for the 5 valueBoxes in the "General Stats" tab
  
  # 1. Unique Users with Logins
  output$unique_users <- renderValueBox({
    unique_users <- filtered_shiny() %>%
      filter(!is.na(user_guid)) %>%
      distinct(user_guid) %>%
      nrow()
    
    valueBox(
      value = unique_users,
      subtitle = "# of Unique Users (with Logins)",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  # 2. Anonymous Users
  output$anonymous_users <- renderValueBox({
    anonymous_users <- filtered_shiny() %>%
      filter(is.na(user_guid)) %>%
      nrow()
    
    valueBox(
      value = anonymous_users,
      subtitle = "# of Anonymous Users",
      icon = icon("user-secret"),
      color = "yellow"
    )
  })
  
  # 3. Sessions Opened
  output$sessions_opened <- renderValueBox({
    sessions_opened <- filtered_shiny() %>%
      nrow()
    
    valueBox(
      value = sessions_opened,
      subtitle = "# of Sessions Opened",
      icon = icon("door-open"),
      color = "green"
    )
  })
  
  # 4. Average Time Spent Daily
  output$avg_time_spent <- renderValueBox({
    avg_time_spent <- filtered_shiny() %>%
      mutate(day = lubridate::floor_date(started, "day")) %>%
      group_by(day) %>%
      summarise(total_time = sum(session_duration, na.rm = TRUE)) %>%
      summarise(avg_time_per_day = mean(total_time, na.rm = TRUE)) %>%
      pull(avg_time_per_day)
    
    valueBox(
      value = prettyunits::pretty_sec(avg_time_spent),
      subtitle = "Average Time Spent Daily",
      icon = icon("clock"),
      color = "purple"
    )
  })
  
  # 5. Days Active
  output$days_active <- renderValueBox({
    days_active <- filtered_shiny() %>%
      mutate(day = lubridate::floor_date(started, "day")) %>%
      distinct(day) %>%
      nrow()
    
    valueBox(
      value = days_active,
      subtitle = "# of Days Active",
      icon = icon("calendar-alt"),
      color = "red"
    )
  })
  
  
  # 1. Unique Users / Opened Sessions Over Time
  output$unique_vs_sessions <- renderPlotly({
    plot_data <- filtered_shiny() %>%
      mutate(day = lubridate::floor_date(started, "day")) %>%
      group_by(day) %>%
      summarise(
        unique_users = n_distinct(user_guid, na.rm = TRUE),
        opened_sessions = n()
      ) %>%
      pivot_longer(cols = c(unique_users, opened_sessions), names_to = "metric", values_to = "count")
    
    if (nrow(plot_data) > 0) {
      plot_ly(plot_data, x = ~day, y = ~count, color = ~metric, type = 'bar') %>%
        layout(title = "Unique Users vs Opened Sessions Over Time",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Count"))
    }
  })
  
  # 2. Average Session Time (hours)
  output$avg_session_time <- renderPlotly({
    plot_data <- filtered_shiny() %>%
      mutate(day = lubridate::floor_date(started, "day")) %>%
      group_by(day) %>%
      summarise(
        avg_session_hours = mean(session_duration, na.rm = TRUE) / 3600
      )
    
    if (nrow(plot_data) > 0) {
      plot_ly(plot_data, x = ~day, y = ~avg_session_hours, type = 'bar') %>%
        layout(title = "Average Session Time (hours) Over Time",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Hours"))
    }
  })
  
  # 3. Total Navigations and Inputs
  # output$total_navigations_inputs <- renderPlotly({
  #   plot_data <- filtered_shiny() %>%
  #     mutate(day = lubridate::floor_date(started, "day")) %>%
  #     group_by(day) %>%
  #     summarise(
  #       total_navigations = sum(navigations, na.rm = TRUE),
  #       total_inputs = sum(inputs, na.rm = TRUE)
  #     ) %>%
  #     pivot_longer(cols = c(total_navigations, total_inputs), names_to = "metric", values_to = "count")
  #   
  #   if (nrow(plot_data) > 0) {
  #     plot_ly(plot_data, x = ~day, y = ~count, color = ~metric, type = 'bar') %>%
  #       layout(title = "Total Navigations and Inputs Over Time",
  #              xaxis = list(title = "Time"),
  #              yaxis = list(title = "Count"))
  #   }
  # })
  
  #################################### Sorties de Users specifics stats #####################
  
  # Helper function to classify users as new or returning
  classify_users <- function(df) {
    df %>%
      group_by(user_guid) %>%
      mutate(
        first_login = min(lubridate::floor_date(started, "day")),
        is_returning = if_else(started > first_login, "Returning", "New")
      ) %>%
      ungroup()
  }
  
  # Reactive data for users logged each day (reactive to content_filter)
  filtered_users_logged_each_day <- reactive({
    data <- filtered_shiny() %>%
      classify_users() %>%
      mutate(day = lubridate::floor_date(started, "day")) %>%
      group_by(day, is_returning) %>%
      summarize(count = n()) %>%
      arrange(day)
    
    return(data)
  })
  
  # Render bar chart for users logged each day (stacked bar chart)
  output$users_logged_each_day <- renderApexchart({
    apexchart() %>%
      ax_chart(type = "bar", stacked = TRUE) %>%
      ax_plotOptions(bar = list(horizontal = FALSE)) %>%
      ax_title("Users Logged Each Day (Returning vs New)") %>%
      ax_xaxis(type = "datetime", categories = filtered_users_logged_each_day()$day) %>%
      ax_series(list(
        name = "Returning Users",
        data = filtered_users_logged_each_day() %>%
          filter(is_returning == "Returning") %>%
          pull(count)
      )) %>%
      ax_series(list(
        name = "New Users",
        data = filtered_users_logged_each_day() %>%
          filter(is_returning == "New") %>%
          pull(count)
      )) %>%
      ax_yaxis(title = list(text = "Number of Users")) %>%
      ax_colors(c("#447099", "#E4572E"))
  })
  
  # Reactive data for total users logged each hour per day (reactive to content_filter)
  filtered_users_logged_each_hour <- reactive({
    filtered_shiny() %>%
      mutate(
        day = lubridate::floor_date(started, "day"),
        hour = lubridate::hour(started)
      ) %>%
      group_by(day, hour) %>%
      summarize(count = n()) %>%
      arrange(day, hour)
  })
  
  # Render heatmap for total users logged each hour per day
  # output$total_users_logged_each_hour <- renderApexchart({
  #   apexchart() %>%
  #     ax_chart(type = "heatmap") %>%
  #     ax_title("Total Users Logged Each Hour per Day") %>%
  #     ax_xaxis(type = "category", labels = list(format = "%b %d", rotate = -45)) %>%
  #     ax_yaxis(labels = list(formatter = JS("function(val) { return val + ':00'; }")), 
  #              title = list(text = "Hour of Day")) %>%
  #     ax_plotOptions(heatmap = list(
  #       shadeIntensity = 0.5,
  #       colorScale = list(
  #         ranges = list(
  #           list(from = 0, to = 0, color = "#f2f2f2"),
  #           list(from = 1, to = 5, color = "#99ccff"),
  #           list(from = 6, to = 10, color = "#0066cc"),  # Ensure comma is present here
  #           list(from = 11, to = 20, color = "#003399"), # Ensure comma is present here
  #           list(from = 21, to = 50, color = "#000066")
  #         )
  #       )
  #     )) %>%
  #     ax_series(list(
  #       name = "Users",
  #       data = purrr::map2(filtered_users_logged_each_hour()$day, 
  #                          filtered_users_logged_each_hour()$hour, 
  #                          filtered_users_logged_each_hour()$count, 
  #                          ~ list(x = .x, y = .y, z = .z))
  #     )) %>%
  #     ax_colors("#0066cc")
  # })
  
  # Populate selectInput choices with users filtered by content
  observe({
    updateSelectInput(
      session,
      inputId = "user_filter",
      choices = unique(filtered_shiny()$user_guid[!is.na(filtered_shiny()$user_guid)])
    )
  })
  
  # Reactive expression to filter by selected user
  filtered_user_data <- reactive({
    req(input$user_filter)
    filtered_shiny() %>%
      filter(user_guid == input$user_filter)
  })
  
  # Value box 1: Hours the user spent on the app
  output$user_hours_spent <- renderValueBox({
    total_hours <- filtered_user_data() %>%
      summarise(hours_spent = sum(session_duration) / 3600) %>%
      pull(hours_spent)
    
    valueBox(
      round(total_hours, 1), "Hours Spent",
      icon = icon("clock"),
      color = "purple"
    )
  })
  
  # Value box 2: Days the user logged
  output$user_days_logged <- renderValueBox({
    days_logged <- filtered_user_data() %>%
      summarise(days_logged = n_distinct(lubridate::floor_date(started, "day"))) %>%
      pull(days_logged)
    
    valueBox(
      days_logged, "Days Logged",
      icon = icon("calendar-alt"),
      color = "green"
    )
  })
  
  # Value box 3: Actions the user executed (Approximating as number of sessions opened)
  output$user_actions_executed <- renderValueBox({
    actions_executed <- filtered_user_data() %>%
      summarise(sessions_opened = n()) %>%
      pull(sessions_opened)
    
    valueBox(
      actions_executed, "Sessions Opened",
      icon = icon("mouse-pointer"),
      color = "blue"
    )
  })
  
  # Value box 4: Date of the user's first login
  output$user_first_login <- renderValueBox({
    first_login <- filtered_user_data() %>%
      summarise(first_login = min(started)) %>%
      pull(first_login)
    
    valueBox(
      as.character(lubridate::date(first_login)), "First Login",
      icon = icon("sign-in-alt"),
      color = "yellow"
    )
  })
  
}

# Lancer l'application
shinyApp(ui = ui, server = server)