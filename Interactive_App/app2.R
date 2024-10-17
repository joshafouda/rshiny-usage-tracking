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
        title = "Example",
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
        fluidRow(
          box(
            title = "Total users logged each hour", 
            width = 12,
            apexchartOutput("total_users_logged_each_hour") %>%
              shinycssloaders::withSpinner()
          )
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
  
  output$users_logged_each_day <- renderApexchart({
    
    # Data preparation for stacked bar chart (Returning vs New)
    logged_users <- shiny_rsc %>%
      mutate(day = lubridate::floor_date(started, "day")) %>%  # Round to the day
      group_by(user_guid) %>%
      summarize(first_login_day = min(day)) %>%
      ungroup() %>%
      right_join(shiny_rsc %>% mutate(day = lubridate::floor_date(started, "day")), by = "user_guid") %>%
      mutate(user_type = if_else(day == first_login_day, "New User", "Returning User")) %>%
      group_by(day, user_type) %>%
      summarise(user_count = n()) %>%
      tidyr::pivot_wider(names_from = user_type, values_from = user_count, values_fill = list(user_count = 0))
    
    # Create the stacked bar chart
    apex(
      data = logged_users,
      type = "bar",
      mapping = aes(x = day)
    ) %>%
      ax_series(list(
        name = "New User",
        data = logged_users$`New User`
      )) %>%
      ax_series(list(
        name = "Returning User",
        data = logged_users$`Returning User`
      )) %>%
      ax_chart(stacked = TRUE) %>%
      ax_xaxis(type = "datetime", title = list(text = "Date"), labels = list(format = "dd MMM yyyy")) %>%
      ax_yaxis(title = list(text = "Number of Users")) %>%
      ax_title(text = "Users logged each day (Returning vs New)")
  })
  
  output$total_users_logged_each_hour <- renderApexchart({
    
    # Data preparation for hourly total users logged
    hourly_users <- shiny_rsc %>%
      mutate(day = lubridate::floor_date(started, "day"), hour = lubridate::hour(started)) %>%
      group_by(day, hour) %>%
      summarise(total_users = n()) %>%
      arrange(day, hour)
    
    # Create bar chart for users logged each hour
    apex(
      data = hourly_users,
      type = "bar",
      mapping = aes(x = day, y = total_users, fill = as.factor(hour))
    ) %>%
      ax_xaxis(type = "datetime", title = list(text = "Date"), labels = list(format = "dd MMM yyyy")) %>%
      ax_yaxis(title = list(text = "Total Users Logged")) %>%
      ax_title(text = "Total users logged each hour") %>%
      ax_legend(position = "right")
  })
  
}

# Lancer l'application
shinyApp(ui = ui, server = server)