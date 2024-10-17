library(shiny)
library(shinydashboard)
library(apexcharter)
library(dplyr)
library(lubridate)
library(shinycssloaders)

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

# UI de l'application
ui <- dashboardPage(
  dashboardHeader(
    title = glue::glue("Shiny Usage - Last {days_back} Days"), titleWidth = "40%"
  ),
  dashboardSidebar(disable = TRUE, collapsed = TRUE),
  dashboardBody(
    fluidRow(
      shinycssloaders::withSpinner(
        apexchartOutput("shiny_time")
      )
    ),
    fluidRow(
      box(
        apexchartOutput("shiny_content"),
        width = 4,
      ),
      box(
        apexchartOutput("shiny_viewer"),
        width = 4,
      ),
      box(
        apexchartOutput("shiny_owner"),
        width = 4,
      )
    ),
    fluidRow(
      verbatimTextOutput("verbatim")
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

# Server de l'application
server <- function(input, output, session) {
  
  # Reactive values for time selection
  minTime <- reactiveVal(report_from)
  maxTime <- reactiveVal(report_to)
  
  # Observe user input for time range selection
  observeEvent(input$time, {
    input_min <- lubridate::as_date(input$time[[1]]$min)
    input_max <- lubridate::as_date(input$time[[1]]$max)
    if (identical(input_min, input_max)) {
      # treat "equals" as nothing selected
      minTime(report_from)
      maxTime(report_to)
    } else {
      minTime(input_min)
      maxTime(input_max)
    }
  })
  
  # Data prep with debounce for performance optimization
  delay_duration <- 500
  
  # Shiny content summary
  shiny_content <- debounce(reactive({
    shiny_rsc %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      group_by(content_guid) %>%
      tally() %>%
      left_join(
        shiny_rsc_titles %>% select(content_guid, content_name),
        by = "content_guid"
      ) %>%
      filter(!is.na(content_name)) %>%
      arrange(desc(n))
  }), delay_duration)
  
  # Viewers summary
  shiny_viewers <- debounce(reactive({
    shiny_rsc %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      group_by(user_guid) %>%
      tally() %>%
      left_join(
        all_users %>% select(guid, username),
        by = c(user_guid = "guid")
      ) %>%
      arrange(desc(n))
  }), delay_duration)
  
  # Owners summary
  shiny_owners <- debounce(reactive({
    shiny_rsc %>%
      safe_filter(min_date = minTime(), max_date = maxTime()) %>%
      left_join(
        shiny_rsc_titles %>% select(content_guid, content_name),
        by = "content_guid"
      ) %>%
      group_by(content_guid) %>%
      tally() %>%
      arrange(desc(n))
  }), delay_duration)
  
  # Shiny usage over time
  shiny_over_time <- debounce(reactive({
    shiny_rsc %>%
      mutate(
        date = lubridate::as_date(lubridate::floor_date(started, "day")),
      ) %>%
      group_by(date) %>%
      tally() %>%
      mutate(
        date_disp = format(date, format="%a %b %d %Y")
      ) %>%
      arrange(date)
  }), delay_duration)
  
  # Apexchart outputs
  output$shiny_time <- renderApexchart({
    apexchart(auto_update = FALSE) %>%
      ax_chart(type = "line") %>%
      ax_title("By Date") %>%
      ax_series(list(
        name = "Count",
        data = purrr::map2(shiny_over_time()$date_disp, shiny_over_time()$n, ~ list(.x, .y))
      )) %>%
      ax_xaxis(type = "datetime") %>%
      set_input_selection("time")
  })
  
  output$shiny_content <- renderApexchart({
    apex(
      data = shiny_content() %>% head(20),
      type = "bar",
      mapping = aes(content_name, n)
    ) %>%
      ax_title("By App (Top 20)") %>%
      set_input_click("content")
  })
  
  output$shiny_viewer <- renderApexchart({
    apex(
      data = shiny_viewers() %>% head(20),
      type = "bar",
      mapping = aes(username, n)
    ) %>%
      ax_title("By Viewer (Top 20)") %>%
      set_input_click("viewer")
  })
  
  output$shiny_owner <- renderApexchart({
    apex(
      data = shiny_owners() %>% head(20),
      type = "bar",
      mapping = aes(content_guid, n)
    ) %>%
      ax_title("By Owner (Top 20)") %>%
      set_input_click("owner")
  })
  
  # Display input values for debugging
  output$verbatim <- renderText({
    capture.output(str(input$time), str(minTime()), str(maxTime()), str(input$content), str(input$viewer), str(input$owner))
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
