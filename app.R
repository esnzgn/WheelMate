source("./sys_req.R")

# Simple user database
user_db <- data.frame(
  username = c("admin", "teacher1"),
  password = c("adminpass", "teachpass"),
  stringsAsFactors = FALSE
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "GroupSpin"),
  dashboardSidebar(
    uiOutput("sidebarUI")
  ),
  dashboardBody(
    uiOutput("bodyUI")
  )
)

# Server
server <- function(input, output, session) {
  user <- reactiveVal(NULL)
  
  observeEvent(input$login, {
    req(input$username, input$password)
    if (any(user_db$username == input$username & user_db$password == input$password)) {
      user(input$username)
    } else {
      showModal(modalDialog("Invalid username or password", easyClose = TRUE))
    }
  })
  
  output$sidebarUI <- renderUI({
    if (is.null(user())) {
      NULL
    } else {
      sidebarMenu(
        menuItem("Create Groups", tabName = "create"),
        menuItem("View Saved", tabName = "saved")
      )
    }
  })
  
  output$bodyUI <- renderUI({
    if (is.null(user())) {
      fluidPage(
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        actionButton("login", "Log in")
      )
    } else {
      tabItems(
        tabItem(tabName = "create",
                fluidPage(
                  textAreaInput("students", "Enter Student Names (one per line)", rows = 10),
                  numericInput("n_groups", "Number of Groups", value = 2, min = 2),
                  actionButton("make_groups", "Make Groups!"),
                  plotlyOutput("wheel_plot"),
                  uiOutput("group_cards"),
                  actionButton("save_groups", "Save Groups")
                )
        ),
        tabItem(tabName = "saved",
                fluidPage(
                  uiOutput("saved_files_ui"),
                  tableOutput("saved_groups_table")
                )
        )
      )
    }
  })
  
  groups <- reactiveVal(NULL)
  
  observeEvent(input$make_groups, {
    req(input$students)
    students <- unlist(strsplit(input$students, "\n"))
    students <- students[students != ""]
    shuffled <- sample(students)
    n <- length(shuffled)
    group_ids <- rep(1:input$n_groups, length.out = n)
    groups(data.frame(Student = shuffled, Group = group_ids))
  })
  
  output$group_cards <- renderUI({
    req(groups())
    groups_data <- groups()
    group_list <- split(groups_data$Student, groups_data$Group)
    fluidRow(
      lapply(seq_along(group_list), function(i) {
        box(
          title = paste("Group", i),
          solidHeader = TRUE,
          status = "primary",
          width = 4,
          collapsible = TRUE,
          lapply(group_list[[i]], function(student) {
            tags$p(student)
          })
        )
      })
    )
  })
  
  output$wheel_plot <- renderPlotly({
    req(groups())
    students <- groups()$Student
    slices <- length(students)
    angles <- seq(0, 2*pi, length.out = slices + 1)
    df <- data.frame(
      x = cos(angles),
      y = sin(angles),
      label = c(students, students[1])
    )
    plot_ly(
      data = df,
      type = 'scatter',
      mode = 'lines+text',
      x = ~x,
      y = ~y,
      text = ~label,
      textposition = "top center",
      line = list(shape = 'spline')
    ) %>% layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      showlegend = FALSE,
      title = "Spinning Wheel (Random)"
    )
  })
  
  observeEvent(input$save_groups, {
    req(groups())
    dir.create(file.path("saved_groups", user()), recursive = TRUE, showWarnings = FALSE)
    filename <- paste0("saved_groups/", user(), "/groups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    saveRDS(groups(), filename)
    showModal(modalDialog("Groups saved!", easyClose = TRUE))
  })
  
  output$saved_files_ui <- renderUI({
    req(user())
    files <- list.files(file.path("saved_groups", user()), full.names = FALSE)
    selectInput("saved_file", "Choose a saved grouping", choices = files)
  })
  
  output$saved_groups_table <- renderTable({
    req(input$saved_file)
    readRDS(file.path("saved_groups", user(), input$saved_file))
  })
}

shinyApp(ui, server)

