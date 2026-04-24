library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

ties_df <- readRDS("ties_df.rds")

ig_choices <- sort(unique(as.character(ties_df$igType2)))


party_colors <- c(
  "PS"             = "#E31B23",  # red
  "Vert·e·s"       = "#84BD00",  # green
  "Centre"         = "#FF8800",  # warm orange
  "Vert'libéral"   = "#BDD437",  # olive green
  "PLR"            = "#003D85",  # blue
  "UDC"            = "#008947"   # dark green
)

selected_parties_default <- names(party_colors)

ui <- fluidPage(
  titlePanel("Chapter 5: MPs' ties to interest groups, 1992-2020"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput(
        inputId = "year_range",
        label = "Time period",
        min = 1992,
        max = 2020,
        value = c(1992, 2020),
        sep = ""
      ),
      radioButtons(
        inputId = "measure_type",
        label = "Measure",
        choices = c(
          "Average number of ties per MP-year" = "average",
          "Total number of ties" = "total"
        ),
        selected = "average"
      ),
      radioButtons(
        inputId = "line_grouping",
        label = "Separate lines by",
        choices = c(
          "None (all ties together)" = "none",
          "Interest Group Types" = "igType2",
          "Parliamentary Party Groups" = "ParlGroupName"
        ),
        selected = "none"
      ),
      uiOutput("facet_by_ui"),
      uiOutput("line_selector"),
      uiOutput("facet_selector"),
      checkboxInput(
        inputId = "free_y",
        label = "Use separate y-axis scale in each facet",
        value = TRUE
      ),
      downloadButton("download_plot", "Download plot"),
      br(), br(),
      helpText(
        "You can show one overall line, or separate lines by interest-group type or parliamentary party group. You can also facet by the other dimension."
      )
    ),
    mainPanel(
      width = 9,
      plotOutput("ties_plot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    req(is.data.frame(ties_df))
    req(all(c("year", "igType2", "ParlGroupName2", "PersonNumber") %in% names(ties_df)))
  })
  
  output$facet_by_ui <- renderUI({
    req(input$line_grouping)
    
    facet_choices <- switch(
      input$line_grouping,
      "none" = c(
        "None" = "none",
        "Interest Group Types" = "igType2",
        "Parliamentary Party Groups" = "ParlGroupName"
      ),
      "igType2" = c(
        "None" = "none",
        "Parliamentary Party Groups" = "ParlGroupName"
      ),
      "ParlGroupName" = c(
        "None" = "none",
        "Interest Group Types" = "igType2"
      )
    )
    
    radioButtons(
      inputId = "facet_by",
      label = "Facet by",
      choices = facet_choices,
      selected = "none"
    )
  })
  
  output$line_selector <- renderUI({
    req(input$line_grouping)
    
    if (input$line_grouping == "igType2") {
      checkboxGroupInput(
        inputId = "selected_lines",
        label = "Interest Group Types to show as lines",
        choices = ig_choices,
        selected = ig_choices
      )
    } else if (input$line_grouping == "ParlGroupName") {
      checkboxGroupInput(
        inputId = "selected_lines",
        label = "Parliamentary Party Groups to show as lines",
        choices = selected_parties_default,
        selected = selected_parties_default
      )
    } else {
      NULL
    }
  })
  
  output$facet_selector <- renderUI({
    req(input$facet_by)
    
    if (input$facet_by == "igType2") {
      checkboxGroupInput(
        inputId = "selected_facets",
        label = "Interest Group Types to display as facets",
        choices = ig_choices,
        selected = ig_choices
      )
    } else if (input$facet_by == "ParlGroupName") {
      checkboxGroupInput(
        inputId = "selected_facets",
        label = "Parliamentary Party Groups to display as facets",
        choices = selected_parties_default,
        selected = selected_parties_default
      )
    } else {
      NULL
    }
  })
  
  filtered_data <- reactive({
    req(input$year_range, input$measure_type, input$line_grouping, input$facet_by)
    
    data <- ties_df %>%
      mutate(
        year = as.integer(year),
        igType2 = as.character(igType2),
        ParlGroupName2 = as.character(ParlGroupName2),
        PersonNumber = as.character(PersonNumber)
      ) %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    
    base_mps <- data %>%
      distinct(PersonNumber, year, ParlGroupName2)
    
    ties_count <- data %>%
      filter(!is.na(igType2))
    
    if (input$line_grouping == "none" && input$facet_by == "none") {
      
      mp_level <- base_mps %>%
        left_join(
          ties_count %>%
            count(PersonNumber, year, ParlGroupName2, name = "n_ties_mp"),
          by = c("PersonNumber", "year", "ParlGroupName2")
        ) %>%
        mutate(n_ties_mp = replace_na(n_ties_mp, 0))
      
      if (input$measure_type == "average") {
        mp_level %>%
          group_by(year) %>%
          summarise(value = mean(n_ties_mp), .groups = "drop")
      } else {
        mp_level %>%
          group_by(year) %>%
          summarise(value = sum(n_ties_mp), .groups = "drop")
      }
      
    } else if (input$line_grouping == "none" && input$facet_by == "igType2") {
      
      req(input$selected_facets)
      req(length(input$selected_facets) > 0)
      
      mp_level <- base_mps %>%
        crossing(igType2 = input$selected_facets) %>%
        left_join(
          ties_count %>%
            filter(igType2 %in% input$selected_facets) %>%
            count(PersonNumber, year, ParlGroupName2, igType2, name = "n_ties_mp"),
          by = c("PersonNumber", "year", "ParlGroupName2", "igType2")
        ) %>%
        mutate(
          igType2 = factor(igType2, levels = input$selected_facets),
          n_ties_mp = replace_na(n_ties_mp, 0)
        )
      
      if (input$measure_type == "average") {
        mp_level %>%
          group_by(year, igType2) %>%
          summarise(value = mean(n_ties_mp), .groups = "drop")
      } else {
        mp_level %>%
          group_by(year, igType2) %>%
          summarise(value = sum(n_ties_mp), .groups = "drop")
      }
      
    } else if (input$line_grouping == "none" && input$facet_by == "ParlGroupName") {
      
      req(input$selected_facets)
      req(length(input$selected_facets) > 0)
      
      mp_level <- base_mps %>%
        filter(ParlGroupName2 %in% input$selected_facets) %>%
        mutate(
          ParlGroupName2 = factor(ParlGroupName2, levels = input$selected_facets)
        ) %>%
        left_join(
          ties_count %>%
            count(PersonNumber, year, ParlGroupName2, name = "n_ties_mp"),
          by = c("PersonNumber", "year", "ParlGroupName2")
        ) %>%
        mutate(n_ties_mp = replace_na(n_ties_mp, 0))
      
      if (input$measure_type == "average") {
        mp_level %>%
          group_by(year, ParlGroupName2) %>%
          summarise(value = mean(n_ties_mp), .groups = "drop")
      } else {
        mp_level %>%
          group_by(year, ParlGroupName2) %>%
          summarise(value = sum(n_ties_mp), .groups = "drop")
      }
      
    } else if (input$line_grouping == "igType2" && input$facet_by == "none") {
      
      req(input$selected_lines)
      req(length(input$selected_lines) > 0)
      
      mp_level <- base_mps %>%
        crossing(igType2 = input$selected_lines) %>%
        left_join(
          ties_count %>%
            filter(igType2 %in% input$selected_lines) %>%
            count(PersonNumber, year, ParlGroupName2, igType2, name = "n_ties_mp"),
          by = c("PersonNumber", "year", "ParlGroupName2", "igType2")
        ) %>%
        mutate(
          igType2 = factor(igType2, levels = input$selected_lines),
          n_ties_mp = replace_na(n_ties_mp, 0)
        )
      
      if (input$measure_type == "average") {
        mp_level %>%
          group_by(year, igType2) %>%
          summarise(value = mean(n_ties_mp), .groups = "drop")
      } else {
        mp_level %>%
          group_by(year, igType2) %>%
          summarise(value = sum(n_ties_mp), .groups = "drop")
      }
      
    } else if (input$line_grouping == "igType2" && input$facet_by == "ParlGroupName") {
      
      req(input$selected_lines, input$selected_facets)
      req(length(input$selected_lines) > 0, length(input$selected_facets) > 0)
      
      base_mps_subset <- base_mps %>%
        filter(ParlGroupName2 %in% input$selected_facets)
      
      mp_level <- base_mps_subset %>%
        crossing(igType2 = input$selected_lines) %>%
        left_join(
          ties_count %>%
            filter(
              igType2 %in% input$selected_lines,
              ParlGroupName2 %in% input$selected_facets
            ) %>%
            count(PersonNumber, year, ParlGroupName2, igType2, name = "n_ties_mp"),
          by = c("PersonNumber", "year", "ParlGroupName2", "igType2")
        ) %>%
        mutate(
          ParlGroupName2 = factor(ParlGroupName2, levels = input$selected_facets),
          igType2 = factor(igType2, levels = input$selected_lines),
          n_ties_mp = replace_na(n_ties_mp, 0)
        )
      
      if (input$measure_type == "average") {
        mp_level %>%
          group_by(year, ParlGroupName2, igType2) %>%
          summarise(value = mean(n_ties_mp), .groups = "drop")
      } else {
        mp_level %>%
          group_by(year, ParlGroupName2, igType2) %>%
          summarise(value = sum(n_ties_mp), .groups = "drop")
      }
      
    } else if (input$line_grouping == "ParlGroupName" && input$facet_by == "none") {
      
      req(input$selected_lines)
      req(length(input$selected_lines) > 0)
      
      mp_level <- base_mps %>%
        filter(ParlGroupName2 %in% input$selected_lines) %>%
        mutate(
          ParlGroupName2 = factor(ParlGroupName2, levels = input$selected_lines)
        ) %>%
        left_join(
          ties_count %>%
            count(PersonNumber, year, ParlGroupName2, name = "n_ties_mp"),
          by = c("PersonNumber", "year", "ParlGroupName2")
        ) %>%
        mutate(n_ties_mp = replace_na(n_ties_mp, 0))
      
      if (input$measure_type == "average") {
        mp_level %>%
          group_by(year, ParlGroupName2) %>%
          summarise(value = mean(n_ties_mp), .groups = "drop")
      } else {
        mp_level %>%
          group_by(year, ParlGroupName2) %>%
          summarise(value = sum(n_ties_mp), .groups = "drop")
      }
      
    } else if (input$line_grouping == "ParlGroupName" && input$facet_by == "igType2") {
      
      req(input$selected_lines, input$selected_facets)
      req(length(input$selected_lines) > 0, length(input$selected_facets) > 0)
      
      base_mps_subset <- base_mps %>%
        filter(ParlGroupName2 %in% input$selected_lines)
      
      mp_level <- base_mps_subset %>%
        crossing(igType2 = input$selected_facets) %>%
        left_join(
          ties_count %>%
            filter(
              ParlGroupName2 %in% input$selected_lines,
              igType2 %in% input$selected_facets
            ) %>%
            count(PersonNumber, year, ParlGroupName2, igType2, name = "n_ties_mp"),
          by = c("PersonNumber", "year", "ParlGroupName2", "igType2")
        ) %>%
        mutate(
          ParlGroupName2 = factor(ParlGroupName2, levels = input$selected_lines),
          igType2 = factor(igType2, levels = input$selected_facets),
          n_ties_mp = replace_na(n_ties_mp, 0)
        )
      
      if (input$measure_type == "average") {
        mp_level %>%
          group_by(year, igType2, ParlGroupName2) %>%
          summarise(value = mean(n_ties_mp), .groups = "drop")
      } else {
        mp_level %>%
          group_by(year, igType2, ParlGroupName2) %>%
          summarise(value = sum(n_ties_mp), .groups = "drop")
      }
      
    }
  })
  
  y_axis_label <- reactive({
    if (input$measure_type == "average") {
      "Average number of ties per MP-year"
    } else {
      "Total number of ties"
    }
  })
  
  base_plot <- reactive({
    plot_data <- filtered_data()
    req(nrow(plot_data) > 0)
    
    if (input$line_grouping == "none" && input$facet_by == "none") {
      
      ggplot(plot_data, aes(x = year, y = value)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_x_continuous(breaks = pretty_breaks(n = 4)) +
        labs(
          x = NULL,
          y = y_axis_label()
        ) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (input$line_grouping == "none" && input$facet_by == "igType2") {
      
      ggplot(plot_data, aes(x = year, y = value, group = 1)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        facet_wrap(
          ~ igType2,
          scales = if (isTRUE(input$free_y)) "free_y" else "fixed"
        ) +
        scale_x_continuous(breaks = pretty_breaks(n = 4)) +
        labs(
          x = NULL,
          y = y_axis_label()
        ) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (input$line_grouping == "none" && input$facet_by == "ParlGroupName") {
      
      ggplot(plot_data, aes(x = year, y = value, group = 1)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        facet_wrap(
          ~ ParlGroupName2,
          scales = if (isTRUE(input$free_y)) "free_y" else "fixed"
        ) +
        scale_x_continuous(breaks = pretty_breaks(n = 4)) +
        labs(
          x = NULL,
          y = y_axis_label()
        ) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (input$line_grouping == "igType2" && input$facet_by == "none") {
      
      ggplot(plot_data, aes(x = year, y = value, color = igType2, group = igType2)) +
        geom_line(linewidth = 0.9, alpha = 0.9) +
        geom_point(size = 1.8, alpha = 0.9) +
        scale_x_continuous(breaks = pretty_breaks(n = 4)) +
        labs(
          x = NULL,
          y = y_axis_label(),
          color = "Interest group type"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (input$line_grouping == "igType2" && input$facet_by == "ParlGroupName") {
      
      ggplot(plot_data, aes(x = year, y = value, color = igType2, group = igType2)) +
        geom_line(linewidth = 0.9, alpha = 0.9) +
        geom_point(size = 1.8, alpha = 0.9) +
        facet_wrap(
          ~ ParlGroupName2,
          scales = if (isTRUE(input$free_y)) "free_y" else "fixed"
        ) +
        scale_x_continuous(breaks = pretty_breaks(n = 4)) +
        labs(
          x = NULL,
          y = y_axis_label(),
          color = "Interest group type"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (input$line_grouping == "ParlGroupName" && input$facet_by == "none") {
      
      ggplot(plot_data, aes(x = year, y = value, color = ParlGroupName2, group = ParlGroupName2)) +
        geom_line(linewidth = 0.9, alpha = 0.9) +
        geom_point(size = 1.8, alpha = 0.9) +
        scale_x_continuous(breaks = pretty_breaks(n = 4)) +
        scale_color_manual(
          values = party_colors,
          breaks = input$selected_lines,
          limits = input$selected_lines,
          drop = FALSE
        ) +
        labs(
          x = NULL,
          y = y_axis_label(),
          color = "Parliamentary party group"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (input$line_grouping == "ParlGroupName" && input$facet_by == "igType2") {
      
      ggplot(plot_data, aes(x = year, y = value, color = ParlGroupName2, group = ParlGroupName2)) +
        geom_line(linewidth = 0.9, alpha = 0.9) +
        geom_point(size = 1.8, alpha = 0.9) +
        facet_wrap(
          ~ igType2,
          scales = if (isTRUE(input$free_y)) "free_y" else "fixed"
        ) +
        scale_x_continuous(breaks = pretty_breaks(n = 4)) +
        scale_color_manual(
          values = party_colors,
          breaks = input$selected_lines,
          limits = input$selected_lines,
          drop = FALSE
        ) +
        labs(
          x = NULL,
          y = y_axis_label(),
          color = "Parliamentary party group"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
  })
  
  output$ties_plot <- renderPlot({
    base_plot()
  }, res = 120)
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(
        "ties_plot_",
        input$measure_type, "_",
        input$line_grouping, "_",
        input$facet_by, "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = base_plot(),
        width = 13,
        height = 8,
        dpi = 300
      )
    }
  )
}

shinyApp(ui, server)
