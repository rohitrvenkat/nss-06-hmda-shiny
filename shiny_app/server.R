library(tidyverse)
library(shiny)
library(fresh)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(shinyDataFilter)
library(plotly)
library(patchwork)
library(leafpop)
library(shinycssloaders)

shinyServer(function(input, output) {
  
  ### VISUALIZE TAB ###
  
  # Render leaflet map
  output$mymap <- renderLeaflet({
    draw_base_map()
  })
  
  # Create scope reactive value
  rv <- reactiveValues(scope = "state")
  
  # Update scope based on zoom level 
  observeEvent(input$mymap_zoom, {
    rv$scope <- check_zoom(input$mymap_zoom)
  })
  
  # Filter census data based on scope
  my_sf <- reactive({
    census_data %>%
      filter(scope == rv$scope)
  })
  
  # Update choropleth based on scope
  observeEvent(c(rv$scope, input$chor_vars), {
    update_choropleth("mymap", my_sf(), input$chor_vars)
  })
  
  # Update map legend based on scope
  observeEvent(c(my_sf(), input$chor_vars), {
    draw_map_legend("mymap", my_sf(), input$chor_vars)
  })

  # HMDA data filter for visualize tab
  visualize_filter <- callModule(
    shiny_data_filter,
    "visualize_filter",
    data = hmda_data,
    verbose = FALSE)
  
  # Toggle filter panel based on filter button click
  observeEvent(input$filterButton, {
    toggle('filterPanel')
  })
  
  observeEvent(input$mymap_shape_click, { 
    showModal(modalDialog(
      plotlyOutput("census_plot_race"),
      title = input$mymap_shape_click[1],
      fade = F,
      size = "l",
      easyClose = T,
      footer = NULL
    ))
  })
  
  output$census_plot_race <- renderPlotly({
    p <- hmda_census_race_data %>%
      filter(NAME == input$mymap_shape_click[1]) %>%
      group_by(Group) %>%
      mutate(Proportion = Population/sum(Population)) %>%
      filter(!(Race %in% c("Free Form Text Only", "Joint", "Other Race", "Race Not Available"))) %>%
      ggplot(aes(x = str_wrap(Race, width = 17), 
                 y = Proportion, 
                 fill = Group,
                 text = sprintf("Group: %s<br>Race: %s<br>Percent: %s",
                                Group, Race, scales::percent(Proportion, scale = 100, accuracy = 0.01)
                                )
                 )
             ) +
      geom_col(position = 'dodge') +
      scale_x_discrete(limits = rev) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Proportion of Applicant Race by Comparison Group",
           x = "Race")
    ggplotly(p, tooltip = 'text')
  })
  
  ### COMPARE TAB ###
  
  # HMDA data filter 1 for compare tab
  compare_filter_1 <- callModule(
    shiny_data_filter,
    "compare_filter_1",
    data = hmda_data,
    verbose = FALSE)
  
  # HMDA data filter 2 for compare tab
  compare_filter_2 <- callModule(
    shiny_data_filter,
    "compare_filter_2",
    data = hmda_data,
    verbose = FALSE)
  
  observeEvent(input$go, {
    
    output$plot_amounts <- renderPlotly({
      isolate(
        p2 <- ggplot() +
          geom_boxplot(data = compare_filter_1(), aes(x = "Group 1", y = `Loan Amount`, color = "Group 1")) +
          geom_boxplot(data = compare_filter_2() %>% setdiff(compare_filter_1()), aes(x = "Group 2", y = `Loan Amount`, color = "Group 2")) +
          scale_y_log10() +
          labs(title = "Boxplot of Comparison Group Loan Amounts",
               x = "Comparison Group")
      )
      ggplotly(p2)
    })
    
    output$plot_action <- renderPlotly({
      isolate(
        data <- rbind(compare_filter_1() %>% mutate(Group = "Group 1"),
                      compare_filter_2() %>% setdiff(compare_filter_1()) %>% mutate(Group = "Group 2")) %>%
          count(Group, `Action Taken`) %>%
          group_by(Group) %>%
          mutate(Proportion = n/sum(n))
      )
      p <- ggplot(data = data, aes(x = str_wrap(`Action Taken`, width = 12), 
                                   y = Proportion, 
                                   fill = Group, 
                                   text = sprintf("Group: %s<br>Action Taken: %s<br>Percent: %s",
                                                  Group, 
                                                  `Action Taken`, 
                                                  scales::percent(Proportion, scale = 100, accuracy = 0.01)
                                   )
      )
      )+
        geom_col(position = 'dodge')+
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "right") +
        labs(title = "Proportion of Action Taken by Comparison Group",
             x = "Action Taken")
      
      ggplotly(p, height = 600, tooltip = 'text')
    })
    
    output$plot_race <- renderPlotly({
      isolate(
        data <- rbind(compare_filter_1() %>% mutate(Group = "Group 1"),
                      compare_filter_2() %>% setdiff(compare_filter_1()) %>% 
                        mutate(Group = "Group 2")) %>%
          filter(Race != "Free Form Text Only") %>%
          count(Group, Race) %>%
          group_by(Group) %>%
          mutate(Proportion = n/sum(n))
      )
      p <- ggplot(data = data, aes(x = str_wrap(Race, width = 17), 
                                   y = Proportion, 
                                   fill = Group,
                                   text = sprintf("Group: %s<br>Race: %s<br>Percent: %s",
                                                  Group, 
                                                  Race, 
                                                  scales::percent(Proportion, scale = 100, accuracy = 0.01)
                                   )
      )
      ) +
        geom_col(position = 'dodge') +
        scale_x_discrete(limits = rev) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Proportion of Applicant Race by Comparison Group",
             x = "Race")
      figr <- ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", xanchor = "right", yanchor = "top", x = 1, y = 1))
    })
    
    output$plot_ethnicity <- renderPlotly({
      isolate(
        data <- rbind(compare_filter_1() %>% mutate(Group = "Group 1"),
                      compare_filter_2() %>% setdiff(compare_filter_1()) %>% mutate(Group = "Group 2")) %>%
          filter(Ethnicity != "Free Form Text Only") %>%
          count(Group, Ethnicity) %>%
          group_by(Group) %>%
          mutate(Proportion = n/sum(n))
      )
      p <- ggplot(data = data, aes(x = Ethnicity, 
                                   y = Proportion, 
                                   fill = Group,
                                   text = sprintf("Group: %s<br>Ethnicity: %s<br>Percent: %s",
                                                  Group, 
                                                  Ethnicity, 
                                                  scales::percent(Proportion, scale = 100, accuracy = 0.01)
                                   )
      )
      ) +
        geom_col(position = 'dodge') +
        scale_x_discrete(limits = rev) +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "none") +
        labs(title = "Proportion of Applicant Ethnicity by Comparison Group")
      fige <- ggplotly(p, tooltip = 'text')
    })
    
    output$plot_sex <- renderPlotly({
      isolate(
        data <- rbind(compare_filter_1() %>% mutate(Group = "Group 1"),
                      compare_filter_2() %>% setdiff(compare_filter_1()) %>% mutate(Group = "Group 2")) %>%
          count(Group, Sex) %>%
          group_by(Group) %>%
          mutate(Proportion = n/sum(n))
      )
      p <- ggplot(data = data, aes(x = Sex, 
                                   y = Proportion, 
                                   fill = Group,
                                   text = sprintf("Group: %s<br>Sex: %s<br>Percent: %s",
                                                  Group, 
                                                  Sex, 
                                                  scales::percent(Proportion, scale = 100, accuracy = 0.01)
                                   )
      )
      ) +
        geom_col(position = 'dodge') +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "none") +
        labs(title = "Proportion of Applicant Sex by Comparison Group")
      figs <- ggplotly(p, tooltip = 'text')
    })
    
    output$plot_age <- renderPlotly({
      isolate(
        data <- rbind(compare_filter_1() %>% mutate(Group = "Group 1"),
                      compare_filter_2() %>% setdiff(compare_filter_1()) %>% mutate(Group = "Group 2")) %>%
          count(Group, `Applicant Age`) %>%
          group_by(Group) %>%
          mutate(Proportion = n/sum(n))
      )
      p <- ggplot(data = data, aes(x = `Applicant Age`, 
                                   y = Proportion, 
                                   fill = Group,
                                   text = sprintf("Group: %s<br>Applicant Age: %s<br>Percent: %s",
                                                  Group, 
                                                  `Applicant Age`, 
                                                  scales::percent(Proportion, scale = 100, accuracy = 0.01)
                                   )
      )
      ) +
        geom_col(position = 'dodge') +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "none") +
        labs(title = "Proportion of Applicant Age by Comparison Group")
      figa <- ggplotly(p, tooltip = 'text')
    })
    
    # Proportions of Denial Reasons by Filter Group
    output$plot_denial <- renderPlotly({
      isolate(
        data <- rbind(compare_filter_1() %>% mutate(Group = "Group 1"),
                      compare_filter_2() %>% setdiff(compare_filter_1()) %>% mutate(Group = "Group 2")) %>%
          count(Group, `Denial Reason #1`) %>%
          group_by(Group) %>%
          mutate(Proportion = n/sum(n))
      )
      p <- ggplot(data = data, aes(x = str_wrap(`Denial Reason #1`, width = 10), 
                                   y = Proportion, 
                                   fill = Group, 
                                   text = sprintf("Group: %s<br>Denial Reason: %s<br>Percent: %s",
                                                  Group, 
                                                  `Denial Reason #1`, 
                                                  scales::percent(Proportion, scale = 100, accuracy = 0.01)
                                   )
      )
      ) +
        geom_col(position = 'dodge')+
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "right") +
        labs(title = "Proportion of Denial Reason by Comparison Group",
             x = "Denial Reason")
      
      ggplotly(p, height = 600, tooltip = 'text')
    })
    
  })

  ### EXPORT TAB ###
  
  # HMDA data filter for export tab
  export_filter <- callModule(
    shiny_data_filter,
    "export_filter",
    data = hmda_data,
    verbose = FALSE)
  
  # Render datatable for export tab
  output$data_table <- renderDT(
    server = TRUE, {
    datatable(
      export_filter(),
      style = 'bootstrap',
      rownames = FALSE,
      selection = "none",
      extensions = c("Buttons"),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'lrtip',
        buttons = list(
          list(
            extend = 'collection',
            buttons = c('columnsToggle'),
            text = 'Columns'
          )
        )
      )
    )
  })
  
  # Download handler for export tab
  output$download <- downloadHandler(
    filename = 'download.csv', 
    content = function(file) {
      write.csv(export_filter(), file, row.names = FALSE)
  })
})
