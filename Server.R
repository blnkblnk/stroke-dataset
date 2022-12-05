source("global.R")
library(shiny)
library(tidyverse)
library(ggplot2)

ui <- source('ui.R')$value

dataset <- read.csv("data/healthcare-dataset-stroke-data.csv")
#fix BMI as it is read as a string
dataset$bmi = as.double(dataset$bmi)
correct_data = function(binary_list) {
  map(binary_list, function(x) {
    if (x == 0) {
      return("No")
    } else {
      return("Yes")
    }
  }) %>%
    simplify()
}
dataset$stroke <- correct_data(dataset$stroke)
dataset$hypertension <- correct_data(dataset$hypertension)
dataset$heart_disease <- correct_data(dataset$heart_disease)

cont <- c("bmi", "avg_glucose_level")
disc <- c("gender", "age", "hypertension", "heart_disease", "ever_married", "work_type", "Residence_type", "smoking_status")

get_consts_transpose = function(dat) {
  df <- get_consts(dat) 
  #transpose to make table vertical
  #https://stackoverflow.com/questions/28917076/transposing-data-frames/28917212#28917212
  df <- df %>%
    gather(key = had_stroke, value = value, 2:ncol(df)) %>% 
    spread_(key = names(df)[1],value = 'value')
  df
}

get_consts = function(dat) {
  dat %>%
    group_by(stroke) %>%
    summarize(count = n(),
              per_male = sum(gender == "Male")/n(),
              per_female = sum(gender == "Female")/n(),
              avg_age = mean(age),
              per_hypertension = sum(hypertension == "Yes")/n(),
              per_heart_disease = sum(heart_disease == "Yes")/n(),
              per_married = sum(ever_married == "Yes")/n(),
              per_work_priv = sum(work_type == "Private")/n(),
              per_work_gov = sum(work_type == "Govt_job")/n(),
              per_work_self = sum(work_type == "Self-employed")/n(),
              per_urban = sum(Residence_type == "Urban")/n(),
              avg_glucose = mean(avg_glucose_level)/n(),
              avg_bmi = mean(bmi),
              per_nonsmoker = sum(smoking_status == "never smoked")/n(),
              per_former_smoker = sum(smoking_status == "formerly smoked")/n(),
              per_smoker = sum(smoking_status == "smokes")/n()
    ) 
}

server <- function(input, output, session) {
  
  data_1 <- eventReactive (input$generate_1, {
    #filter the dataset based on sidebar input
    dataset %>%
      filter(gender %in% input$gender_1) %>%
      filter(age > input$age_1[1]) %>%
      filter(age < input$age_1[2]) %>%
      filter(hypertension %in% input$hypertension_1) %>%
      filter(heart_disease %in% input$heart_disease_1) %>%
      filter(ever_married %in% input$married_1) %>%
      filter(work_type %in% input$work_1) %>%
      filter(Residence_type %in% input$residence_1) %>%
      filter(avg_glucose_level > input$glucose_1[1]) %>%
      filter(avg_glucose_level < input$glucose_1[2]) %>%
      filter(bmi > input$bmi_1[1]) %>%
      filter(bmi < input$bmi_1[2]) %>%
      filter(smoking_status %in% input$smoke_1)
  })
  output$stats1 <- renderTable({
    #summarize to get statistics like percent and count
    data_1() %>%
      get_consts_transpose()
  })
  #data used for 2nd tab
  data_2 <- eventReactive (c(input$generate_2, input$dataset_selection_2), {
    if (input$dataset_selection_2 == "Generated") {
      df <- data_1()
    } else {
      df <- dataset
    }
    df
  })
  #different possible x and y values for plot
  observe({
    pt <- input$plot_selection_2
    xchoices <- c("None")
    ychoices <- c("None")
    splitplot <- append(disc, "None")
    if (pt == "Box") {
      xchoices <- c("stroke")
      ychoices <- cont
    } else if (pt == "Scatter") {
      xchoices <- cont
      ychoices <- cont
    } else if (pt == "Grid") {
      xchoices <- c("stroke")
      ychoices <- disc
    } else if (pt == "Bar") {
      #add constants later
      xchoices <- c("stroke")
      ychoices <- data_2() %>%
        get_consts_transpose() %>%
        .[["had_stroke"]]
    }
    updateSelectInput(session, "xval_2",
                  choices = xchoices)
    updateSelectInput(session, "yval_2",
                  choices = ychoices)
  })
  #sample size
  size_2_generate <- eventReactive(input$generate_2,{
    nrow(data_2())
  })
  output$size_2 <- renderText({
    paste("Output Size: ", size_2_generate())
  })
  #generate plot
  plot_2_generate <- eventReactive(input$generate_2,{
    output <- NULL
    pt <- input$plot_selection_2
    xval <- input$xval_2
    yval <- input$yval_2
    if (pt == "Box") {
      ggplot_2 <- data_2() %>%
        ggplot(.)
      output <- ggplot_2 +
        geom_boxplot(aes(factor(.data[[xval]]), .data[[yval]]))
    } else if (pt == "Scatter") {
      ggplot_2 <- data_2() %>%
        ggplot(.)
      output <- ggplot_2 +
        geom_point(aes(.data[[xval]], .data[[yval]], colour = factor(stroke)), alpha = .3)
    } else if (pt == "Grid") {
      ggplot_2 <- data_2() %>%
        ggplot(.)
      output <- ggplot_2 +
        geom_bin_2d(aes(factor(.data[[xval]]), factor(.data[[yval]])))
    } else if (pt == "Bar") {
      ggplot_2 <- data_2() %>%
        filter(., !is.na(bmi)) %>%
        get_consts() %>%
        ggplot(.)
      output <- ggplot_2 +
        geom_bar(aes(.data[[xval]], .data[[yval]]), stat="identity")
    }
    output <- output +
      labs(x = xval, y = yval)
    return(output)
  })
  output$plot_2 <- renderPlot({
    plot_2_generate()
  })
  output$analysis_2 <- renderTable({
    test_output()
  }, digits = 4)
  test_output <- eventReactive(input$generate_2,{
    table_output <- NULL
    pt <- input$plot_selection_2
    xval <- input$xval_2
    yval <- input$yval_2
    if(pt == "Box") {
      test_results <- data_2() %>%
        t.test(.[[yval]] ~ .[[xval]], data = .)
      table_output <- data.frame(
        "t test output" = c("mean with stroke", "mean without stroke", "p-value"),
        "values" = c(test_results$estimate[[1]], test_results$estimate[[2]], test_results$p.value)
      )
    } else if (pt == "Grid") {
      dat <- data_2()
      test_results <- chisq.test(dat[[xval]], dat[[yval]])
      table_output <- data.frame(
        "Chi-squared test output" = c("X-squared", "df", "p-value"),
        "values" = c(test_results$statistic[[1]], test_results$parameter[[1]], test_results$p.value)
      )
    }
    return(table_output)
  })
  #download plot
  output$download_2 <- downloadHandler(
    "plot.png",
    function(file) {
      ggsave(file,plot_2_generate())
    }
  )
}