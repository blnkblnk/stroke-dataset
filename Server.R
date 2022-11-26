library(shiny)
library(tidyverse)
library(ggplot2)



dataset <- read.csv("data/healthcare-dataset-stroke-data.csv")
dataset$bmi = as.double(dataset$bmi)
  
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
  data_2 <- eventReactive (input$generate_2, {
    if (input$dataset_selection_2 == "Generated") {
      df <- data_1()
    } else {
      df <- dataset
    }
    df
  })
  output$boxplot_2 <- renderPlot({
    df <- data_2()
    df %>%
      ggplot() +
      geom_boxplot(aes(x=factor(stroke), y=bmi))
  })
  #eventReactive(input$dataset_selection_2, {
  #  
  #})
  output$stats <- renderTable({
    #summarize to get statistics like percent and count
    df <- data_1() %>%
      group_by(stroke) %>%
      summarize(count = n(),
                male = sum(gender == "Male")/n(),
                female = sum(gender == "Female")/n(),
                avg_age = mean(age),
                hypertension = sum(hypertension == 1)/n(),
                heart_disease = sum(heart_disease == 1)/n(),
                married = sum(ever_married == "Yes")/n(),
                work_priv = sum(work_type == "Private")/n(),
                work_gov = sum(work_type == "Govt_job")/n(),
                work_self = sum(work_type == "Self-employed")/n(),
                urban = sum(Residence_type == "Urban")/n(),
                glucose = mean(avg_glucose_level)/n(),
                bmi = mean(bmi),
                nonsmoker = sum(smoking_status == "never smoked")/n(),
                former_smoker = sum(smoking_status == "formerly smoked")/n(),
                smoker = sum(smoking_status == "smokes")/n(),
                )
    #transpose to make table vertical
    df <- df %>%
      gather(key = had_stroke, value = value, 2:ncol(df)) %>% 
      spread_(key = names(df)[1],value = 'value')
    df
  })
}
# shinyApp(ui = ui, server = server)