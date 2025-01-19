## Libraries

library(shiny)
library(ggplot2)
library(dplyr)

# Function for data experiment simulation
simulate_experiment <- function(funnels, N) {
  traffic_split <- sapply(funnels, function(x) x[2])
  observations <- matrix(0, nrow = length(funnels), ncol = length(funnels[[1]][[1]]))
  for (i in 1:N) {
    which_funnel <- sample(1:length(funnels), 1, prob = traffic_split)
    funnel_outcome <- sample(1:length(funnels[[which_funnel]][[1]]), 1, prob = funnels[[which_funnel]][[1]])
    observations[which_funnel, funnel_outcome] <- observations[which_funnel, funnel_outcome] + 1
  }
  return(observations)
}

# Perform interim analysis and store pvalue
experiment_episode <- function(funnels, N, prior_observations = NULL) {
  observations <- simulate_experiment(funnels, N)
  if (!is.null(prior_observations)) {
    observations <- observations + prior_observations
  }
  p_value <- chisq.test(observations)$p.value
  return(list(observations = observations, p_value = p_value))
}

# Function to simulate early stopping
early_stopping_simulation <- function(funnels, num_simulations, episodes, alphas) {
  hits <- 0
  for (i in 1:num_simulations) {
    observations <- NULL
    for (j in 1:length(episodes)) {
      result <- experiment_episode(funnels, episodes[j], observations)
      observations <- result$observations
      p_value <- result$p_value
      if (p_value <= alphas[j]) {
        hits <- hits + 1
        break
      }
    }
  }
  return(hits / num_simulations)
}

# Function to calculate FPR for each experiment
calculate_fpr <- function(funnels, episodes, num_experiments, alpha = 0.05) {
  fpr_values <- numeric(num_experiments)
  for (exp_id in 1:num_experiments) {
    hits <- 0
    for (i in 1:length(episodes)) {
      result <- experiment_episode(funnels, episodes[i])
      if (result$p_value <= alpha) {
        hits <- hits + 1
      }
    }
    fpr_values[exp_id] <- hits / length(episodes)
  }
  
  fpr_df <- data.frame(ExperimentID = 1:num_experiments, FPR = fpr_values)
  avg_fpr <- mean(fpr_values)
  fpr_df <- rbind(fpr_df, data.frame(ExperimentID = "Average", FPR = avg_fpr))
  
  return(fpr_df)
}

# UI layout
ui <- fluidPage(
  titlePanel("A/B Testing Simulation with Early Stopping"),
  tabsetPanel(
    tabPanel("What is early stopping?",
             HTML("<h3><strong>About Early Stopping in A/B Testing</strong></h3>"),
             p("In experiments it is essential not to look at the results too early. Doing so can lead to false conclusions, especially when tests are stopped prematurely based on preliminary data. This behavior is known as 'peeking' or 'early stopping', and it can lead to incorrect rejection of the null hypothesis."),
             p("Why can't we look at the results early? If you continuously check the results and stop the test when a result is significant, you're introducing bias. You’re increasing the likelihood of false positives, which can lead to a completely different conclusion"),
             p("Here are some methods for dealing with early stopping in A/B testing:"),
             tags$ul(
               tags$li(HTML("1. <strong>Use sequential testing techniques</strong>. There are methods such as the Pocock or O’Brien-Fleming boundaries that adjust the significance level as the experiment progresses.")),
               tags$li(HTML("2. <strong>Use really low alpha</strong>. It works like p1 but without any complicated math.")),
               tags$li(HTML("3. <strong>Don't use peaking</strong>. Avoid data-driven decisions until the test is finalized."))
             ),
             p("If you're interested, consider following resources:"),
             tags$ul(
               tags$li(a(href = "https://bytepawn.com/early-stopping-in-ab-testing.html", "Early stopping in A/B testing - Bytepawn")),
               tags$li(a(href = "https://engineering.atspotify.com/2023/03/choosing-sequential-testing-framework-comparisons-and-discussions/", "Choosing a Sequential Testing Framework - Spotify Engineering"))
             )
    ),
    tabPanel("How to Run Simulations",
             HTML("<h3><strong>Running the Simulation</strong></h3>"),
             p("In this app, you can simulate A/B tests with early stopping. Here's how you can use the tool:"),
             tags$ul(
               tags$li(HTML("1. <strong> Number of Simulations</strong>: This determines the number of episodes (samples) to be simulated for each experiment.")),
               tags$li(HTML("2. <strong> Number of Experiments</strong>: This defines how many distinct experiments are simulated, each with a different sample size.")),
               tags$li(HTML("3. <strong> Run Simulation</strong>: Click this button to run the simulations and generate results."))
             ),
             HTML("<h3><strong>Interpreting the Results</strong></h3>"),
             p("After running the simulation, you will see the following outputs:"),
             tags$ul(
               tags$li(HTML("1. <strong> P-value Plot</strong>: Shows the p-values for each experiment across different sample sizes. The red dashed line is the significance threshold (alpha = 0.05).")),
               tags$li(HTML("2. <strong> FPR Table</strong>: Displays the False Positive Rate (FPR) for each experiment and the average FPR across all experiments. A higher FPR indicates that early stopping was more likely to cause false positives."))
             ),
             p("The goal of these simulations is to understand the behavior of A/B tests under early stopping conditions. If you are running real experiments, make sure to follow the best practices to avoid invalid conclusions.")
    ),
    tabPanel("Simulation",
             sidebarLayout(
               sidebarPanel(
                 numericInput("num_simulations", "Number of simulations:", 3, min = 1, max = 10),
                 numericInput("num_experiments", "Number of experiments:", 1, min = 1, max = 20),
                 actionButton("run_simulation", "Run Simulation")
               ),
               mainPanel(
                 plotOutput("p_value_plot"),
                 tableOutput("fpr_table")
               )
             )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Observe the run_simulation button click
  observeEvent(input$run_simulation, {
    # Example funnels: Conversion rates and traffic split
    funnels <- list(
      list(c(0.05, 0.95), 0.5),  # Control group: 5% conversion rate, 50% of traffic
      list(c(0.07, 0.93), 0.5)   # Treatment group: 7% conversion rate, 50% of traffic
    )
    
    # Calculate the number of samples (episodes) based on number of simulations
    episodes <- seq(input$num_simulations * 100, input$num_simulations * 1000, by = 100)
    alphas <- rep(0.05, length(episodes))  # Significance level for A/B testing
    
    # Run the early stopping simulation
    fpr <- early_stopping_simulation(funnels, input$num_simulations, episodes, alphas)
    print("fpr")
    print(fpr)
    
    # Calculate FPR for each experiment
    fpr_df <- calculate_fpr(funnels, episodes, input$num_experiments)
    print("fpr_df")
    print(fpr_df)
    
    # Display the FPR table
    output$fpr_table <- renderTable({
      fpr_df
    })
    
    # Function to generate results for multiple experiments
    results_df_multiple_experiments <- function(funnels, episodes, num_experiments) {
      all_results <- list()
      for (exp_id in 1:num_experiments) {
        p_values <- numeric(length(episodes))
        for (i in 1:length(episodes)) {
          result <- experiment_episode(funnels, episodes[i])
          p_values[i] <- result$p_value
        }
        results_df <- data.frame(
          SampleSize = episodes,
          PValue = p_values,
          ExperimentID = exp_id
        )
        all_results[[exp_id]] <- results_df
      }
      combined_results_df <- do.call(rbind, all_results)
      return(combined_results_df)
    }
    
    # Generate the results data frame
    results_df <- results_df_multiple_experiments(funnels, episodes, input$num_experiments)
    
    # Check if data is generated correctly
    print(head(results_df))  # Print the first few rows for debugging
    
    # Plot the results for all experiments
    output$p_value_plot <- renderPlot({
      ggplot(results_df, aes(x = SampleSize, y = PValue, group = ExperimentID, color = factor(ExperimentID))) +
        geom_line() +
        geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
        labs(title = "P-value Trajectories with Early Stopping",
             x = "Sample Size", y = "P-value", color = "Experiment ID") +
        theme(legend.position = "right")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
