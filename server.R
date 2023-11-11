source("segment.R")

library(shiny)
function(input, output, session){
 
  #retrieving data for segment1 and performing the segmentation
  credit_card <- read.csv("credit_card_transactions.csv")
  
  credit_card_data <- select(credit_card, cust_id, acct_id, card_no, 
                        transaction_date,transaction_amount, transaction_category)
  
  # Assuming 'transaction_date' is a character vector in the format "YYYY-MM-DD HH:MM:SS"
  credit_card_data$transaction_date <- as.POSIXct(credit_card_data$transaction_date, format = "%Y-%m-%d %H:%M:%S")
  
  credit_card_data$month <- lubridate::month(credit_card_data$transaction_date)
  credit_card_data$year <- lubridate::year(credit_card_data$transaction_date)
  
  # Step 2.1: Group on cardno, month, and category and get total month level purchase
  monthly_purchase <- credit_card_data %>%
    group_by(card_no, transaction_category, month, year) %>%
    summarize(total_monthly_purchase = sum(transaction_amount))
  
  # Step 2.2: Group on cardno and category and get the average of month level purchase from data in step 2.1
  avg_monthly_purchase <- monthly_purchase %>%
    group_by(card_no, transaction_category) %>%
    summarize(avg_purchase = mean(total_monthly_purchase))
  
  # Step 2.3: Convert step 2.2 into a feature matrix (using dcast)
  
  feature_matrix <- dcast(avg_monthly_purchase, card_no ~ transaction_category, value.var = "avg_purchase")
  
  # Replace NA values with zero
  feature_matrix[is.na(feature_matrix)] = 0
  feature_matrix
  
  #normalizing and standardizing the data 
  min_max_normalized_data <- as.data.frame(apply(feature_matrix, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  
  output$cluster1 <- renderPlot({
    #using the elbow method to find the optimal number of clusters
    wcss<-numeric(10)
    for (k in 1:10){
      kmeans_model<-kmeans(min_max_normalized_data, centers=k)
      wcss[k]<-kmeans_model$tot.withinss
    }
    plot(1:10, wcss, type = "b", pch=19, frame=FALSE, xlab = "Number of Clusters(k)", 
         ylab = "Within-cluster Sum of Squares(WCSS)")
    
  })
  
  
  # Step 2.4: 40 clusters on the feature matrix (exclude cardno when building it)
  library(cluster)
  set.seed(123)
  num_clusters <- 4
  kmeans_model <- kmeans(min_max_normalized_data, centers = num_clusters)
  
  
  # Create a new column "cluster" and assign cluster labels
  feature_matrix$num_cluster<-kmeans_model$cluster
  

  # Step 2.6: For each cluster, calculate the number of card customers, and average of other columns
  cluster_summary <- feature_matrix %>%
    group_by(num_cluster) %>%
    summarize(
      no_of_card_customers = n(),
      avg_purchase_category_accomodation = mean(accomodation),
      avg_purchase_category_bill_payments = mean(bill_payments),
      avg_purchase_category_cosmetics = mean(cosmetics),
      avg_purchase_category_electronics = mean(electronics),
      avg_purchase_category_entertaiment = mean(entertaiment),
      avg_purchase_category_fashion = mean(fashion),
      avg_purchase_category_food_kiosk = mean(food_kiosk),
      avg_purchase_category_fuel = mean(fuel)
    )
  cluster_summary
  # Display the summary table using DT::renderDataTable
  output$summary1 <- DT::renderDataTable({
    datatable(cluster_summary, options = list(
      columnDefs = list(list(targets = "_all", className = "dt-center"))
    ))
  })
  # Naming clusters based on total spending per cluster
  cluster_summary <- cluster_summary %>%
    mutate(
      cluster_name = case_when(
        num_cluster == 3 ~ "Highest Spenders",
        num_cluster == 4 ~ "High Spenders",
        num_cluster == 1 ~ "Medium spenders",
        num_cluster == 2 ~ "Low Spenders"
      )
    )
  output$cluster_pie1 <- renderPlotly({
    library(plotly)
    pie_chart <- plot_ly(cluster_summary, labels = ~paste(cluster_name),
                         values = ~no_of_card_customers, type = "pie",
                         textinfo = "label+percent",
                         textposition = "inside", insidetextfont = list(color = '#FFFFFF'))
    
    pie_chart %>% layout(title = "Number of Customers per Cluster")
  })
  
  #barplot1# Create the bar plot
  output$barplot1 <- renderPlotly({
  barplot <- plot_ly(data = cluster_summary, x = ~cluster_name) %>%
    add_trace(y = ~avg_purchase_category_accomodation, name = "Accommodation", type = "bar") %>%
    add_trace(y = ~avg_purchase_category_bill_payments, name = "Bill Payments", type = "bar") %>%
    add_trace(y = ~avg_purchase_category_cosmetics, name = "Cosmetics", type = "bar") %>%
    add_trace(y = ~avg_purchase_category_electronics, name = "Electronics", type = "bar") %>%
    add_trace(y = ~avg_purchase_category_entertaiment, name = "Entertainment", type = "bar") %>%
    add_trace(y = ~avg_purchase_category_fashion, name = "Fashion", type = "bar") %>%
    add_trace(y = ~avg_purchase_category_food_kiosk, name = "Food Kiosk", type = "bar") %>%
    add_trace(y = ~avg_purchase_category_fuel, name = "Fuel", type = "bar") %>%
    layout(
      title = "Average Transaction Amount by Category",
      xaxis = list(title = "Cluster"),
      yaxis = list(title = "Average Amount")
    )
  
  # Show the plot
  barplot
  })
  
  ##the second segment visualizations
  debit_card <- read.csv("debit_card_transactions.csv")
  
  
  debit_card_data <- select(debit_card, cust_id, acct_id, card_no, 
                            transaction_date,transaction_amount, transaction_category)
  
  # Assuming 'transaction_date' is a character vector in the format "YYYY-MM-DD HH:MM:SS"
  debit_card_data$transaction_date <- as.POSIXct(debit_card_data$transaction_date, format = "%Y-%m-%d %H:%M:%S")
  
  debit_card_data$month <- lubridate::month(debit_card_data$transaction_date)
  debit_card_data$year <- lubridate::year(debit_card_data$transaction_date)
  
  #Step2
  # Step 2.1: Group on cardno, month, and category and get total month level purchase
  monthly_purchase <- debit_card_data %>%
    group_by(card_no, transaction_category, month, year) %>%
    summarize(total_monthly_purchase = sum(transaction_amount))
  
  # Step 2.2: Group on cardno and category and get the average of month level purchase from data in step 2.1
  avg_monthly_purchase <- monthly_purchase %>%
    group_by(card_no, transaction_category) %>%
    summarize(avg_purchase = mean(total_monthly_purchase))
  
  # Step 2.3: Convert step 2.2 into a feature matrix (using dcast)
  
  feature_matrix <- dcast(avg_monthly_purchase, card_no ~ transaction_category, value.var = "avg_purchase")
  
  # Replace NA values with zero
  feature_matrix[is.na(feature_matrix)] = 0
  
  #normalizing and standardizing the data 
  min_max_normalized_data <- as.data.frame(apply(feature_matrix, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  
  
  output$cluster2 <- renderPlot({
    #using the elbow method to find the optimal number of clusters
    wcss<-numeric(10)
    for (k in 1:10){
      kmeans_model<-kmeans(min_max_normalized_data, centers=k)
      wcss[k]<-kmeans_model$tot.withinss
    }
    plot(1:10, wcss, type = "b", pch=19, frame=FALSE, xlab = "Number of Clusters(k)", 
         ylab = "Within-cluster Sum of Squares(WCSS)")
    
  })
  
  # Step 2.4: 40 clusters on the feature matrix (exclude cardno when building it)
  library(cluster)
  set.seed(123)
  num_clusters <- 4
  kmeans_model <- kmeans(min_max_normalized_data, centers = num_clusters)
  
  
  # Step 2.5: Attach cluster number back to min_max_normalized_data
  feature_matrix$num_clusters <- kmeans_model$cluster
  
  
  # Step 2.6: For each cluster, calculate the number of card customers, and average of other columns
  cluster_summary2 <- feature_matrix %>%
    group_by(num_clusters) %>%
    summarize(
      no_of_card_customers = n(),
      avg_purchase_category_accomodation = mean(accomodation),
      avg_purchase_category_bill_payments = mean(bill_payments),
      avg_purchase_category_cosmetics = mean(cosmetics),
      avg_purchase_category_electronics = mean(electronics),
      avg_purchase_category_entertaiment = mean(entertaiment),
      avg_purchase_category_fashion = mean(fashion),
      avg_purchase_category_food_kiosk = mean(food_kiosk),
      avg_purchase_category_fuel = mean(fuel),
      avg_purchase_category_restaurent=mean(restaurent),
      avg_purchase_category_salon_spa=mean(salon_spa),
      avg_purchase_category_retail_outlet=mean(retail_outlet)
    )
  cluster_summary2
  
  #Calculate the number of card customers, and average of other columns
  cluster_summary2 <- feature_matrix %>%
    group_by(num_clusters) %>%
    summarize(
      no_of_card_customers = n(),
      avg_purchase_category_accomodation = mean(accomodation),
      avg_purchase_category_bill_payments = mean(bill_payments),
      avg_purchase_category_cosmetics = mean(cosmetics),
      avg_purchase_category_electronics = mean(electronics),
      avg_purchase_category_entertaiment = mean(entertaiment),
      avg_purchase_category_fashion = mean(fashion),
      avg_purchase_category_food_kiosk = mean(food_kiosk),
      avg_purchase_category_fuel = mean(fuel),
      avg_purchase_category_restaurent=mean(restaurent),
      avg_purchase_category_salon_spa=mean(salon_spa),
      avg_purchase_category_retail_outlet=mean(retail_outlet)
    )
  cluster_summary2
  #aggregating the clusters 
  cluster_summary2 <- cluster_summary2 %>%
    mutate(
      total_spending = avg_purchase_category_accomodation +
        avg_purchase_category_bill_payments +
        avg_purchase_category_cosmetics +
        avg_purchase_category_electronics +
        avg_purchase_category_entertaiment +
        avg_purchase_category_fashion +
        avg_purchase_category_food_kiosk +
        avg_purchase_category_fuel +
        avg_purchase_category_retail_outlet +
        avg_purchase_category_salon_spa +
        avg_purchase_category_restaurent
    )
  cluster_summary2
  
  #naming the 4 clusters according to their total spending
  cluster_summary2 <- cluster_summary2 %>%
    mutate(
      cluster_name = case_when(
        num_clusters == 4 ~ "Top Spenders",
        num_clusters == 1 ~ "High Spenders",
        num_clusters == 3 ~ "Medium spenders",
        num_clusters == 2 ~ "Low spenders"
      ))
  cluster_summary2
  
  #cluster summary table
  output$summary2 <- DT::renderDataTable({
    datatable(cluster_summary2, options = list(
      columnDefs = list(list(targets = "_all", className = "dt-center"))
    ))
  })
  
  
  #plot for the data points in the segment 
  output$cluster_pie2 <- renderPlotly({
    library(plotly)
    pie_chart <- plot_ly(cluster_summary2, labels = ~paste(cluster_name),
                         values = ~no_of_card_customers, type = "pie",
                         textinfo = "label+percent",
                         textposition = "inside", insidetextfont = list(color = '#FFFFFF'))
    
    pie_chart %>% layout(title = "Number of Customers per Cluster")
    
  })
  
  
  
  ###the third segment visualizations####
  core_data<- read.csv("core_transactions.csv")
  internet_banking_data <- select(core_data, cust_id, trans_date, amount, service)
  
  internet_banking_data<-subset(internet_banking_data, service=="internet_banking")
  str(internet_banking_data)
  
  # Assuming 'transaction_date' is a character vector in the format "YYYY-MM-DD"
  internet_banking_data$trans_date <- as.POSIXct(internet_banking_data$trans_date, format = "%Y-%m-%d")
  
  internet_banking_data$month <- lubridate::month(internet_banking_data$trans_date)
  internet_banking_data$year <- lubridate::year(internet_banking_data$trans_date)
  
  internet_banking_data
  
  # Calculate average transaction amount and total number of transactions for each customer
  customer_summary <- internet_banking_data%>%
    group_by(cust_id,year, service) %>%
    summarize(avg_internet_transactions = mean(amount),
              total_transactions = n())
  customer_summary
  
  # Perform min-max scaling for the features
  numeric_cols<-customer_summary[,c("avg_internet_transactions", "total_transactions")]
  
  
  numeric_cols_normalized <- as.data.frame(apply(numeric_cols, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  
  output$cluster3 <- renderPlot({
    # elbow method to find optimal clusters
    wcss <- numeric(10)
    for (k in 1:10) {
      kmeans_model <- kmeans(numeric_cols_normalized, centers = k)
      wcss[k] <- kmeans_model$tot.withinss
    }
    
    plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", 
         ylab = "Within-cluster Sum of Squares (WCSS)")
    
  })
  
  
  # running the kmeans algorithmn 
  library(cluster)
  set.seed(123)
  No_clusters <- 4
  kmeans_model3 <- kmeans(numeric_cols_normalized, centers = No_clusters)
  kmeans_model3
  
  
  # Step 2.5: Attach cluster number back to min_max_normalized_data
  customer_summary$No_clusters <- kmeans_model3$cluster
  str(customer_summary)
  
  # Create a data frame with the cluster summaries
  cluster_summary3 <- customer_summary %>%
    group_by(No_clusters) %>%
    summarize(
      no_of_card_customers = n(),
      avg_internet_transactions = mean(avg_internet_transactions),
      count_transactions = sum(total_transactions))
  str(cluster_summary3)
  
  #naming the clusters based on the number of transactions per cluster
  cluster_summary3_naming <- cluster_summary3 %>%
    mutate(
      cluster_name = case_when(
        No_clusters == 1 ~"Moderate internet banking users",
        No_clusters == 2 ~"High internet banking users",
        No_clusters == 3 ~"Lowest internet banking users",
        No_clusters == 4 ~"Highest internet banking users"
      )
    )
  cluster_summary3_naming
  
  # Display the summary table using DT::renderDataTable
  output$summary3 <- DT::renderDataTable({
    datatable( cluster_summary3_naming, options = list(
      columnDefs = list(list(targets = "_all", className = "dt-center"))
    ))
  })
  
  output$cluster_pie3 <- renderPlotly({
    #plotting a piechart showing number of customers per cluster
    library(plotly)
    pie_chart3 <- plot_ly(cluster_summary3_naming, labels = ~paste(cluster_name),
                          values = ~count_transactions, type = "pie",
                          textinfo = "label+percent",
                          textposition = "inside", insidetextfont = list(color = '#FFFFFF'))
    
    pie_chart3 %>% layout(title = "Number of Customers per Cluster")
    
    
  })
  
  
  ##the fourth segment####
  
  customer_demographics<- read.csv("savings_accounts.csv")
  customer_demographics <- select(customer_demographics, cust_id, gender, date_of_birth, 
           occupation,state, education, monthly_income)
 
  #calculating the age of the customers
  customer_demographics$Age <- year(today()) - year(customer_demographics$date_of_birth)
  customer_demographics$date_of_birth <- customer_demographics$age
  
  # Encoding my data
  encoded_data <- model.matrix(~gender + education + occupation -1, customer_demographics)
  encoded_data
  
  #scaling the numerical data
  numeric_cols1 <- customer_demographics[, c("Age", "monthly_income")]
  
  min_max_normalized4 <- as.data.frame(apply(numeric_cols1, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  
  #binding the columns
  binded_customer_demographics <- bind_cols(min_max_normalized4, encoded_data)
  binded_customer_demographics
  
  output$cluster4 <- renderPlot({
    #clustering using kmeans
    wcss <- numeric(10)
    for (k in 1:10) {
      kmeans_model <- kmeans(binded_customer_demographics, centers = k)
      wcss[k] <- kmeans_model$tot.withinss
    }
    
    plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", 
         ylab = "Within-cluster Sum of Squares (WCSS)")
    
  })
  
  #kmeans clustering
  library(cluster)
  set.seed(123)
  num_clusters <- 4
  kmeans_model4 <- kmeans(min_max_normalized4, centers = num_clusters)
  
  #attaching the clusters to the dataset
  customer_demographics$num_clusters <- kmeans_model4$cluster
  customer_demographics
  
  # Create a data frame with the cluster summaries
  cluster_summary4 <- customer_demographics %>%
    group_by(num_clusters)
  str(cluster_summary4)
  
  # Calculate the number of customers per cluster
  customers_per_cluster <- cluster_summary4 %>%
    count(num_clusters)
  
  # Join customers_per_cluster with cluster_summary4
  cluster_summary4 <- left_join(cluster_summary4, customers_per_cluster, by = "num_clusters")
  
  
  #Calculate the total monthly income per cluster
  total_income_per_cluster <- cluster_summary4 %>%
    group_by(num_clusters) %>%
    summarise(total_income = sum(monthly_income, na.rm = TRUE))
  
  # Calculate the average monthly income per cluster
  average_income_per_cluster <- cluster_summary4 %>%
    group_by(num_clusters) %>%
    summarise(average_income = mean(monthly_income, na.rm = TRUE))
  
  
  
  #naming the clusters based on the average monthly income per cluster
  cluster_summary_naming <- cluster_summary4 %>%
    mutate(
      cluster_name = case_when(
        num_clusters == 1 ~"Medium Earners",
        num_clusters == 2 ~"High Earners",
        num_clusters == 3 ~"Top Earners",
        num_clusters == 4 ~"Least Earners"
      )
    )
  
  
  output$cluster_pie4 <- renderPlotly({
    library(plotly)
    pie_chart <- plot_ly(cluster_summary_naming, labels = ~paste(cluster_name),
                         values = ~n, type = "pie",
                         textinfo = "label+percent",
                         textposition = "inside", insidetextfont = list(color = '#FFFFFF'))
    
    pie_chart %>% layout(title = "Number of Customers per Cluster")
  })
  
  output$summary4 <- renderPlot({
    #histogram of customer distribution per cluster
    ungrouped_df <- cluster_summary_naming %>% ungroup()
    
    # Create a histogram for each cluster
    histograms <- ungrouped_df %>%
      ggplot(aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
      facet_wrap(~ cluster_name, ncol = 3) +
      labs(x = "Age", y = "Frequency", title = "Age Distribution by Cluster") +
      theme_minimal()
    
    # Print the histograms
    print(histograms)
    
    
  })
 
  
  ########End of day balance segment################3
  EOD_balance<- read.csv("eod_balance.csv")
  EOD_balance <- select(EOD_balance, cust_id, trans_date, amount, balance)
  str( EOD_balance)
  
  EOD_balance$trans_date <- as.POSIXct(EOD_balance$trans_date, format = "%Y-%m-%d")
  
  EOD_balance$month <- lubridate::month(EOD_balance$trans_date)
  EOD_balance$year <- lubridate::year(EOD_balance$trans_date)
  
  # isolating the columns to be normalized and doing the normalization
  numeric_cols4 <- EOD_balance[,c("balance", "amount")]
  normalized4 <- as.data.frame(apply(numeric_cols4, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  
  output$cluster5 <- renderPlot({
    #visual plots for the clusters 
    #clustering using kmeans
    wcss <- numeric(10)
    for (k in 1:10) {
      kmeans_model <- kmeans(normalized4, centers = k)
      wcss[k] <- kmeans_model$tot.withinss
    }
    
    plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", 
         ylab = "Within-cluster Sum of Squares (WCSS)")
    #from the above plot, the optimal clusters is 3
  })
  
  
  # Import the necessary libraries
  library(cluster)
  
  clusters <- 3
  # Run the k-means clustering algorithm
  kmeans_model <- kmeans(normalized4, centers = clusters)
  
  # Get the cluster labels
  cluster_labels <- kmeans_model$cluster
  
  # Combine the cluster labels with the dataset
  EOD_balance <- cbind(EOD_balance, cluster_labels)
  str(EOD_balance)
  #performing cluster summary
  # Group the data by the cluster labels
  EOD_balance <- EOD_balance %>% group_by(cluster_labels)
  
  # Summarize the data within each cluster
  EOD_balance_summary <- EOD_balance %>% summarize(
    count = n(),
    mean_balance = mean(balance)
  )
  
  #naming the clusters according to the mean balance
  EOD_balance_naming <- EOD_balance_summary %>%
    mutate(
      cluster_name = case_when(
        cluster_labels == 1 ~"Low Balance",
        cluster_labels == 2 ~"High Balance",
        cluster_labels == 3 ~"Medium Balance"
      )
    )
  output$summary5 <- DT::renderDataTable({
    datatable(EOD_balance_naming, options = list(
      columnDefs = list(list(targets = "_all", className = "dt-center"))
    ))
  })
  
  output$cluster_pie5 <- renderPlotly({
    #piechart for the datapoints in the cluster summary
    library(plotly)
    # Create a pie chart
    pie_chart <- plot_ly(
      data = EOD_balance_naming,
      labels = ~cluster_name,
      values = ~count,
      type = "pie",
      textinfo = "label+percent"
    )
    
    # Add a title to the pie chart
    pie_chart <- pie_chart %>%
      layout(title = "Cluster Distribution")
    
    # Display the pie chart
    pie_chart
    
  })
  
  output$EOD_bar1 <- renderPlot({
    #bar graph of customer balance per cluster 
    bar_graph <- ggplot(EOD_balance_naming, aes(x = cluster_name, y = mean_balance, label = scales::comma(mean_balance))) +
      geom_bar(stat = "identity", fill = "green") +
      geom_text(aes(label = scales::comma(mean_balance)), vjust = -0.5) +  # Add hover text labels
      labs(
        x = "Cluster Name",
        y = "Mean Customer Balance",
        title = "Customer Balance per Cluster"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    
    # Display the bar graph
    bar_graph
    
  })
  
}