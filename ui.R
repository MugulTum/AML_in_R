library(shiny)
library(shinydashboard)
library(ggvis)
library(plotly)
library(DT)

source("segment.R")

dashboardPage(
  dashboardHeader(title = "Anti-Money Laundering project", titleWidth = 650),
  dashboardSidebar(
    # Menu items
    sidebarMenu(id = "sidebar",
                menuItem(text = "Credit Card Segment", tabName = "credit-card", icon = icon("credit-card"),
                         menuSubItem("Summary of the clusters", tabName = "cluster_summaries", icon = icon("address-card")),
                         menuSubItem("Filtering the clusters", tabName = "cluster_filters", icon = icon("address-card"))),
                menuItem(text = "Debit Card Segment", tabName = "debit-card", icon = icon("credit-card"),
                         menuSubItem("summary of debit card clusters", tabName = "debit_cluster_summary", icon=icon("address-card")),
                         menuSubItem("filtering the debit clusters", tabName = "debit_filters", icon = icon("address-card"))),
                menuItem(text = "Internet Banking Segment", tabName = "Internet_Banking", icon = icon("globe"),
                         menuSubItem("Internet Banking cluster summary", tabName = "internet_banking", icon = icon("address-card")),
                         menuSubItem("Internet banking filters", tabName = "internet_filters", icon = icon("address-card"))),
                menuItem(text = "Demographics Segment", tabName = "demographics", icon = icon("users"),
                         menuSubItem("Demographic cluster summaries", tabName = "demographic_summary", icon = icon("address-card")),
                         menuSubItem("demographic filters", tabName = "demographic_filter", icon = icon("address-card"))),
                menuItem(text = "EOD Balance Segment", tabName = "EOD-balance", icon = icon("chart-line"),
                         menuSubItem("EOD cluster summary", tabName = "EOD_summary", icon = icon("address-card")),
                         menuSubItem("EOD filters", tabName = "EOD_filter", icon=icon("address-card")))
    )
  ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "cluster_summaries", 
                tabBox(id="t1", width = 12,
                       tabPanel("Description", tabName = "description-content1", h2("Credit card segmentation"),
                                p("This project starts with the loading of the credit card transactions data")),
                       
                       tabPanel("Clusters", icon = icon("sitemap"), plotOutput("cluster1")),
                       tabPanel("Data Points", icon = icon("circle"), plotlyOutput("cluster_pie1")),
                       tabPanel("Cluster Summary", icon = icon("table"), DTOutput("summary1")))),
        tabItem(tabName = "cluster_filters",
                tabBox(id = "t11", width = 12,
                       tabPanel("credit segment barplot", icon = icon("chart-simple"), plotlyOutput("barplot1")),
                       tabPanel("filter barplot", icon = icon("chart-simple"), h2("filter barplot")))),
        tabItem(tabName = "debit_cluster_summary",
                tabBox(id = "t2", width = 12,
                       tabPanel("description2", tabName = "description-content2", h2("Debit card segment"),
                                p("debit card segment starts with the following")),
                       tabPanel("clusters2", icon = icon("sitemap"), plotOutput("cluster2")),
                       tabPanel("Data points2", icon = icon("circle"), plotlyOutput("cluster_pie2")),
                       tabPanel("cluster summary2", icon = icon("table"), DTOutput("summary2")))),
        tabItem(tabName = "debit_filters",
                tabBox(id="t21", width = 12,
                       tabPanel("debit segment barplot", icon = icon("chart-simple"), h2("debit_bar")),
                       tabPanel("debit segment barplot2", icon = icon("chart-simple"), h2("debit_bar2")))),
        tabItem(tabName = "internet_banking",
                tabBox(id = "t3", width = 12,
                       tabPanel("description3", tabName = "description-content3", h2("internet banking"),
                                p("internet banking segment")),
                       tabPanel("clusters3", icon = icon("sitemap"), plotOutput("cluster3")),
                       tabPanel("data points3", icon = icon("circle"), plotlyOutput("cluster_pie3")),
                       tabPanel("cluster_summary3", icon = icon("table"), DTOutput("summary3")))),
        tabItem(tabName = "internet_filters",
                tabBox(id="t31", width = 12,
                       tabPanel("internet barplot1", icon = icon("chart-simple"), h2("internet_bar1")),
                       tabPanel("internet barplot2", icon = icon("chart-simple"), h2("internet_bar2")))),
        tabItem(tabName = "demographic_summary",
                tabBox(id = "t4", width = 12,
                       tabPanel("description4", tabName = "description-content4", h2("demographic stuff"),
                                p("demographic segment is about the customer's unique features")),
                       tabPanel("clusters4", icon = icon("sitemap"), plotOutput("cluster4")),
                       tabPanel("data points4", icon = icon("circle"), plotlyOutput("cluster_pie4")),
                       tabPanel("cluster_summary4", icon = icon("table"), plotOutput("summary4")))),
        tabItem(tabName = "demographic_filter",
                tabBox(id="t41", width = 12,
                       tabPanel("demographic barplot", icon = icon("chart-simple"), h2("demographic barplot1")),
                       tabPanel("demographic barplot2", icon = icon("chart-simple"), h2("demographic barplot2")))),
        tabItem(tabName = "EOD_summary",
                tabBox(id = "t5", width = 12,
                       tabPanel("description5", tabName = "description-content5", h2("EOD balance segment"),
                                p("End of day balance segment is about the balance in the customer's account
                                  at the end of closing hours in a particular day")),
                       tabPanel("clusters5", icon = icon("sitemap"), plotOutput("cluster5")),
                       tabPanel("data points5", icon = icon("circle"), plotlyOutput("cluster_pie5")),
                       tabPanel("cluster_summary5", icon = icon("table"), DTOutput("summary5")))),
        tabItem(tabName = "EOD_filter",
                tabBox(id="t51", width = 12,
                       tabPanel("EOD barplot", icon = icon("chart-simple"), plotOutput("EOD_bar1"))))
                       
)))