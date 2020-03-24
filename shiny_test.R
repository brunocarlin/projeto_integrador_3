library(shiny)
library(shinydashboard)
library(vroom)
library(tidyverse)

df_customer <- vroom('olist/olist_customers_dataset.csv')
df_geo <- vroom('olist/olist_geolocation_dataset.csv')
df_order_itens <- vroom('olist/olist_order_items_dataset.csv')
df_order_payments <- vroom('olist/olist_order_payments_dataset.csv')
df_order_reviews <- vroom('olist/olist_order_reviews_dataset.csv')
df_orders <- vroom('olist/olist_orders_dataset.csv')
df_products <- vroom('olist/olist_products_dataset.csv')
df_sellers <- vroom('olist/olist_sellers_dataset.csv')


br_sigla <- readxl::read_excel('table_brazil.xlsx')

# join itens ordens -----------------------------------------------------------------------------------------------


df_orders_itens <- df_orders %>% 
  left_join(df_order_itens) %>% 
  filter(product_id %>% is.na %>% `!`) %>% 
  mutate(total_price = price + freight_value) %>% 
  filter(order_status == 'delivered')



# join reviews ----------------------------------------------------------------------------------------------------

df_order_reviews2 <- df_order_reviews %>% 
  group_by(order_id) %>% 
  filter(review_creation_date == max(review_creation_date),
         review_answer_timestamp == max(review_answer_timestamp)) %>%
  ungroup()

df_orders_itens_reviews <- df_orders_itens %>% 
  left_join(df_order_reviews2) %>% 
  mutate(review_simple = case_when(review_score %in% c(1,2,3) ~ 'baixa',
                                   review_score %in% c(4,5) ~ 'alta')) %>% 
  filter(review_simple %>% is.na %>% `!`)


# join payments ---------------------------------------------------------------------------------------------------

df_order_payments2 <- df_order_payments %>% 
  group_by(order_id,payment_type) %>% 
  summarise(
    value_to_pay = sum(payment_value)) %>% 
  ungroup() %>% 
  filter(payment_type != 'not_defined')

df_order_payments3 <-
  df_order_payments2 %>% pivot_wider(
    names_from = payment_type,
    values_from = c(value_to_pay)) %>% 
  rowwise() %>% 
  mutate(total_payment_value = sum(c(credit_card,boleto,voucher,debit_card),na.rm = T))

df_orders_itens_reviews_payments <- df_orders_itens_reviews %>% 
  left_join(df_order_payments3)


# join products ---------------------------------------------------------------------------------------------------


df_orders_itens_reviews_payments_products <- df_orders_itens_reviews_payments %>%
  left_join(df_products)

# join customer ---------------------------------------------------------------------------------------------------

df_orders_itens_reviews_payments_products_customer <- df_orders_itens_reviews_payments_products %>% 
  left_join(df_customer) %>% 
  left_join(br_sigla,by = c('customer_state' = 'Sigla'))

# theme set -------------------------------------------------------------------------------------------------------


theme_set(new = theme_minimal())

# shiny structure -------------------------------------------------------------------------------------------------

header <- dashboardHeader(title = "Presentation Data Science")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Exploração das categorias",
    tabName = "categories",
    icon = icon("bullseye")
  ),
  menuItem(
    "Exploração dos pedidos",
    tabName = "pies",
    icon = icon("chart-pie")
  ),
  sliderInput(
    "number",
    NULL,
    value = 10,
    min = 1,
    max = 20
  ),
  radioButtons(
    "var",
    NULL,
    choiceNames = list("Valor", "Quantidade"),
    choiceValues = list("total_gross_sale", "number_products")
  ),
  selectInput("region_brasil", "Regiões do Brasil", choices = setNames(df_orders_itens_reviews_payments_products_customer$Region %>% unique(),c("Sudeste","Nordeste",
                                                                                                                                                "Centro-Oeste","Sul","Norte")),
              multiple = TRUE,selected = 'Southeast',selectize = T),
  textInput("new_category_name","Nome da nova categoria",value = "casa_nova"),
  selectInput("category_name", "Categorias", choices = df_orders_itens_reviews_payments_products_customer$product_category_name %>% unique(),
              multiple = TRUE,selected = c('cama_mesa_banho',
                                           'moveis_decoracao',
                                           'moveis_escritorio'),selectize = T),
  sliderInput("range", "Range:",round = TRUE,
              min = min(df_orders_itens_reviews_payments_products_customer$total_price), max = max(df_orders_itens_reviews_payments_products_customer$total_price),
              value = c(min(df_orders_itens_reviews_payments_products_customer$total_price),max(df_orders_itens_reviews_payments_products_customer$total_price)))
))

body <- dashboardBody(
  tabItems(
    tabItem(
    tabName = "categories",
    fluidRow(valueBoxOutput("titulo", width = 12)),
    conditionalPanel(condition = "input.var == 'number_products'",
                      fluidRow(
                        column(
                          width = 6,
                          plotOutput(outputId = "number_products2")
                          ),
                        column(
                          width = 6,
                          plotOutput(outputId = "number_products3")
                          )
                      ),
                     fluidRow(
                       box(width = 12,
                           tableOutput(outputId = "number_products1")
                       )
                     )
                    ),
    conditionalPanel(condition = "input.var == 'total_gross_sale'",
                     fluidRow(
                       column(
                         width = 6,
                         plotOutput(outputId = "total_gross_sale2")
                       ),
                       column(
                         width = 6,
                         plotOutput(outputId = "total_gross_sale3")
                       )
                     ),
                     fluidRow(
                       box(width = 12,
                           tableOutput(outputId = "total_gross_sale1")
                          )
                      )
                    )
    
                  ),
    tabItem(tabName = "pies",
            fluidRow(valueBoxOutput("header_pie_chart", width = 12)),
            fluidRow(
              column(
                width = 6,
                plotlyOutput(outputId = "pie_chart_low")
              ),
              column(
                width = 6,
                plotlyOutput(outputId = "pie_chart_high")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotlyOutput(outputId = "hist_order_low")
              ),
              column(
                width = 6,
                plotlyOutput(outputId = "hist_order_high")
              )
            )
            
            
    ))
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session){


# reactive data ---------------------------------------------------------------------------------------------------
  
  
  
  selected <- reactive(df_orders_itens_reviews_payments_products_customer %>%
                         filter(Region %in% input$region_brasil,
                                total_price >= input$range[1],
                                total_price <= input$range[2]) %>% 
                         mutate(
                           product_category_name = case_when(
                             product_category_name %in% input$category_name ~ input$new_category_name,
                             TRUE ~ product_category_name
                           ))
  )
  
  df_gross_sales_category <-  reactive(selected() %>% 
    group_by(product_category_name,review_simple) %>% 
    summarise(total_gross_sale = sum(total_price,na.rm = T),
              mean_price = mean(total_price,na.rm = T),
              number_products = n()) %>% 
    group_by(product_category_name) %>%
    mutate(percent_notas = number_products/sum(number_products)))
  
  products_to_use <- reactive(df_gross_sales_category() %>% 
                                summarise(total_values = sum(number_products)) %>% 
                                arrange(-total_values) %>% 
                                top_n(input$number) %>%
                                filter(product_category_name %>% is.na() %>% `!`) %>% 
                                pull(product_category_name))
  
  df_gross_sales_category2 <- reactive(df_gross_sales_category() %>% filter(product_category_name %in% products_to_use()))
  
  df_gross_sales_category3 <- reactive(df_gross_sales_category2() %>%
                                         group_by(product_category_name, review_simple, percent_notas) %>%
                                         summarise_all(sum) %>% pivot_wider(names_from = review_simple, values_from = percent_notas) %>%
                                         summarise(
                                           total_gross_sale = sum(total_gross_sale),
                                           number_products = sum(number_products),
                                           alta = mean(alta, na.rm = T),
                                           baixa = mean(baixa, na.rm = T)
                                         ) %>% 
                                         mutate(mean_price = total_gross_sale / number_products) %>% 
                                         arrange(-number_products))
  
  df_pie_chart <- reactive(selected() %>%
    filter(product_category_name == input$new_category_name) %>% 
    group_by(order_id,review_simple) %>% 
    summarise(total_gross_sale = sum(total_price,na.rm = T),
              mean_price = mean(total_price,na.rm = T),
              number_products = n()) %>%
    mutate(bucket_itens = if_else(number_products >= 3,'3 ou mais',number_products %>% as.character())) %>% 
    group_by(review_simple,bucket_itens) %>% 
    summarise(number_products = n()) %>% 
    group_by(bucket_itens) %>% 
    mutate(percent_notas = number_products/sum(number_products)) %>% 
    group_by(review_simple) %>% 
    mutate(percent_prod = number_products/sum(number_products)))
  
  df_hist_order <- reactive(selected() %>% 
    group_by(order_id,review_simple) %>% 
    summarise(total_gross_sale = sum(total_price,na.rm = T),
              mean_price = mean(total_price,na.rm = T),
              number_products = n()) %>%
    group_by(review_simple,number_products) %>% 
    summarise(number_instances = n()) %>% 
    group_by(number_products) %>% 
    mutate(percent_notas = number_instances/sum(number_instances)) %>% 
    group_by(review_simple) %>% 
    mutate(percent_prod = number_instances/sum(number_instances)) %>% 
    ungroup() %>% 
    mutate(number_products = number_products %>% as_factor(),
           percent_prod_p = scales::percent(percent_prod)))
  
  
  
  

# server outputs --------------------------------------------------------------------------------------------------


  output$titulo <- renderValueBox(
    valueBox(paste("Top", input$number, "resultados"), subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )


# categories total_gross_sale1 outputs ----------------------------------------------------------------------------------------

  output$total_gross_sale1 <- renderTable(
    df_gross_sales_category3()
  )
  
  output$total_gross_sale2 <- renderPlot(
    df_gross_sales_category2() %>%
      ungroup() %>% 
      ggplot(aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas), y = percent_notas,text = number_products,fill = review_simple)) +
      geom_bar(stat = "identity",
               position = "fill") +
      scale_fill_manual(values = c("steelblue","red")) +
      scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
      geom_text(aes(label = scales::percent(percent_notas,accuracy = 2)),
                size = 4.5,
                position = position_stack(vjust = 0.5)) +
      coord_flip() +
      labs(x = "Categorias", y = "Porcentagem de avaliações") +
      theme(legend.position = "none",
            axis.text=element_text(size=16),
            axis.title = element_text(size = 16))
  )
  
  output$total_gross_sale3 <- renderPlot(
    df_gross_sales_category2() %>% 
      ggplot(aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas),y = total_gross_sale,fill = review_simple)) +
      geom_col() +
      labs(y = "Tamanho do Mercado",fill = 'Nota') +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18)) +
      coord_flip() +
      scale_fill_manual(values = c("steelblue","red")) +
      scale_y_continuous(label =scales::dollar_format(scale = 1/1000,suffix = "k",largest_with_cents = 0)) +
      geom_text(aes(label = scales::dollar(total_gross_sale,scale = 1/1000,suffix = "k",largest_with_cents = 0)),
                size = 4.5,
                position = position_stack(vjust = 0.5))
  )
  
  
# categories number_products outputs --------------------------------------------------------------------------------------

  
  output$number_products1 <- renderTable(
    df_gross_sales_category3()
  )
  
  output$number_products2 <- renderPlot(
    df_gross_sales_category2() %>%
      ungroup() %>% 
      ggplot(aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas), y = percent_notas,text = number_products,fill = review_simple)) +
      geom_bar(stat = "identity",
               position = "fill") +
      scale_fill_manual(values = c("steelblue","red")) +
      scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
      geom_text(aes(label = scales::percent(percent_notas,accuracy = 2)),
                size = 4.5,
                position = position_stack(vjust = 0.5)) +
      coord_flip() +
      labs(x = "Categorias", y = "Porcentagem de avaliações") +
      theme(legend.position = "none",
            axis.text=element_text(size=16),
            axis.title = element_text(size = 16))
  )
  
  output$number_products3 <- renderPlot(
    df_gross_sales_category2() %>% 
      ggplot(aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas),y = number_products,fill = review_simple)) +
      geom_col() +
      labs(y = "Número de itens comprados",fill = 'Nota') +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18)) +
      coord_flip() +
      scale_fill_manual(values = c("steelblue","red")) +
      geom_text(aes(label = number_products),
                size = 4.5,
                position = position_stack(vjust = 0.5))
  )
  
# orders -----------------------------------------------------
  
  output$header_pie_chart <- renderValueBox(
    valueBox(paste("Graficos de pizza da variavel ", input$new_category_name), subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )
  
  output$pie_chart_low <- renderPlotly(
    df_pie_chart() %>% 
      filter(review_simple == "baixa") %>% 
      plot_ly(labels = ~bucket_itens, values = ~number_products,
                     marker = list(colors = c("steelblue", "grey", "red"),
                                   line = list(color = '#FFFFFF', width = 1)),
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     showlegend = FALSE) %>% 
      add_pie(hole = .5) %>%
      layout(title = 'Quantidade de itens por pedido com notas baixas',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
   
  )
  
  output$pie_chart_high <- renderPlotly(
    df_pie_chart() %>% 
      filter(review_simple == "alta") %>% 
      plot_ly(labels = ~bucket_itens, values = ~number_products,
              marker = list(colors = c("steelblue", "grey", "red"),
                            line = list(color = '#FFFFFF', width = 1)),
              textposition = 'inside',
              textinfo = 'label+percent',
              showlegend = FALSE) %>% 
      add_pie(hole = .5) %>%
      layout(title = 'Quantidade de itens por pedido com notas altas',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  
  output$hist_order_low <- renderPlotly({
    ggplotly( 
      ggplot(data = df_hist_order() %>% 
               filter(review_simple == "baixa"),
             aes(x = number_products,
                 y = percent_prod,
                 text = paste(percent_prod_p,"\n",number_instances,sep = "")
                 )
             ) +
      geom_col(fill = "red") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = 'Quantidade de produtos no pedido',
           y = 'Porcentagem de pedidos'),
      tooltip = c('text','number_instances'))
    
})
  
  output$hist_order_high <- renderPlotly({
    ggplotly(
      ggplot(data = df_hist_order() %>% filter(review_simple == "alta"),
             aes(x = number_products,
                 y = percent_prod,
                 text = paste(percent_prod_p,"\n",number_instances,sep = "")
                 )
             ) +
      geom_col(fill = "steelblue") +
      scale_y_continuous(labels = scales::percent) +
        labs(x = 'Quantidade de produtos no pedido',
             y = 'Porcentagem de pedidos'),
      tooltip = c('text','number_instances'))
    
})
  
}



shinyApp(ui, server)


