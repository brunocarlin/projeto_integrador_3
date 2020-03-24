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



ui <- navbarPage(
  title="SADDAS",
  sidebarLayout(
    sidebarPanel(uiOutput("range_select")),
    mainPanel(tableOutput("reportOutput"))
  ))

server <- function(input, output) {
  
    selected <- reactive(df_orders_itens_reviews_payments_products_customer %>%
    filter(
           total_price > input$range[1],total_price < input$range[2]))
  
  testing <- reactive(selected() %>% count(Region))
  output$var1_select <- renderUI({
    selectInput("region_brasil", "Regiões do Brasil", choices = df_orders_itens_reviews_payments_products_customer$Region %>% unique(),
                multiple = TRUE,selected = 'Southeast',selectize = T)
  })
  
  output$range_select <-  renderUI(sliderInput("range", "Range:",
              min = min(df_orders_itens_reviews_payments_products_customer$total_price) + 1, max = max(df_orders_itens_reviews_payments_products_customer$total_price) - 1,
              value = c(min(df_orders_itens_reviews_payments_products_customer$total_price),max(df_orders_itens_reviews_payments_products_customer$total_price))))
  
  output$reportOutput = renderTable({
    # Filter it
    testing()
  })

}

shinyApp(ui,server)
