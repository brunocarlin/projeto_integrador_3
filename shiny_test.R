library(shiny)
library(shinydashboard)
library(vroom)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)
library(scales)
library(readxl)
library(sf)

df_customer <- vroom('olist/olist_customers_dataset.csv')
df_geo <- vroom('olist/olist_geolocation_dataset.csv')
df_order_itens <- vroom('olist/olist_order_items_dataset.csv')
df_order_payments <- vroom('olist/olist_order_payments_dataset.csv')
df_order_reviews <- vroom('olist/olist_order_reviews_dataset.csv')
df_orders <- vroom('olist/olist_orders_dataset.csv')
df_products <- vroom('olist/olist_products_dataset.csv')
df_sellers <- vroom('olist/olist_sellers_dataset.csv')


br_sigla <- readxl::read_excel('table_brazil.xlsx')

br_maps <- brazilmaps::get_brmap("State")

# join itens ordens ----------------------------------------------------------------------------------------------------


df_orders_itens <- df_orders %>% 
  left_join(df_order_itens) %>% 
  filter(product_id %>% is.na %>% `!`) %>% 
  mutate(total_price = price + freight_value) %>% 
  filter(order_status == 'delivered')



# join reviews ---------------------------------------------------------------------------------------------------------

df_order_reviews2 <- df_order_reviews %>% 
  group_by(order_id) %>% 
  filter(review_creation_date == max(review_creation_date),
         review_answer_timestamp == max(review_answer_timestamp)) %>%
  ungroup()

df_orders_itens_reviews <- df_orders_itens %>% 
  left_join(df_order_reviews2) %>%
  filter(review_score %>% is.na %>% `!`)


# join payments --------------------------------------------------------------------------------------------------------

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


# join products --------------------------------------------------------------------------------------------------------


df_orders_itens_reviews_payments_products <- df_orders_itens_reviews_payments %>%
  left_join(df_products)

# join customer --------------------------------------------------------------------------------------------------------

df_orders_itens_reviews_payments_products_customer <- df_orders_itens_reviews_payments_products %>% 
  left_join(df_customer) %>% 
  left_join(br_sigla,by = c('customer_state' = 'Sigla'))

# theme set ------------------------------------------------------------------------------------------------------------


theme_set(new = theme_minimal())


# functions -------------------------------------------------------------------------------------------------------


reais <-  scales::label_dollar(prefix = "R$",big.mark = ".",decimal.mark = ",")
k_reais <- scales::label_dollar(scale = 1/1000,
                                prefix = "R$",
                                suffix = "k",
                                big.mark = ".",
                                decimal.mark = ",",
                                largest_with_cents = 0)

number_log <- trans_new(name = "number_log",
                       transform = log10_trans()$transform,
                       inverse = log10_trans()$inverse,
                       breaks = breaks_log(),
                       format = label_dollar())

real_log <- trans_new(name = "real_log",
                        transform = log10_trans()$transform,
                        inverse = log10_trans()$inverse,
                        breaks = breaks_log(),
                        format = reais)

'%!in%' <- function(x,y)!('%in%'(x,y))

quantile_maker <- function(quantile_number,precision,position){
  (10^precision/quantile_number)*position
}

quantile_6 <- partial(quantile_maker,6,4)

# shiny structure ------------------------------------------------------------------------------------------------------


# header ---------------------------------------------------------------------------------------------------------------

header <- dashboardHeader(title = "Presentation Data Science")


# sidebar --------------------------------------------------------------------------------------------------------------

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Categorias dos produtos",
    tabName = "categories",
    icon = icon("bullseye")
  ),
  menuItem(
    "Pedidos",
    tabName = "pies",
    icon = icon("shopping-basket")
  ),
  menuItem(
    "Regiões do Brasil",
    icon = icon("map"),
    menuSubItem(text = paste(emo::ji('Brazil'),'Todas as Notas',emo::ji('Brazil')),tabName = "maps",icon = NULL),
    menuSubItem(text = paste(emo::ji('angry'),'Notas Baixas',emo::ji('angry')),tabName = "maps_low",icon = NULL),
    menuSubItem(text = paste(emo::ji('smile'),'Notal Altas',emo::ji('smile')),tabName = "maps_high",icon = NULL)
  ),
  menuItem(
    "Preços",
    tabName = "prices",
    icon = icon("tag")
  ),
  menuItem(
    "Meios de pagamento",
    tabName = "payment_methods",
    icon = icon("money-check")
  ),
  menuItem(
    "Anúncio",
    tabName = "announcement",
    icon = icon("bullhorn")
  ),
  menuItem(
    "Simulador",
    tabName = "simulator",
    icon = icon("edit")
  ),
  radioButtons(
    "var",
    NULL,
    choiceNames = list("Quantidade", "Valor"),
    choiceValues = list("number_products", "total_gross_sale")
  ),
  selectInput("region_brasil",
              "Regiões do Brasil",
              choices = setNames(df_orders_itens_reviews_payments_products_customer$Region %>%
                                   unique(),
                                 c("Sudeste","Nordeste","Centro-Oeste","Sul","Norte")),
              multiple = TRUE,
              #selected = 'Southeast',
              selected = c("Southeast","Northeast","Center West","South","North"),
              selectize = T),
  textInput("new_category_name",
            "Nome da nova categoria",
            #value = "casa_nova"
            value = "cama_mesa_banho"
            ),
  selectInput("category_name",
              "Categorias",
              choices = df_orders_itens_reviews_payments_products_customer$product_category_name %>% unique(),
              multiple = TRUE,
              selected = c('cama_mesa_banho'),
              # selected = c('cama_mesa_banho',
              #                              'moveis_decoracao',
              #                              'moveis_escritorio'),
              selectize = T),
  sliderInput("range", "Preços",round = TRUE,
              min = min(df_orders_itens_reviews_payments_products_customer$total_price),
              max = max(df_orders_itens_reviews_payments_products_customer$total_price),
              value = c(min(df_orders_itens_reviews_payments_products_customer$total_price),
                        max(df_orders_itens_reviews_payments_products_customer$total_price)
              )
  ),
  checkboxGroupButtons(
    inputId = "review_scores",
    label = "Escolha o critério",
    choices = c("1","2", "3", "4","5"),
    status = "primary",
    checkIcon = list(
      yes = icon("ok", 
                 lib = "glyphicon"),
      no = icon("remove",
                lib = "glyphicon")),selected = c("3","4","5")
  )
))


# body -----------------------------------------------------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    
    # tabs -------------------------------------------------------------------------------------------------------------
    tabItem(
      
# categories -----------------------------------------------------------------------------------------------------------
      tabName = "categories",
      fluidRow(valueBoxOutput("titulo", width = 12)),
      fluidRow(
        valueBoxOutput("approval_box"),
        valueBoxOutput("gross_sales_box"),
        valueBoxOutput("number_products_box")
      ),
      conditionalPanel(condition = "input.var == 'number_products'",
                       fluidRow(
                         column(
                           width = 6,
                           plotlyOutput(outputId = "number_products2")
                         ),
                         column(
                           width = 6,
                           plotlyOutput(outputId = "number_products3")
                         )
                       )
      ),
      conditionalPanel(condition = "input.var == 'total_gross_sale'",
                       fluidRow(
                         column(
                           width = 6,
                           plotlyOutput(outputId = "total_gross_sale2")
                         ),
                         column(
                           width = 6,
                           plotlyOutput(outputId = "total_gross_sale3")
                         )
                       )
      ),
      fluidRow(DTOutput(outputId = "dt_table")),
      sliderInput(
        "number",
        label = "Top n",
        value = 10,
        min = 1,
        max = 20
      )

    ),
    
# pies -----------------------------------------------------------------------------------------------------------------
    tabItem(
      tabName = "pies",
      fluidRow(valueBoxOutput("header_pie_chart", width = 12)),
      conditionalPanel(
        condition = "input.var == 'number_products'",
        fluidRow(
          column(width = 6,
                 plotlyOutput(outputId = "pie_chart_low1")),
          column(width = 6,
                 plotlyOutput(outputId = "pie_chart_high1"))
        ),
        fluidRow(
          column(width = 6,
                 plotlyOutput(outputId = "hist_order_low1")),
          column(width = 6,
                 plotlyOutput(outputId = "hist_order_high1"))
        )
      ),
      conditionalPanel(
        condition =  "input.var == 'total_gross_sale'",
        fluidRow(
          column(width = 6,
                 plotlyOutput(outputId = "pie_chart_low2")),
          column(width = 6,
                 plotlyOutput(outputId = "pie_chart_high2"))
        ),
        fluidRow(
          column(width = 6,
                 plotlyOutput(outputId = "hist_order_low2")),
          column(width = 6,
                 plotlyOutput(outputId = "hist_order_high2"))
        )
      )
    ),
    
# maps -----------------------------------------------------------------------------------------------------------------
    tabItem(tabName = "maps",
            fluidRow(valueBoxOutput("header_map_charts", width = 12)),
            conditionalPanel(condition = "input.var == 'number_products'",
                             fluidRow(plotlyOutput("plot_map1",height = "1200px"))),
            conditionalPanel(condition = "input.var == 'total_gross_sale'",
                             fluidRow(plotlyOutput("plot_map2",height = "1200px")))
    ),
    
    tabItem(tabName = "maps_low",
            conditionalPanel(condition = "input.var == 'number_products'",
                             fluidRow(plotlyOutput("plot_map1_low",height = "1200px"))),
            conditionalPanel(condition = "input.var == 'total_gross_sale'",
                             fluidRow(plotlyOutput("plot_map2_low",height = "1200px")))
    ),
    
    tabItem(tabName = "maps_high",
            conditionalPanel(condition = "input.var == 'number_products'",
                             fluidRow(plotlyOutput("plot_map1_high",height = "1200px"))),
            conditionalPanel(condition = "input.var == 'total_gross_sale'",
                             fluidRow(plotlyOutput("plot_map2_high",height = "1200px")))
    ),
    
    
# prices ---------------------------------------------------------------------------------------------------------------
    tabItem(tabName = "prices",
              fluidRow(valueBoxOutput(outputId = "header_prices",width = 12)),
              conditionalPanel(
                condition = "input.var == 'number_products'",
                fluidRow(plotlyOutput("quantile_quantity"))
              ),
              conditionalPanel(
                condition = "input.var == 'total_gross_sale'",
                fluidRow(plotlyOutput("quantile_sales"))
              ),
              fluidRow(plotOutput("density_plot"))
            ),

# payment methods ------------------------------------------------------------------------------------------------------

    tabItem(tabName = "payment_methods",
              fluidRow(valueBoxOutput(outputId = "header_payment_methods",width = 12)),
              conditionalPanel(
                condition = "input.var == 'number_products'",
                fluidRow(
                  column(width = 6, plotlyOutput("payment_methods_qnt1")),
                  column(width = 6, plotlyOutput("payment_methods_qnt2"))
                )
              ),
              conditionalPanel(
                condition = "input.var == 'total_gross_sale'",
                fluidRow(
                  column(width = 6,plotlyOutput("payment_methods_sales1")),
                  column(width = 6,plotlyOutput("payment_methods_sales2"))
                )
              )
          ),



# announcement ----------------------------------------------------------------------------------------------------

    tabItem(tabName = "announcement",
              fluidRow(valueBoxOutput(outputId = "header_announcement",width = 12)),
              conditionalPanel(
                condition = "input.var == 'number_products'",
                fluidRow(plotlyOutput("announcement_qnt1")),
                fluidRow(plotlyOutput("announcement_qnt2"))
              ),
              conditionalPanel(
                condition = "input.var == 'total_gross_sale'",
                fluidRow(
                  column(width = 6,plotlyOutput("announcement_sales1")),
                  column(width = 6,plotlyOutput("announcement_sales2"))
                )
              )
            ),

# simulator -------------------------------------------------------------------------------------------------------

    tabItem(tabName = "simulator",
            fluidRow(valueBoxOutput(outputId = "header_simulator",width = 12)),
            fluidRow(
              column(width = 4,numericInput("caracteres",label = "Tamanho do texto em caracteres",value = 1000)),
              column(width = 4,numericInput("fotos",label = "Quantidade de fotos",value = 3)),
              column(width = 4,numericInput("frete",label = "Custo estimado no frete",value = 2.5))
              ),
            conditionalPanel(
              condition = "input.var == 'number_products'",
              fluidRow(textOutput(outputId = "current_situation_quantity")),
              fluidRow(textOutput(outputId = "possible_situation_quantity")),
              fluidRow(
                column(width = 8,plotlyOutput("simulator_qnt1")),
                column(width = 4,plotlyOutput("simulator_qnt2"))
                )
            ),
            conditionalPanel(
              condition = "input.var == 'total_gross_sale'",
              fluidRow(textOutput(outputId = "current_situation_sales")),
              fluidRow(textOutput(outputId = "possible_situation_sales")),
              fluidRow(
                column(width = 8,plotlyOutput("simulator_sales1")),
                column(width = 4,plotlyOutput("simulator_sales2"))
              )
        )
    )
)

    
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")


server <- function(input, output, session){
  
  
# reactive data --------------------------------------------------------------------------------------------------------
  
  
  
  selected <- reactive(df_orders_itens_reviews_payments_products_customer %>%
                         mutate(review_simple = case_when(review_score %in% input$review_scores ~ 'alta',
                                                          review_score %!in% input$review_scores ~ 'baixa')) %>% 
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
                                         mutate(percent_notas = number_products/sum(number_products),
                                                percent_sales = total_gross_sale/sum(total_gross_sale)))
  
  products_to_use <- reactive(df_gross_sales_category() %>% 
                                summarise(total_values = sum(number_products)) %>% 
                                arrange(-total_values) %>%
                                filter(product_category_name %>% is.na() %>% `!`) %>% 
                                top_n(input$number) %>%
                                pull(product_category_name))
  
  
  df_gross_sales_category2 <- reactive(df_gross_sales_category() %>%
                                         filter(product_category_name %in% products_to_use()))
  
  df_gross_sales_category3 <- reactive(df_gross_sales_category2() %>%
                                         group_by(product_category_name, review_simple, percent_notas) %>%
                                         summarise_all(sum) %>% pivot_wider(names_from = review_simple,
                                                                            values_from = percent_notas) %>%
                                         summarise(
                                           total_gross_sale = sum(total_gross_sale),
                                           number_products = sum(number_products),
                                           alta = mean(alta, na.rm = T),
                                           baixa = mean(baixa, na.rm = T)
                                         ) %>% 
                                         mutate(mean_price = total_gross_sale / number_products) %>% 
                                         arrange(-number_products))
  
  
  df_created_category <- reactive(selected() %>%
                                    filter(product_category_name == input$new_category_name))
  
  created_category_approval <- reactive(df_created_category() %>% 
                                          count(review_simple) %>%
                                          mutate(percent_notas = n/sum(n)) %>% 
                                          pull(percent_notas) %>% 
                                          head(1) %>% 
                                          scales::percent())
  
  created_category_gross_sales <- reactive(df_created_category() %>%
                                             summarise(total_gross = sum(total_price)) %>%
                                             pull() %>%
                                             k_reais())
  
  created_category_number_products <- reactive(df_created_category() %>%
                                                 count() %>% 
                                                 pull())
  
  df_pie_chart <- reactive(df_created_category() %>% 
                             group_by(order_id,review_simple) %>% 
                             summarise(total_gross_sale = sum(total_price,na.rm = T),
                                       mean_price = mean(total_price,na.rm = T),
                                       number_products = n()) %>%
                             mutate(bucket_itens = if_else(number_products >= 3,
                                                           '3 ou mais',
                                                           number_products %>% as.character())) %>% 
                             group_by(review_simple,bucket_itens) %>% 
                             summarise(number_products = n(),total_gross_sale = sum(total_gross_sale)) %>% 
                             group_by(bucket_itens) %>% 
                             mutate(percent_notas = number_products/sum(number_products),
                                    percent_notas_gs = total_gross_sale/sum(total_gross_sale)))
  
  df_hist_order <- reactive(df_created_category() %>% 
                              group_by(order_id,review_simple) %>% 
                              summarise(total_gross_sale = sum(total_price,na.rm = T),
                                        mean_price = mean(total_price,na.rm = T),
                                        number_products = n()) %>%
                              group_by(review_simple,number_products) %>% 
                              summarise(number_instances = n(),total_gross_sale = sum(total_gross_sale)) %>% 
                              group_by(number_products) %>% 
                              mutate(percent_notas = number_instances/sum(number_instances),
                                     percent_gross_sales = total_gross_sale/sum(total_gross_sale)) %>% 
                              group_by(review_simple) %>% 
                              mutate(percent_prod = number_instances/sum(number_instances),
                                     percent_prod_gs = total_gross_sale/sum(total_gross_sale)) %>% 
                              ungroup() %>% 
                              mutate(number_products = number_products %>% as_factor(),
                                     percent_prod_p = scales::percent(percent_prod,2),
                                     percent_prod_gs_p = scales::percent(percent_prod_gs,2)))
  
  df_state <- reactive(df_created_category() %>% 
                         group_by(Estado) %>% 
                         summarise(n_orders = n(),gross_sales = sum(total_price)) %>% 
                         mutate(prop = n_orders/sum(n_orders),
                                nome = Estado %>% str_to_upper(),
                                prop_gross = gross_sales/sum(gross_sales),
                                label_prod = paste(scales::percent(prop,2),
                                                   n_orders)))
  
  df_map <- reactive(br_maps %>% 
                       left_join(df_state(),by = c("nome")) %>% 
                       as.data.frame() %>% 
                       sf::st_sf()
                     )
  
  
  df_state_low <- reactive(df_created_category() %>%
                         filter(review_simple == "baixa") %>% 
                         group_by(Estado) %>% 
                         summarise(n_orders = n(),gross_sales = sum(total_price)) %>% 
                         mutate(prop = n_orders/sum(n_orders),
                                nome = Estado %>% str_to_upper(),
                                prop_gross = gross_sales/sum(gross_sales),
                                label_prod = paste(scales::percent(prop,2),
                                                   n_orders)))
  
  df_map_low <- reactive(br_maps %>% 
                       left_join(df_state_low(),by = c("nome")) %>% 
                         as.data.frame() %>% 
                         sf::st_sf()
                       )
  
  df_state_high <- reactive(df_created_category() %>%
                             filter(review_simple == "alta") %>% 
                             group_by(Estado) %>% 
                             summarise(n_orders = n(),gross_sales = sum(total_price)) %>% 
                             mutate(prop = n_orders/sum(n_orders),
                                    nome = Estado %>% str_to_upper(),
                                    prop_gross = gross_sales/sum(gross_sales),
                                    label_prod = paste(scales::percent(prop,2),
                                                       n_orders)))
  
  df_map_high <- reactive(br_maps %>% 
                           left_join(df_state_high(),by = c("nome")) %>% 
                            as.data.frame() %>% 
                            sf::st_sf()
                          )
  
  
  
  df_quantiles <- reactive(
    df_created_category() %>%
       mutate(probabilidade_quantil = total_price %>% ntile(10000),
              score = case_when(probabilidade_quantil < 2500 ~ 'primeiro',
                                probabilidade_quantil >= 2500 & probabilidade_quantil < 5000 ~ 'segundo',
                                probabilidade_quantil >= 5000 & probabilidade_quantil < 7500 ~ 'terceiro',
                                probabilidade_quantil >= 7500 ~ 'quarto')) %>% 
       group_by(score,review_simple) %>% 
       summarise(n_orders = n(),
                 gross_sales = sum(total_price),
                 min_order = min(total_price),
                 max_order = max(total_price)) %>% 
       group_by(score) %>% 
       mutate(prop = n_orders/sum(n_orders),
              prop_sales = gross_sales/sum(gross_sales)) %>% 
       ungroup())
  
  df_payments <- reactive(df_created_category() %>% 
    select(review_simple,credit_card,boleto,voucher,debit_card) %>% 
    pivot_longer(-review_simple,values_drop_na = TRUE,names_to = "payment_method",values_to = "gross_sales") %>% 
    group_by(review_simple,payment_method) %>% 
    summarise(n_count = n(),
              gross_sales = sum(gross_sales)) %>% 
    group_by(payment_method) %>% 
    mutate(prop_qnt = n_count/sum(n_count),
           prop_sales = gross_sales/sum(gross_sales)) %>% 
    ungroup())
  
  df_solution <- reactive(df_created_category() %>% 
    mutate(bucket_photos = case_when(product_photos_qty == 1 ~ "1",
                                     product_photos_qty >= 2 & product_photos_qty <=3 ~ "2-3",
                                     product_photos_qty >= 4 & product_photos_qty <=5 ~ "4-5",
                                     product_photos_qty >= 6 ~ "6+"),
           probabilidade_quantil_desc = product_description_lenght %>% ntile(10000),
           quantil_desc = case_when(probabilidade_quantil_desc < quantile_6(1) ~ '1º',
                                    probabilidade_quantil_desc >= quantile_6(1) & probabilidade_quantil_desc < quantile_6(2) ~ '2º',
                                    probabilidade_quantil_desc >= quantile_6(2) & probabilidade_quantil_desc < quantile_6(3) ~ '3º',
                                    probabilidade_quantil_desc >= quantile_6(3) & probabilidade_quantil_desc < quantile_6(4) ~ '4º',
                                    probabilidade_quantil_desc >= quantile_6(4) & probabilidade_quantil_desc < quantile_6(5) ~ '5º',
                                    probabilidade_quantil_desc > quantile_6(5) ~ '6º'
           ) %>%
             as_factor() %>% 
             forcats::fct_relevel("1º","2º","3º","4º","5º","6º"))
  )
  
  df_photos <- reactive(df_solution() %>% 
    group_by(bucket_photos,review_simple) %>%
    summarise(total_gross_sale = sum(total_price,na.rm = T),
              mean_price = mean(total_price,na.rm = T),
              number_products = n()) %>% 
    group_by(bucket_photos) %>%
    mutate(percent_notas = number_products/sum(number_products),
           percent_sales = total_gross_sale/sum(total_gross_sale)) %>%
    ungroup()
    )
  
  df_quantile_desc <- reactive(df_solution() %>% 
    group_by(quantil_desc,review_simple) %>%
    summarise(total_gross_sale = sum(total_price,na.rm = T),
              mean_price = mean(total_price,na.rm = T),
              number_products = n(),
              min_caracters_desc = min(product_description_lenght),
              max_caracters_desc = max(product_description_lenght)) %>% 
    group_by(quantil_desc) %>%
    mutate(percent_notas = number_products/sum(number_products),
           percent_sales = total_gross_sale/sum(total_gross_sale)) %>%
    ungroup()
    )
  
  
  df_solution_2 <- reactive(df_solution() %>% 
    group_by(bucket_photos,quantil_desc,review_simple) %>%
    summarise(total_gross_sale = sum(total_price,na.rm = T),
              number_products = n(),
              total_caracters_desc = sum(product_description_lenght),
              min_caracters_desc = min(product_description_lenght),
              max_caracters_desc= max(product_description_lenght)) %>% 
    mutate(multiplier = if_else(review_simple == "alta",1,0)) %>% 
    group_by(bucket_photos,quantil_desc) %>%
    summarise(gross_sales1 = sum(total_gross_sale),
              total_number_products = sum(number_products),
              prop_quantity = sum(number_products * multiplier)/total_number_products,
              prop_sales = sum(total_gross_sale * multiplier)/gross_sales1,
              mean_caracters_desc = sum(total_caracters_desc)/total_number_products,
              min_caracters_desc = min(min_caracters_desc) + 1,
              max_caracters_desc= max(max_caracters_desc)) %>% 
    ungroup())
  
  simulated <- reactive(
    df_solution() %>% 
    select(product_photos_qty,product_description_lenght) %>% 
    add_row(product_photos_qty = input$fotos,product_description_lenght = input$caracteres) %>%
    mutate(bucket_photos = case_when(product_photos_qty == 1 ~ "1",
                                     product_photos_qty >= 2 & product_photos_qty <=3 ~ "2-3",
                                     product_photos_qty >= 4 & product_photos_qty <=5 ~ "4-5",
                                     product_photos_qty >= 6 ~ "6+"),
           probabilidade_quantil_desc = product_description_lenght %>% ntile(10000),
           quantil_desc = case_when(probabilidade_quantil_desc < quantile_6(1) ~ '1º',
                                    probabilidade_quantil_desc >= quantile_6(1) & probabilidade_quantil_desc < quantile_6(2) ~ '2º',
                                    probabilidade_quantil_desc >= quantile_6(2) & probabilidade_quantil_desc < quantile_6(3) ~ '3º',
                                    probabilidade_quantil_desc >= quantile_6(3) & probabilidade_quantil_desc < quantile_6(4) ~ '4º',
                                    probabilidade_quantil_desc >= quantile_6(4) & probabilidade_quantil_desc < quantile_6(5) ~ '5º',
                                    probabilidade_quantil_desc > quantile_6(5) ~ '6º'
           ) %>%
             as_factor() %>% 
             forcats::fct_relevel("1º","2º","3º","4º","5º","6º")) %>%
    group_by(quantil_desc ,bucket_photos) %>% 
    mutate(min_characters = min(product_description_lenght),
           max_characters = max(product_description_lenght)) %>% 
    tail(1)
  )
  
  simulated_bucket_photos <- reactive(
    simulated() %>% pull(bucket_photos)
  )
  
  simulated_quantil_desc <- reactive(
    simulated() %>% pull(quantil_desc)
  )
  
  best_bucket_photos <- reactive(
    df_solution_2() %>% 
    arrange(-prop_quantity) %>%
    head(1) %>% 
    pull(bucket_photos)
  )
  
  best_quantil_desc <- reactive(
    df_solution_2() %>% 
    arrange(-prop_quantity) %>%
    head(1) %>% 
    pull(quantil_desc)
  )
  
  best_min_caracters_desc <- reactive(
    df_solution_2() %>% 
    arrange(-prop_quantity) %>%
    head(1) %>% 
    pull(min_caracters_desc)
  )
  
  best_max_caracters_desc <- reactive(
    df_solution_2() %>% 
    arrange(-prop_quantity) %>%
    head(1) %>% 
    pull(max_caracters_desc)
  )
  
  simulated_prop_quantity <- reactive(
    df_solution_2() %>% 
    filter(bucket_photos == simulated_bucket_photos(),quantil_desc == simulated_quantil_desc()) %>%
    pull(prop_quantity)
  )
  
  best_prop_quantity <- reactive(
    df_solution_2() %>% 
    arrange(-prop_quantity) %>%
    head(1) %>% 
    pull(prop_quantity)
  )
  
  estimated_increase_quantity <- reactive(
    (best_prop_quantity() - simulated_prop_quantity())/simulated_prop_quantity()
  )
  
  estimated_freight_gain_quantity <- reactive(
    input$frete * 2 * estimated_increase_quantity()
  )
  
  simulated_prop_sales <- reactive(
    df_solution_2() %>% 
    filter(bucket_photos == simulated_bucket_photos(),quantil_desc == simulated_quantil_desc()) %>%
    pull(prop_sales)
  )
  
  
  best_prop_sales <- reactive(
    df_solution_2() %>% 
    arrange(-prop_sales) %>%
    head(1) %>% 
    pull(prop_sales)
  )
  
  
  estimated_increase_sales <- reactive(
    (best_prop_sales() - simulated_prop_sales())/simulated_prop_sales()
  )
  
  estimated_freight_gain_sales <- reactive(
    input$frete * 2 * estimated_increase_sales()
    )
  
  
  
# server outputs -------------------------------------------------------------------------------------------------------
  
  
  output$titulo <- renderValueBox(
    valueBox(paste("Top", input$number, "resultados"), subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )
  
  # shared table categories and sales
  output$dt_table <- renderDT(
    df_gross_sales_category3() %>% 
      mutate(alta = scales::percent(alta,accuracy = .01),
             baixa = scales::percent(baixa,accuracy = .01),
             total_gross_sale = k_reais(total_gross_sale),
             mean_price = reais(mean_price),
      ),
    escape = FALSE,
    server = FALSE,
    options = list(
      dom = 't',
      buttons = c('copy', 'excel', 'pdf', 'print', 'colvis'),
      columnDefs = list(list(type = 'natural', targets = 6))
    ),
    extensions = c('Buttons',"Responsive")
  )
# categories total_gross_sale outputs ----------------------------------------------------------------------------------
  
  
  
  output$total_gross_sale2 <- renderPlotly(
      ggplotly(
        ggplot(data = df_gross_sales_category2(),
               aes(x = fct_reorder2(product_category_name,review_simple,-percent_sales),
                   y = percent_sales,fill = review_simple,
                   text =  paste(product_category_name,"\n",
                                 scales::percent(percent_sales,.01), " do faturamento com nota ",review_simple,"\n",
                                 number_products, " produtos", "\n",
                                 k_reais(total_gross_sale), " de faturamento",
                                 sep = ""))) +
      geom_bar(stat = "identity",
               position = "fill") +
      scale_fill_manual(values = c("steelblue","red")) +
      scale_y_continuous(breaks = seq(0, 1, .2),label = scales::percent) +
      geom_text(aes(label = scales::percent(percent_sales,accuracy = 2)),
                position = position_stack(vjust = 0.5)) +
      coord_flip() +
      labs(x = "Categorias", y = "Porcentagem de avaliações") +
      theme(legend.position = "none"),tooltip = "text")
  )
  
  output$total_gross_sale3 <- renderPlotly(
    
      ggplotly(ggplot(data = df_gross_sales_category2(),
                      aes(x = fct_reorder2(product_category_name,review_simple,-percent_sales),
                          y = total_gross_sale,fill = review_simple,
                 text = paste(product_category_name, "\n",
                              scales::percent(percent_sales,.01), " do faturamento com nota ",review_simple, "\n",
                              number_products, " produtos", "\n",
                              k_reais(total_gross_sale), " de faturamento",
                              sep = ""))) +
      geom_col() +
      labs(y = "Tamanho do Mercado",fill = 'Nota') +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      coord_flip() +
      scale_y_continuous(label = k_reais) +
      scale_fill_manual(values = c("steelblue","red")) +
      geom_text(aes(label = k_reais(total_gross_sale)),
                position = position_stack(vjust = 0.5)),tooltip = "text")
  )
  
  
# categories number_products outputs -----------------------------------------------------------------------------------
  
  output$number_products2 <- renderPlotly(
      ggplotly(ggplot(data = df_gross_sales_category2(),
             aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas),
                 y = percent_notas,
                 fill = review_simple,
                 text = paste(product_category_name,
                              "\n",scales::percent(percent_notas,.01), " dos produtos com nota ",review_simple,
                              "\n",number_products, " produtos",
                              "\n",k_reais(total_gross_sale), " de faturamento",sep = ""))) +
      geom_col() +
      scale_fill_manual(values = c("steelblue","red")) +
      scale_y_continuous(breaks = seq(0, 1, .2),label = scales::percent) +
      geom_text(aes(label = scales::percent(percent_notas,accuracy = 2)),
                position = position_stack(vjust = 0.5)) +
      coord_flip() +
      labs(x = "Categorias", y = "Porcentagem de avaliações") +
      theme(legend.position = "none"),tooltip = "text")
  )
  
  output$number_products3 <- renderPlotly(
      ggplotly(ggplot(data = df_gross_sales_category2(),
             aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas),
                 y = number_products,
                 fill = review_simple,
                 text = paste(product_category_name,
                              "\n",scales::percent(percent_notas,.01), " dos produtos com nota ",review_simple,
                              "\n",number_products, " produtos",
                              "\n",k_reais(total_gross_sale), " de faturamento",sep = ""))) +
      geom_col() +
      labs(y = "Número de itens comprados",fill = 'Nota') +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      coord_flip() +
      scale_fill_manual(values = c("steelblue","red")) +
      scale_y_continuous(labels = scales::label_comma(big.mark = ".",decimal.mark = ",")) +
      geom_text(aes(label = number_products),
                position = position_stack(vjust = 0.5)),tooltip = "text")
  )
  
  # value boxes/ infoBox
  
  output$approval_box <- renderValueBox({
    valueBox(
      created_category_approval(), "Notas Altas %", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$gross_sales_box <- renderValueBox({
    valueBox(
      created_category_gross_sales(), "Tamanho do mercado", icon = icon("credit-card", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$number_products_box <- renderValueBox({
    valueBox(
      created_category_number_products(), "Quantidade de itens", icon = icon("shopping-basket"),
      color = "green"
    )
  })
  
  
# orders -----------------------------------------------------
  
  output$header_pie_chart <- renderValueBox(
    valueBox(paste("Graficos de pizza da variavel ", input$new_category_name), subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )
  
  output$pie_chart_low1 <- renderPlotly(
    df_pie_chart() %>% 
      filter(review_simple == "baixa") %>% 
      plot_ly(labels = ~bucket_itens, values = ~number_products,
              marker = list(colors = c("steelblue", "grey", "red"),
                            line = list(color = '#FFFFFF', width = 1)),
              textposition = 'inside',
              textinfo = 'label+percent',
              showlegend = FALSE) %>% 
      add_pie(hole = .5) %>%
      layout(title = paste(emo::ji('angry'),'Quantidade de itens por pedido com notas baixas',emo::ji('angry')),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
   
  )
  
  output$pie_chart_high1 <- renderPlotly(
    df_pie_chart() %>% 
      filter(review_simple == "alta") %>% 
      plot_ly(labels = ~bucket_itens, values = ~number_products,
              marker = list(colors = c("steelblue", "grey", "red"),
                            line = list(color = '#FFFFFF', width = 1)),
              textposition = 'inside',
              textinfo = 'label+percent',
              showlegend = FALSE) %>% 
      add_pie(hole = .5) %>%
      layout(title = paste(emo::ji('smile'),'Quantidade de itens por pedido com notas altas',emo::ji('smile')),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  
  output$hist_order_low1 <- renderPlotly({
    ggplotly( 
      ggplot(data = df_hist_order() %>% 
               filter(review_simple == "baixa"),
             aes(x = number_products,
                 y = percent_prod,
                 text = paste(percent_prod_p,"\n",number_instances,sep = "")
                 )
             ) +
      geom_col(fill = "red") +
      scale_y_continuous(labels = scales::percent,breaks = scales::breaks_width(.25),,limits = c(0,1)) +
      labs(x = 'Quantidade de produtos no pedido',
           y = 'Porcentagem de pedidos'),
      tooltip = c('text'))
    
})
  
  output$hist_order_high1 <- renderPlotly({
    ggplotly(
      ggplot(data = df_hist_order() %>% filter(review_simple == "alta"),
             aes(x = number_products,
                 y = percent_prod,
                 text = paste(percent_prod_p,"\n",number_instances,sep = "")
                 )
             ) +
      geom_col(fill = "steelblue") +
      scale_y_continuous(labels = scales::percent,breaks = scales::breaks_width(.25),,limits = c(0,1)) +
        labs(x = 'Quantidade de produtos no pedido',
             y = 'Porcentagem de pedidos'),
      tooltip = c('text'))
    
})

# pie market -----------------------------------------------------------------------------------------------------------

  output$pie_chart_low2 <- renderPlotly(
    df_pie_chart() %>% 
      filter(review_simple == "baixa") %>% 
      plot_ly(labels = ~bucket_itens, values = ~total_gross_sale,
              marker = list(colors = c("steelblue", "grey", "red"),
                            line = list(color = '#FFFFFF', width = 1)),
              textposition = 'inside',
              textinfo = 'label+percent',
              showlegend = FALSE) %>% 
      add_pie(hole = .5) %>%
      layout(title = paste(emo::ji('angry'),'Tamanho do mercado com notas baixas',emo::ji('angry')),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  )
  
  output$pie_chart_high2 <- renderPlotly(
    df_pie_chart() %>% 
      filter(review_simple == "alta") %>% 
      plot_ly(labels = ~bucket_itens, values = ~total_gross_sale,
              marker = list(colors = c("steelblue", "grey", "red"),
                            line = list(color = '#FFFFFF', width = 1)),
              textposition = 'inside',
              textinfo = 'label+percent',
              showlegend = FALSE) %>% 
      add_pie(hole = .5) %>%
      layout(title = paste(emo::ji('smile'),'Tamanho do mercado com notas altas',emo::ji('smile')),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  
  output$hist_order_low2 <- renderPlotly({
    ggplotly(
      ggplot(data = df_hist_order() %>% 
               filter(review_simple == "baixa"),
             aes(x = number_products,
                 y = percent_prod_gs,
                 text = paste(percent_prod_gs_p,"\n",k_reais(total_gross_sale),sep = "")
             )
      ) +
        geom_col(fill = "red") +
        scale_y_continuous(labels = scales::percent,breaks = scales::breaks_width(.25),limits = c(0,1)) +
        labs(x = 'Quantidade de produtos no pedido',
             y = 'Tamanho do Mercado'),
      tooltip = c('text'))
    
  })
  
  output$hist_order_high2 <- renderPlotly({
    ggplotly(
      ggplot(data = df_hist_order() %>% 
               filter(review_simple == "alta"),
             aes(x = number_products,
                 y = percent_prod_gs,
                 text = paste(percent_prod_gs_p,"\n",k_reais(total_gross_sale),sep = "")
             )
      ) +
        geom_col(fill = "steelblue") +
        scale_y_continuous(labels = scales::percent,breaks = scales::breaks_width(.25),,limits = c(0,1)) +
        labs(x = 'Quantidade de produtos no pedido',
             y = 'Tamanho do Mercado'),
      tooltip = c('text'))
    
  })
  
# maps -----------------------------------------------------------------------------------------------------------------
  
  output$plot_map1 <- renderPlotly(
    ggplotly(ggplot(data = df_map()) +
      geom_sf(aes(fill = n_orders,geometry = geometry)) +
      geom_sf_text(aes(label = scales::percent(prop,2))) +
      geom_sf_text(aes(label = n_orders),nudge_y = .5) +
      scale_fill_distiller(palette = 'Purples',direction = 1) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x =element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(fill = "Quantidade de compras")
  )
)
  
  
  
  output$plot_map2 <- renderPlotly(
   ggplotly(
      ggplot(data = df_map()) +
      geom_sf(aes(fill = gross_sales)) +
      geom_sf_text(aes(label = scales::percent(prop_gross,2))) +
      geom_sf_text(aes(label = k_reais(gross_sales)),nudge_y = .5) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x =element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(fill = "Tamanho do Mercado") +
      scale_fill_distiller(label = k_reais,
                           palette = 'Purples',direction = 1)
  ))
  
  output$plot_map1_low <- renderPlotly(
    ggplotly(
      ggplot(data = df_map_low()) +
      geom_sf(aes(fill = n_orders,geometry = geometry)) +
      geom_sf_text(aes(label = scales::percent(prop,2))) +
      geom_sf_text(aes(label = n_orders),nudge_y = .5) +
      scale_fill_distiller(palette = 'Reds',direction = 1) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x =element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(fill = "Quantidade de compras")
  ))
  
  output$plot_map2_low <- renderPlotly(
    ggplotly(
      ggplot(data = df_map_low()) +
      geom_sf(aes(fill = gross_sales)) +
      geom_sf_text(aes(label = scales::percent(prop_gross,2))) +
      geom_sf_text(aes(label = k_reais(gross_sales)),nudge_y = .5) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x =element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(fill = "Tamanho do Mercado") +
      scale_fill_distiller(label = k_reais,
                           palette = 'Reds',direction = 1)
  ))
  
  output$plot_map1_high <- renderPlotly(
    ggplotly(
      ggplot(data = df_map_high()) +
      geom_sf(aes(fill = n_orders,geometry = geometry)) +
      geom_sf_text(aes(label = scales::percent(prop,2))) +
      geom_sf_text(aes(label = n_orders),nudge_y = .5) +
      scale_fill_distiller(palette = 'Blues',direction = 1) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x =element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(fill = "Quantidade de compras")
  ))
  
  output$plot_map2_high <- renderPlotly(
    ggplotly(
      ggplot(data = df_map_high()) +
      geom_sf(aes(fill = gross_sales)) +
      geom_sf_text(aes(label = scales::percent(prop_gross,2))) +
      geom_sf_text(aes(label = k_reais(gross_sales)),nudge_y = .5) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x =element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(fill = "Tamanho do Mercado") +
      scale_fill_distiller(label = k_reais,
                           palette = 'Blues',direction = 1)
  ))
  
  output$header_map_charts <- renderValueBox(
    valueBox(paste("Mapa da variavel ", input$new_category_name), subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )
  

# prices quantiles -----------------------------------------------------------------------------------------------------

  output$quantile_quantity <- renderPlotly(
    ggplotly(ggplot(data = df_quantiles(),aes(x = fct_reorder(score,max_order),
                                              y = prop,
                                              fill = review_simple,
                                              text = paste(n_orders," produtos","\n",
                                                           scales::percent(prop,.01)," do ",score," quartil","\n",
                                                           "preço máximo = ",reais(max_order),"\n",
                                                           "preço mínimo = ",reais(min_order),
                                                           sep = ""))) +
               geom_bar(stat = "identity",
                        position = "fill") +
               scale_fill_manual(values = c("steelblue","red")) +
               scale_y_continuous(breaks = seq(0, 1, .2),label = scales::percent) +
               labs(x = 'Quartis Preço Total do produto',y = 'Porcentagem',fill = "Nota"),tooltip = "text")
  )
  
  output$quantile_sales <- renderPlotly(
    ggplotly(ggplot(data = df_quantiles(),
                    aes(x = fct_reorder(score,max_order),
                        y = prop_sales,
                        fill = review_simple,
                        text = paste(k_reais(gross_sales), "\n",
                                     scales::percent(prop_sales,.01)," do ",score," quartil","\n",
                                     "preço máximo = ",reais(max_order),"\n",
                                     "preço mínimo = ",reais(min_order),
                                     sep = ""))) +
               geom_bar(stat = "identity",
                        position = "fill") +
               scale_fill_manual(values = c("steelblue","red")) +
               scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
               labs(x = 'Quartis Preço Total do produto',y = 'Porcentagem',fill = "Nota"),tooltip = "text")
  )
  
  output$density_plot <- renderPlot(
    df_created_category() %>% 
    ggplot() +
      aes(x = total_price,color = review_simple) +
      geom_density() +
      scale_color_manual(values = c("steelblue","red")) +
      labs(y = 'Densidade',x = 'Preço Total do produto',color = 'Nota') +
      scale_x_log10(labels = reais)
  )
  
  output$header_prices <- renderValueBox(
    valueBox(paste("Analise de preços totais", input$new_category_name),
             subtitle = "Preço total = Preço do produto + Frete",
             icon = NULL,
             color = "blue"
    )
  )


# payment methods ------------------------------------------------------------------------------------------------------
  output$header_payment_methods <- renderValueBox(
    valueBox(paste("Análise de Meio de pagamento", input$new_category_name),subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )
  
  output$payment_methods_qnt1 <- renderPlotly(
    ggplotly(ggplot(data = df_payments(),
                    aes(x = fct_reorder2(payment_method,review_simple,-prop_qnt),
                        y = prop_qnt,
                        fill = review_simple,
                        text = paste(payment_method,"\n",
                                     scales::percent(prop_qnt,.01), " dos produtos com nota ",review_simple,"\n",
                                     n_count, " produtos","\n",
                                     k_reais(gross_sales), " de faturamento",
                                     sep = ""))) +
               geom_col() +
               scale_fill_manual(values = c("steelblue","red")) +
               scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
               geom_text(aes(label = scales::percent(prop_qnt,accuracy = 2)),
                         position = position_stack(vjust = 0.5)) +
               coord_flip() +
               labs(x = "Meio de pagamento", y = "Porcentagem de avaliações") +
               theme(legend.position = "none"),tooltip = "text")
  )
  
  output$payment_methods_qnt2 <- renderPlotly(
    ggplotly(
      ggplot(data = df_payments(),
             aes(x = fct_reorder2(payment_method,review_simple,-prop_qnt),
                 y = n_count,
                 fill = review_simple,
                 text = paste(payment_method,"\n",
                              scales::percent(prop_qnt,.01), " dos produtos com nota ",review_simple,"\n",
                              n_count, " produtos","\n",
                              k_reais(gross_sales), " de faturamento",
                              sep = ""))) +
               geom_col() +
               scale_y_continuous(labels = scales::label_comma(big.mark = ".")) +
               scale_fill_manual(values = c("steelblue","red")) +
               geom_text(aes(label = scales::percent(prop_qnt,accuracy = 2)),
                         position = position_stack(vjust = 0.5)) +
               coord_flip() +
               labs(x = "Meio de pagamento", y = "Quantidade de produtos comprados") +
               theme(legend.position = "none"),tooltip = "text")
  )
  
  output$payment_methods_sales1 <- renderPlotly(
    ggplotly(ggplot(data = df_payments(),
                    aes(x = fct_reorder2(payment_method,review_simple,-prop_sales),
                        y = prop_sales,
                        fill = review_simple,
                        text = paste(payment_method,"\n",
                                     scales::percent(prop_sales,.01)," do faturamento com nota ",review_simple,"\n",
                                                          n_count, " produtos", "\n",
                                                          k_reais(gross_sales), " de faturamento",
                                                          sep = ""))) +
               geom_col() +
               scale_fill_manual(values = c("steelblue","red")) +
               scale_y_continuous(breaks = seq(0, 1, .2),label = scales::percent) +
               geom_text(aes(label = scales::percent(prop_sales,accuracy = 2)),
                         position = position_stack(vjust = 0.5)) +
               coord_flip() +
               labs(x = "Meio de pagamento", y = "Porcentagem de avaliações") +
               theme(legend.position = "none"),tooltip = "text")
  )
  
  output$payment_methods_sales2 <- renderPlotly(
    ggplotly(ggplot(data = df_payments(),
                    aes(x = fct_reorder2(payment_method,review_simple,-prop_sales),
                        y = gross_sales,
                        fill = review_simple,
                        text = paste(payment_method,"\n",
                                     scales::percent(prop_sales,.01)," do faturamento com nota ",review_simple,"\n",
                                     n_count, " produtos","\n",
                                     k_reais(gross_sales), " de faturamento",
                                     sep = ""))) +
               geom_col() +
               scale_fill_manual(values = c("steelblue","red")) +
               scale_y_continuous(labels = k_reais) + 
               geom_text(aes(label = scales::percent(prop_sales,accuracy = 2)),
                         position = position_stack(vjust = 0.5)) +
               coord_flip() +
               labs(x = "Meio de pagamento", y = "Tamanho do Mercado") +
               theme(legend.position = "none"),tooltip = "text")
  )
  
# announcement ----------------------------------------------------------------------------------------------------
  
  output$header_announcement <- renderValueBox(
    valueBox(paste("Análise das variáveis do anúncio", input$new_category_name),subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )
  
  output$announcement_qnt1 <- renderPlotly(
    ggplotly(ggplot(data = df_photos(),
                    aes(x = bucket_photos,
                        y = percent_notas,
                        fill = review_simple,
                        text = paste(scales::percent(percent_notas,.01),"\n",
                                     "Tamanho do mercado ",k_reais(total_gross_sale),"\n",
                                     number_products," Produtos",
                                     sep = ""))) +
               geom_bar(stat = "identity",
                        position = "fill") +
               scale_fill_manual(values = c("steelblue","red")) +
               scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
               geom_text(aes(label = scales::percent(percent_notas,accuracy = .1)),
                         position = position_stack(vjust = 0.5)) +
               labs(x = 'Quantidade de fotos',y = 'Porcentagem',fill = "Nota"),
             tooltip = "text")
  )
  
  output$announcement_qnt2 <- renderPlotly(
    ggplotly(
      ggplot(data = df_quantile_desc(),
             aes(x = quantil_desc,
                 y = percent_notas,
                 fill = review_simple,
                 text = paste(scales::percent(percent_notas,.01),"\n",
                              "Tamanho do mercado ",k_reais(total_gross_sale),"\n",
                              number_products," Produtos","\n",
                              "De ",min_caracters_desc," caracteres ate ",max_caracters_desc," caracteres",
                              sep = ""))) +
        geom_bar(stat = "identity",
                 position = "fill") +
        scale_fill_manual(values = c("steelblue","red")) +
        scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
        geom_text(aes(label = scales::percent(percent_notas,accuracy = .1)),
                  position = position_stack(vjust = 0.5)) +
        labs(x = 'Quantidade de caracteres',y = 'Porcentagem',fill = "Nota"),
      tooltip = "text")
  )
  
  output$announcement_sales1 <- renderPlotly(
    ggplotly(
      ggplot(data = df_photos(),
             aes(x = bucket_photos,
                 y = percent_sales,
                 fill = review_simple,
                 text = paste(scales::percent(percent_sales,.01),"\n",
                              "Tamanho do mercado ",k_reais(total_gross_sale),"\n",
                              number_products," Produtos",
                              sep = ""))) +
        geom_bar(stat = "identity",
                 position = "fill") +
        scale_fill_manual(values = c("steelblue","red")) +
        scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
        geom_text(aes(label = scales::percent(percent_sales,accuracy = .1)),
                  position = position_stack(vjust = 0.5)) +
        labs(x = 'Quantidade de fotos',y = 'Porcentagem',fill = "Nota"),
      tooltip = "text")
  )
  
  
  output$announcement_sales2 <- renderPlotly(
    ggplotly(
      ggplot(data = df_quantile_desc(),
             aes(x = quantil_desc,
                 y = percent_sales,
                 fill = review_simple,
                 text = paste(scales::percent(percent_sales,.01),"\n",
                              "Tamanho do mercado ",k_reais(total_gross_sale),"\n",
                              number_products," Produtos","\n",
                              "De ",min_caracters_desc," caracteres ate ",max_caracters_desc," caracteres",
                              sep = ""))) +
        geom_bar(stat = "identity",
                 position = "fill") +
        scale_fill_manual(values = c("steelblue","red")) +
        scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
        geom_text(aes(label = scales::percent(percent_sales,accuracy = .1)),
                  position = position_stack(vjust = 0.5)) +
        labs(x = 'Quantidade de caracteres',y = 'Porcentagem',fill = "Nota"),
      tooltip = "text"
    )
  )

  
  

# simulator -------------------------------------------------------------------------------------------------------
  
  output$header_simulator <- renderValueBox(
    valueBox(paste("Simulador da", input$new_category_name),subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )
  
  output$simulator_qnt1 <- renderPlotly(
    plot_ly(data = df_solution_2(),
            x = ~bucket_photos,y = ~quantil_desc,z = ~prop_quantity,type = "heatmap",colors = colorRamp(c("white", "steelblue")),
            text = ~paste("Quantidade de fotos: ",bucket_photos,"\n",
                          "Quantil de caracteres: ",quantil_desc,"\n",
                          "Porcentagem de notas altas: ",scales::percent(prop_quantity),"\n",
                          "Tamanho do Mercado: ",k_reais(gross_sales1),"\n",
                          "Numero de produtos: ",total_number_products ,"\n",
                          "De ",min_caracters_desc," caracteres ate ",max_caracters_desc," caracteres",
                          sep = "")
    )  %>% 
      layout(xaxis = list(title = 'Quantidade de fotos'),
             yaxis = list(title = 'Quantil de caracteres')) %>% 
      colorbar(title = "% notas altas")
  )
  
  output$simulator_qnt2 <- renderPlotly(
    plot_ly(data = df_solution_2(),
            x = ~bucket_photos,y = ~quantil_desc,z = ~total_number_products ,size = ~total_number_products ,
            marker = list(color = ~prop_quantity,
                          symbol = 'circle',
                          sizemode = 'diameter',
                          showscale = TRUE,
                          scalesize = .5,
                          colorscale = list(c(0, 1), c("white", "steelblue")),
                          colorbar = list(title = "% notas altas")),
            text = ~paste("Quantidade de fotos: ",bucket_photos,"\n",
                          "Quantil de caracteres: ",quantil_desc,"\n",
                          "Porcentagem de notas altas: ",scales::percent(prop_quantity),"\n",
                          "Tamanho do Mercado: ",k_reais(gross_sales1),"\n",
                          "Numero de produtos: ",total_number_products ,"\n",
                          "De ",min_caracters_desc," caracteres ate ",max_caracters_desc," caracteres",
                          sep = ""),
            hoverinfo = 'text'
    ) %>%
      layout(scene = list(xaxis = list(title = 'Quantidade de fotos'),
                          yaxis = list(title = 'Quantil de caracteres'),
                          zaxis = list(title = 'Numero de produtos')))
    
  )
  
  output$current_situation_quantity <- renderText(
    paste0(
            "Você está usando ",
             simulated_bucket_photos(),
             " fotos e seu anúncio se encaixa no ",
             simulated_quantil_desc(),
             " quantil em termos de caracteres, sua aprovação estimada é de ",
             scales::percent(
               simulated_prop_quantity()
             )
           )
  )
  
  
  output$possible_situation_quantity <- renderText(
      paste0(
        "Você deveria usar ",
        best_bucket_photos(),
        " fotos e manter a descrição do produto entre ",
        best_min_caracters_desc(),
        " e ",
        best_max_caracters_desc(),
        " caracteres podendo aumentar sua aprovação para ",
        scales::percent(best_prop_quantity()),
        " um aumento de ",
        scales::percent(estimated_increase_quantity()),
        " reduzindo o seu custo por produto em até ",reais(estimated_freight_gain_quantity())
      )
  )
  
  output$simulator_sales1 <- renderPlotly(
    plot_ly(data = df_solution_2(),
            x = ~bucket_photos,y = ~quantil_desc,z = ~prop_sales,type = "heatmap",colors = colorRamp(c("white", "steelblue")),
            text = ~paste("Quantidade de fotos: ",bucket_photos,"\n",
                          "Quantil de caracteres: ",quantil_desc,"\n",
                          "Porcentagem de notas altas: ",scales::percent(prop_sales),"\n",
                          "Tamanho do Mercado: ",k_reais(gross_sales1),"\n",
                          "Numero de produtos: ",total_number_products ,"\n",
                          "De ",min_caracters_desc," caracteres ate ",max_caracters_desc," caracteres",
                          sep = ""),
            hoverinfo = "text") %>% 
      layout(xaxis = list(title = 'Quantidade de fotos'),
             yaxis = list(title = 'Quantil de caracteres')) %>% 
      colorbar(title = "Porcentagem")
  )
  
  output$simulator_sales2 <- renderPlotly(
    plot_ly(data = df_solution_2(),
            x= ~bucket_photos,y = ~quantil_desc,z = ~gross_sales1,size = ~gross_sales1,
            marker = list(color = ~prop_quantity,
                          symbol = 'circle',
                          sizemode = 'diameter',
                          showscale = TRUE,
                          scalesize = .5,
                          colorscale = list(c(0, 1), c("white", "steelblue")),
                          colorbar = list(len = .25,title = "% notas alta")),
            text = ~paste("Quantidade de fotos: ",bucket_photos,"\n",
                          "Quantil de caracteres: ",quantil_desc,"\n",
                          "Porcentagem de notas altas: ",scales::percent(prop_sales),"\n",
                          "Tamanho do Mercado: ",k_reais(gross_sales1),"\n",
                          "Numero de produtos: ",total_number_products ,"\n",
                          "De ",min_caracters_desc," caracteres ate ",max_caracters_desc," caracteres",
                          sep = ""),
            hoverinfo = 'text'
    ) %>% 
      layout(scene = list(xaxis = list(title = 'Quantidade de fotos'),
                          yaxis = list(title = 'Quantil de caracteres'),
                          zaxis = list(title = 'Tamanho do mercado (reais)')))
    
  )
  
  output$current_situation_sales <- renderText(
    paste0(
      "Você está usando ",
      simulated_bucket_photos(),
      " fotos e seu anúncio se encaixa no ",
      simulated_quantil_desc(),
      " quantil em termos de caracteres, sua aprovação estimada é de ",
      scales::percent(
        simulated_prop_sales()
      )
    )
  )
  
  
  output$possible_situation_sales <- renderText(
    paste0(
      "Você deveria usar ",
      best_bucket_photos(),
      " fotos e manter a descrição do produto entre ",
      best_min_caracters_desc(),
      " e ",
      best_max_caracters_desc(),
      " caracteres podendo aumentar sua aprovação para ",
      scales::percent(best_prop_sales()),
      " um aumento de ",
      scales::percent(estimated_increase_sales()),
      " reduzindo o seu custo por produto em até ",reais(estimated_freight_gain_sales())
    )
  )
  
}



# shinny app -----------------------------------------------------------------------------------------------------------

shinyApp(ui, server)


