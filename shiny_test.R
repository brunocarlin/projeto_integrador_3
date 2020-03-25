library(shiny)
library(shinydashboard)
library(vroom)
library(tidyverse)
library(plotly)
library(DT)
library(emojifont)

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
  menuItem(
    "Exploração das Regiões",
    tabName = "maps",
    icon = icon("map")
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
    fluidRow(
      valueBoxOutput("approval_box"),
      valueBoxOutput("gross_sales_box"),
      valueBoxOutput("number_products_box")
    ),
    conditionalPanel(condition = "input.var == 'number_products'",
                      fluidRow(
                        column(
                          width = 4,
                          plotOutput(outputId = "number_products2")
                          ),
                        column(
                          width = 8,
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
                         width = 4,
                         plotOutput(outputId = "total_gross_sale2")
                       ),
                       column(
                         width = 8,
                         plotOutput(outputId = "total_gross_sale3")
                       )
                     ),
                     fluidRow(
                       box(width = 12,
                           DTOutput(outputId = "total_gross_sale1")
                                    
                          )
                      )
                    )
    
                  ),
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
    tabItem(tabName = "maps",
            fluidRow(valueBoxOutput("header_map_charts", width = 12)),
            conditionalPanel(condition = "input.var == 'number_products'",
                             fluidRow(plotOutput("plot_map1",height = "1200px"))),
            conditionalPanel(condition = "input.var == 'total_gross_sale'",
                             fluidRow(plotOutput("plot_map2",height = "1200px")))
    )
    )
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
  

  df_created_category <- reactive(selected() %>%
    filter(product_category_name == input$new_category_name))
  
  created_category_approval <- reactive(df_created_category() %>% 
                                          count(review_simple) %>%
                                          mutate(percent_notas = n/sum(n)) %>% 
                                          pull(percent_notas) %>% 
                                          head(1) %>% 
                                          scales::percent())
  
  created_category_gross_sales <- reactive(df_created_category() %>%
    summarise(total_gross =sum(total_price)) %>%
    pull() %>%
    scales::dollar(scale = 1/1000,suffix = "k",largest_with_cents = 0))
  
  created_category_number_products <- reactive(df_created_category() %>%
    count() %>% 
    pull())
  
  df_pie_chart <- reactive(df_created_category() %>% 
    group_by(order_id,review_simple) %>% 
    summarise(total_gross_sale = sum(total_price,na.rm = T),
              mean_price = mean(total_price,na.rm = T),
              number_products = n()) %>%
    mutate(bucket_itens = if_else(number_products >= 3,'3 ou mais',number_products %>% as.character())) %>% 
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
    left_join(df_state(),by = c("nome")))
  
  
# server outputs --------------------------------------------------------------------------------------------------


  output$titulo <- renderValueBox(
    valueBox(paste("Top", input$number, "resultados"), subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )


# categories total_gross_sale1 outputs ----------------------------------------------------------------------------------------

  output$total_gross_sale1 <- renderDT(
    df_gross_sales_category3() %>% 
      mutate(alta = scales::percent(alta,accuracy = .01),
             baixa = scales::percent(baixa,accuracy = .01),
             total_gross_sale = scales::dollar(total_gross_sale,
                                               scale = 1/1000,
                                               suffix = "k",
                                               largest_with_cents = 0),
             mean_price = scales::number(mean_price,accuracy = 0.01),
             ),
    options = list(
      dom = 'Bfrtip', buttons = c('copy', 'excel', 'pdf', 'print', 'colvis')
    ),
    extensions = c('Buttons',"Responsive")
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
                position = position_stack(vjust = 0.5)) +
      coord_flip() +
      labs(x = "Categorias", y = "Porcentagem de avaliações") +
      theme(legend.position = "none")
  )
  
  output$total_gross_sale3 <- renderPlot(
    df_gross_sales_category2() %>% 
      ggplot(aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas),y = total_gross_sale,fill = review_simple)) +
      geom_col() +
      labs(y = "Tamanho do Mercado",fill = 'Nota') +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      coord_flip() +
      scale_fill_manual(values = c("steelblue","red")) +
      scale_y_continuous(label =scales::dollar_format(scale = 1/1000,suffix = "k",largest_with_cents = 0)) +
      geom_text(aes(label = scales::dollar(total_gross_sale,scale = 1/1000,suffix = "k",largest_with_cents = 0)),
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
                position = position_stack(vjust = 0.5)) +
      coord_flip() +
      labs(x = "Categorias", y = "Porcentagem de avaliações") +
      theme(legend.position = "none")
  )
  
  output$number_products3 <- renderPlot(
    df_gross_sales_category2() %>% 
      ggplot(aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas),y = number_products,fill = review_simple)) +
      geom_col() +
      labs(y = "Número de itens comprados",fill = 'Nota') +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      coord_flip() +
      scale_fill_manual(values = c("steelblue","red")) +
      geom_text(aes(label = number_products),
                position = position_stack(vjust = 0.5))
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
      layout(title = paste(emoji('angry'),'Quantidade de itens por pedido com notas baixas',emoji('angry')),
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
      layout(title = paste(emoji('smile'),'Quantidade de itens por pedido com notas altas',emoji('smile')),
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
      scale_y_continuous(labels = scales::percent) +
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
      scale_y_continuous(labels = scales::percent) +
        labs(x = 'Quantidade de produtos no pedido',
             y = 'Porcentagem de pedidos'),
      tooltip = c('text'))
    
})


# pie market ------------------------------------------------------------------------------------------------------

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
      layout(title = paste(emoji('angry'),'Tamanho do mercado com notas baixas',emoji('angry')),
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
      layout(title = paste(emoji('smile'),'Tamanho do mercado com notas altas',emoji('smile')),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
  
  output$hist_order_low2 <- renderPlotly({
    ggplotly(
      ggplot(data = df_hist_order() %>% 
               filter(review_simple == "baixa"),
             aes(x = number_products,
                 y = percent_prod_gs,
                 text = paste(percent_prod_gs_p,"\n",scales::dollar(total_gross_sale,scale = 1/1000,
                                                                    suffix = "k",
                                                                    largest_with_cents = 0),sep = "")
             )
      ) +
        geom_col(fill = "red") +
        scale_y_continuous(labels = scales::percent) +
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
                 text = paste(percent_prod_gs_p,"\n",scales::dollar(total_gross_sale,scale = 1/1000,
                                                                    suffix = "k",
                                                                    largest_with_cents = 0),sep = "")
             )
      ) +
        geom_col(fill = "steelblue") +
        scale_y_continuous(labels = scales::percent) +
        labs(x = 'Quantidade de produtos no pedido',
             y = 'Tamanho do Mercado'),
      tooltip = c('text'))
    
  })
  
# maps ------------------------------------------------------------------------------------------------------------
  
  output$plot_map1 <- renderPlot(
    df_map() %>% sf::st_sf() %>% 
      ggplot() +
      geom_sf(aes(fill = n_orders,geometry = geometry)) +
      geom_sf_text(aes(label = scales::percent(prop,2))) +
      geom_sf_text(aes(label = n_orders),nudge_y = .5) +
      scale_fill_distiller(palette = 'PuBu',direction = 1) +
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
  
  output$plot_map2 <- renderPlot(
    df_map() %>% sf::st_sf() %>%
      ggplot() +
      geom_sf(aes(fill = gross_sales)) +
      geom_sf_text(aes(label = scales::percent(prop_gross,2))) +
      geom_sf_text(aes(label = scales::dollar(gross_sales,
                                              scale = 1/1000,
                                              suffix = "k",
                                              largest_with_cents = 0)),nudge_y = .5) +
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
      scale_fill_distiller(label = scales::dollar_format(scale = 1/1000,
                                                         suffix = "k",
                                                         largest_with_cents = 0),
                           palette = 'PuBu',direction = 1)
  )
  
  output$header_map_charts <- renderValueBox(
    valueBox(paste("Mapa da variavel ", input$new_category_name), subtitle = NULL, icon = NULL,
             color = "blue"
    )
  )
  
}



shinyApp(ui, server)


