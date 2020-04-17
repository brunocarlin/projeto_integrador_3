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



# start plots -----------------------------------------------------------------------------------------------------


# explore product category ----------------------------------------------------------------------------------------


df_gross_sales_category <- df_orders_itens_reviews_payments_products %>% 
  group_by(product_category_name,review_simple) %>% 
  summarise(total_gross_sale = sum(total_price,na.rm = T),
            mean_price = mean(total_price,na.rm = T),
            number_products = n()) %>% 
  group_by(product_category_name) %>%
  mutate(percent_notas = number_products/sum(number_products))

products_to_use <- df_gross_sales_category %>% 
  summarise(total_values = sum(number_products)) %>% 
  arrange(-total_values) %>% 
  top_n(20) %>%
  filter(product_category_name %>% is.na() %>% `!`) %>% 
  pull(product_category_name)

df_gross_sales_category2 <- df_gross_sales_category %>% filter(product_category_name %in% products_to_use)

p2 <- df_gross_sales_category2 %>% 
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
  
p2

p1 <- df_gross_sales_category2 %>%
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

p1

gridExtra::grid.arrange(p1,p2,nrow = 1)


# filtra casa nova ------------------------------------------------------------------------------------------------

df_casa_nova <- df_orders_itens_reviews_payments_products %>% 
  mutate(
    new_categories = case_when(
      product_category_name %in% c('cama_mesa_banho',
                                   'moveis_decoracao',
                                   'moveis_escritorio') ~ 'df_casa_nova',
      TRUE ~ product_category_name
    ),
  ) %>%
  filter(new_categories == 'df_casa_nova')



# grafico pizza itens ordem ---------------------------------------------------------------------------------------

# graficos de pedidos -----------------------------------------------------

df_gross_sales_category5 <- df_casa_nova %>% 
  group_by(order_id,review_simple) %>% 
  summarise(total_gross_sale = sum(total_price,na.rm = T),
            mean_price = mean(total_price,na.rm = T),
            number_products = n()) %>%
  mutate(bucket_itens = if_else(number_products >= 3,'3 ou mais',number_products %>% as.character())) %>% 
  group_by(bucket_itens)

df_gross_sales_category6 <- df_gross_sales_category5 %>% 
  group_by(review_simple,bucket_itens) %>% 
  summarise(number_products = n(),total_gross_sale = sum(total_gross_sale)) %>% 
  group_by(bucket_itens) %>% 
  mutate(percent_notas = number_products/sum(number_products),
         percent_notas_gs = total_gross_sale/sum(total_gross_sale))


df_gross_sales_category6 %>%
  group_by(bucket_itens) %>% 
  summarise(number_products = sum(number_products)) %>% 
  ungroup() %>% 
  mutate(percent_prod = number_products/sum(number_products)) %>% 
  ungroup() %>% 
  ggplot(aes(x = '',y = percent_prod,fill = bucket_itens)) +
  scale_fill_manual(values=c("steelblue", "grey", "red"))+
  geom_bar(position="fill", stat="identity",color = 'white') +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(percent_prod*100), "%")),size =8, position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = 'Quantidade de itens por pedido', title = "") +
  theme_classic() +
  theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))


# grafico da regiao -----------------------------------------------------------------------------------------------

library(brazilmaps)

br_maps <- brazilmaps::get_brmap("State")

br_sigla <- readxl::read_excel('table_brazil.xlsx')

br_maps_2 <- br_maps %>%
  as.data.frame() %>% 
  left_join(br_sigla %>% mutate(Estado = Estado %>% str_to_upper),by = c("nome" ="Estado")) %>% 
  rename(customer_state = Sigla)
# join customer -----------------------------------------------------------

df_casa_nova_cust <- df_casa_nova %>%
  left_join(df_customer)



# join region -----------------------------------------------------------------------------------------------------

df_casa_nova_cust_state <- df_casa_nova_cust %>% 
  left_join(br_sigla,by = c('customer_state' = 'Sigla'))

df_to_plot <- df_casa_nova_cust_state %>% 
  group_by(Region,review_simple) %>% 
  summarise(number_products = n()) %>% 
  group_by(Region) %>% 
  mutate(percent_notas = number_products/sum(number_products))

p7 <- df_to_plot %>%
  ungroup() %>% 
  ggplot(aes(x = fct_reorder2(Region,review_simple,-percent_notas), y = percent_notas,text = number_products,fill = review_simple)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  geom_text(aes(label = scales::percent(percent_notas,accuracy = 2)),
            size = 4.5,
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  labs(x = NULL, y = "Porcentagem de avaliações") +
  theme(legend.position = "none",
        text = element_text(size = 14),axis.title.x = element_text(size = 14))

p7

# plot cust -------------------------------------------------------------------------------------------------------


df_casa_nova_cust_pre <- df_casa_nova_cust %>% 
  group_by(customer_state) %>% 
  summarise(n_orders = n()) %>% 
  mutate(prop = n_orders/sum(n_orders))

df_state <-  df_orders_itens_reviews_payments_products_customer %>%
  group_by(Estado) %>% 
  summarise(n_orders = n(),gross_sales = sum(total_price)) %>% 
  mutate(prop = n_orders/sum(n_orders),
         nome = Estado %>% str_to_upper(),
         prop_gross = gross_sales/sum(gross_sales),
         label_prod = paste(scales::percent(prop,2),
                            n_orders))


br_maps_3 <- br_maps_2 %>% 
  left_join(df_state) %>%
  sf::st_sf()

p <- ggplot(data = br_maps_3) +
  aes(text = n_orders) +
  geom_sf(aes(fill = n_orders)) +
  geom_sf_text(aes(label = scales::percent(prop,2))) +
  scale_fill_distiller(palette = 'PuBu',trans = "reverse") +
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

ggplot(data = br_maps_3) +
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

# casa sudeste ------------------------------------------------------------

filter_states <- c('SP','RJ','MG','ES')

df_casa_nova_cust_sudeste <- df_casa_nova_cust %>%
  filter(customer_state %in% filter_states)


# valor plots -------------------------------------------------------------

df_to_plot <- df_casa_nova_cust_sudeste %>% 
  mutate(probabilidade_quantil = total_price %>% ntile(10000),
         score = case_when(probabilidade_quantil < 2500 ~ 'primeiro',
                           probabilidade_quantil >= 2500 & probabilidade_quantil < 5000 ~ 'segundo',
                           probabilidade_quantil >= 5000 & probabilidade_quantil < 7500 ~ 'terceiro',
                           probabilidade_quantil >= 7500 ~ 'quarto'))


p3 <- df_to_plot %>% 
  group_by(score,review_simple) %>% 
  summarise(n_orders = n(),
            min_order = min(total_price),
            max_order = max(total_price)) %>% 
  group_by(score) %>% 
  mutate(prop = n_orders/sum(n_orders)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(score,max_order), y = prop,fill = review_simple,text = )) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  geom_text(aes(label = max_order),
            size = 10,
            position = position_stack(vjust = .8)) +
  geom_text(aes(label = min_order),
            size = 10,
            position = position_stack(vjust = .2)) +
  labs(x = 'Quartis Preço Total do produto (entrega + produto)',y = 'Porcentagem',fill = "Nota") +
  theme(text = element_text(size = 24))


p3

ggplotly(p3,tooltip = c("min_order","max"))
p4 <- df_casa_nova_cust_sudeste %>% 
  ggplot() +
  aes(x = total_price,color = review_simple) +
  geom_density() +
  scale_color_manual(values = c("steelblue","red")) +
  labs(y = 'Densidade',x = 'Preço Total do produto',color = 'Nota') +
  scale_x_log10()

p4
gridExtra::grid.arrange(p3,p4)


# heatmap meio de pagamento -----------------------------------------------

one_if_present <- function(x1,x2,x3,x4) {
  ifelse(x1 %>% is.na,0,1) +ifelse(x2 %>% is.na,0,1) + ifelse(x3%>% is.na,0,1)+ ifelse(x4%>% is.na,0,1)
}

name_if_present <- function(x1,x2,x3,x4) {
  text_vector <- character(4)
  ifelse(x1 %>% is.na,NA_character_,text_vector[1] <- "credit_card")
  ifelse(x2 %>% is.na,NA_character_,text_vector[2] <- "boleto")
  ifelse(x3 %>% is.na,NA_character_,text_vector[3] <- "voucher")
  ifelse(x4 %>% is.na,NA_character_,text_vector[4] <- "debit_card")
  text_vector %>% list()
}

df_to_unnest <- df_orders_itens_reviews_payments_products %>% 
  mutate(formas = one_if_present(credit_card,boleto,voucher,debit_card)) %>% 
  rowwise() %>% 
  mutate(names_to_extend = name_if_present(credit_card,boleto,voucher,debit_card))

df_unnest <- df_to_unnest %>% 
  unnest() %>% 
  filter(names_to_extend != "")

df_to_plot <- df_unnest %>% 
  group_by(formas,names_to_extend) %>% 
  summarise(quantidade_compras = n()) %>% 
  ungroup() %>% 
  mutate(formas = formas %>% as.character(),
         percent_compras = quantidade_compras/sum(quantidade_compras))



df_to_plot %>% 
  ggplot(aes(x = fct_reorder(names_to_extend,quantidade_compras),y = formas,fill = quantidade_compras)) +
  geom_tile() +
  geom_text(aes(label = quantidade_compras),
            size = 5) +
  geom_text(aes(label = scales::percent(percent_compras,2)),
            size = 5,position = position_nudge(y = -.1)) +
  scale_fill_distiller(palette = 'PuBu',trans = "reverse") +
  labs(x = "Meio de pagamento",y = 'Quantidade de meios de pagamento',fill = "Quantidade de compras") +
  theme(text = element_text(size = 16))

# testing variables -------------------------------------------------------

df_casa_nova_cust_sudeste %>% names()

df_to_plot <- df_casa_nova_cust_sudeste %>% 
  mutate(probabilidade_quantil = product_description_lenght %>% ntile(10000),
         score = case_when(probabilidade_quantil < 1000 ~ '1º',
                           probabilidade_quantil >= 1000 & probabilidade_quantil < 2000 ~ '2º',
                           probabilidade_quantil >= 2000 & probabilidade_quantil < 3000 ~ '3º',
                           probabilidade_quantil >= 3000 & probabilidade_quantil < 4000 ~ '4º',
                           probabilidade_quantil >= 4000 & probabilidade_quantil < 5000 ~ '5º',
                           probabilidade_quantil >= 5000 & probabilidade_quantil < 6000 ~ '6º',
                           probabilidade_quantil >= 6000 & probabilidade_quantil < 7000 ~ '7º',
                           probabilidade_quantil >= 7000 & probabilidade_quantil < 8000 ~ '8º',
                           probabilidade_quantil >= 8000 & probabilidade_quantil < 9500 ~ '9º',
                           probabilidade_quantil >= 9000 ~ '10º'))


p3 <- df_to_plot %>% 
  group_by(probabilidade_quantil,review_simple) %>% 
  summarise(n_orders = n(),
            min_order = min(product_description_lenght),
            max_order = max(product_description_lenght)) %>% 
  group_by(probabilidade_quantil) %>% 
  mutate(prop = n_orders/sum(n_orders)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(probabilidade_quantil,max_order), y = prop,fill = review_simple,text = )) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  geom_text(aes(label = max_order),
            size = 10,
            position = position_stack(vjust = .8)) +
  geom_text(aes(label = min_order),
            size = 10,
            position = position_stack(vjust = .2)) +
  labs(x = 'product_description_lenght',y = 'Porcentagem',fill = "Nota") +
  theme(text = element_text(size = 24))


p3




df_to_plot <- df_casa_nova_cust_sudeste %>% 
  mutate(bucket_photos = case_when(product_photos_qty == 1 ~ "1 foto",
                                   product_photos_qty > 1 & product_photos_qty <=6 ~ "2-6 fotos",
                                   product_photos_qty > 6 ~ "6+ fotos"))


p3 <- df_to_plot %>% 
  group_by(bucket_photos,review_simple) %>% 
  summarise(n_orders = n(),
            min_order = min(product_photos_qty),
            max_order = max(product_photos_qty)) %>% 
  group_by(bucket_photos) %>% 
  mutate(prop = n_orders/sum(n_orders)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(bucket_photos,max_order), y = prop,fill = review_simple,text = )) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  labs(x = 'product_photos_qty',y = 'Porcentagem',fill = "Nota") +
  theme(text = element_text(size = 24))


p3

quantile_maker <- function(quantile_number,precision,position){
  (10^precision/quantile_number)*position
}

quantile_6 <- partial(quantile_maker,6,4)


quantile_6(2)

df_test <- df_casa_nova_cust_sudeste %>% 
  mutate(bucket_photos = case_when(product_photos_qty == 1 ~ "1 foto",
                                   product_photos_qty > 1 & product_photos_qty <=3 ~ "2-3 fotos",
                                   product_photos_qty > 3 & product_photos_qty <=6 ~ "3-6 fotos",
                                   product_photos_qty > 6 ~ "6+ fotos"),
         probabilidade_quantil = product_description_lenght %>% ntile(10000),
         quantil = case_when(probabilidade_quantil < quantile_6(1) ~ '1º',
                           probabilidade_quantil >= quantile_6(1) & probabilidade_quantil < quantile_6(2) ~ '2º',
                           probabilidade_quantil >= quantile_6(2) & probabilidade_quantil < quantile_6(3) ~ '3º',
                           probabilidade_quantil >= quantile_6(3) & probabilidade_quantil < quantile_6(4) ~ '4º',
                           probabilidade_quantil >= quantile_6(4) & probabilidade_quantil < quantile_6(5) ~ '5º',
                           probabilidade_quantil < quantile_6(6) ~ '6º'
         ) %>%
           as_factor() %>% 
           forcats::fct_relevel("1º","2º","3º","4º","5º","6º")) 




p3 <- df_test %>% 
  group_by(bucket_photos,review_simple) %>% 
  summarise(n_orders = n(),
            min_order = min(product_photos_qty),
            max_order = max(product_photos_qty)) %>% 
  group_by(bucket_photos) %>% 
  mutate(prop = n_orders/sum(n_orders)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(bucket_photos,max_order), y = prop,fill = review_simple,text = )) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  labs(x = 'product_photos_qty',y = 'Porcentagem',fill = "Nota") +
  theme(text = element_text(size = 24))

p3

#photos


df_test %>% 
  group_by(bucket_photos,quantil,review_simple) %>%
  summarise(n_orders = n(),
            gross_sales = sum(total_price),
            caracteres = mean(product_description_lenght) %>% round) %>% 
  group_by(bucket_photos,quantil) %>% 
  mutate(prop = n_orders/sum(n_orders),
         prop_sales = gross_sales/sum(gross_sales)) %>% 
  filter(review_simple == "alta") %>% 
  plot_ly(x= ~bucket_photos,y = ~quantil,z = ~prop,type = "heatmap",colors = colorRamp(c("white", "steelblue")),
          text = ~paste("gross_sales: ",gross_sales,"\n",
                        "num: ",n_orders))


df_test %>% 
  group_by(bucket_photos,quantil,review_simple) %>%
  summarise(n_orders = n(),
            gross_sales = sum(total_price)) %>% 
  group_by(bucket_photos,quantil) %>% 
  mutate(prop = n_orders/sum(n_orders),
         prop_sales = gross_sales/sum(gross_sales)) %>% 
  filter(review_simple == "alta") %>% 
  arrange(prop_sales) %>% 
  head(1) %>% 
  pull(bucket_photos)


df_test %>% 
  group_by(bucket_photos,quantil,review_simple) %>%
  summarise(n_orders = n(),
            gross_sales = sum(total_price)) %>% 
  group_by(bucket_photos,quantil) %>% 
  mutate(prop = n_orders/sum(n_orders),
         prop_sales = gross_sales/sum(gross_sales)) %>% 
  filter(review_simple == "alta") %>% 
  arrange(quantil) %>% 
  head(1) %>% 
  pull(quantil)



