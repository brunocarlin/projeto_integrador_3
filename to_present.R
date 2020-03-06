library(tidyverse)
library(vroom)

df_customer <- vroom('olist/olist_customers_dataset.csv')
df_geo <- vroom('olist/olist_geolocation_dataset.csv')
df_order_itens <- vroom('olist/olist_order_items_dataset.csv')
df_order_payments <- vroom('olist/olist_order_payments_dataset.csv')
df_order_reviews <- vroom('olist/olist_order_reviews_dataset.csv')
df_orders <- vroom('olist/olist_orders_dataset.csv')
df_products <- vroom('olist/olist_products_dataset.csv')
df_sellers <- vroom('olist/olist_sellers_dataset.csv')


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
df_gross_sales_category2 %>% count()

df_gross_sales_category%>% ungroup %>%  summarise(soma_itens = sum(number_products))


p2 <- df_gross_sales_category2 %>% 
  ggplot(aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas),y = number_products,fill = review_simple)) +
  geom_col() +
  labs(y = "Número de itens comprados") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
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
  theme(legend.position = "none")

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
  summarise(number_products = n()) %>% 
  group_by(bucket_itens) %>% 
  mutate(percent_notas = number_products/sum(number_products))


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


