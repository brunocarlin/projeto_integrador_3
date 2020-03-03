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


df_orders %>% add_count(order_id) %>% filter(n >1) %>% View()

# Uma compra pode ter mais de um item

df_order_itens %>% add_count(order_id) %>% filter(n >1) %>% View()

# ver datas de entrega (uma data de entrega por seller)

df_order_itens %>% group_by(order_id,seller_id) %>% summarise(quantas_datas = n_distinct(shipping_limit_date)) %>% View('shipping')

# carrinho de compras pode ter mais de um vendedor

df_order_itens %>% group_by(order_id) %>% summarise(many = n_distinct(seller_id)) %>% View("seller")

df_order_itens %>% filter(order_id == '1c11d0f4353b31ac3417fbfa5f0f2a8a') %>% View()

# quando isso acontece podemos ter mais de uma data de entrega para o mesmo pedido

df_order_itens %>% group_by(order_id) %>% mutate(many = n_distinct(shipping_limit_date)) %>% View()

# grafico dos status da entrega

df_orders_itens_reviews %>%
  filter(order_status != 'delivered') %>% 
  group_by(review_simple) %>%
  count(order_status) %>% 
  ggplot(aes(x= order_status,y = n,fill = review_simple)) +
  geom_col()

# Items

df_orders %>% count(order_status)
df_orders_itens <- df_orders %>% 
  left_join(df_order_itens) %>% 
  filter(product_id %>% is.na %>% `!`) %>% 
  mutate(total_price = price + freight_value)

df_orders_itens %>% count(order_status)
df_orders_itens %>% 
  skimr::skim()

# Fazer grafico distribuicao
df_orders_itens %>% count(order_item_id) %>% View()

# Reviews

df_orders_itens %>% 
  anti_join(df_order_reviews) %>% 
  View()

df_order_reviews %>% add_count(order_id) %>% filter(n >1) %>% arrange(order_id) %>% View()

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

# Payments

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


# df_orders_itens_reviews_payments %>% 
#   mutate(diffs = (total_payment_value -total_price) %>% round) %>% 
#   filter(diffs > 1|diffs < -1) %>% 
#   View()

theme_set(new = theme_minimal())

# tipos de produto
df_orders_itens_reviews_payments_products <- df_orders_itens_reviews_payments %>%
  left_join(df_products)

# boxplot ficou com muitos outliers, deixar pos escolha de pedidos

df_orders_itens_reviews_payments_products %>% 
  ggplot(aes(x = review_simple,y = total_price)) +
  geom_boxplot()



# category analysis



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



df_gross_sales_category2 %>% 
  ggplot(aes(x = fct_reorder(product_category_name,number_products),y = number_products,fill = review_simple)) +
  geom_col() +
  coord_flip()


df_gross_sales_category2 %>% 
  ggplot(aes(x = fct_reorder(product_category_name,total_gross_sale),y = total_gross_sale,fill = review_simple)) +
  geom_col() +
  coord_flip()

df_gross_sales_category2 %>%
  select(review_simple,percent_notas) %>% 
  pivot_wider(names_from = review_simple,values_from = percent_notas) %>% 
  ggplot(aes(x = fct_reorder(product_category_name,baixa),y = baixa)) +
  geom_col() +
  coord_flip()

df_gross_sales_category3 <- df_orders_itens_reviews_payments_products %>% 
  mutate(
    new_categories = case_when(
      product_category_name %in% c('cama_mesa_banho',
                                   'beleza_saude',
                                   'moveis_escritorio') ~ 'nova_casa',
      TRUE ~ product_category_name
    )) %>%
  group_by(new_categories,review_simple) %>% 
  summarise(total_gross_sale = sum(total_price,na.rm = T),
            mean_price = mean(total_price,na.rm = T),
            number_products = n()) %>% 
  group_by(new_categories) %>%
  mutate(percent_notas = number_products/sum(number_products))

products_to_use <- df_gross_sales_category3 %>% 
  summarise(total_values = sum(number_products)) %>% 
  arrange(-total_values) %>% 
  top_n(20) %>%
  filter(new_categories %>% is.na() %>% `!`) %>% 
  pull(new_categories)

df_gross_sales_category4 <- df_gross_sales_category3 %>% filter(new_categories %in% products_to_use)


df_gross_sales_category4 %>%
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(new_categories,number_products),y = number_products,fill = review_simple)) +
  geom_col() +
  coord_flip()

# fazer grafico por estado do nova_casa


