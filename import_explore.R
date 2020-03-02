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

# carrinho de compras pode ter mais de um vendedor

df_order_itens %>% group_by(order_id) %>% summarise(many = n_distinct(seller_id)) %>% View()

df_orders_itens %>% filter(order_id == '1c11d0f4353b31ac3417fbfa5f0f2a8a') %>% View()

# quando isso acontece podemos ter mais de uma data de entrega para o mesmo pedido

df_order_itens %>% group_by(order_id) %>% mutate(many = n_distinct(shipping_limit_date)) %>% View()

# Items

df_orders_itens <- df_orders %>% 
  left_join(df_order_itens) %>% 
  filter(product_id %>% is.na %>% `!`) %>% 
  mutate(total_price = price + freight_value)


df_orders_itens %>% 
  skimr::skim()

# Reviews

df_orders_itens %>% 
  anti_join(df_order_reviews) %>% 
  View()


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
    payment_installments = mean(payment_installments),
    payment_sequential_max = max(payment_sequential),
    value_to_pay = sum(payment_value)) %>% 
  ungroup() %>% 
  filter(payment_type != 'not_defined')

df_order_payments3 <-
  df_order_payments2 %>% pivot_wider(
    names_from = payment_type,
    values_from = c(payment_installments, payment_sequential_max, value_to_pay)) %>% 
  rowwise() %>% 
  mutate(total_payment_value = sum(c(value_to_pay_credit_card,value_to_pay_boleto,value_to_pay_voucher,value_to_pay_debit_card),na.rm = T))

df_orders_itens_reviews_payments <- df_orders_itens_reviews %>% 
  left_join(df_order_payments3)

#

theme_set(new = theme_minimal())

# tipos de produto
df_orders_itens_reviews_payments_products <- df_orders_itens_reviews_payments %>%
  left_join(df_products)
# boxplot ficou com muitos outliers

df_orders_itens_reviews_payments_products %>% 
  ggplot(aes(x = review_simple,y = total_price)) +
  geom_boxplot()



# category analysis

df_gross_sales_category <- df_orders_itens_reviews_payments_products %>% 
  group_by(product_category_name,review_simple) %>% 
  summarise(total_gross_sale = sum(total_price,na.rm = T),
            mean_price = mean(total_price,na.rm = T),
            number_products = n())


df_gross_sales_category %>% 
  ggplot(aes(x = fct_reorder(product_category_name,number_products),y = number_products,fill = review_simple)) +
  geom_col() +
  coord_flip()


df_gross_sales_category %>% 
  ggplot(aes(x = fct_reorder(product_category_name,total_gross_sale),y = total_gross_sale,fill = review_simple)) +
  geom_col() +
  coord_flip()




