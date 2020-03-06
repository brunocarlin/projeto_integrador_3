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

# 
df_gross_sales_category2 %>%
  ungroup() %>% 
  ggplot(aes(x = fct_reorder2(product_category_name,review_simple,-percent_notas), y = percent_notas,text = number_products,fill = review_simple)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  geom_text(aes(label = scales::percent(percent_notas,accuracy = 2)),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  coord_flip()





df_gross_sales_category3 <- df_orders_itens_reviews_payments_products %>% 
  mutate(
    new_categories = case_when(
      product_category_name %in% c('cama_mesa_banho',
                                   'moveis_decoracao',
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


# graficos de pedidos -----------------------------------------------------

df_gross_sales_category5 <- df_orders_itens_reviews_payments_products %>% 
  mutate(
    new_categories = case_when(
      product_category_name %in% c('cama_mesa_banho',
                                   'moveis_decoracao',
                                   'moveis_escritorio') ~ 'nova_casa',
      TRUE ~ product_category_name
    ),
    ) %>%
  filter(new_categories == 'nova_casa') %>% 
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
  ungroup() %>% 
  ggplot(aes(x = fct_reorder2(bucket_itens,review_simple,-percent_notas), y = percent_notas,text = number_products,fill = review_simple)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  geom_text(aes(label = scales::percent(percent_notas,accuracy = 2)),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  coord_flip()

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

df_gross_sales_category6 %>%
  group_by(bucket_itens) %>% 
  summarise(number_products = sum(number_products)) %>% 
  ungroup() %>% 
  mutate(percent_prod = number_products/sum(number_products)) %>% 
  ungroup() %>% 
  ggplot(aes(x = '',y = percent_prod,fill = bucket_itens)) +
  geom_bar(position="fill", stat="identity",color = 'white') +
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent_prod*100), "%")),size =20, position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "") +
  theme_classic() + theme(axis.line = element_blank(),
                            axis.text = element_blank(),
                            axis.ticks = element_blank(),
                            plot.title = element_text(hjust = 0.5, color = "#666666"))

  

  
  
  
  
  
  coord_flip() +
  coord_polar("y", start=0) +
  scale_alpha() +
  blank_theme +
  geom_text(aes(label = scales::percent(percent_prod,2)),
            size = 3,
            position = position_stack(vjust = 0.5))
  

# bucket valor compra -----------------------------------------------------

df_gross_sales_order <- df_orders_itens_reviews_payments_products %>% 
  mutate(
    new_categories = case_when(
      product_category_name %in% c('cama_mesa_banho',
                                   'moveis_decoracao',
                                   'moveis_escritorio') ~ 'nova_casa',
      TRUE ~ product_category_name
    ),
  ) %>%
  filter(new_categories == 'nova_casa') %>% 
  group_by(order_id,review_simple) %>% 
  summarise(total_price = sum(total_price)) %>% 
  ungroup()
  

df_gross_sales_order %>%
  skimr::skim()


df_gross_sales_order %>% 
  mutate(first_quantile = quantile(total_price,probs = .25),
         second_quantile = quantile(total_price,probs = .50),
         third_quantile = quantile(total_price,probs = .75))

df_gross_sales_order_quantiles <- df_gross_sales_order %>% 
  mutate(probabilidade_quantil = total_price %>% ntile(10000),
  score = case_when(probabilidade_quantil < 2500 ~ 'first',
                  probabilidade_quantil >= 2500 & probabilidade_quantil < 5000 ~ 'second',
                  probabilidade_quantil >= 5000 & probabilidade_quantil < 7500 ~ 'third',
                  probabilidade_quantil >= 7500 ~ 'fourth'))


df_gross_sales_order_quantiles %>% 
  group_by(score,review_simple) %>% 
  summarise(n_orders = n(),
            max_order = max(total_price)) %>% 
  group_by(score) %>% 
  mutate(prop = n_orders/sum(n_orders)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder2(score,review_simple,-prop), y = prop,fill = review_simple)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  geom_text(aes(label = scales::percent(prop,accuracy = 2)),
            size = 3,
            position = position_stack(vjust = 0.25)) +
  geom_text(aes(label = n_orders),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = max_order),
            size = 3,
            position = position_stack(vjust = 0.75))

# tentar heatplot


# mapas -------------------------------------------------------------------


df_casa_nova_cust <- df_orders_itens_reviews_payments_products %>% 
  mutate(
    new_categories = case_when(
      product_category_name %in% c('cama_mesa_banho',
                                   'moveis_decoracao',
                                   'moveis_escritorio') ~ 'nova_casa',
      TRUE ~ product_category_name
    ),
  ) %>%
  filter(new_categories == 'nova_casa') %>% 
  left_join(df_customer)

df_casa_nova_cust %>% 
  group_by(customer_state,review_simple) %>% 
  summarise(n_orders = n()) %>% 
  group_by(customer_state) %>% 
  mutate(prop = n_orders/sum(n_orders)) %>%
  ggplot(aes(x = fct_reorder2(customer_state,review_simple,-prop), y = prop,fill = review_simple)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  geom_text(aes(label = scales::percent(prop,accuracy = 2)),
            size = 3,
            position = position_stack(vjust = 0.25)) +
  geom_text(aes(label = n_orders),
            size = 3,
            position = position_stack(vjust = 0.5))
  

df_casa_nova_cust %>% 
  group_by(customer_state,review_simple) %>% 
  summarise(n_orders = n()) %>% 
  group_by(customer_state) %>% 
  mutate(prop = n_orders/sum(n_orders)) %>%
  ggplot(aes(x = fct_reorder2(customer_state,review_simple,-n_orders), y = n_orders,fill = review_simple)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("steelblue","red")) +
  scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
  geom_text(aes(label = scales::percent(prop,accuracy = 2)),
            size = 3,
            position = position_stack(vjust = 0.25)) +
  geom_text(aes(label = n_orders),
            size = 3,
            position = position_stack(vjust = 0.5))
  
