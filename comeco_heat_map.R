df_orders_itens_reviews_payments_products %>% 
  mutate(
    new_categories = case_when(
      product_category_name %in% c('cama_mesa_banho',
                                   'moveis_decoracao',
                                   'moveis_escritorio') ~ 'nova_casa',
      TRUE ~ product_category_name
    ),
  ) %>%
  filter(new_categories == 'nova_casa') %>% 
  select(order_id,review_simple,credit_card,boleto,voucher,debit_card) %>% 
  mutate(formas = one_if_present(credit_card,boleto,voucher,debit_card))


one_if_present <- function(x1,x2,x3,x4) {
  ifelse(x1 %>% is.na,0,1) +ifelse(x2 %>% is.na,0,1) + ifelse(x3%>% is.na,0,1)+ ifelse(x4%>% is.na,0,1)
}

one_if_present <- function(x1,x2,x3,x4) {
  ifelse(x1 %>% is.na,0,1) +ifelse(x2 %>% is.na,0,1) + ifelse(x3%>% is.na,0,1)+ ifelse(x4%>% is.na,0,1)
}

one_if_present(1,NA,2,0)
