# page 10: try it yourself
# answer to q1, q2 and q3
housing_data %>%
  filter(city == "Brazoria County") %>%
  group_by(year) %>%
  summarize(total_sales = sum(sales, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = total_sales)) + 
  geom_point()

# pages 20-21: try it yourself
addr <- data.frame(name = c("Alice","Bob",
                            "Carol","Dave",
                            "Eve"),
                   email = c("alice@company.com",
                             "bob@company.com",
                             "carol@company.com",
                             "dave@company.com",
                             "eve@company.com"),
                   stringsAsFactors = FALSE)

phone <- data.frame(firstname = c("Bob","Carol",
                                 "Dave","Eve",
                                 "Frank"),
                    phone = c("919 555-1111",
                              "919 555-2222",
                              "919 555-3333",
                              "310 555-4444",
                              "919 555-5555"),
                    stringsAsFactors = FALSE)

# Left join
addr %>% 
  left_join(phone, by = c("name" = "firstname"))

# Right join
addr %>% 
  right_join(phone, by = c("name" = "firstname"))

# Inner join
addr %>% 
  inner_join(phone, by = c("name" = "firstname"))

# Full join
addr %>% 
  full_join(phone, by = c("name" = "firstname"))
