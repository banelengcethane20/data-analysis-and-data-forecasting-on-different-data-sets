```{r ,echo = FALSE}
pwc=algae %>%
  gather(key="variables", value="value",chlorophyll,biovolume)%>%
  group_by(variables) %>%
  games_howell_test(value~class)%>%
  select(-estimate,-conf.low,-conf.high)
pwc
pwc <- pwc %>% add_xy_position(x = "class")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 15.985, p= "9.5e-13<0.0001", parameter = "4,1162",
  type = "expression", detailed = TRUE
)
pwc

ggboxplot(
  algae, x = "class", y = c("chlorophyll", "biovolume"), 
  merge = TRUE, palette = "jco"
) + 
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, tip.length = 0, 
    step.increase = 0.1, step.group.by = "variables",
    color = "variables"
  ) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression"))
```