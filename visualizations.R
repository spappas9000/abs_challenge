library(tidyverse)
library(ggpubr)
library(gt)
library(gtExtras)
library(mlbplotR)
library(splines)
library(broom)

plate_width = 17 + 2 * (9/pi)

home_plate = data.frame(x = c(-0.708, -0.708, -0.708, 0, 0.708),
                        y = c(0.5, 0.3, 0.3, 0.15, 0.5),
                        xend = c(0.708, -0.708, 0, 0.708, 0.708),
                        yend = c(0.5, 0.5, 0.15, 0.3, 0.3))

strike_zone = data.frame(x = c(-0.3156886, 0.3156886, -plate_width/24, -plate_width/24),
                         xend = c(-0.3156886, 0.3156886, plate_width/24, plate_width/24),
                         y = c(1.4, 1.4, 2.2, 2.9),
                         yend = c(3.6, 3.6, 2.2, 2.9))

hitterplot_data <- modeldata %>%
  mutate(match = paste0(stand, "HH v. ", p_throws, "HP")) %>%
  filter(challenge_hitter == 1) %>%
  add_count(match, name = "n") %>%
  mutate(
    match_label = paste0(match, "\n", "n = ", n)
  )

hitterplot <- hitterplot_data %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                  linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = "black", alpha = 0, linewidth = 1.2) +
  geom_segment(data = home_plate,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1) +
  geom_segment(data = strike_zone,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1, linetype = "dashed", alpha = 0.9) +
  scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_x_continuous(limits = c(-1.25, 1.25)) +
  scale_y_continuous(limits = c(0, 3.8)) +
  labs(title = "Hitters",
       subtitle = "Challenges attempt to overturn a called strike to a ball") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank()) +
  facet_wrap(vars(match_label), ncol = 4)

hitterplot

ggsave("plots/hitterdensityplot.jpg", hitterplot, width = 11)

catcherplot_data <- modeldata %>%
  mutate(match = paste0(stand, "HH v. ", p_throws, "HP")) %>%
  filter(challenge_catcher == 1) %>%
  add_count(match, name = "n") %>%
  mutate(
    match_label = paste0(match, "\n", "n = ", n)
  )

catcherplot <- catcherplot_data %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                  linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = "black", alpha = 0, linewidth = 1.2) +
  geom_segment(data = home_plate,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1) +
  geom_segment(data = strike_zone,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1, linetype = "dashed", alpha = 0.9) +
  scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_x_continuous(limits = c(-1.25, 1.25)) +
  scale_y_continuous(limits = c(0, 3.8)) +
  labs(title = "Catchers",
       subtitle = "Challenges attempt to overturn a called ball to a strike") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank()) +
  facet_wrap(vars(match_label), ncol = 4)

catcherplot

ggsave("plots/catcherdensityplot.jpg", catcherplot, width = 11)

challenge_densityplot <- annotate_figure(
  ggarrange(hitterplot, catcherplot, ncol = 2),
  top = text_grob("Automatic Ball Strike Density", size = 20, face = "bold")
)

ggsave("plots/challenge_densityplot.jpg", challenge_densityplot, width = 13, height = 11.22384)

mod1_hitter_data <- list(
  summary = as.data.frame(coef(summary(mod1_call_change_hitter))) %>%
    rownames_to_column(var = "var") %>%
    mutate(Estimate_exp = round(exp(Estimate), 3), Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|z|)`, digits = 3, eps = 1e-3)),
  conf95 = as.data.frame(confint(mod1_call_change_hitter, level = 0.95, method = "Wald")) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(`2.5 %`, 3), conf_2 = round(`97.5 %`, 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>%
  reduce(merge, by = "var") %>%
  mutate(n = nobs(mod1_call_change_hitter)) %>%
  select(1, 2, 6:7, 14:15)

mod1_hitter_table <- mod1_hitter_data %>%
  mutate(
    var = case_when(
       var == "(Intercept)" ~ "Intercept",
       var == "bat_score_diff" ~ "Score Difference (Hitter)",
       var == "challenges_remaining" ~ "Challenges Remaining",
       var == "delta" ~ "Change in Run Expectancy",
       var == "I(plate_x_adj^2)" ~ "Horizontal Plate Location Sq",
       var == "I(plate_z_adj^2)" ~ "Vertical Plate Location Sq",
       var == "inning" ~ "Inning",
       var == "plate_x_adj" ~ "Horizontal Plate Location",
       var == "plate_z_adj" ~ "Vertical Plate Location"
    )
  ) %>%
  gt(rowname_col = "var") %>%
  tab_options(row_group.as_column = T) %>%
  cols_label(
    Estimate = md("$\\hat{\\beta}$"),
    Estimate_exp = md("$e^{\\hat{\\beta}}$"),
    pval = md("$p$"),
    confidence = md("$\\hat{\\beta}: 95\\% CI$"),
    confidence_exp = md("$e^{\\hat{\\beta}}: 95\\% CI$")
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = md("Logistic Regression Model Parameters for **Hitter ABS Challenges**")
  ) %>%
  gtsave("hitter_table.png")

mod1_catcher_data <- list(
  summary = as.data.frame(coef(summary(mod1_call_change_catcher))) %>%
    rownames_to_column(var = "var") %>%
    mutate(Estimate_exp = round(exp(Estimate), 3), Estimate = round(Estimate, 3), pval = format.pval(`Pr(>|z|)`, digits = 3, eps = 1e-3)),
  conf95 = as.data.frame(confint(mod1_call_change_catcher, level = 0.95, method = "Wald")) %>%
    rownames_to_column(var = "var") %>%
    mutate(conf_1 = round(`2.5 %`, 3), conf_2 = round(`97.5 %`, 3), conf_exp_1 = round(exp(conf_1), 3), 
           conf_exp_2 = round(exp(conf_2), 3), confidence = paste0(conf_1, ", ", conf_2),
           confidence_exp = paste0(conf_exp_1, ", ", conf_exp_2))
) %>%
  reduce(merge, by = "var") %>%
  mutate(n = nobs(mod1_call_change_catcher)) %>%
  select(1, 2, 6:7, 14:15)

mod1_catcher_table <- mod1_catcher_data %>%
  mutate(
    var = case_when(
      var == "(Intercept)" ~ "Intercept",
      var == "bat_score_diff" ~ "Score Difference (Hitter)",
      var == "challenges_remaining" ~ "Challenges Remaining",
      var == "delta" ~ "Change in Run Expectancy",
      var == "I(plate_x_adj^2)" ~ "Horizontal Plate Location Sq",
      var == "I(plate_z_adj^2)" ~ "Vertical Plate Location Sq",
      var == "inning" ~ "Inning",
      var == "plate_x_adj" ~ "Horizontal Plate Location",
      var == "plate_z_adj" ~ "Vertical Plate Location"
    )
  ) %>%
  gt(rowname_col = "var") %>%
  tab_options(row_group.as_column = T) %>%
  cols_label(
    Estimate = md("$\\hat{\\beta}$"),
    Estimate_exp = md("$e^{\\hat{\\beta}}$"),
    pval = md("$p$"),
    confidence = md("$\\hat{\\beta}: 95\\% CI$"),
    confidence_exp = md("$e^{\\hat{\\beta}}: 95\\% CI$")
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = md("Logistic Regression Model Parameters for **Catcher ABS Challenges**")
  )

re_exploration <- list(
  hitter_re_exploration %>%
    filter(lower >= 0.63278047) %>%
    arrange(desc(lower)) %>%
    mutate(rowid = row_number()),
  catcher_re_exploration %>%
    mutate(delta = ifelse(upper < 0, abs(upper), abs(lower))) %>%
    filter(lower >= 0.10113620 | catcher == 592663) %>%
    arrange(desc(delta)) %>%
    mutate(rowid = row_number())
) %>%
  reduce(merge, by = "rowid") %>%
  select(batter, estimate_batter = estimate.x, se_batter = se.x, lower_batter = lower.x, upper_batter = upper.x, catcher, 
         estimate_catcher = estimate.y, se_catcher = se.y, lower_catcher = lower.y, upper_catcher = upper.y)

re_exploration %>%
  gt() %>%
  tab_header(
    title = md("Largest Player Random Effects"),
    subtitle = md("Furthest Away From Zero at the Extremes of the 95th Percent Confidence Interval")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_label(
    -c(batter, catcher) ~ "",
    estimate_batter ~ "hEstimate",
    se_batter ~ "hSE",
    lower_batter ~ "hLower",
    upper_batter ~ "hUpper",
    estimate_catcher ~ "cEstimate",
    se_catcher ~ "cSE",
    lower_catcher ~ "cLower",
    upper_catcher ~ "cUpper"
  ) %>%
  gt_fmt_mlb_dot_headshot(columns = c("batter", "catcher"), height = 60) %>%
  tab_spanner(
    label = "Hitter",
    columns = c(1:5),
    id = "Hitter"
  ) %>%
  tab_spanner(
    label = "Catcher",
    columns = c(6:10),
    id = "Catcher"
  )
  # tab_style(
  #   style = cell_text(weight = "bold"),
  #   locations = cells_body(columns = c(Number_Hitting, Pos_Hitting, `Matchup (Hitter)`,
  #                                      Number_Pitching, Pos_Pitching, `Matchup (Pitcher)`))
  # ) %>%
  # tab_style(
  #   style = cell_text(color = "#CC3433"),
  #   locations = cells_title(groups = c("title", "subtitle"))
  # ) %>%
  # tab_style(
  #   style = list(
  #     cell_fill(color = "#0E3386"),
  #     cell_text(weight = "bold", color = "#CC3433")
  #   ),
  #   locations = cells_column_spanners()
  # ) %>%
  # tab_style(
  #   style = list(
  #     cell_fill(color = "#0E3386"),
  #     cell_text(weight = "bold", color = "#CC3433")
  #   ),
  #   locations = cells_column_labels()
  # ) %>%
  # tab_style(
  #   style = cell_text(align = "center"),
  #   locations = cells_column_spanners()
  # ) %>%
  # tab_style(
  #   style = cell_text(align = "center"),
  #   locations = cells_column_labels()
  # ) %>%
  # tab_style(
  #   style = cell_text(size = px(20)),
  #   locations = cells_body(columns = c(Hitter, Pitcher))
  # ) %>%
  # tab_style(
  #   style = cell_borders(
  #     sides = "right",
  #     color = "gray",
  #     weight = px(3)
  #   ),
  #   locations = cells_body(columns = Opponent_Hitting)
  # ) %>%
  # opt_stylize(
  #   style = 1, color = "gray"
  # ) %>%
  # tab_options(
  #   heading.background.color = "#0E3386",
  #   heading.title.font.size = px(60),
  #   heading.subtitle.font.size = px(18)
  # )

challenge %>%
  filter(fielder_2 %in% c(592663, 700337), challenger == "Catcher") %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                  linewidth = .5, bins = 5, show.legend = F, alpha = 0.8) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = "black", alpha = 0, linewidth = 1.2) +
  geom_segment(data = home_plate,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1) +
  geom_segment(data = strike_zone,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1, linetype = "dashed", alpha = 0.9) +
  scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(0, 4.25)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.75), axis.ticks = element_blank(),
        strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank()) +
  facet_wrap(vars(fielder_2), labeller = as_labeller(c("592663" = "Realmuto, J.T.", "700337" = "Quero, Edgar")))

hitterplot_naylor <- modeldata %>%
  filter(batter == 647304) %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                  linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = "black", alpha = 0, linewidth = 1.2) +
  geom_segment(data = home_plate,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1) +
  geom_segment(data = strike_zone,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1, linetype = "dashed", alpha = 0.9) +
  scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_y_continuous(limits = c(0, 4.25)) +
  annotate(geom = "label", label = "Josh Naylor", x = 0, y = 3.85, fontface = "bold", size = 10) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.75), axis.ticks = element_blank(),
        strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank())

challenge_densityplot2 <- annotate_figure(
  ggarrange(hitterplot_turang, hitterplot_naylor, ncol = 2),
  top = text_grob("Automatic Ball Strike Density", size = 20, face = "bold")
)

ggsave("plots/challenge_densityplot2.jpg", challenge_densityplot2, width = 13, height = 11.22384)

model_results <- bind_rows(
  tidy(mod1_call_change_catcher, conf.int = TRUE, exponentiate = F) %>%
    mutate(model = "Catcher"),
  tidy(mod1_call_change_hitter, conf.int = TRUE, exponentiate = F) %>%
    mutate(model = "Hitter")
) %>%
  filter(term %in% c("inning", "delta", "challenges_remaining", "bat_score_diff"))

modelresultsplot <- model_results %>%
  mutate(term = case_when(
    term == "inning" ~ "Inning",
    term == "delta" ~ "Delta Run Exp",
    term == "challenges_remaining" ~ "Challenges Remaining",
    term == "bat_score_diff" ~ "Score Difference (Hitting Team)"
  )) %>%
  ggplot(aes(x = estimate, y = term, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.5),
    height = 0.2
  ) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2.5
  ) +
  labs(  
    x = "Log Odds (95% CI)",
    y = NULL,
    color = "Model"
  ) +
  theme_minimal()

ggsave("plots/modelresultsplot.jpg", modelresultsplot)

## Grid for predictions

polynomial_zones <- expand.grid(
  plate_x_adj = seq(-7, 7, by = 0.01),
  plate_z_adj = seq(-7, 7, by = 0.01),
  delta = 0.1,
  bat_score_diff = 2,
  inning = 6,
  challenges_remaining = 1
)

mod1_call_change_hitter <- glm(
  challenge_hitter ~ ns(plate_z_adj, 6) + ns(plate_x_adj, 6) + delta + bat_score_diff + inning + challenges_remaining,
  data = modeldata_hitter,
  family = "binomial"
)

mod1_call_change_catcher <- glm(
  challenge_catcher ~ ns(plate_z_adj, 6) + ns(plate_x_adj, 6) + delta + bat_score_diff + inning + challenges_remaining,
  data = modeldata_catcher,
  family = "binomial"
)

mod1_call_change_catcher <- glm(
  challenge_catcher ~ plate_z_adj + plate_x_adj + delta + bat_score_diff + inning * challenges_remaining + I(plate_z_adj^2) + 
    I(plate_x_adj^2) + I(plate_z_adj^3) + I(plate_x_adj^3) + I(plate_z_adj^4) + I(plate_x_adj^4),
  data = modeldata_catcher,
  family = "binomial"
)

mod1_catcher_pred <- predict(mod1_call_change_catcher, newdata = polynomial_zones, type = "response")

auc(modeldata_catcher$challenge_catcher, mod1_catcher_pred)

catcher_polynomial_zones <- cbind(polynomial_zones, mod1_catcher_pred)

catcherpredplot <- catcher_polynomial_zones %>%
  ggplot(aes(x = plate_x_adj, y = plate_z_adj, color = mod1_catcher_pred)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(-2, 2)) +
  annotate("label", label = "← Inside", x = -1.5, y = 1.5) +
  annotate("label", label = "Outside →", x = 1.5, y = 1.5) +
  labs(
    title = "Probability of an ABS Challenge for Hitters",
    subtitle = "Plate Location Normalized and All Other Predictors Held Constant",
    x = "Horizontal Plate Location (ft.)",
    y = "Vertical Plate Location (ft.)",
    color = NULL
  ) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.75), axis.ticks = element_blank(),
        strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank())

ggsave("plots/catcherpredplot.jpg", catcherpredplot)

## General Pitch Density

pitchdensityplot <- rbind(modeldata_catcher %>% select(plate_x, plate_z), modeldata_hitter %>% select(plate_x, plate_z)) %>%
  ggplot(aes(x = plate_x, y = plate_z)) +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = as.numeric(after_stat(..level..))),
                  linewidth = .5, bins = 9, show.legend = F, alpha = 0.8) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = "black", alpha = 0, linewidth = 1.2) +
  geom_segment(data = home_plate,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1) +
  geom_segment(data = strike_zone,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "black", size = 1, linetype = "dashed", alpha = 0.9) +
  scale_fill_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_color_gradientn(colors = c('dodgerblue4', 'skyblue1', 'white', 'red', 'firebrick2'), na.value = NA) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  labs(title = "Heatmap of Pitches in Data",
       subtitle = "Non-waste takes up to July 31, 2026") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"), axis.line = element_blank(), axis.title = element_blank(),
        axis.text = element_blank())

ggsave("plots/pitchdensity.jpg", pitchdensityplot)

# AUC Curves

plot(roc(modeldata_catcher$challenge_catcher, predict(mod2_call_change_catcher, newdata = modeldata_catcher, type = "response")),
     main = "ROC Curve for Catcher Model")

plot(roc(modeldata_hitter$challenge_hitter, predict(mod2_call_change_hitter, newdata = modeldata_hitter, type = "response")),
     main = "ROC Curve for Hitter Model")
