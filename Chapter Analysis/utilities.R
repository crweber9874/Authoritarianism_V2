# General GG theme
ggtheme <- theme(
  plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0.5, colour = "#3C3C3C", size = 15),
  axis.text.x = element_text(size = 13, colour = "#535353", face = "bold"),
  axis.text.y = element_text(size = 13, colour = "#535353", face = "bold"),
  axis.title = element_text(size = 13, colour = "#535353", face = "bold"),
  axis.title.y = element_text(size = 13, colour = "#535353", face = "bold", vjust = 1.5),
  axis.ticks = element_blank(),
  strip.text.x = element_text(size = 13),
  panel.grid.major = element_line(colour = "#D0D0D0", size = .25),
  panel.background = element_rect(fill = "white"),
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 16)
)

## Zero one normalization
zero.one <- function(x) {
  min.x <- min(x, na.rm = T)
  max.x <- max(x - min.x, na.rm = T)
  return((x - min.x) / max.x)
}

# BRMS Estimation Function,
logit_linear_models <- function(data = ANES_2020, dependent.variable = "trans.military",
                                independent.variables = c("female", "age", "college", "income", "authoritarianism"),
                                race = "white",
                                model = "logit_model",
                                chains = 3, cores = 8,
                                seed = 1234, iter = 1000) {
  if (!require(brms)) {
    install.packages("brms")
    library(brms)
  }


  model_type <- NA
  if (model == "logit_model") {
    model_type <- "bernoulli"
  }
  if (model == "linear_model") {
    model_type <- "gaussian"
  }

  # Misc Functions
  firstL <- function(x) {
    sapply(x, function(y) {
      paste(toupper(substring(y, 1, 1)), substring(y, 2, nchar(y)), sep = "")
    })
  }

  print(paste("This returns three models, one for each group. The DV is", dependent.variable, "with", iter, "posterior draws. It takes a bit"))
  # The data
  group <- race
  data_t <- filter(data, race == group) %>% mutate(authoritarianism_2 = authoritarianism^2)
  # The Model
  formula <- paste(dependent.variable, paste(c(independent.variables, "authoritarianism_2"), collapse = " + "), sep = " ~ ")
  suppressWarnings({
    fit <- brm(formula,
      family = model_type,
      data = data_t,
      chains = chains,
      cores = cores,
      seed = seed,
      iter = iter,
      refresh = 0
    )
    # The predictions
    predData <- data %>%
      tibble() %>%
      subset(select = c(independent.variables, dependent.variable)) %>%
      na.omit() %>%
      data_grid(
        female = mean(female), age = mean(age),
        college = mean(college), income = mean(income),
        authoritarianism = seq_range(authoritarianism, n = 5)
      ) %>%
      mutate(authoritarianism_2 = authoritarianism^2) %>%
      add_epred_draws(fit)
    # The plot
    plot <- predData %>% ggplot(aes(x = authoritarianism, y = .epred, color = "black")) +
      stat_summary(fun = median, geom = "point", shape = 20, size = 5, color = "black") +
      ggtheme +
      geom_point(size = 1.5, alpha = 0.05, shape = 18, color = "black") +
      ggtitle(firstL(race)) +
      theme(legend.key = element_blank())
    return(plot)
  })
}


## Create the final plot for each item
## Function input
# race values
# plot labels and styling,
# dependent variable, outcome.name
predPlot <- function(RACE = c("white", "black", "latino"), plot.title = "trans.military", dependent.variable = "trans.military",
                     data = ANES_2020, y.limits = c(0, 0.5), plot.y = "logit", ...) {
  for (race in c("white", "black", "latino")) {
    assign(paste0("race_", race), logit_linear_models(ANES_2020, dependent.variable = dependent.variable, race = race, ...))
  }
  #   Construct the cowplot

  y.label <- NA
  if (plot.y == "logit") {
    y.label <- paste0("Pr(", plot.title, ")")
  }
  if (plot.y == "linear") {
    y.label <- paste0(plot.title)
  }



  row.grid <- plot_grid(
    race_white + xlab("") + theme(legend.position = "none") + ylab(y.label) + ylim(y.limits),
    race_black + theme(legend.position = "none") + ylab("") + xlab("Authoritarianism") + ylim(y.limits),
    race_latino + theme(legend.position = "none") + xlab("") + ylab("") + ylim(y.limits),
    rel_widths = c(1, 1, 1),
    ncol = 3,
    align = "v",
    axis = "b"
  )
  ### Draw a title and combine with row.grid

  title <- ggdraw() +
    draw_label(
      plot.title,
      fontface = "bold",
      x = 0,
      hjust = 0,
      size = 20,
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )

  plot <- plot_grid(title, row.grid, ncol = 1, rel_heights = c(0.1, 1))

  return(plot)
}
