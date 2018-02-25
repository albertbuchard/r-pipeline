
#' Does the correlations between left and right vars using corr.test
#' Then plots a vertical graph with correlations confidence intervals.
#'
#'
#' @param data data.frame contains all the columns to correlate
#' @param left_vars character contains the variable names to plot on the left
#' @param right_vars character contains the variable names to plot on the right (nested)
#' @param .adjust character adjust procedure default to holm (see corr.test)
#' @param .method character statistical method default to spearman
#' @param .xlab character x label name
#' @param .ylab character y label name (left variables)
#' @param .right_var_legend_name character name for the right_variable legend
#'
#' @export
plot_correlation_ci = function (data, left_vars, right_vars,
                                .adjust = "holm", .method="spearman", .xlab = "Correlations CI", .ylab = "Left variables",
                                .labels_left = NULL,
                                .right_var_legend_name = NULL,
                                ...) {
  columns = c(left_vars, right_vars)
  numeric_data = data %>% select(columns) %>% mutate_all(funs(as.numeric))
  correlations = corr.test(numeric_data, method = .method, adjust=.adjust, ...)

  # melt the correlations into a tibble, keep only the lower triangular and filter out the diagonal
  # This allow to use the CI table from the corr.test function
  correlation_gathered = correlations %$% r
  correlation_gathered[upper.tri(correlation_gathered)] = NA
  diag(correlation_gathered) = NA

  correlation_gathered %<>% as.tibble %>% mutate(variable_1 = names(.)) %>% select(variable_1, everything(.)) %>%
    gather(key = "variable_2", value = "correlation", -variable_1, na.rm = T)  %>% filter(variable_1 != variable_2)

  p_gathered = correlations %$% p
  p_gathered[upper.tri(p_gathered)] = NA
  diag(p_gathered) = NA

  p_gathered %<>% as.tibble %>% mutate(variable_1 = names(.)) %>% select(variable_1, everything(.)) %>%
    gather(key = "variable_2", value = "p", -variable_1, na.rm = T)  %>% filter(variable_1 != variable_2)


  # Join the CI
  correlation_gathered %<>% cbind(correlations$ci) %>% # cbind(p = p_gathered$p) %>% if need adjustment
    mutate_at(vars(contains("variable")), funs(as.factor))

  l = pipeline::generate_log_interpolation_function(from_x = 0,to_x = 0.05,from_y = 1,to_y = 0.4,t=5)

  correlation_gathered %<>% mutate(significativity = ifelse(p<0.05, p, 1)) %>% mutate(alpha = ifelse(significativity ==1, 0, l(significativity)))


  correlation_to_plot = correlation_gathered %>% filter(variable_2 %in% left_vars) %>% filter(variable_1 %in% right_vars)
  correlation_to_plot %<>% mutate(nudge = 1:50 * 0.1 * rep(c(1,-1),25)) %>% arrange(desc(variable_1))


  nudge_factor = function(v) {
    max_step = 2
    counts = table(as.character(v)) %>% as.tibble %>% mutate(count = 0, step = 2*max_step/(n+4),
                                                             base_position = 2*max_step* 1:NROW(.),
                                                             mid = floor(n/2))
    s = 1
    positions = jitters = rects = rep(0, NROW(v))
    labels = rep("", NROW(v))
    for (i in 1:NROW(v)) {
      c = counts %>% filter(Var1 == v[i]) %>% select(count, step, base_position, mid)
      jitters[i] = (c$count+2)*c$step*s
      positions[i] = c$base_position + jitters[i]
      labels[i] = ifelse(c$count == c$mid, as.character(v[i]), "")
      rects[i] = ifelse(c$count == 0, c$base_position, NA)
      # s = -s
      counts %<>% mutate(count =  ifelse(Var1 == v[i], count+1, count))
    }
    return(tibble(labels, positions, jitters, rects))
  }


  correlation_to_plot %<>% cbind(nudge_factor(correlation_to_plot$variable_2))

  position_labels_to_show = correlation_to_plot$positions[correlation_to_plot$labels!=""]
  labels_to_show = correlation_to_plot$labels[correlation_to_plot$labels!=""]
  rect_positions = sort(correlation_to_plot$rects[!is.na(correlation_to_plot$rects)])

  ymax = c(rect_positions[1:NROW(rect_positions)], +Inf)
  rects = data.frame(xmin=-Inf, xmax = +Inf, ymin = c(-Inf,rect_positions[1:NROW(rect_positions)]),
                     ymax = ymax,
                     color_back = as.factor(rep(c(1,2),NROW(ymax))[1:NROW(ymax)]))

  if (!is.null(.labels_left)) {
    labels_to_show = .labels_left
  }

  legend_name = "Variable"
  if (!is.null(.right_var_legend_name)) {
    legend_name = .right_var_legend_name
  }


  # position_labels_to_show = position_labels_to_show + by/2
  pl = ggplot(correlation_to_plot) +
    geom_rect(data = rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=color_back), alpha=0.3)+
    # scale_fill_brewer(direction = -1) +
    scale_fill_manual(values=c("white", "gray"))+
    #scale_fill_manual(values=randomColor(n_vars, luminosity="light", hue="blue"))+
    geom_errorbarh(aes(xmax = upper, xmin = lower, y = positions, x = correlation, color = variable_1, alpha=alpha), height=.3) +
    geom_point(aes(y = positions, x = correlation, color = variable_1, alpha=alpha, size=alpha)) +
    scale_colour_brewer(palette = "Paired", name=legend_name) +
    geom_vline(xintercept = 0, linetype=2, color="darkgray") +
    xlab("Correlation CI") +
    scale_y_continuous(.ylab, breaks = position_labels_to_show, labels = labels_to_show) +
    guides(fill=FALSE) +
    guides(alpha=FALSE) +
    guides(size=FALSE) +
    theme_bw()

  return(pl)
}