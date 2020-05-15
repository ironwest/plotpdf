#' Plot PDF by ggplot.
#'
#' This function will plot PDF of any built in function.
#'
#' @param distribution Character string indicating probability distribution function. Eg "norm", "t", "chisq". Internally, "d", "p" are added as prefix of string. eg. dnorm(), pnorm().
#' @param xrange X axis range for plot.
#' @param ... Arguments passed on to distribution. eg mean=1, sd = 2 for "norm".
#' @import ggplot2
#' @import magrittr
#' @import tibble
#' @import dplyr
#' @import stringr
#' @importFrom rlang .data
#' @export

plot_pdf <- function(distribution, xrange, ...){
  dFUN <- eval(parse(text = str_c("d", distribution)))
  pFUN <- eval(parse(text = str_c("p", distribution)))

  gdat <- tibble(x = seq(xrange[1], xrange[2], length=1000)) %>%
    mutate(y = dFUN(.data$x, ...))

  max_density <- gdat$y %>% max()

  baseg <- ggplot(gdat) +
    geom_line(aes(x = .data$x, y = .data$y)) +
    labs(x = "X", y = "Density")

  return(baseg)
}

#' Plot PDF with statistical value.
#'
#' This function will plot PDF with statistical value.
#'
#' @param distribution Character string indicating probability distribution function. Eg "norm", "t", "chisq". Internally, "d-", "p-" are added as prefix of the string. eg. dnorm(), pnorm().
#' @param xrange X axis range for plot.
#' @param stat_value Statistical value to show on graph.
#' @param side Either, "lower" "upper" "two-side-inner" or "two-side-outer"
#' @param line_color color for vertical line at stat_value.
#' @param area_color color for fill aesthetics in graph.
#' @param ... Arguments passed on to distribution. eg mean=1, sd = 2 for "norm".
#' @import ggplot2
#' @import magrittr
#' @import scales
#' @export
#' @examples plot_pdf_by_stat(stat_value = 1.4,xrange = c(-3,3), side = "lower")

plot_pdf_by_stat <- function(distribution = "norm",
                             xrange = c(-3,3),
                             stat_value, side = "lower",
                             line_color = "red", area_color = "deeppink2",...){

  dFUN <- eval(parse(text = str_c("d", distribution)))
  pFUN <- eval(parse(text = str_c("p", distribution)))

  baseg <- plot_pdf(distribution = distribution, xrange = xrange, ...)

  if(side == "two_side_inner" | side == "two_side_outer"){
    if(length(stat_value) != 2){
      print("WARNING!: stat_value need to have length 2. The first value used for plot.")

      stat_value <- stat_value[1]
      side <- "lower"
    }else{
      lower_stat <- min(stat_value)
      upper_stat <- max(stat_value)

      if(side == "two_side_inner"){
        g2 <- baseg +
          geom_area(aes(x = .data$x,
                        y = case_when(
                          .data$x < lower_stat ~ 0,
                          .data$x > upper_stat ~ 0,
                          TRUE           ~ .data$y
                        )),
                    fill = area_color, alpha = 0.5)
      }

      if(side == "two_side_outer"){
        g2 <- baseg +
          geom_area(aes(x = .data$x,
                        y = case_when(
                          .data$x < lower_stat ~ .data$y,
                          .data$x > upper_stat ~ .data$y,
                          TRUE           ~ 0
                        )),
                    fill = area_color, alpha = 0.5)
      }

      lower_perc <- pFUN(q = lower_stat, lower.tail = TRUE, ...)
      lower_perc_label <- lower_perc %>%
        scales::percent(accuracy = 0.01)

      upper_perc <- pFUN(q = upper_stat, lower.tail = FALSE, ...)
      upper_perc_label <- upper_perc %>%
        scales::percent(accuracy = 0.01)

      middle_perc <- 1 - lower_perc - upper_perc
      middle_perc_label <- middle_perc %>%
        scales::percent(accuracy = 0.01)

      lower_stat_label <- lower_stat %>% scales::number(accuracy = 0.01)
      upper_stat_label <- upper_stat %>% scales::number(accuracy = 0.01)

      max_density <- baseg$data$y %>% max()

      final_graph <- g2 +
        annotate(geom = "text",
                 x = mean(c(xrange[1],lower_stat)),
                 y = -max_density/15,
                 label = lower_perc_label) +
        annotate(geom = "text",
                 x = mean(c(xrange[2],upper_stat)),
                 y = -max_density/15,
                 label = upper_perc_label)  +
        annotate(geom = "text",
                 x = mean(stat_value),
                 y = -max_density/15,
                 label = middle_perc_label) +
        annotate(geom = "text", x = lower_stat, y = -max_density/15,
                 label = lower_stat_label) +
        annotate(geom = "text", x = upper_stat, y = -max_density/15,
                 label = upper_stat_label)
    }

  } #else

  if(side == "lower"){

    g2 <- baseg +
      geom_area(aes(x = .data$x, y = if_else(.data$x <= stat_value, .data$y, 0)),
                fill = area_color, alpha = 0.5)

  }

  if(side == "upper"){
    g2 <- baseg +
      geom_area(aes(x = .data$x, y = if_else(.data$x >= stat_value, .data$y, 0)),
                fill = area_color, alpha = 0.5)
  }

  if(side %in% c("lower","upper")){
    p_lower <- pFUN(stat_value, lower.tail = TRUE, ... ) %>%
      scales::percent(accuracy = 0.01)
    p_upper <- pFUN(stat_value, lower.tail = FALSE, ...) %>%
      scales::percent(accuracy = 0.01)

    stat_label <- str_glue("{p_lower} <= {scales::number(stat_value,accuracy = 0.01)} => {p_upper}")

    max_density <- baseg$data$y %>% max()

    final_graph <- g2 +
      geom_segment(aes(x = stat_value,
                       xend = stat_value,
                       y = 0,
                       yend = dFUN(stat_value, ...)),
                   color = line_color) +
      annotate(geom = "text", x = stat_value,
               y = -max_density/15, label = stat_label, size = 5)


  }


  return(final_graph)
}

#' Plot PDF with desired percentage.
#'
#' This function will plot PDF with desired percentage. Wrapper function for plot_pdf_by_stat.
#'
#' @param distribution Character string indicating probability distribution function. Eg "norm", "t", "chisq". Internally, "d-", "p-" are added as prefix of the string. eg. dnorm(), pnorm().
#' @param xrange X axis range for plot.
#' @param quantile_value Desired quantile(s) for plot.
#' @param side Either, "lower" "upper" "two-side-inner" or "two-side-outer"
#' @param line_color color for vertical line at stat_value.
#' @param area_color color for fill aesthetics in graph.
#' @param ... Arguments passed on to distribution. eg mean=1, sd = 2 for "norm".
#' @import ggplot2
#' @import magrittr
#' @export

plot_pdf_by_percentage <- function(distribution = "norm",
                                   xrange = c(-3,3),
                                   quantile_value, side = "lower",
                                   line_color = "red", area_color = "deeppink2", ...){
  qFUN <- eval(parse(text = str_c("q",distribution)))

  if(side == "lower"){
    quantile_value <- quantile_value
  }

  if(side == "upper"){
    quantile_value <- 1 - quantile_value
  }

  if(side %in% c("two_side_outer","two_side_inner")){
    quantile_value <- c(quantile_value/2, 1-quantile_value/2)
  }

  stat_value <- qFUN(quantile_value, ...)

  graph <- plot_pdf_by_stat(distribution = distribution,
                            xrange = xrange,
                            stat_value = stat_value,
                            side = side,
                            line_color = line_color,
                            area_color = area_color,
                            ...)

  return(graph)
}
