plt_scores <- function() {
  d <- readRDS('data/score-metric-driving.rds')
  dd <- d %>%
    dplyr::slice(1:23) %>%
    dplyr::mutate(assignment = 1 - control) %>%
    dplyr::select(score, assignment) %>%
    dplyr::mutate(`Double Up` = dplyr::if_else(assignment == 1, 'Yes', 'No'))

  g1 <- ggplot2::ggplot(data = dd) +
      ggplot2::geom_point(
        ggplot2::aes(x = score, y = assignment, colour = `Double Up`),
        size = 3) +
      theme_clean()

  g1 + ggplot2::geom_vline(aes(xintercept = .57), color = 'blue', linetype = 'dashed')
}