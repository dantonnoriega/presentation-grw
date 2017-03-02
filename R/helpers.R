# packages
library(tidyverse)
library(xtable)
library(extrafont)
devtools::load_all('~/GitHub/spartannash')

# functions
getOutputFormat <- function() {
  knitr::opts_knit$get("rmarkdown.pandoc.to")
}

printTable <- function(tbl, format = getOutputFormat()) {
    cap <- attr(tbl, "caption")
    if(format == 'latex') print(tbl, type = format, comment = FALSE, include.rownames=FALSE) else knitr::kable(tbl, caption=cap)
}

# ggplot themes
theme_clean <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          legend.position="bottom",
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y=unit(1, "lines"))

  ret
}