##'
##' Functions
##' =========
##'
##' Custom functions used for analyses.
##'

foodGroupsPlot <- function(data, food, ylim, ylab) {
    data %>%
        filter(FoodGroup == food) %>%
        ggplot(aes(FoodSecure, Value)) +
        geom_bar(aes(fill = FoodSecure), stat = 'identity', colour = 'grey40') +
        geom_text(aes(label = Value), vjust = 0) +
        coord_cartesian(ylim = ylim) +
        facet_grid(Sex ~ Years) +
        ggthemes::theme_tufte(base_family = 'Helvetica') +
        ylab(ylab) +
        scale_fill_brewer() +
        bar_theme()
}

budgetTable <- function(household) {
    budget %>%
        filter(Household == household) %>%
        select(-Household) %>%
        knitr::kable()
}

budgetRemaining <- function(household){
    budget %>%
        filter(Household == household) %>%
        summarise(sum = sum(Dollar))

}

bar_theme <- function() {
    ggthemes::theme_tufte(base_family = 'Helvetica') %+replace%
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            strip.background = element_rect(fill = 'grey90', colour = 'grey90'),
            legend.title = element_blank(),
            axis.title.x = element_blank()
        )
}
