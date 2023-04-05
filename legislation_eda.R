################################################################################
##         Explore Legislation Text
################################################################################


library(readr)
library(ggplot2)
library(scales)
library(tidytext)
library(hash)
library(qs)
library(qdapTools)
library(dplyr)


data = read.csv('legislation.csv')
head(data)
str(data)

# get a view of the policy information
table(as.factor(data$policy))

keys = unique(sort(data$policy))
keys

names = c('Macroeconomics',
          'Civil Rights, Minority Issues & Civil Liberties',
          'Health',
          'Agriculture',
          'Labor & Employment',
          'Education',
          'Environment',
          'Energy',
          'Immigration',
          'Transportation',
          'Law, Crime & Family Issues',
          'Social Welfare',
          'Community Dev & Housing',
          'Banking, Finance, Dom Commerce',
          'Defense',
          'Space, Science, Tech & Comm',
          'Foreign Trade',
          'Int Affairs and Aid',
          'Government Operations',
          'Public Lands, Water Mgmt',
          'Other'
          )

# make a hash table
h_table = hash::hash(keys = keys, values = names)

# now map them for speed
data$policy_name = hash_look(data$policy, h_table)

################################################################################
##         make some visualizations
################################################################################

# roll up for metric
rollup_fun = function(df, grouping){
    df %>% group_by(policy_name, {{grouping}}) %>% 
        summarise(Count = n())
}

by_congress = rollup_fun(data, cong) %>% filter(!is.na(policy_name))
by_year = rollup_fun(data, year)


# make a facet bar plot
# facet bar chart of counts
theme_small <- function () { 
    theme(axis.text.x = element_text(size = 10, color = 'blue', angle = 0),
          legend.position = 'bottom',
          axis.text.y = element_text(size = 9, color = 'blue', angle = 0),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(color = 'darkblue', size = 10, face = 'bold'),
          strip.background = element_rect(fill = 'light blue'))
}





rollup_bar_facet = function(df, x, y, title, lab='', num_cols = 3, xmax = 225){
    g = ggplot(df, aes(y = y, x=x)) +
        geom_bar(stat = 'identity', fill='steelblue') + 
        scale_y_continuous(labels = comma) + 
        facet_wrap(~policy_name, ncol = num_cols) + 
        theme_small() + 
        theme(plot.title = element_text(size = rel(1.5), face = "bold", color = "blue")) +
        ggtitle(title) +
        theme(legend.position = '') 
        # xlim(0,xmax)
    return(g)
    
}

bar = rollup_bar_facet(df = by_congress,
                       x = by_congress$cong,
                       y = by_congress$Count,
                       num_cols = 3,
                       title = 'Counts of Bills by Congress & Policy')
bar




g = ggplot(by_congress, aes(x = cong, y = Count)) + 
    scale_y_continuous(labels = comma) + 
    geom_bar(stat = 'identity', fill='steelblue') + 
    facet_wrap(~policy_name, ncol = 3) +
    theme_small()
g
