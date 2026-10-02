# load pkgs
pacman::p_load( "readxl", "dplyr", "ggplot2" )

input_file <- "inputs/G606-Mutaciones, diversidad biológica y salud.xlsx"

# read data
kah.df <- read_excel( path = input_file, sheet = "Final Scores", skip = 2 )

kah_2.df <- kah.df %>%
  arrange( -`Total score (points)` ) %>%
  head( n = 3 )

kah_3.df <- kah.df %>%
  filter( ! Player %in% kah_2.df$Player ) %>%
  mutate( Q1_Tag = if_else(`Total score (points)` >= quantile(`Total score (points)`, 0.75),
                           "Q1", NA_character_ ) ) %>%
  filter( !is.na( Q1_Tag ) )

# Select students
students.v <- c( kah_2.df$Player, kah_3.df$Player )

kah_4.df <- kah.df %>%
  mutate( color = ifelse( test = Player %in% students.v,
                          yes = "yes",
                          no = "no" ) )

# plot quntiles
ggplot( data = kah_4.df,
        mapping = aes( x = 1,
                       y = `Total score (points)` ) ) +
  geom_boxplot( outliers = FALSE, width = 0.2, color = "gray70" ) +
  geom_jitter( shape = 21, width = 0.5,
               mapping = aes( fill = color ) ) +
  scale_fill_manual( values = c( "gray", "tomato" ) ) +
  labs( title = "Resultados Kahoot",
        subtitle = paste( students.v, collapse = ", " ) ) +
  theme_classic( base_size = 15 ) +
  theme( legend.position = "none",
         axis.title.x = element_blank( ),
         axis.text.x = element_blank( ),
         axis.ticks.x = element_blank( ) )
