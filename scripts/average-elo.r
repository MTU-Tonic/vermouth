#!/usr/bin/env Rscript

### parse command line arguments
library(argparser, quietly=TRUE)
parser <- arg_parser("Average Elo Ranking Plot")
parser <- add_argument(parser, '--run', nargs=1, help="training data files")
args   <- parse_args(parser)

### load and process the melted data
rounds  <- read.csv(unz(args$run, "rounds.csv"))
players <- read.csv(unz(args$run, "players.csv"))
rounds$AVG_RANKING <- ave(rounds$RANKING, rounds$PLAYER, FUN=cumsum) / ave(rounds$RANKING, rounds$PLAYER, FUN=seq_along)
rounds$PLAYER <- players$NAME[factor(rounds$PLAYER)]

rounds[1:12,]

library(ggplot2)
ggplot(rounds, aes(x=ROUND, y=AVG_RANKING, group=PLAYER, color=PLAYER)) +
	geom_line() +
	scale_colour_discrete(name="Player") +
	xlab("Rounds") +
	ylab("Average ELO Ranking") +
	theme_bw()
ggsave(paste(args$run, "avg-elo", "pdf", sep="."), device="pdf", width=9, height=4.5)
