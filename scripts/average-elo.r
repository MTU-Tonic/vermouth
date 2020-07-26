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
rounds$STD_RANKING <- ave((rounds$RANKING - rounds$AVG_RANKING)^2, rounds$PLAYER, FUN=cumsum) / (ave(rounds$RANKING, rounds$PLAYER, FUN=seq_along) - 1)
rounds$SDE_RANKING <- rounds$STD_RANKING / sqrt(ave(rounds$RANKING, rounds$PLAYER, FUN=seq_along))
rounds$PLAYER <- players$NAME[factor(rounds$PLAYER)]

library(ggplot2)
ggplot(rounds, aes(x=ROUND, y=AVG_RANKING, color=PLAYER, fill=PLAYER)) +
	geom_line() +
	geom_ribbon(aes(ymin=AVG_RANKING-SDE_RANKING, ymax=AVG_RANKING+SDE_RANKING), linetype=0, alpha=0.3) +
	xlab("Rounds") +
	ylab("Average ELO Ranking") +
	labs(color="Player", fill="Player") +
	theme_bw()
ggsave(paste(args$run, "avg-elo", "pdf", sep="."), device="pdf", width=9, height=4.5)
