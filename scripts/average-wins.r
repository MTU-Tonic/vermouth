#!/usr/bin/env Rscript

# Calculates the running average Elo ranking and the standard error of the mean
# as a confidence interval using an Elo tournament output file as a command line
# argument.
#
# Usage: ./average-elo.r --run <data file>
# Be sure to make this file as executable!
#
# Requires packages argparser and ggplot2 in order to run. Using the R REPL, run
#     install.package("argparser")
#     install.package("ggplot2")

### parse command line arguments
library(argparser, quietly=TRUE)
parser <- arg_parser("Win Rate Plot")
parser <- add_argument(parser, '--output', nargs=Inf, help="output file")
parser <- add_argument(parser, '--run', nargs=Inf, help="training data files")
args   <- parse_args(parser)

load.zip.data <- function(file) {
	games   <- read.csv(unz(file, "games.csv"))
	players <- read.csv(unz(file, "players.csv"))
	games$PLAYER  <- factor(players$NAME[factor(games$PLAYER)])
	games$SCORE   <- games$WIN / (nlevels(games$PLAYER) - 1)
	games$FILE    <- file
	games
}

games <- do.call(rbind, lapply(args$run, load.zip.data))
games <- aggregate(SCORE~PLAYER+ROUND+FILE, games, FUN=sum)
games$AVG_SCORE <- ave(games$SCORE, games$PLAYER, games$FILE, FUN=cumsum) / ave(games$SCORE, games$PLAYER, games$FILE, FUN=seq_along)
games$STD_SCORE <- ave((games$SCORE - games$AVG_SCORE)^2, games$PLAYER, games$FILE, FUN=cumsum) / (ave(games$SCORE, games$PLAYER, games$FILE, FUN=seq_along) - 1)
games$SDE_SCORE <- games$STD_SCORE / sqrt(ave(games$SCORE, games$PLAYER, games$FILE, FUN=seq_along))

games

games.agg <- aggregate(cbind(AVG_SCORE,STD_SCORE,SDE_SCORE)~PLAYER+ROUND, data=games, FUN=mean)

library(ggplot2)
ggplot(mapping=aes(x=ROUND, y=AVG_SCORE, color=PLAYER, fill=PLAYER, group=interaction(PLAYER, FILE)), data=games) +
	geom_line(alpha=0.5) +
	geom_line(mapping=aes(x=ROUND, y=AVG_SCORE, group=PLAYER), data=games.agg) +
	geom_ribbon(mapping=aes(x=ROUND, ymin=AVG_SCORE-SDE_SCORE, ymax=AVG_SCORE+SDE_SCORE,  group=PLAYER), data=games.agg, linetype=0, alpha=0.3) +
	xlab("Rounds") +
	ylab("Win Rate") +
	labs(color="Player", fill="Player") +
	theme_bw()
ggsave(paste(args$output, "pdf", sep="."), device="pdf", width=9, height=4.5)
