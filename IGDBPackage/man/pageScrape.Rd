\name{pageScrape}
\alias{pageScrape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pageScrape
%%  ~~function to do ... ~~
}
\description{
Compile data of video game on page to a dataframe
}
\usage{
pageScrape(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    IGDB page link
}
}
\examples{
gameData <- pageScrape("https://www.igdb.com/games/recently_released")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
