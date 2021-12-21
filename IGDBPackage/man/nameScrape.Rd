\name{nameScrape}
\alias{nameScrape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{nameScrape
%%  ~~function to do ... ~~
}
\description{
 Compile names of video games on page to a dataframe
}
\usage{
nameScrape(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    IGDB page link
}
}
\examples{
names <- nameScrape("https://www.igdb.com/games/recently_released")
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
