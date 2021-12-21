\name{linkScrape}
\alias{linkScrape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{linkScrape
%%  ~~function to do ... ~~
}
\description{
 Compile links to video games on page to a dataframe
}
\usage{
linkScrape(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    IGDB page link
}
}
\examples{
links <- linkScrape("https://www.igdb.com/games/recently_released")
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
