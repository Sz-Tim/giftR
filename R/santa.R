#' secret santa function
#'
#' This function generates random assignments or secret santa and emails the
#' participants if desired.
#' @param p.df Data frame with information for each individual. Columns need
#' to be 'giver' and 'couple', with 'email' optional. The 'couple' identifies
#' significant others numerically (i.e., spouses have the same number)
#' @param email Logical argument for whether or not participants should
#' automatically be emailed; defaults to FALSE
#' @param organizer If email=TRUE, email address of the organizer
#' @param subject If email=TRUE, subject of the email
#' @param host.nm If email=TRUE, host.name for mailR; defaults to "smtp.gmail.com"
#' @param prt If email=TRUE, smtp port; defaults to 465
#' @param usr.nm If email=TRUE, email username
#' @param pswd If email=TRUE, email/application email
#' @param giftLim If email=TRUE, the gift limit entered just as a number
#' @keywords secret santa
#' @note Sending emails requires installation of the mailR package. If you are using
#' Gmail, the two-factor authentication requires an application password - not your
#' normal account password.
#' @examples
#' # A group with two couples and two additional individuals
#' p.df <- data.frame(giver=LETTERS[1:6],
#'                    couple=c(1,1,2,2,3,4))
#' santa(p.df, email=FALSE)
#'
#' p.df$email <- "tisz1545@colorado.edu"
#' santa(p.df, email=TRUE, organizer="tim.szewczyk@gmail.com",
#'       subject="2017 Secret Santa", usr.nm="tim.szewczyk",
#'       pswd="xxxxxxxxxxxx", giftLim=20)

santa <- function(p.df, email=FALSE, organizer=NULL, subject=NULL,
                  host.nm="smtp.gmail.com", prt=465, usr.nm=NULL, pswd=NULL,
                  giftLim=20) {
  cp <- p.df$couple; names(cp) <- p.df$giver
  p.df$recip <- p.df$giver
  while( sum(cp[p.df$giver]==cp[p.df$recip]) > 0 ) {
    p.df$recip <- sample(p.df$recip, nrow(p.df))
  }

  nPs <- NA
  for(i in 1:nrow(p.df)) {
    nPs[i] <- sum(p.df$couple != p.df$couple[i])
  }
  tPr <- round((1-(prod(1 - 1/nPs)))*100, 1)

  if(email) {
    require(mailR)
    for(i in 1:nrow(p.df)) {
      mailR::send.mail(from=organizer,
                       to=p.df$email[i],
                       subject=subject,
                       body=paste0("Hi ", p.df$giver[i], ",\n\n",
                                   "This year, you have been randomly assigned",
                                   " to give a gift to ", p.df$recip[i], ".\n\n",
                                   "The pairings were generated and sent ",
                                   "automatically, so apologies if you are giving",
                                   " a gift to the same person as last year. ",
                                   "Since there are only ", nPs[i], " possibilities",
                                   " for you, there is actually a ",
                                   round(1/nPs[i]*100, 1), "% chance of that ",
                                   "happening each year, and a ", tPr, "% chance",
                                   " of that happening to at least one person.\n\n",
                                   "Please keep in mind that the gift limit ",
                                   "is $", giftLim, "."),
                       smtp=list(host.name=host.nm, port=prt,
                                 user.name=usr.nm, passwd=pswd, ssl=TRUE),
                       authenticate=TRUE, send=TRUE)
    }
  } else {
    return(p.df)
  }
}






