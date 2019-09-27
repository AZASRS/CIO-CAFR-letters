# This is to put composites built so that they 
# can be reference by other files

# to add this to your file put in the line
# source('composites.R')

#load data & mapping from SSBT
library(readxl)

source('performance_functions.R')

path.ssbt <- "P:/IMD/2018 Database Project/MYSS Data/Copy of SSBT Data.xlsx"

policy.tree <- read_excel(path.ssbt, sheet = "Policy Tree") %>%
  mutate(Inception = ymd(Inception), Defunding = ymd(Defunding))

composite.tree <- read_excel(path.ssbt, sheet = "Composite Table")


eq.top <- c("ASRSA070", "ASRSA055", "ASRSA056")
top.names <- c("Total Equity", "Public Equity", "Private Equity")
eq.second <- c("ASRSA009", "ASRSA010")
eq.public <- c("ASRSA011", "ASRSA013", "ASRSA012", 
               "ASRSA023", "ASRSA024", "ASRSA028")

factor.portfolios <- c("A1S4", "A1TH", "A1TO")
e11_factor <- "A1S4"
eafe_factor <- "A1TH"
em_factor <- "A1TO"

e.port <- c("A1VE", "A1V4", "A1WV")

e.portfolios <- c("A1VE", "A1V4", "A1WV")

e10_internal <- "A1V4"
e2_internal <- "A1VE"
e6_internal <- "A1WV"
closed.e.portfolios <- c("A1VN", "A1VR", "A1RV", "A1RW")


br.intl <- c( "A1ZY", "A1SU", "A1QA", "A1Q0")
br_eafe_index <- "A1ZY"
br_em_lending <- "A1Q0"
br_em_mkts <- "A1QA"
br_eafe_small <- "A1SU"

eq.opp <- "A1U6"
overlay <- "A1Z1"
transitions <- "ASRSA062"
exFutures <- c("ASRSA072", "ASRSA073", "ASRSA074", "ASRSA075")

activeComposites <- c("ASRSA063", "ASRSA064", "ASRSA065","ASRSA066","ASRSA067", "ASRSA068")

eq.second <- composite.tree %>% 
  filter(is.na(Sub_Portfolio) == FALSE & Asset_Class == "Equity") %>%
  select(SSBT_Composite_ID) %>% unlist
public.ids <- composite.tree %>% 
  filter(is.na(Category) == FALSE) %>% 
  select(SSBT_Composite_ID) %>% unlist

all.eqID <- c(eq.top, eq.second, public.ids, e.portfolios, br.intl, 
              factor.portfolios, eq.opp, closed.e.portfolios)

all.eq <- c(eq.top, eq.second, eq.public, factor.portfolios, e.port, br.intl, 
            eq.opp, overlay, transitions)
namesEq <- as.vector(get_shortName(all.eq))
names(namesEq) <- all.eq


