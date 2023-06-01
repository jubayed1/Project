# install.packages('tibble')
# install.packages('readxl')
# install.packages('dplyr')
# install.packages('tidygeocoder')
# install.packages('ggrepel')
# install.packages("cli", type="binary")

library(readxl)
library(tibble)
library(dplyr)
library(tidygeocoder)
library(ggrepel)
library(ggplot2)
library(writexl)

off<- read_excel("offendersdata.xlsx")
geocoded_offenders <- geocode(off, "Address",method = "census", verbose = TRUE)
geocoded_offender<- geocoded_offenders%>%
  na.omit()
write_xlsx(geocoded_offender, "geocoded_offender.xlsx")

# some_addresses <- tibble::tribble(~name, ~address,
#                                   
#   "MORRIS, AARON BRUCE",      "1406 Calgary WAY  Bowling Green KY 42101",
#   "JONES, ALLEN",               '1009 Brookwood DR  Bowling Green KY 42101',
#   'HENDERSON, ANTHONY LAWRENCE', '230 Lovers Ln #A   Bowling Green KY 42103',
#   'SIMMONS, ANTHONY WILLLIS',  '1974 Garrett Hollow RD  Bowling Green KY 42101',
#   'CRIPPS, BARRY LEIGH',      '520 Village Creek Drive Apt D   Bowling Green KY 42101',
#   'JOYNER, BRADLEY LEE', '292 Clyde Harrod RD  Bowling Green KY 42101  ',
#   'HARDT, BRADLEY W',   '424 State ST  Bowling Green KY 42101  ',
#   # WALLACE, BRADY LEON
#   # BRATCHER, BRUCE ALLEN
#   # DAVIS, BRYAN WESLEY
#   # PRUDEN, CASEY THOMAS
#   # SECORA, CHAD
#   # NEW, CHARLES ANTHONY
#   # PARSONS, CHARLES RAY
#   # MILLER, CHRISTOPHER DALE
#   # MADDOX, CHRISTOPHER MATTHEW
#   # FLOYD, CINDY LEE
#   # CHILDRESS, CRAIG A
#   # KING, DALLAS CURMAN
#   # COFFELT, DAN JACK
#   # ESTESS, DANA
#   # MURRAY, DANIEL ALLEN
#   # JOHNSON, DANIEL LEON
#   # VANDERMEULEN, DAVID CHRISTIAN
#   # MORRIS, DAVID LEE
#   # STILES, DAVID RILEY
#   # SEBASTIAN, DIEGO
#   # HOLLAND, DONALD
#   # ZACHARY, DONALD RAY
#   # FROGGE, DONALD SIDWELL
#   # BROWN, FRANCISCO ANTONIO
#   # BRUCE, FRANK D
#   # DAVIDSON, GEORGE
#   # WILLIAMS, GERALD DELAWRENCE
#   # COWLES, HOMER SHIRLEY
#   # DUNCAN, JACKIE WAYNE
#   # WOLFE, JAMES
#   # BERRY, JAMES WILLIAMS
#   # TREPANIER, JASON JOSEPH
#   # GREEN, JEFFREY ALLEN
#   # MILLER, JERRY WAYNE
#   # BINCKLEY, JESSICA RENEE
#   # LONG, JIMMY DAVID
#   # MORRIS, JOHN HOWARD
#   # JOHNSON, JOHN TEDEY
#   # ALLEN, JOHN WILLIAM
#   # STAFFORD, JOHNNY A
#   # LEWIS, JONATHAN KEITH
#   # MCCRARY, JOSEPH A
#   # SHARER, JOSEPH ALLAN
#   # WARD, JOSEPH E
#   # BLAIR, JOSH LEROY
#   # FIELDS, JOSHUA LANA
#   # MUNG, KHUP
#   # SUTTON, LARRY TARRENCE
#   # RAGLAND, LEON CORTEZ
#   # DALTON, LEONARD DALE
#   # BROWN, LESTER EARL
#   # WHITAKER, LONNIE DALE
#   # BRABHAM, LORIE DENISE
#   # HANSEN, MARCO
#   # SHADDOCK, MARK
#   # ELLIOTT, MARK ANTHONY
#   # TAYLOR, MARTHA LEE
#   # MANNING, MICHAEL DALE
#   # JOHNSON, MICHAEL RAY
#   # BIRDWELL, MICHAEL WELDON
#   # TURTON, MICHELLE MARIE
#   # DAVILA, PABLO
#   # SMITH, PHILLIP JOSEPH
#   # HUDSON, PRENTICE WAYNE
#   # HASTEDT, RANDALL JAMES
#   # SIZEMORE, RANDALL LEE
#   # SMITH, RANDY GENE
#   # KEEN, RANDY KIRK
#   # CLARK, RICARDO JUNIOR
#   # SPROUSE, RICHARD JUSTIN
#   # HUFF, RICHARD PAUL
#   # FRANCE, RICKY
#   # CLINE, ROBERT ALAN
#   # TAYLOR, ROBERT JAMES
#   # WEBSTER, RODERICK DEON
#   # MCKEOWN, ROGER
#   # COWAN, ROGER DALE
#   # GIPSON, RONALD
#   # JOHNSON, RONNIE DARYL
#   # HANN, SAMUEL YARDLEY
#   # MEDINA, SERGIO
#   # HANSEN, STEPHEN DUANE
#   # KIRBY, STEPHEN LEVI
#   # KAMARA, STEVEN
#   # SUTHERLAND, STEVEN JAY
#   # CANNON, TERRY
#   # GREGORY, TERRY ALLEN
#   # HENDERSON, TERRY WAYNE
#   # SIMMONS, THOMAS JAY
#   # JAGGERS, THOMAS MARTIN
#   # BIEBEL, TIFFANY ANN
#   # CRUMPTON, TIMOTHY DALE
#   # CARVER, TRENTON D
#   # COLTON, TYSON BERNARD
#   # GUYTON, WILLIE C
#   # BROWN, BECKY SUE
#   # WHITAKER, BILLY EDWARD
#   # HACKWORTH, EVERETT HENRY
#   # FULLERTON, JAMES MORGAN
#   # COULTER, JAMES RONNIE
#   # HUSTON, JOSHUA THOMAS
#   # MACK, JOSIAH MIKE
#   # TEAGUE, LOUIS R
#   # HAMMONS, MCKINLEY LEE
#   # BOYD, MICHAEL DWAIN
#   # JOHNSON, RICKY LEE
#   # BRYANT, BRANDON MONTEZ
#   # GIFFORD, BRIAN D
#   # MASTOVICH, DANIEL
#   # WELLER, JAMES ALLEN
#   # WRIGHT, JAMES DOUGLAS
#   # MOREHEAD, JAMES WOODFORD
#   # FLECK, MARK WALTER
#   # KING, MARNEDA GAYLE
#   # MEFFORD, MARTY
#   # GREENE, MARVIN
#   # LUNSFORD, RUSSELL LEE
#   # PATEL, SAMIRKUMAR MANUBHAI
#   # GRISWOLD, STEVEN ERIK
#   # SELLERS, THOMAS
#   # YOUNG, TOUNCIE REED
#   # WARD, TRACY ALLEN
#   # SYKES, DERRICK EDWARD
#   # DANIELS, JIMMY NEAL
#   # KEEN, MICHAEL DEAN
#   # DUKE, NICKY RICHARD
#   # MELTON, ROBERT RAY
#   # CREEK, ROY LEE
#   # LANZINE, DANIEL MICHAEL
#   # FULTON, JASON ALAN
#   # HESSON, KEVIN NICHOLAS
# 
# )
# 
# lat_longs <- some_addresses %>%
# geocode(address= address, method = "census", verbose = TRUE)
# 
# ggplot(geocoded_offenders, aes(long, lat), color = "grey99") +
#   borders("state") + geom_point() +
#   ggrepel::geom_label_repel(aes(label = name)) + theme_void()
