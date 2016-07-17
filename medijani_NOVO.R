library(psych)


tbl2011 <- function(x, SK) { 
  if (SK == 0) {
    return ( table(data.unt.2011$dob)[x] ) }
  if (SK == 1) {
    return ( table(data.unt.2011$dob[data.unt.2011$ZD.dijelovi == "Poluotok"])[x] ) }
  if (SK > 1 ) {
    return ( table(data.unt.2011$dob[data.unt.2011$SK == SK])[x] )
  }
}


tbl2001 <- function(x, SK) { 
  if (SK == 0) {
    return ( table(data.unt.2001$dob)[x] ) }
  if (SK == 1) {
    return ( table(data.unt.2001$dob[data.unt.2001$ZD.dijelovi == "Poluotok"])[x] ) }
  if (SK > 1 ) {
    return ( table(data.unt.2001$dob[data.unt.2001$SK == SK])[x] )
  }
}

med2011 <- function(SK) {
  interp.median(
    c(
  rep(0:4, each = tbl2011(1, SK) / 5),
  rep(5:9, each = tbl2011(2, SK) / 5),
  rep(10:14, each = tbl2011(3, SK) / 5),
  rep(15:19, each = tbl2011(4, SK) / 5),
  rep(20:24, each = tbl2011(5, SK) / 5),
  rep(25:29, each = tbl2011(6, SK) / 5),
  rep(30:34, each = tbl2011(7, SK) / 5),
  rep(35:39, each = tbl2011(8, SK) / 5),
  rep(40:44, each = tbl2011(9, SK) / 5),
  rep(45:49, each = tbl2011(10, SK) / 5),
  rep(50:54, each = tbl2011(11, SK) / 5),
  rep(55:59, each = tbl2011(12, SK) / 5),
  rep(60:64, each = tbl2011(13, SK) / 5),
  rep(65:69, each = tbl2011(14, SK) / 5),
  rep(70:74, each = tbl2011(15, SK) / 5),
  rep(75:79, each = tbl2011(16, SK) / 5),
  rep(80:84, each = tbl2011(17, SK) / 5),
  rep(85:94, each = tbl2011(18, SK) / 10)
  )
  ) 
  }
                  
med2001 <- function(SK) {
  interp.median(
    c(
  rep(0:4, each = tbl2001(1, SK) / 5),
  rep(5:9, each = tbl2001(2, SK) / 5),
  rep(10:14, each = tbl2001(3, SK) / 5),
  rep(15:19, each = tbl2001(4, SK) / 5),
  rep(20:24, each = tbl2001(5, SK) / 5),
  rep(25:29, each = tbl2001(6, SK) / 5),
  rep(30:34, each = tbl2001(7, SK) / 5),
  rep(35:39, each = tbl2001(8, SK) / 5),
  rep(40:44, each = tbl2001(9, SK) / 5),
  rep(45:49, each = tbl2001(10, SK) / 5),
  rep(50:54, each = tbl2001(11, SK) / 5),
  rep(55:59, each = tbl2001(12, SK) / 5),
  rep(60:64, each = tbl2001(13, SK) / 5),
  rep(65:69, each = tbl2001(14, SK) / 5),
  rep(70:74, each = tbl2001(15, SK) / 5),
  rep(75:79, each = tbl2001(16, SK) / 5),
  rep(80:84, each = tbl2001(17, SK) / 5),
  rep(85:89, each = tbl2001(18, SK) / 5),
  rep(90:94, each = tbl2001(19, SK) / 5),
  rep(95:99, each = tbl2001(20, SK) / 5)
    )
  ) 
  }


# 


  





