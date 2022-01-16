library(plumber)
library(DBI)
library(jose)

#
db <- "farms"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_pass <- "sa2143"
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = db,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)

#* @filter cors
cors <- function(res) {  
  res$setHeader("Access-Control-Allow-Origin", "*") 
  plumber::forward()
}

#* @filter checkAuth
function(req, res, token=""){
	postToken <- req$postBody["token"]
	queryStringToken <- req$QUERY_STRING["token"]
	if (token!="1234"){
		res$status <- 401 # Unauthorized
		return(list(error="Authentication required"))
	  } 
	  else {
		plumber::forward()
	}
}

#* @get /login
login <- function(username="",password="") {    
  if(username=="fernando" && password=="1234") {
    claim <- jwt_claim(username = username, session_key = 123456)
    key <- charToRaw("SuperSecret")
    jwt <- jwt_encode_hmac(claim, secret = key)
	#list(token=jwt)
	return(token=jwt)
  } else {
    return(message="Failed to log in")
  }
}



#* Return the farms list
#* @serializer unboxedJSON
#' @param type TYPE param farm type
#' @param top TOP  param limit number of records
#* @post /list
list <- function(type="", top=""){ 
  sql="select * from farms"
  if(type!="") sql=paste0(sql, " where farm_type='", type, "'")
  if(top!="") sql=paste0(sql, " limit ", top)
  farms <- dbGetQuery(conn, sql)
  return(farms)
}

#* Return the sum of farms
#* @serializer unboxedJSON
#' @param type TYPE param farm type
#* @post /sum
sum <- function(type=""){
  sql="select cast(sum(animals) as int) as sum from farms"
  if(type!="") sql=paste0(sql, " where farm_type='", type, "'")
  farms <- dbGetQuery(conn, sql)
  return(unlist(farms, use.names = FALSE))
}

#* Return the avg number of animals 
#* @serializer unboxedJSON
#' @param type TYPE param farm type
#* @post /avg
avg <- function(type=""){
  sql="select cast(avg(animals) as int) as avg from farms"
  if(type!="") sql=paste0(sql, " where farm_type='", type, "'")
  farms <- dbGetQuery(conn, sql)
  return(unlist(farms, use.names = FALSE))
}

#* Return the min number of animals 
#* @serializer unboxedJSON
#' @param type TYPE param farm type
#* @post /min
min <- function(type=""){
  sql="select cast(min(animals) as int) as avg from farms"
  if(type!="") sql=paste0(sql, " where farm_type='", type, "'")
  farms <- dbGetQuery(conn, sql)
  return(unlist(farms, use.names = FALSE))
}

#* Return the max number of animals 
#* @serializer unboxedJSON
#' @param type TYPE param farm type
#* @post /max
max <- function(type=""){
  sql="select cast(max(animals) as int) as max from farms"
  if(type!="") sql=paste0(sql, " where farm_type='", type, "'")
  farms <- dbGetQuery(conn, sql)
  return(unlist(farms, use.names = FALSE))
}

#* Return the number of farms
#* @serializer unboxedJSON
#' @param type TYPE param farm type
#* @post /count
count <- function(type=""){
  sql="select cast(count(0) as int) as max from farms"
  if(type!="") sql=paste0(sql, " where farm_type='", type, "'")
  farms <- dbGetQuery(conn, sql)
  return(unlist(farms, use.names = FALSE))
}


#* Summary
#* @get /summary
summary <- function(){
  sql="select farm_type, count(0) as count, min(animals::int) as min, max(animals::int) as max, count(animals::int) as farms_quantity, CAST(AVG(animals::int) AS int) as average, sum(animals::int) as sum from farms group by farm_type"
  farms <- dbGetQuery(conn, sql)
  return(farms)
}

#* Plot a histogram
#' @param type TYPE param description
#* @get /plot
#* @png
plot <- function(type=""){
  sql="select animals::int from farms"
  if(type!="") sql=paste0(sql, " where farm_type='", type, "'")
  farms <- dbGetQuery(conn, sql)
  hist(unlist(farms, use.names = FALSE), main="Animals Histogram", xlab="Animals")
}

