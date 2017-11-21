#### 需搭配「捷運附近人口流量」
#### 需搭配「雙北最小統計區」
proj4string(newtaipei1) <- CRS("+init=EPSG:3826")
newtaipei1 = spTransform(newtaipei1, CRS("+init=EPSG:4326"))

type_result = NULL
TYPE = tibble("捷運站" = "類型")
n=0
for(i in 1:dim(MRT)[1]){
  part_MRT = MRT_shape@polygons[i] %>% SpatialPolygons()
  proj4string(part_MRT) = CRS("+init=EPSG:4326")
  
  type = 0
  try(
  {s1 =  intersect(part_MRT, newtaipei1)
  table = tibble("型態" = s1@data$USE_CODE %>% as.character(), "面積比例" = {sapply(s1@polygons, FUN=function(x) {slot(x, 'area')})}/ part_MRT@polygons[[1]]@area
  ) %>% group_by(型態) %>% summarise("面積和" = sum(面積比例)) %>% arrange(desc(面積和)) 
  type = table[1,1] %>% as.character()
  if({table %>% dim()}[1] != 1){
  view = which({table[,2][-1,]*3 %>% as.numeric() } > {table[,2][1,] %>% as.numeric()}) 
  if(view %>% sum() != 0 ){type = c(type, table$型態[-1][view])}}}  , silent =T)
   
  
  type = list(type)
  type_result = c(type_result, type)
  TYPE = cbind(TYPE, tibble("name"=type ))
  n = n+1
}
names(TYPE) = c("捷運站",MRT$捷運站名稱)

TYPE_RESULT =NULL
for(i in 1:dim(MRT)[1]){
  index = TYPE[[i+1]][[1]] %>% as.numeric() %>% sort()
  a = rep(0,5)
  a[index]=1
  TYPE_RESULT = cbind(TYPE_RESULT, a)
}
TYPE_RESULT  = TYPE_RESULT %>% as.data.frame()
names(TYPE_RESULT) = MRT$捷運站名稱
row.names(TYPE_RESULT) = c("工業區", "商業區", "住宅區", "行政區", "文教區")

which(TYPE_RESULT[1:3,] %>% apply(2, sum) == 0)

###########
TYPE_RESULT1 = TYPE_RESULT[1:3,]
TYPE_RESULT1[,c(32,41)][2,] <- 1 # 將公館台大醫院歸為商業地區
write.csv(TYPE_RESULT1, file = "環狀線捷運站最小統計區分類.csv", fileEncoding = "big5")
