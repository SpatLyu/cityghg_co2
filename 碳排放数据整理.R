setwd('./georhythm/碳排放数据整理/')

library(sf)
library(readxl)
library(writexl)
library(tidyverse)

city= read_sf('./data/city/') |> 
  st_transform(4326)

address = read_xlsx('./data/address.xlsx')

address %>% 
  filter(str_detect(city,'直辖'))

# 处理湖北
city %>% 
  filter(省=='湖北省') %>% 
  filter(类型=='省直辖县') %>% 
  filter(市!='神农架林区') %>% 
  st_union() %>% 
  st_sf(省='湖北',
        市='湖北直辖',
        类型='省直辖县',
        geometry=.) -> hubei.zx

city %>% 
  filter(省=='湖北省') %>% 
  filter(类型!='省直辖县') %>% 
  filter(市!='神农架林区') %>% 
  select(省,市,类型) %>% 
  rbind(hubei.zx) -> hubei

# 处理海南
city %>% 
  filter(省=='海南省') %>% 
  filter(类型=='省直辖县') %>% 
  st_union() %>%
  st_sf(省='海南',
        市='海南直辖',
        类型='省直辖县',
        geometry=.) -> hainan.zx

city %>% 
  filter(省=='海南省') %>% 
  filter(类型!='省直辖县') %>% 
  filter(市!='三沙市') %>% 
  select(省,市,类型) %>% 
  rbind(hainan.zx) -> hainan

# 矢量面重新组合：
city %>% 
  filter(!省 %in% c('湖北省','海南省')) %>% 
  select(省,市,类型) %>%
  rbind(hubei,hainan) -> carbon.polygon

# 连接地址和矢量面
address %>% 
  left_join(carbon.polygon,
            by=c('city'='市')) %>% 
  select(city,province,geometry) -> address

albers = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=110 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

st_set_geometry(address,address$geometry) %>% 
  st_transform(st_crs(albers)) -> polygon

# 面积:平方千米
polygon$area = units::set_units(st_area(polygon), km^2)

st_geometry(polygon) %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  st_coordinates() -> polygon[c('long','lat')]

process_co2 = function(year_time){
  if (year_time != 2020){
    co2 = str_glue('./data/{year_time}.xlsx') |> 
      read_xlsx(sheet = '二氧化碳排放(万吨)') |> 
      select(-c(1:3)) 
  } else{
    co2 = str_glue('./data/{year_time}.xlsx') |> 
      read_xlsx(sheet = '二氧化碳排放(万吨)') |> 
      select(-c(1:3,21)) 
  }
   
  ecom = str_glue('./data/{year_time}.xlsx') |> 
    read_xlsx(sheet = '社会经济数据') |> 
    select(-c(1:2))
  carbon = bind_cols(co2,ecom)
  names(carbon) |> 
    str_replace('\r\n','') |> 
    str_replace_all(' ','_')-> varname
  varname[18:23] = c("人均排放_吨每人","单位总GDP二氧化碳排放_吨每万元",
                     "城市英文名称","常住人口_万人","GDP_亿元","人均GDP_万元")
  carbon |> 
    set_names(varname) %>% 
    bind_cols(polygon,.) |> 
    select(城市英文名称,everything()) -> carbon
   return(carbon) 
}


for (t in (seq(2005,2020,5))){
  process_co2(t) %>% 
    write_sf('./data/cityghg.gdb',layer=str_glue('碳排放_{t}年'),overwrite=TRUE)
}


library(ggspatial)
province = read_sf('./data/province.geojson') |> 
  st_transform(st_crs(albers))

nineline = read_sf('./data/nineline.geojson') |> 
  st_transform(st_crs(albers))

province = st_union(province,nineline)

colors=c(
  '#2166ac',
  '#99d594',
  '#e6f598',
  '#fee08b',
  '#fc8d59',
  '#d53e4f'
)

mapping = function(t){
  filename = paste0('./data/',t,'.xlsx')
  polygon$carbon=read_xlsx(filename,
                           sheet = '二氧化碳排放(万吨)')$总排放Total
  fig1 = ggplot() +
    geom_sf(data = polygon,
            mapping = aes(fill=carbon,color=carbon))+
    geom_sf(size = .2, fill = "transparent", color = "#060d1b", data=province)+
    scale_fill_gradientn(colours = colors)+ 
    scale_color_gradientn(colours = colors)+
    labs(fill = "Carbon", color = "Carbon")
  fig2 = fig1+
    coord_sf(crs = st_crs('epsg:4326')) + ## 将投影坐标转换为大地坐标
    scale_x_continuous(expand = c(0, 0), limits = c(107, 122), breaks = seq(70, 140, 10)) +
    scale_y_continuous(expand = c(0, 0), limits = c(2, 24), breaks = seq(10, 60, 10)) +
    guides(fill = "none", color = "none") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  fig1 +
    coord_sf(crs = st_crs(albers), default_crs = st_crs('epsg:4326')) +
    scale_x_continuous(expand = c(0, 0),
                       limits=c(72,142),
                       breaks=seq(70, 140, 10)) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(17,55.5), 
                       breaks = seq(10, 60, 10)) +
    annotation_scale(location = "bl") + 
    annotation_north_arrow(location = "tl", style = north_arrow_nautical(
      fill = c("grey40", "white"), line_col = "grey20")) +
    annotation_custom(ggplotGrob(fig2),
                      xmin= 122,xmax = 138,
                      ymin=15,ymax = 29)+
    theme_bw() +
    theme(
      panel.grid=element_blank(),
      legend.position = c(0.99,0.85),
      legend.justification = c(1,1),
      axis.text= element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )-> fig
  return(fig)
}

f1 = mapping(2005)
f2 = mapping(2010)
f3 = mapping(2015)
f4 = mapping(2020)

library(patchwork)

f1 + labs(title="(a)2005")+
  f2 + labs(title="(b)2010")+
  f3 + labs(title="(c)2015")+
  f4 + labs(title="(d)2020")+
  plot_layout(nrow=2,
              widths = c(8,8), heights = c(6,6))->  fig

ggsave("./carbon2020.png", 
       fig, 
       height=12,width=16,
       dpi=600)