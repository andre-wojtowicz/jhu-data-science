library(ggplot2)
library(R.utils)

# print(paste("Download time:", Sys.time()))
# 
# temp = tempfile()
# 
# setInternet2(use = TRUE)
# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",temp, mode="wb")
# bunzip2(temp, "repdata-data-StormData.csv")
# dt = read.table("repdata-data-StormData.csv", sep=",", header=T)
# 
# unlink(temp)

codes2statename = read.table(sep="|", header=T,
text="code|name
1|Alabama
2|Alaska
3|American Samoa
4|Arizona
5|Arkansas
6|California
7|Canal Zone
8|Colorado
9|Connecticut
10|Delaware
11|D.C.
12|Florida
13|Georgia
14|Guam
15|Hawaii
16|Idaho
17|Illinois
18|Indiana
19|Iowa
20|Kansas
21|Kentucky
22|Louisiana
23|Maine
24|Maryland
25|Massachusetts
26|Michigan
27|Minnesota
28|Mississippi
29|Missouri
30|Montana
31|Nebraska
32|Nevada
33|New Hampshire
34|New Jersey
35|New Mexico
36|New York
37|North Carolina
38|North Dakota
39|Ohio
40|Oklahoma
41|Oregon
42|Pennsylvania
43|Puerto Rico
44|Rhode Island
45|South Carolina
46|South Dakota
47|Tennessee
48|Texas
49|Utah
50|Vermont
51|Virginia
52|Virgin Islands
53|Washington
54|West Virginia
55|Wisconsin
56|Wyoming
60|American Samoa
64|F.S. of Micronesia
66|Guam
67|Johnston Atoll
68|Marshall Islands
69|Nt. Mariana Islands
70|Palau
71|Midway Islands
72|Puerto Rico
74|Minor Outlying Islands
76|Navassa Island
78|Virgin Islands
79|Wake Island
81|Baker Island
84|Howland Island
86|Jarvis Island
89|Kingman Reef
95|Palmyra Atoll
57|Pacific coast
58|Alaskan coast
59|Hawaiian coast
61|American Samoa waters
65|Mariana Islands waters
73|Atlantic coast (Maine to Virginia)
75|Atlantic coast (the rest)
77|Gulf of Mexico
91|Lake Superior
92|Lake Michigan
93|Lake Huron
94|Detroit waters
96|Lake Erie
97|Niagara River and Lake Ontario
98|St. Lawrence River
83|Other
85|Other
87|Other
88|Other
90|Other")

strexp2num = function(x)
{
    if (x %in% c("h", "H"))
        return(100)
    
    if (x %in% c("k", "K"))
        return(1000)
    
    if (x %in% c("m", "M"))
        return(1000000)
    
    if (x %in% c("b", "B"))
        return(1000000000)
    
    return(1)
}

calc_cost_economy  = function(row)
{
    return( as.numeric(row[['PROPDMG']]) * 
            strexp2num(row[['PROPDMGEXP']]) + 
            as.numeric(row[['CROPDMG']]) * 
            strexp2num(row[['CROPDMGEXP']]) )
}

calc_cost_health = function(row)
{
    return( as.numeric(row[['FATALITIES']]) * 50 + 
            as.numeric(row[['INJURIES']]) )
}



#____________________________

COLS = c("STATE__", "EVTYPE", "FATALITIES", "INJURIES",
         "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")

STATE_IDS_TO_OMIT = c(83,85,87,88,90)

cp = dt[,COLS]
cp = cp[!cp$STATE__ %in% STATE_IDS_TO_OMIT,]

cp$COST_HEALTH  = 0
cp$COST_ECONOMY = 0

cp$COST_HEALTH  = apply(cp, 1, calc_cost_health)
cp$COST_ECONOMY = apply(cp, 1, calc_cost_economy)

#______

h_stats = data.frame(state=character(0), 
                     event=character(0), 
                     cost=numeric(0))

e_stats = data.frame(state=character(0), 
                     event=character(0), 
                     cost=numeric(0))

for (i in levels(factor(cp$STATE__)))
{
    sid = as.numeric(i)
    sname = as.character(codes2statename[codes2statename$code==sid,"name"])
    
    nd = cp[cp$STATE__==sid, c("EVTYPE",
                               "COST_HEALTH", 
                               "COST_ECONOMY")]
    
    h_costs = aggregate(nd$COST_HEALTH, list(event=nd$EVTYPE), sum)
    e_costs = aggregate(nd$COST_ECONOMY, list(event=nd$EVTYPE), sum)
    
    hid = which.max(h_costs$x)
    eid = which.max(e_costs$x)
    
    h_add = data.frame(state=sname, 
                       event=h_costs[hid,"event"], 
                       cost=h_costs[hid,"x"])
    
    e_add = data.frame(state=sname, 
                       event=e_costs[eid,"event"], 
                       cost=e_costs[eid,"x"])
    
    if (as.numeric(h_add$cost)>0)
        h_stats = rbind(h_stats, h_add)
    
    if (as.numeric(e_add$cost)>0)
        e_stats = rbind(e_stats, e_add)
    
}

e_stats$cr_cost = pmin(e_stats$cost, 4.0e+10)

# --------- PLOTS

qplot(x=state, y=cost, ylim=c(0,max(h_stats$cost*1.3)), 
      data=h_stats, geom="bar", stat="identity",
      main="Harmfulness of events to population health across the United States") + 
    xlab("Region name") + 
    ylab("Human cost") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    geom_text(aes(y=cost, ymax=cost, label=paste(cost, event, sep=" - ")), 
              position=position_dodge(width=1), 
              angle=90, hjust=0)



qplot(x=state, y=cr_cost, ylim=c(0,max(e_stats$cr_cost*1.4)), 
      data=e_stats, geom="bar", stat="identity",
      main="Harmfulness of events to economy across the United States (bars cropped to 4.0e+10)") + 
    xlab("Region name") + 
    ylab("Damage cost in USD") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    geom_text(aes(y=cr_cost, ymax=cr_cost, label=paste(cost, event, sep=" - ")), 
              position=position_dodge(width=1), 
              angle=90, hjust=0)

