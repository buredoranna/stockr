build_sp500_tkr_df <- function(sd) {
return_df <- data.frame(
aa$Date[1:sd],
aa$Open[1:sd],
aapl$Open[1:sd],
abbv$Open[1:sd],
abc$Open[1:sd],
abt$Open[1:sd],
ace$Open[1:sd],
acn$Open[1:sd],
a$Open[1:sd],
act$Open[1:sd],
adbe$Open[1:sd],
adi$Open[1:sd],
adm$Open[1:sd],
adp$Open[1:sd],
ads$Open[1:sd],
adsk$Open[1:sd],
adt$Open[1:sd],
aee$Open[1:sd],
aep$Open[1:sd],
aes$Open[1:sd],
aet$Open[1:sd],
afl$Open[1:sd],
agn$Open[1:sd],
aig$Open[1:sd],
aiv$Open[1:sd],
aiz$Open[1:sd],
akam$Open[1:sd],
all$Open[1:sd],
alle$Open[1:sd],
altr$Open[1:sd],
alxn$Open[1:sd],
amat$Open[1:sd],
ame$Open[1:sd],
amgn$Open[1:sd],
amp$Open[1:sd],
amt$Open[1:sd],
amzn$Open[1:sd],
an$Open[1:sd],
aon$Open[1:sd],
apa$Open[1:sd],
apc$Open[1:sd],
apd$Open[1:sd],
aph$Open[1:sd],
arg$Open[1:sd],
ati$Open[1:sd],
avb$Open[1:sd],
avp$Open[1:sd],
avy$Open[1:sd],
axp$Open[1:sd],
azo$Open[1:sd],
bac$Open[1:sd],
ba$Open[1:sd],
bax$Open[1:sd],
bbby$Open[1:sd],
bbt$Open[1:sd],
bby$Open[1:sd],
bcr$Open[1:sd],
bdx$Open[1:sd],
beam$Open[1:sd],
ben$Open[1:sd],
bf.b$Open[1:sd],
bhi$Open[1:sd],
biib$Open[1:sd],
bk$Open[1:sd],
blk$Open[1:sd],
bll$Open[1:sd],
bms$Open[1:sd],
bmy$Open[1:sd],
brcm$Open[1:sd],
bsx$Open[1:sd],
btu$Open[1:sd],
bwa$Open[1:sd],
bxp$Open[1:sd],
ca$Open[1:sd],
cag$Open[1:sd],
cah$Open[1:sd],
cam$Open[1:sd],
cat$Open[1:sd],
cb$Open[1:sd],
cbg$Open[1:sd],
cbs$Open[1:sd],
cce$Open[1:sd],
cci$Open[1:sd],
ccl$Open[1:sd],
c$Open[1:sd],
celg$Open[1:sd],
cern$Open[1:sd],
cf$Open[1:sd],
cfn$Open[1:sd],
chk$Open[1:sd],
chrw$Open[1:sd],
ci$Open[1:sd],
cinf$Open[1:sd],
cl$Open[1:sd],
clf$Open[1:sd],
clx$Open[1:sd],
cma$Open[1:sd],
cmcsa$Open[1:sd],
cme$Open[1:sd],
cmg$Open[1:sd],
cmi$Open[1:sd],
cms$Open[1:sd],
cnp$Open[1:sd],
cnx$Open[1:sd],
cof$Open[1:sd],
cog$Open[1:sd],
coh$Open[1:sd],
col$Open[1:sd],
cop$Open[1:sd],
cost$Open[1:sd],
cov$Open[1:sd],
cpb$Open[1:sd],
crm$Open[1:sd],
csc$Open[1:sd],
csco$Open[1:sd],
csx$Open[1:sd],
ctas$Open[1:sd],
ctl$Open[1:sd],
ctsh$Open[1:sd],
ctxs$Open[1:sd],
cvc$Open[1:sd],
cvs$Open[1:sd],
cvx$Open[1:sd],
dal$Open[1:sd],
d$Open[1:sd],
dd$Open[1:sd],
de$Open[1:sd],
dfs$Open[1:sd],
dg$Open[1:sd],
dgx$Open[1:sd],
dhi$Open[1:sd],
dhr$Open[1:sd],
disca$Open[1:sd],
dis$Open[1:sd],
dlph$Open[1:sd],
dltr$Open[1:sd],
dnb$Open[1:sd],
dnr$Open[1:sd],
do$Open[1:sd],
dov$Open[1:sd],
dow$Open[1:sd],
dps$Open[1:sd],
dri$Open[1:sd],
dte$Open[1:sd],
dtv$Open[1:sd],
duk$Open[1:sd],
dva$Open[1:sd],
dvn$Open[1:sd],
ea$Open[1:sd],
ebay$Open[1:sd],
ecl$Open[1:sd],
ed$Open[1:sd],
efx$Open[1:sd],
eix$Open[1:sd],
el$Open[1:sd],
emc$Open[1:sd],
emn$Open[1:sd],
emr$Open[1:sd],
eog$Open[1:sd],
eqr$Open[1:sd],
eqt$Open[1:sd],
esrx$Open[1:sd],
esv$Open[1:sd],
etfc$Open[1:sd],
etn$Open[1:sd],
etr$Open[1:sd],
ew$Open[1:sd],
exc$Open[1:sd],
expd$Open[1:sd],
expe$Open[1:sd],
fast$Open[1:sd],
fb$Open[1:sd],
f$Open[1:sd],
fcx$Open[1:sd],
fdo$Open[1:sd],
fdx$Open[1:sd],
fe$Open[1:sd],
ffiv$Open[1:sd],
fis$Open[1:sd],
fisv$Open[1:sd],
fitb$Open[1:sd],
flir$Open[1:sd],
flr$Open[1:sd],
fls$Open[1:sd],
fmc$Open[1:sd],
fosl$Open[1:sd],
foxa$Open[1:sd],
frx$Open[1:sd],
fslr$Open[1:sd],
fti$Open[1:sd],
ftr$Open[1:sd],
gas$Open[1:sd],
gci$Open[1:sd],
gd$Open[1:sd],
ge$Open[1:sd],
ggp$Open[1:sd],
ghc$Open[1:sd],
gild$Open[1:sd],
gis$Open[1:sd],
glw$Open[1:sd],
gm$Open[1:sd],
gme$Open[1:sd],
gnw$Open[1:sd],
goog$Open[1:sd],
gpc$Open[1:sd],
gps$Open[1:sd],
grmn$Open[1:sd],
gs$Open[1:sd],
gspc$Open[1:sd],
gt$Open[1:sd],
gww$Open[1:sd],
hal$Open[1:sd],
har$Open[1:sd],
has$Open[1:sd],
hban$Open[1:sd],
hcbk$Open[1:sd],
hcn$Open[1:sd],
hcp$Open[1:sd],
hd$Open[1:sd],
hes$Open[1:sd],
hig$Open[1:sd],
hog$Open[1:sd],
hon$Open[1:sd],
hot$Open[1:sd],
hp$Open[1:sd],
hpq$Open[1:sd],
hrb$Open[1:sd],
hrl$Open[1:sd],
hrs$Open[1:sd],
hsp$Open[1:sd],
hst$Open[1:sd],
hsy$Open[1:sd],
hum$Open[1:sd],
ibm$Open[1:sd],
ice$Open[1:sd],
iff$Open[1:sd],
igt$Open[1:sd],
intc$Open[1:sd],
intu$Open[1:sd],
ip$Open[1:sd],
ipg$Open[1:sd],
ir$Open[1:sd],
irm$Open[1:sd],
isrg$Open[1:sd],
itw$Open[1:sd],
ivz$Open[1:sd],
jbl$Open[1:sd],
jci$Open[1:sd],
jec$Open[1:sd],
jnj$Open[1:sd],
jnpr$Open[1:sd],
joy$Open[1:sd],
jpm$Open[1:sd],
jwn$Open[1:sd],
k$Open[1:sd],
key$Open[1:sd],
kim$Open[1:sd],
klac$Open[1:sd],
kmb$Open[1:sd],
kmi$Open[1:sd],
kmx$Open[1:sd],
ko$Open[1:sd],
kors$Open[1:sd],
kr$Open[1:sd],
krft$Open[1:sd],
kss$Open[1:sd],
ksu$Open[1:sd],
lb$Open[1:sd],
l$Open[1:sd],
leg$Open[1:sd],
len$Open[1:sd],
lh$Open[1:sd],
lll$Open[1:sd],
lltc$Open[1:sd],
lly$Open[1:sd],
lm$Open[1:sd],
lmt$Open[1:sd],
lnc$Open[1:sd],
lo$Open[1:sd],
low$Open[1:sd],
lrcx$Open[1:sd],
lsi$Open[1:sd],
luk$Open[1:sd],
luv$Open[1:sd],
lyb$Open[1:sd],
mac$Open[1:sd],
ma$Open[1:sd],
mar$Open[1:sd],
mas$Open[1:sd],
mat$Open[1:sd],
mcd$Open[1:sd],
mchp$Open[1:sd],
mck$Open[1:sd],
mco$Open[1:sd],
m$Open[1:sd],
mdlz$Open[1:sd],
mdt$Open[1:sd],
met$Open[1:sd],
mhfi$Open[1:sd],
mhk$Open[1:sd],
mjn$Open[1:sd],
mkc$Open[1:sd],
mmc$Open[1:sd],
mmm$Open[1:sd],
mnst$Open[1:sd],
mo$Open[1:sd],
mon$Open[1:sd],
mos$Open[1:sd],
mpc$Open[1:sd],
mrk$Open[1:sd],
mro$Open[1:sd],
ms$Open[1:sd],
msft$Open[1:sd],
msi$Open[1:sd],
mtb$Open[1:sd],
mu$Open[1:sd],
mur$Open[1:sd],
mwv$Open[1:sd],
myl$Open[1:sd],
nbl$Open[1:sd],
nbr$Open[1:sd],
ndaq$Open[1:sd],
ne$Open[1:sd],
nee$Open[1:sd],
nem$Open[1:sd],
nflx$Open[1:sd],
nfx$Open[1:sd],
ni$Open[1:sd],
nke$Open[1:sd],
nlsn$Open[1:sd],
noc$Open[1:sd],
nov$Open[1:sd],
nrg$Open[1:sd],
nsc$Open[1:sd],
ntap$Open[1:sd],
ntrs$Open[1:sd],
nu$Open[1:sd],
nue$Open[1:sd],
nvda$Open[1:sd],
nwl$Open[1:sd],
nwsa$Open[1:sd],
oi$Open[1:sd],
oke$Open[1:sd],
omc$Open[1:sd],
orcl$Open[1:sd],
orly$Open[1:sd],
oxy$Open[1:sd],
payx$Open[1:sd],
pbct$Open[1:sd],
pbi$Open[1:sd],
pcar$Open[1:sd],
pcg$Open[1:sd],
pcl$Open[1:sd],
pcln$Open[1:sd],
pcp$Open[1:sd],
pdco$Open[1:sd],
peg$Open[1:sd],
pep$Open[1:sd],
petm$Open[1:sd],
pfe$Open[1:sd],
pfg$Open[1:sd],
pg$Open[1:sd],
pgr$Open[1:sd],
ph$Open[1:sd],
phm$Open[1:sd],
pki$Open[1:sd],
pld$Open[1:sd],
pll$Open[1:sd],
pm$Open[1:sd],
pnc$Open[1:sd],
pnr$Open[1:sd],
pnw$Open[1:sd],
pom$Open[1:sd],
ppg$Open[1:sd],
ppl$Open[1:sd],
prgo$Open[1:sd],
pru$Open[1:sd],
psa$Open[1:sd],
psx$Open[1:sd],
pvh$Open[1:sd],
pwr$Open[1:sd],
px$Open[1:sd],
pxd$Open[1:sd],
qcom$Open[1:sd],
qep$Open[1:sd],
rai$Open[1:sd],
r$Open[1:sd],
rdc$Open[1:sd],
regn$Open[1:sd],
rf$Open[1:sd],
rhi$Open[1:sd],
rht$Open[1:sd],
rig$Open[1:sd],
rl$Open[1:sd],
rok$Open[1:sd],
rop$Open[1:sd],
rost$Open[1:sd],
rrc$Open[1:sd],
rsg$Open[1:sd],
rtn$Open[1:sd],
sbux$Open[1:sd],
scg$Open[1:sd],
schw$Open[1:sd],
se$Open[1:sd],
see$Open[1:sd],
shw$Open[1:sd],
sial$Open[1:sd],
sjm$Open[1:sd],
slb$Open[1:sd],
slm$Open[1:sd],
sna$Open[1:sd],
sndk$Open[1:sd],
sni$Open[1:sd],
so$Open[1:sd],
spg$Open[1:sd],
spls$Open[1:sd],
srcl$Open[1:sd],
sre$Open[1:sd],
sti$Open[1:sd],
stj$Open[1:sd],
stt$Open[1:sd],
stx$Open[1:sd],
stz$Open[1:sd],
swk$Open[1:sd],
swn$Open[1:sd],
swy$Open[1:sd],
syk$Open[1:sd],
symc$Open[1:sd],
syy$Open[1:sd],
tap$Open[1:sd],
t$Open[1:sd],
tdc$Open[1:sd],
te$Open[1:sd],
teg$Open[1:sd],
tel$Open[1:sd],
tgt$Open[1:sd],
thc$Open[1:sd],
tif$Open[1:sd],
tjx$Open[1:sd],
tmk$Open[1:sd],
tmo$Open[1:sd],
trip$Open[1:sd],
trow$Open[1:sd],
trv$Open[1:sd],
tsco$Open[1:sd],
tsn$Open[1:sd],
tso$Open[1:sd],
tss$Open[1:sd],
twc$Open[1:sd],
twx$Open[1:sd],
txn$Open[1:sd],
txt$Open[1:sd],
tyc$Open[1:sd],
unh$Open[1:sd],
unm$Open[1:sd],
unp$Open[1:sd],
ups$Open[1:sd],
urbn$Open[1:sd],
usb$Open[1:sd],
utx$Open[1:sd],
var$Open[1:sd],
v$Open[1:sd],
vfc$Open[1:sd],
viab$Open[1:sd],
vlo$Open[1:sd],
vmc$Open[1:sd],
vno$Open[1:sd],
vrsn$Open[1:sd],
vrtx$Open[1:sd],
vtr$Open[1:sd],
vz$Open[1:sd],
wag$Open[1:sd],
wat$Open[1:sd],
wdc$Open[1:sd],
wec$Open[1:sd],
wfc$Open[1:sd],
wfm$Open[1:sd],
whr$Open[1:sd],
win$Open[1:sd],
wlp$Open[1:sd],
wmb$Open[1:sd],
wm$Open[1:sd],
wmt$Open[1:sd],
wpx$Open[1:sd],
wu$Open[1:sd],
wy$Open[1:sd],
wyn$Open[1:sd],
wynn$Open[1:sd],
x$Open[1:sd],
xel$Open[1:sd],
xl$Open[1:sd],
xlnx$Open[1:sd],
xom$Open[1:sd],
xray$Open[1:sd],
xrx$Open[1:sd],
xyl$Open[1:sd],
yhoo$Open[1:sd],
yum$Open[1:sd],
zion$Open[1:sd],
zmh$Open[1:sd],
zts$Open[1:sd])

colnames(return_df) <- c("Date",
"aa",
"aapl",
"abbv",
"abc",
"abt",
"ace",
"acn",
"a",
"act",
"adbe",
"adi",
"adm",
"adp",
"ads",
"adsk",
"adt",
"aee",
"aep",
"aes",
"aet",
"afl",
"agn",
"aig",
"aiv",
"aiz",
"akam",
"all",
"alle",
"altr",
"alxn",
"amat",
"ame",
"amgn",
"amp",
"amt",
"amzn",
"an",
"aon",
"apa",
"apc",
"apd",
"aph",
"arg",
"ati",
"avb",
"avp",
"avy",
"axp",
"azo",
"bac",
"ba",
"bax",
"bbby",
"bbt",
"bby",
"bcr",
"bdx",
"beam",
"ben",
"bf.b",
"bhi",
"biib",
"bk",
"blk",
"bll",
"bms",
"bmy",
"brcm",
"bsx",
"btu",
"bwa",
"bxp",
"ca",
"cag",
"cah",
"cam",
"cat",
"cb",
"cbg",
"cbs",
"cce",
"cci",
"ccl",
"c",
"celg",
"cern",
"cf",
"cfn",
"chk",
"chrw",
"ci",
"cinf",
"cl",
"clf",
"clx",
"cma",
"cmcsa",
"cme",
"cmg",
"cmi",
"cms",
"cnp",
"cnx",
"cof",
"cog",
"coh",
"col",
"cop",
"cost",
"cov",
"cpb",
"crm",
"csc",
"csco",
"csx",
"ctas",
"ctl",
"ctsh",
"ctxs",
"cvc",
"cvs",
"cvx",
"dal",
"d",
"dd",
"de",
"dfs",
"dg",
"dgx",
"dhi",
"dhr",
"disca",
"dis",
"dlph",
"dltr",
"dnb",
"dnr",
"do",
"dov",
"dow",
"dps",
"dri",
"dte",
"dtv",
"duk",
"dva",
"dvn",
"ea",
"ebay",
"ecl",
"ed",
"efx",
"eix",
"el",
"emc",
"emn",
"emr",
"eog",
"eqr",
"eqt",
"esrx",
"esv",
"etfc",
"etn",
"etr",
"ew",
"exc",
"expd",
"expe",
"fast",
"fb",
"f",
"fcx",
"fdo",
"fdx",
"fe",
"ffiv",
"fis",
"fisv",
"fitb",
"flir",
"flr",
"fls",
"fmc",
"fosl",
"foxa",
"frx",
"fslr",
"fti",
"ftr",
"gas",
"gci",
"gd",
"ge",
"ggp",
"ghc",
"gild",
"gis",
"glw",
"gm",
"gme",
"gnw",
"goog",
"gpc",
"gps",
"grmn",
"gs",
"gspc",
"gt",
"gww",
"hal",
"har",
"has",
"hban",
"hcbk",
"hcn",
"hcp",
"hd",
"hes",
"hig",
"hog",
"hon",
"hot",
"hp",
"hpq",
"hrb",
"hrl",
"hrs",
"hsp",
"hst",
"hsy",
"hum",
"ibm",
"ice",
"iff",
"igt",
"intc",
"intu",
"ip",
"ipg",
"ir",
"irm",
"isrg",
"itw",
"ivz",
"jbl",
"jci",
"jec",
"jnj",
"jnpr",
"joy",
"jpm",
"jwn",
"k",
"key",
"kim",
"klac",
"kmb",
"kmi",
"kmx",
"ko",
"kors",
"kr",
"krft",
"kss",
"ksu",
"lb",
"l",
"leg",
"len",
"lh",
"lll",
"lltc",
"lly",
"lm",
"lmt",
"lnc",
"lo",
"low",
"lrcx",
"lsi",
"luk",
"luv",
"lyb",
"mac",
"ma",
"mar",
"mas",
"mat",
"mcd",
"mchp",
"mck",
"mco",
"m",
"mdlz",
"mdt",
"met",
"mhfi",
"mhk",
"mjn",
"mkc",
"mmc",
"mmm",
"mnst",
"mo",
"mon",
"mos",
"mpc",
"mrk",
"mro",
"ms",
"msft",
"msi",
"mtb",
"mu",
"mur",
"mwv",
"myl",
"nbl",
"nbr",
"ndaq",
"ne",
"nee",
"nem",
"nflx",
"nfx",
"ni",
"nke",
"nlsn",
"noc",
"nov",
"nrg",
"nsc",
"ntap",
"ntrs",
"nu",
"nue",
"nvda",
"nwl",
"nwsa",
"oi",
"oke",
"omc",
"orcl",
"orly",
"oxy",
"payx",
"pbct",
"pbi",
"pcar",
"pcg",
"pcl",
"pcln",
"pcp",
"pdco",
"peg",
"pep",
"petm",
"pfe",
"pfg",
"pg",
"pgr",
"ph",
"phm",
"pki",
"pld",
"pll",
"pm",
"pnc",
"pnr",
"pnw",
"pom",
"ppg",
"ppl",
"prgo",
"pru",
"psa",
"psx",
"pvh",
"pwr",
"px",
"pxd",
"qcom",
"qep",
"rai",
"r",
"rdc",
"regn",
"rf",
"rhi",
"rht",
"rig",
"rl",
"rok",
"rop",
"rost",
"rrc",
"rsg",
"rtn",
"sbux",
"scg",
"schw",
"se",
"see",
"shw",
"sial",
"sjm",
"slb",
"slm",
"sna",
"sndk",
"sni",
"so",
"spg",
"spls",
"srcl",
"sre",
"sti",
"stj",
"stt",
"stx",
"stz",
"swk",
"swn",
"swy",
"syk",
"symc",
"syy",
"tap",
"t",
"tdc",
"te",
"teg",
"tel",
"tgt",
"thc",
"tif",
"tjx",
"tmk",
"tmo",
"trip",
"trow",
"trv",
"tsco",
"tsn",
"tso",
"tss",
"twc",
"twx",
"txn",
"txt",
"tyc",
"unh",
"unm",
"unp",
"ups",
"urbn",
"usb",
"utx",
"var",
"v",
"vfc",
"viab",
"vlo",
"vmc",
"vno",
"vrsn",
"vrtx",
"vtr",
"vz",
"wag",
"wat",
"wdc",
"wec",
"wfc",
"wfm",
"whr",
"win",
"wlp",
"wmb",
"wm",
"wmt",
"wpx",
"wu",
"wy",
"wyn",
"wynn",
"x",
"xel",
"xl",
"xlnx",
"xom",
"xray",
"xrx",
"xyl",
"yhoo",
"yum",
"zion",
"zmh",
"zts")

return(return_df)
}
