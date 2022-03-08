setwd("E:/ATEBSc/3Felev/StatBeadando")
fogasponty=read.table("mollies.csv", sep=";", dec=",", header=T)
fix(fogasponty)
str(fogasponty)
names(fogasponty)
table(fogasponty)

# ELső számú populáció adatai (*átlag,*szórás)
#RAYNO-uszonyban lévő sugarak száma
#SL-populációban szereplő egyedek standard hossza
#FINAREA-uszony mérete négyzet mm-ben
#TAREA-farok mérete négyzet mm-ben
populacio1=fogasponty[fogasponty$POP=="1",]
mean(populacio1$RAYNO)
mean(populacio1$SL)
#Legkisebb átlag a három populáció közül.
mean(populacio1$FINAREA)
#Legkisebb átlag a három populáció közül.
mean(populacio1$TAREA)
#Legkisebb átlag a három populáció közül.

sd(populacio1$RAYNO)
#Legnagyobb szórás.
sd(populacio1$SL)
#Legkisebb szórás.
sd(populacio1$FINAREA)
#Legkisebb szórás.
sd(populacio1$TAREA)
#Legkisebb szórás.

# Kettes számú populáció átlaga és szórása az előzőleg említett adatokra vonatkozóan
populacio2=fogasponty[fogasponty$POP=="2",]
mean(populacio2$RAYNO)
#Legkisebb átlag a három populáció közül.
mean(populacio2$SL)
mean(populacio2$FINAREA)
mean(populacio2$TAREA)

sd(populacio2$RAYNO)
sd(populacio2$SL)
sd(populacio2$FINAREA)
sd(populacio2$TAREA)

# Ötös számú populáció átlaga és szórása ugyanazon adatokra nézve, mint az előző populációknál
populacio5=fogasponty[fogasponty$POP=="5",]
mean(populacio5$RAYNO)
mean(populacio5$SL)
mean(populacio5$FINAREA)
mean(populacio5$TAREA)
#Mindegyik változó esetén ez a populáció vette fel a legnagyobb értékeket.

sd(populacio5$RAYNO)
#Legkisebb szórás.
sd(populacio5$SL)
#Legnagyobb szórás.
sd(populacio5$FINAREA)
#Legnagyobb szórás.
sd(populacio5$TAREA)
#Legnagyobb szórás.

#Nem csinálom meg az egész mintára egyszerre az sd-t és a mean-t, mert az anovaval úgyis kiadja

#Előbb lefuttatott kódok szemléltetése boxplottal
par(mfrow=c(2,2))
with(fogasponty, boxplot(populacio1$RAYNO,populacio2$RAYNO,populacio5$RAYNO))
with(fogasponty, boxplot(populacio1$SL,populacio2$SL,populacio5$SL))
with(fogasponty, boxplot(populacio1$FINAREA,populacio2$FINAREA,populacio5$FINAREA))
with(fogasponty, boxplot(populacio1$TAREA,populacio2$TAREA,populacio5$TAREA))

#*A "plots" fület teljesen ki kell nyújtani, különben nem leolvashatóak az ábrák
#Érdemes egy ábrán megjeleníteni mindegyik boxplotot, így összehasonlíthatjuk a künönboző válzotókat is egymással
#Látható, hogy a sugarak száma az első két populációnál megegyezik, azonban az ötös populáció egyedei nagyobb értékeket vesznek fel
#Az első és az ötös populáció mediánja ugyanazon értékre esik 
#(*Nagyon szimmetrikusnak tűnik az összes ábra, mármint nem az egyes populációk boxplotja, hanem maga az ábra ránézésre, ami számomra furcsa)
#Az első populációban vannak átlagosan nézve a legkisebb egyedek, míg az ötösben a legnagyobbak
#A testhossz mindenképpen összefüggésben lehet a standard testhosszal, mivel
#az úszók és a farok területére készített boxplotokon is az látszik, hogy
#az egyes populációk értékei ugyanolyan arányban vannak egymással
#(nem tudom ez értelmes volt e, de arra gondoltam, hogy a mediánok között hasonló a különbség
#és szinten növekvő sorrendben helyezkednek el)

#Paraméterek közötti kapcsolat vizsgálata
#Coplot-tal
library(coin)
library(multcomp)
#Ezeket csak a biztonság kedvéért futtattam le.


fogasponty$POP=factor(fogasponty$POP, levels=c("1","2","5"))
with(fogasponty, coplot(FINAREA~RAYNO|POP, columns=3,
                        panel=function(x,y,...) {
                          panel.smooth(x,y,span=.8,iter=5,...)
                          abline(lm(y ~ x), col="blue") } ))
#Az x-tengelyen az uszony sugarainak értékeit láthatjuk, míg az y-tengelyen a farok területének értékei vannak
#A legfelső ábrán a populációkat láthatjuk, amelyekhez az adatok tartoznak
#Látjuk, hogy az első két populációban kisebb a szóródás, mint az ötötdikben
#Az ötös populáció veszi fel a legnagyobb értékeket a sugárszámra és a FINAREA-ra vonatkozólag is
#Az első két populációban a FINAREA növekedésével a RAYNO is egyértelműen nő
#az ötös ppopulációról ez nem mondható el

with(fogasponty, coplot(TAREA~RAYNO|POP, columns=3,
                        panel=function(x,y,...) {
                          panel.smooth(x,y,span=.8,iter=5,...)
                          abline(lm(y ~ x), col="blue") } ))
#Uyganazokat a következtetéseket lehet levonni a TAREA és a RAYNO kapcsolatára is
#A TAREA értékei nagyobb intervallumon fordulnak elő.


#Wilcoxon-próba
#A boxplotokból levont következtetések alapján, szerintem érdemes egy Wilcoxon-próbát
#elvégezni és megnézni, hogy az első és ötös populáció sugarak számának mediánja között van e különbség

#H0:Nincs különbség a populacio1 és a populacio5 RAYNO mediánja között.
#H1:Van különbség a populacio1 és a populacio5 RAYNO mediánja között.

wilcox.test(populacio1$RAYNO, populacio5$RAYNO, alt="two.sided",paired = F)

#Látjuk, hogy a p-érték nem éri el az általában kiszabott 5%-ot
#Megtartjuk a H0-t
#Azért is végeztem el a tesztet, mert gyanúsak voltak a boxplotok és ezt egyfajta ellenőrzésnek szántam


#Korreláció vizsgálata
#A következő tesztet szintén a boxplotokból származó gyanúm miatt végzem el
#A testhossz és az uszonyméret, valamint a testhossz és a farokméret közötti korrelációt szeretném vizsgálni
#AZt hiszem a Pearson korrelációs tesztet fogom elvégezni
#(igazabol meg sem kellene adnom a methodot, mert automatikusan pearson)
#->paraméterekről van szó
#->engem az érdekelne, hogy lineáris e a kapcsolat a paraméterek között
#->szerencsére lehet alkalmazni, mert nincs kiugró érték azokban a paraméterekben, 
#amire lefuttatom

#Populáció1
?cor.test
par(mfrow=c(2,2))
plot(populacio1$SL,populacio1$FINAREA,main="CorelationStage1",las=1)
cor.test(populacio1$SL,populacio1$FINAREA,method="pearson")

#A kapott p-érték (nagyon kicsi) és a 0.89 értékű korreláció alapján megállapíthatjuk, hogy nincs 
#köztük korreláció


plot(populacio1$SL,populacio1$TAREA,main="CorelationStage1",las=1)
cor.test(populacio1$SL,populacio1$TAREA,method="pearson")
#Szintén van korreláció és összefüggés

#Populáció2
plot(populacio2$SL,populacio2$TAREA,main="CorelationStage2",las=2)
cor.test(populacio2$SL,populacio2$FINAREA, method="pearson")
#Erősen korrelál, szuper KI

plot(populacio2$SL,populacio2$FINAREA,main="Corelationstage2",las=2)
cor.test(populacio2$SL,populacio2$FINAREA,method="pearson")
#Erősen korrelál, szuper KI

#Populacio5
plot(populacio5$SL,populacio5$FINAREA,main = "Corelationstage5",las=3)
cor.test(populacio5$SL,populacio5$FINAREA,method="pearson")
#Erősen korrelál, szuper KI

plot(populacio5$SL,populacio5$TAREA,main="Corelationstage5",las=3)
cor.test(populacio5$SL,populacio5$TAREA,method="pearson")
#Erősen korrelál, szuper KI
#Mindegyik eddig lefuttatott korrelációs teszt erős és pozitív volt, szuper KI-vel
#KI is azt erősíti meg, hogy erős a korreláció (és lineáris)

#Változókhoz tartozó átlagok eltérőségének vizsgálata 
par(mfrow=c(2,2))
modsl = lm(SL ~ POP, data = fogasponty) 
summary(modsl)
plot(modsl) 
#*A p-érték  elég kicsi, tehát az st. egyedméretek átlaga populációnként eltér.
#A  bal felső és alsó reziduumok szórása homogén.
#A jobb felsőnél a reziduumok eloszlása normális, nagyon kevés kiugró érték van.


modfa=aov(FINAREA ~ POP, data = fogasponty) 
summary(modfa)
plot(modfa)
#A p-érték nagyon kicsi, tehát az átlagos FINAREA értékek populációnként eltérőek.
#A  bal felső és alsó reziduumok szórása heterogén.
#A jobb felsőnél a reziduumok eloszlása normális, de a kiugró értékek távolabbiknak tűnnek,
#mint a st. egyedméretek esetében.

modta=aov(TAREA ~ POP, data = fogasponty) 
summary(modta)
plot(modta)
#A p-érték nagyon kicsi, tehát az átlagos TAREA értékek populációnként eltérőek.
#A  bal felső és alsó reziduumok szórása NAGYON heterogén.
#A jobb felsőnél a reziduumok eloszlása normális.

modro=aov(RAYNO ~ POP, data = fogasponty) 
summary(modro)
plot(modro)
#A p-érték>0.005,tehát a RAYNO átlaga populációnként megegyezik
#Nagyon kevés adat van, ezért nehéz leolvasni, de heterogén(nak gondolnám).
#A jobb felső ábrán a reziduumok eloszlása biztos, hogy nem normális.

#ANCOVA
#Rájöttem, hogy nem jó, de úgy gondolkodtam, mintha az egyed azonosítóját a méretei alapján
#adták volna és nem egy spontán számként
#H0:Az egyed és a populáció között nincs interakció.
#H1:Az egyed és a populáció között van interakció.
modno=aov(TAREA~IDNO*POP, data=fogasponty)
summary(modno)


modne=aov(FINAREA~IDNO*POP, data=fogasponty)
summary(modne)
#a p-érték szignifikáns, tehát elvetjük a H0-t mindkét esetben
#A tesztet azzal a céllal futtattam le, hogy megnézzem fontos e 
#a változók szempontjából, hogy egy egyed, melyik populációba tartozik,
#csak azokra az adatcsoportokra néztem meg, amelyeknél az ANOVA szignifikáns p-értéket adott.
#Eleinte elég értelmetlennek tűnt ANCOVA-t lefuttatni erre az adattáblára, de
#"biológiai" szempontból ez megmutatja, hogy, ha véletlenszerűen kivennénk egy egyedet
#a populációból, anélkül, hogy tudnánk, melyik populációt néztük
#az egyed méretei alapján tudnánk következtetni, hogy, melyik populációba tartozik.
#(Nem vagyok benne biztos.)



