# Odkazy:
- RDF (Resource Description Framework):
	- [RDF intro](https://www.w3.org/TR/rdf11-concepts/)
	- [wiki](https://en.wikipedia.org/wiki/Resource_Description_Framework)
	- [wiki: formats](https://en.wikipedia.org/wiki/Resource_Description_Framework#Serialization_formats)
	- [RDF primer W3](https://www.w3.org/TR/rdf11-primer/)
	- [RDF xml W3](https://www.w3.org/TR/rdf-syntax-grammar/)
- Other research:
	- [Scene understanding using natural language description based on 3D semantic graph map](https://link.springer.com/article/10.1007/s11370-018-0257-x)
- other useful urls: 
	- [český národní korpus](https://wiki.korpus.cz/doku.php/cnk:syn)
    - [internetová jazyková příručka](https://prirucka.ujc.cas.cz)

# TODO:
1. problém: jak řešit nejednoznačné hodnoty?
	- např: pes má barvu, která je mezi hnědou a černou 
		1. možnost: přidat logické operátory (AND, OR) do atributů => barva = hnědá OR černá (obě správně, nulová ztráta)
		2. možnost: udělat variabilní hodnocení (když je barva = černá a uživatel řekne hnědá, tak bude chyba minimální, na rozdíl od např červené)

2. problém: když mám text (rozpoznanou přirozenou řeč) a referenční popis (triplety), jak z té řeči určit, které triplety byly určené? 
	1. možnost: ručně sepsat gramatiky pro podchycení objektů, atributů a predikátů (=složky tripletů)
		- nevýhoda: závisí na ručně psaných gramatikách + pracné
	2. možnost: sestavit gramatiky pro podchycení tripletů automaticky
		- nevýhoda: potřeba anotovaná trénovací data
	3. možnost: využít strojové učení pro dynamické určení vzdálenosti promluvy od každého tripletu (a podle hranice určit zda byl triplet řečen, případně jak dobře)
		- nevýhoda: potřeba neuronové sítě schopné porovnat přirozenou řeč a triplet => custom architektura (= potřeba trénovacích dat)
			-> experimentálně vyzkoušeno, že lze generovat syntetická trénovací anotovaná data (ChatGPT), ale riziko degenerace sítě?

vícevrstvá hierarchie

# ASPEKTY POPISU:
1. objekty
	- objekty na obrázku (kočka, pes, člověk, strom, ...)
	- objekty stejného typu (např stromy na obrázku) je třeba rozlišit (číselné ID: `strom #1, strom #2`)
3. hierarchie objektů
	- objekt je součástí jiného objektu, skupina objektů tvoří celek, ...
	- objekty mohou tvořit stromovou strukturu, kde každý uzel je dalším objektem (větve + kmen tvoří strom, stromy tvoří les, ...)
2. atributy objektů
	- takové vlastnosti, které by šlo určit i kdyby na obrázku nebylo nic jiného než daný objekt (barva, tvar, ...)
	- statické vlastnosti nemají vliv na dynamiku obrázku, nejsou závislé na akcích nebo jiných objektech
	- patří sem i činnosti, pokud jsou nezávislé na ostatních objektech (spí, přemýšlí, běží, ...)
5. vazby mezi objekty
	- všechny vlastnosti a činnosti, které závisí na více objektech (triplety `objekt --predikát--> předmět`)
6. (?) extrapolace objektů a dějů
	- co by mohlo nastat dále, co mohlo předcházet
	- co by mohlo být příčinou nějakých dějů/stavů

# NÁPADY NA OBRÁZKY:
- les (různá roční období), farma, domácnost (kuchyně, dílna, ...), zoo, město, hřiště (dětské, různé sporty), pláž


# Poznámky ke schůzce:
- Data-flow diagram - je to takhle ok?

- Ruční psaní gramatik -> problém (na testy a srovnání triviálních příkladů OK, ale na plné použitý bych radši něco jiného)
	- je to hodně pracné
	- dokáže zachytit pouze ty formulace, které mě napadnou při psaní gramatiky
	- nápad na řešení (částečně automatizovat jako alternativní cestu lemmatizér):
		1. tahání slovních tvarů z wikislovníku + chybějící doplnit ručně (nemá univerzita nějaké slovníky/datasety interní co by šly použít?)
		2. formulace tripletů tahat z ChatGPT + doplnění ručně (stále ale pracné a nespolehlivé)

- SRGS má výstup se kterým se obtížně pracuje:
	- výstupy mají tvar závislý na pravidlech => nekonzistentní => nelze snadno parsovat => podchycení tripletů by bylo "špagetové"
		- lze částečně vyřešit vlastním parserem + úmluvou pro tvar pravidel
			- klade omezení na tvar pravidel
			- nespolehlivé v okamžik, kdy začne psát gramatiky někdo jiný
		- můžu udělat nějaký pre-processor, který by schoval problematiku stranou, ale to je "žvejkačkové řešení"
		- mohl bych navrhnout vlastní formát gramatik založený na SRGS, který by lépe vyhovoval mým potřebám
			-> pracné, ale věřím že zvládnutelné (šlo by o drobnou změnu pravidel SRGS, něco ubrat, něco přidat)

- Zpracování ASR chvíli trvá a vyžaduje celé audio soubory => nebude možná vizualizace v reálném čase

- Chtěl bych zkusit tu sémantiku latentního prostoru (pokud zbude čas samozřejmě)
	- je možnost dostat se k nějakým natrénovaným modelům?

- Jaké metriky používat při vyhodnocení?
	- mohu určit zda byl triplet (nebo jen objekty) řečen nebo ne (a případně počítat váženou ztrátu)
		-> šly by sestavit statistiky chyb (který typ objektů například je zapomínán často, kde jsou časté chyby)
	- mohu zkusit najít nejbližší referenční triplet (podle minimální ztráty)
		-> šlo by počítat různé druhy ztrát (ztráta na zapomenutých objektech, ztráta na zapomenutých atributech, ztráta na špatně určených barvách...)
	- mohu sledovat časové známky projevu (jak dlouho popisuje obrázek, jak dělá pauzy, ...)
	- mohu sledovat pořadí v jakém člověk popisuje obrázek (jestli si jako první všimne zřetelné věci uprostřed nebo detailu v rohu)
	-> tohle by se možná hodilo prodiskutovat s někým, kdo má vhled do té medicínské stránky, aby řekl jestli to má vůbec cenu?

- Kolik by tak mělo být rešerše ve finální práci? 
