# Odkazy:
- RDF (Resource Description Framework):
	- [RDF intro](https://www.w3.org/TR/rdf11-concepts/)
	- [wiki](https://en.wikipedia.org/wiki/Resource_Description_Framework)
	- [wiki: formats](https://en.wikipedia.org/wiki/Resource_Description_Framework#Serialization_formats)
	- [RDF primer W3](https://www.w3.org/TR/rdf11-primer/)
	- [RDF xml W3](https://www.w3.org/TR/rdf-syntax-grammar/)
- Research: 
	[Scene understanding using natural language description based on 3D semantic graph map](https://link.springer.com/article/10.1007/s11370-018-0257-x)

# MAIN POINTS: 
1. DONE: jak reprezentovat popis scény?
2. SKIP: jak převést řeč do zvoleného popisu scény?
3. WIP : jak porovnat referenční popis s generovaným?
4. TODO: jak vyhodnotit rozdíly?

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
