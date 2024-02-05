# Odkazy:
- RDF (Resource Description Framework):
	- [RDF intro](https://www.w3.org/TR/rdf11-concepts/)
	- [wiki](https://en.wikipedia.org/wiki/Resource_Description_Framework)
	- [wiki: formats](https://en.wikipedia.org/wiki/Resource_Description_Framework#Serialization_formats)
	- [RDF primer W3](https://www.w3.org/TR/rdf11-primer/)
	- [RDF xml W3](https://www.w3.org/TR/rdf-syntax-grammar/)

# MAIN POINTS: 
1. jak reprezentovat popis scény?
2. jak převést řeč do zvoleného popisu scény?
3. jak porovnat referenční popis s generovaným?
4. jak vyhodnotit rozdíly?

# TODO: 
jak řešit nejednodznačné hodnoty?
např: pes má barvu, která je mezi hnědou a černou -> přidat logické operátory do atributů?

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
