
//Primero, abrí la base.
//La base la descargué en SPSS como está en el INEC (Dic 2019) y luego la 
//tranformé en Stata usando SPSS. No hice nada más, todo es reproducible;)
//La base la sque de aqui (use la de personas): 
//https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2019/Diciembre/BDD_ENEMDU_2019_12_SPSS.zip


use "C:\Users\mfayt\Google Drive\Quantificador Marco\Desempleo y autoidentificación étnica\enemdu_person_201912.dta", clear

//Para esta pequeña idea, solo quiero hogares donde pueda identificar padres/madres e hijos.
//Para hacer eso, por la estructura de la encuesta, puedo identificar a esos hogares donde hay hijos presentes!

//Estoy usando Stata y debo usar muchos artilugios para hacer eso.
//Primero, quiero un buen identificador de hogar.
egen hhid = concat(area ciudad conglomerado panelm vivienda hogar)
//Ahora, quiero ver en cada hogar si hay un hijo.
bysort hhid: gen hijo1 = 1 if p04 ==3
bysort hhid: egen hijo2 = min(hijo1) 
recode hijo2 (. = 0)
gen hogarconhijos = hijo2
drop if hogarconhijo == 0

//Ok, ahora estoy muy seguro que solo se quedaron los hogares que tienen un
//jefe o jefa de hogar e hijos. En este análisis no me interesan muchos 
//los y las cónyuges de jefes de hogar porque no necesariamente estos son
//padres o madres de los hijos, así que voy a ahcer la base para quedarme solo
//con padres/madres e hijos.

fre p04
keep if p04 == 1 | p04 == 3

//Bien, ahora tengo una base solo de jefes de hogar con sus hij@s, justo lo que
//quería. Ciertos hijos son muy pequeños para ser sujetos de la ENEMDU y no
//contestaron preguntas de autuidentificación étnica. Específicamente desde los 5
//años el cuestionario permite contestar algunas preguntas. 
//Vamos a hacer esto para este ejercicio! La Constitución permite que jóvenes 
//voten desde los 16, entonces voy a sacar hogar que no tengan hijos que 
//hayan cumplido al menos 16 años. Así el análisis va a tener solo padres
//y "jóvenes" quen pueden tomar una decisón tan importante como votar.
//El supuesto es que si puedes votar puedes hacer una decisión consciente
//de cómo te autoidentifiques según tus costumbres, color de piel, etc.

//Ahora sí, veamos qué hogares tienen hijos mayores a 16 años.
gen dieciseis = 1 if p04 == 3 & p03 >= 16
bysort hhid: egen hijo16 = min(dieciseis) 
//Ok, de aquí podemos descartar los hogares que no tienen hijos 16 o más.
drop if hijo16 == .

//Hogares que tienen hijos mayor a 16 puedes tener hijos menores.
//En este ejercicio no vamos a usar a estos hijos. Simplemente los
//sacaremos de la muestra analítica.

drop if p03 < 16

//Estamos casi ahí, casi. Finalmente quiero quedarme solo con un padre o madre
//y un hijo por hogar! De los hijos que quedan, voy a seleccionar de forma
//aleatorio solo 1 por hogar!!

//Para hacer todo reproducible, vamos a ponerle un seed porque sabemos que el
//verdadero random no existe ;)

set seed 12345
bysort hhid: sample 1 if p04 == 3, count

//Yupi, tenemos una base de padres/madres e hijos/hijas. En cada hogar hay
//un padre/madre y un hijo/hija. Ahora sí, vamos a intentar poner
//la autoidentificación del padre/madre y del hijo/a como variables a nivel
//de hogar, pero antes en la aleatorización algo pasó con el orden. Quiero
//que la base tenga primero padres/madres y dsps hij@s.

sort area ciudad conglomerado panelm vivienda hogar p01 p04


//ahora sí...

gen auto_progenitor0 = p15 if p04 == 1
bysort hhid: egen iden_pro = min(auto_progenitor0)

gen auto_hijo0 = p15 if p04 ==3
bysort hhid: egen iden_hijoa = min(auto_hijo0)

// y ponemos los values respectivos

label values iden_pro iden_hijoa p15

//Uff, casi 70 líneas de código para algo tan sencillo.

//Finalmente, hagos que la base sea solo por hogar.

egen byte tag = tag(hhid)
drop if tag == 0

//Tenemos 15.5k hogares que cumplen con todas las características mencionadas.
//1) En l hogar hay un/a jefe de hogar y al menos un hijo de 16 años.
//2) Solo nos quedamos con un 1 hijo de 16 anos x hogar.

tab iden_pro iden_hijoa, row nofreq
keep iden_pro iden_hijoa
save "C:\Users\mfayt\Google Drive\Quantificador Marco\Desempleo y autoidentificación étnica\enemdu_person_201912_quantidep.dta", replace

