libname dane "C:\Justyna\Zadania\Mgr\sem IV\Regresja_logistyczna_SAS\projekt\ESS9BG.sas";

data bulgaria;
	set dane.ess9bg;
	keep happy 
			agea gndr marsts hinctnta nbthcld 
			hhmmb estsz njbspv inprdsc health mocntr;
run;


%let varlist=agea gndr marsts hinctnta nbthcld 
			hhmmb estsz njbspv inprdsc health mocntr;


%macro analiza_zmiennych(var, data);

	proc freq data=&data 
		order=internal;
		tables &var/ trend
			plots=freqplot;
	run;

%mend analiza_zmiennych;
/*
ods graphics on;
%analiza_zmiennych(&varlist, bulgaria)
*/

/* USUNIÊCIE PUSTYCH OBSERWACJI */
data bulgaria2;
set bulgaria;
if hinctnta in (77,88)
	or nbthcld in (77,88)
	or hhmmb in (77,88,99)
	or estsz in (6,7,8,9)
	or njbspv in (77777,88888,99999)
	or agea = 999
	or marsts in (77,88)
	or inprdsc in (77,88)
	or health in (7,8,9)
	or mocntr in (7,8,9)
	or happy in (77,88) then
	delete;
keep happy &varlist;
run;

/*
%analiza_zmiennych(&varlist, bulgaria2)
*/

/* KATEGORYZACJA ZMIENNYCH */
/* ZMIENNA ZALE¯NA */
data bulgaria2;
set bulgaria2;
if happy in (0,1,2,3,4,5) then happy =0;
if happy in (6,7,8,9,10) then happy=1;
run;

proc freq data=bulgaria2;
	table agea*marsts;
run;

/* ZMIENNE NIEZALE¯NE */
data temp;
	set bulgaria2;

	if hhmmb = 1 then
		hhmmb_kat="1 osoba            ";
	if hhmmb = 2 then
		hhmmb_kat="2 osoby";
	if hhmmb in (3,4,5,6,7,8,9,11) then
		hhmmb_kat="3 i wiêcej osób";

	if nbthcld = 66 then
		nbthcld_kat="Brak dzieci          ";
	if nbthcld = 1 then
		nbthcld_kat="1 dziecko";
	if nbthcld = 2 then
		nbthcld_kat="2 dzieci";
	if nbthcld in (3,4,5,6,7,8,9) then
		nbthcld_kat="3 i wiêcej dzieci";

	if health=1 then
		health_kat="Bardzo dobre       ";
	if health=2 then
		health_kat="Dobre";
	if health in(3,4,5) then
		health_kat="Srednie i gorsze";

	if   hinctnta in(1,2) then
		hinctnta_kat="1-2 decyl";
	if   hinctnta in(3,4) then
		hinctnta_kat="3-4 decyl";
	if   hinctnta in(5,6) then
		hinctnta_kat="5-6 decyl";
	if   hinctnta in(7,8,9,10) then
		hinctnta_kat="7-10 decyl";

	if mocntr =1 then
		mocntr_kat="Tak";
	if mocntr=2 then
		mocntr_kat="Nie";

	if marsts=1 then
		marsts_kat="W zwiazku malzenskim                        ";
	if marsts in(4,5) then
		marsts_kat="Rozwiedziony/owdowiony";
	if marsts in(6) then
		marsts_kat="Nigdy w formalnym zwiazku";
	if marsts in(66) then
		marsts_kat="Nie w zwi¹zku lub w zwi¹zku nieformalnym";

	if estsz in (1,2) then
		estsz_kat="Poni¿ej 25       ";
	if estsz = 3 then
		estsz_kat="Miêdzy 25 a 99";
	if estsz in (4,5) then
		estsz_kat="Powy¿ej 99";

	if njbspv = 66666 then
		njbspv_kat="Zwyk³e stanowisko       ";
	if njbspv < 66666 then
		njbspv_kat="Stanowisko kierownicze";

	if inprdsc  = 0 then
		inprdsc_kat="Z nikim                ";
	if inprdsc = 1 then
		inprdsc_kat="Z 1 osob¹";
	if inprdsc in(2,3,4,5,6) then
		inprdsc_kat="Z 2 osobami i wiêcej";

	if gndr=1 then gndr_kat='M';
	if gndr=2 then gndr_kat='K';

run;



%let varlist_kat = gndr_kat marsts_kat hinctnta_kat nbthcld_kat
					hhmmb_kat estsz_kat njbspv_kat inprdsc_kat health_kat mocntr_kat;
/*
%analiza_zmiennych(&varlist_kat, temp);
*/

data temp2;
	set bulgaria2;
	if hhmmb = 1 then
		hhmmb=0;
	if hhmmb = 2 then
		hhmmb=1;
	if hhmmb in (3,4,5,6,7,8,9,11) then
		hhmmb=2;

	if nbthcld = 66 then
		nbthcld=0;
	if nbthcld = 1 then
		nbthcld=1;
	if nbthcld = 2 then
		nbthcld=2;
	if nbthcld in (3,4,5,6,7,8,9) then
		nbthcld=3;

	if health=1 then
		health=0;
	if health=2 then
		health=1;
	if health in(3,4,5) then
		health=2;

	if   hinctnta in(1,2) then
		hinctnta=0;
	if   hinctnta in(3,4) then
		hinctnta=1;
	if   hinctnta in(5,6) then
		hinctnta=2;
	if   hinctnta in(7,8,9,10) then
		hinctnta=3;

	if mocntr =1 then
		mocntr=0;
	if mocntr=2 then
		mocntr=1;


	if marsts=1 then
		marsts=0;
	if marsts in(4,5) then
		marsts=1;
	if marsts in(6) then
		marsts=2;
	if marsts in(66) then
		marsts=3;

	if estsz in (1,2) then
		estsz=0;
	if estsz = 3 then
		estsz=1;
	if estsz in (4,5) then
		estsz=2;

	if njbspv = 66666 then
		njbspv= 0;
	if njbspv > 0 then
		njbspv= 1;

	if inprdsc  = 0 then
		inprdsc=0;
	if inprdsc = 1 then
		inprdsc=1;
	if inprdsc in(2,3,4,5,6) then
		inprdsc=2;

	if gndr=1 then gndr=0;
	if gndr=2 then gndr=1;
run;

/*
%analiza_zmiennych(&varlist, temp2)
*/
/* ZMIANA NAZW I DODANIE ETYKIET */

data dane.bulgaria_przed_kat(rename=(
	happy=Szczesliwy
	agea = wiek
	gndr = plec
	marsts = stan_cywilny
	hinctnta =dochod_gospodarstwa_net
	nbthcld =liczba_dzieci
	hhmmb = liczba_os_w_gosp_dom
	estsz =liczba_os_w_pracy
	njbspv =liczba_podwladnych
	inprdsc =liczba_os_do_rozmow
	health =subiektyny_stan_zdrowia
	mocntr = miejsce_urodzenia_matki

	));
	label
		happy= 'Jak bardzo jesteœ szczêœliwy'
		agea = 'Wiek'
		gndr = 'P³eæ'
		marsts = 'Stan cywilny'
		hinctnta = '£¹czny dochód netto gospodarstwa domowego'
		nbthcld = 'Liczba dzieci'
		hhmmb = 'Liczba osób w gospodarstwie domowym'
		estsz = 'Liczba osób w miejscu pracy'
		njbspv = 'Jakiego typu stanowsko respondent obejmuje w pracy zwyk³e/kierownicze'
		inprdsc = 'Liczba osób z którymi respondent mo¿e porozmawiaæ o sprawach osobistych'
		health = 'Subiektywny stan zdrowia'
		mocntr = 'Czy matka urodzona jest w kraju zamieszkania respondenta';
	set bulgaria2;
	keep happy &varlist;
run;
data dane.bulgaria_kat_opisowe (rename=(
	happy =Szczesliwy
	agea = wiek
	gndr_kat = plec
	marsts_kat = stan_cywilny
	hinctnta_kat =dochod_gospodarstwa_net
	nbthcld_kat =liczba_dzieci
	hhmmb_kat = liczba_os_w_gosp_dom
	estsz_kat =liczba_os_w_pracy
	njbspv_kat =liczba_podwladnych
	inprdsc_kat =liczba_os_do_rozmow
	health_kat =subiektyny_stan_zdrowia
	mocntr_kat = miejsce_urodzenia_matki
	));
	label
	happy= 'Jak bardzo jesteœ szczêœliwy'
		agea = 'Wiek'
		gndr_kat = 'P³eæ'
		marsts_kat = 'Stan cywilny'
		hinctnta_kat = '£¹czny dochód netto gospodarstwa domowego'
		nbthcld_kat = 'Liczba dzieci'
		hhmmb_kat = 'Liczba osób w gospodarstwie domowym'
		estsz_kat = 'Liczba osób w miejscu pracy'
		njbspv_kat = 'Jakiego typu stanowsko respondent obejmuje w pracy zwyk³e/kierownicze'
		inprdsc_kat = 'Liczba osób z którymi respondent mo¿e porozmawiaæ o sprawach osobistych'
		health_kat = 'Subiektywny stan zdrowia'
		mocntr_kat = 'Czy matka urodzona jest w kraju zamieszkania respondenta';
	set temp;
	keep happy agea &varlist_kat;
run;

data dane.bulgaria_kat_num (rename=(
	happy=Szczesliwy
	agea = wiek
	gndr = plec
	marsts = stan_cywilny
	hinctnta =dochod_gospodarstwa_net
	nbthcld =liczba_dzieci
	hhmmb = liczba_os_w_gosp_dom
	estsz =liczba_os_w_pracy
	njbspv =liczba_podwladnych
	inprdsc =liczba_os_do_rozmow
	health =subiektyny_stan_zdrowia
	mocntr = miejsce_urodzenia_matki

	));
	label
		happy= 'Jak bardzo jesteœ szczêœliwy'
		agea = 'Wiek'
		gndr = 'P³eæ'
		marsts = 'Stan cywilny'
		hinctnta = '£¹czny dochód netto gospodarstwa domowego'
		nbthcld = 'Liczba dzieci'
		hhmmb = 'Liczba osób w gospodarstwie domowym'
		estsz = 'Liczba osób w miejscu pracy'
		njbspv = 'Jakiego typu stanowsko respondent obejmuje w pracy zwyk³e/kierownicze'
		inprdsc = 'Liczba osób z którymi respondent mo¿e porozmawiaæ o sprawach osobistych'
		health = 'Subiektywny stan zdrowia'
		mocntr = 'Czy matka urodzona jest w kraju zamieszkania respondenta';
	set temp2;
	keep happy &varlist;
run;

/*
%analiza_zmiennych(szczesliwy wiek plec stan_cywilny dochod_gospodarstwa_net
			liczba_dzieci liczba_os_w_gosp_dom liczba_os_w_pracy liczba_podwladnych
			liczba_os_do_rozmow subiektyny_stan_zdrowia miejsce_urodzenia_matki, dane.bulgaria_kat_opisowe)
*/

/* TABLICE KONTYNGENCJI */

%let variables = plec stan_cywilny dochod_gospodarstwa_net
			liczba_dzieci liczba_os_w_gosp_dom liczba_os_w_pracy liczba_podwladnych
			liczba_os_do_rozmow subiektyny_stan_zdrowia miejsce_urodzenia_matki;

%macro values_check(var,dataset);
%do i=1 %to %sysfunc(countw(&var));
%let zmienna=%sysfunc(scan(&var,&i.));
%put &=zmienna;
proc freq
data=dane.&dataset order=FORMATTED;
tables &zmienna.*szczesliwy /nopercent ;
tables &zmienna./ nocol norow nopercent nocum nofreq plots=freqplot;
title "Tablica kontyngencji";
run;
%end;
%mend;
/*przed kategoryzacja */
ods graphics on;
%values_check(&variables,bulgaria_przed_kat)

/*po kategoryzacji */
ods graphics on;
%values_check(&variables,bulgaria_kat_opisowe)
 
/*histogramy porównawcze*/
ods graphics on;
proc capability data=dane.bulgaria_kat_num noprint;
comphist &variables / class = ( szczesliwy )
intertile = 1.0
vscale=count
ncols = 2
nrows = 1
BARLABEL=percent
midpoints =  0 1 
cfill = ligr
cframetop = blue
cframeside = blue;
inset cpk (4.2) / noframe pos = n;
run;


%macro wykresy;
%do i=1 %to %sysfunc(countw(&variables.));
%let zm=%scan(&variables.,&i.,' ' );

proc sgplot data=dane.bulgaria_kat_opisowe;
vbar &zm. / stat=freq group=szczesliwy;
run;

%end;
%mend;
%wykresy
%analiza_zmiennych(happy,bulgaria2)


/*TABLICE KORELACJI*/

libname dane "C:\Justyna\Zadania\Mgr\sem IV\Regresja_logistyczna_SAS\projekt\ESS9BG.sas";

/*ods rtf file= "C:\Justyna\Zadania\Mgr\sem IV\Regresja_logistyczna_SAS\projekt\korealcje.rtf" style=Sapphire;*/

proc freq data=dane.bulgaria_kat_num;
tables wiek*Szczesliwy
		plec*Szczesliwy
		stan_cywilny*Szczesliwy 
		dochod_gospodarstwa_net*Szczesliwy 
		liczba_dzieci*Szczesliwy 
		liczba_os_w_gosp_dom*Szczesliwy 
		liczba_os_w_pracy*Szczesliwy
		liczba_podwladnych*Szczesliwy 
		liczba_os_do_rozmow*Szczesliwy 
		subiektyny_stan_zdrowia*Szczesliwy 
		miejsce_urodzenia_matki*Szczesliwy;
run;

/*Badanie wspólliniowosci
Macierz korelacji Pearsona*/
proc corr data=dane.bulgaria_kat_num out=bulgaria_pearson noprint pearson nosimple;
	var   
		wiek
		plec
		stan_cywilny
		dochod_gospodarstwa_net
		liczba_dzieci
		liczba_os_w_gosp_dom
		liczba_os_w_pracy
		liczba_podwladnych
		liczba_os_do_rozmow
		subiektyny_stan_zdrowia
		miejsce_urodzenia_matki;
run;


/*Pearson*/
proc report
	data = bulgaria_pearson;
	title 'Macierz korelacji Pearsona';
	define _name_ / display '' style=[font_weight=bold];
	define 	wiek	 / format = 5.2;
	define 	plec	 / format = 5.2;
	define 	stan_cywilny	 / format = 5.2;
	define 	dochod_gospodarstwa_net	 / format = 5.2;
	define 	liczba_dzieci	 / format = 5.2;
	define 	liczba_os_w_gosp_dom	 / format = 5.2;
	define 	liczba_os_w_pracy	 / format = 5.2;
	define 	liczba_podwladnych	 / format = 5.2;
	define 	liczba_os_do_rozmow	 / format = 5.2;
	define 	subiektyny_stan_zdrowia	 / format = 5.2;
	define 	miejsce_urodzenia_matki	 / format = 5.2;
run;

proc format;
value corr
 0 - 0.3 = 's³aba'
 0.3 - 0.6 = 'umiarkowana'
 0.6-1 = 'silna'
 -0.3 - 0 = 's³aba'
 -0.6 - -0.3 = 'umiarkowana'
 -1 - -0.6 = 'silna'
;
run;



/*Raport wyników Pearsona po formatowaniu*/

proc report
	data = bulgaria_pearson;
	title 'Macierz korelacji Pearsona';
	define _name_ / display '' style=[font_weight=bold];
	define 	wiek	/ format = corr.;
	define 	plec	/ format = corr.;
	define 	stan_cywilny	/ format = corr.;
	define 	dochod_gospodarstwa_net	/ format = corr.;
	define 	liczba_dzieci	/ format = corr.;
	define 	liczba_os_w_gosp_dom	/ format = corr.;
	define 	liczba_os_w_pracy	/ format = corr.;
	define 	liczba_podwladnych	/ format = corr.;
	define 	liczba_os_do_rozmow	/ format = corr.;
	define 	subiektyny_stan_zdrowia	/ format = corr.;
	define 	miejsce_urodzenia_matki	/ format = corr.;
run;


/*VIF - Variance Inflation Factor*/
proc reg data=dane.bulgaria_kat_num;
	model Szczesliwy=
		wiek
		plec
		stan_cywilny
		dochod_gospodarstwa_net
		liczba_dzieci
		liczba_os_w_gosp_dom
		liczba_os_w_pracy
		liczba_podwladnych
		liczba_os_do_rozmow
		subiektyny_stan_zdrowia
		miejsce_urodzenia_matki/
		vif tol collin;
run;
%let varlist= wiek
		plec
		stan_cywilny
		dochod_gospodarstwa_net
		liczba_dzieci
		liczba_os_w_gosp_dom
		liczba_os_w_pracy
		liczba_podwladnych
		liczba_os_do_rozmow
		subiektyny_stan_zdrowia
		miejsce_urodzenia_matki;

proc freq data=dane.bulgaria_kat_num;
tables Szczesliwy*(&varlist)/chisq noprint;
run;

/*ods rtf close;*/