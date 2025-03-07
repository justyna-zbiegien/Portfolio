/* ----------------------------------------
Kod wyeksportowany z SAS Enterprise Guide
DATA: sobota, 21 maja 2022     GODZINA: 13:32:18
PROJEKT: Projekt_RL
SCIEZKA PROJEKTU: Z:\RL_projekt\Projekt_RL.egp
---------------------------------------- */

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend _sas_pushchartsize;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend _sas_popchartsize;


%*--------------------------------------------------------------*
 * Tests the current version against a required version. A      *
 * negative result means that the SAS server version is less    *
 * than the version required.  A positive result means that     *
 * the SAS server version is greater than the version required. *
 * A result of zero indicates that the SAS server is exactly    *
 * the version required.                                        *
 *                                                              *
 * NOTE: The parameter maint is optional.                       *
 *--------------------------------------------------------------*;
%macro _SAS_VERCOMP(major, minor, maint);
    %_SAS_VERCOMP_FV(&major, &minor, &maint, &major, &minor, &maint)
%mend _SAS_VERCOMP;

%*--------------------------------------------------------------*
 * Tests the current version against either the required        *
 * foundation or Viya required version depending on whether the *
 * SYSVLONG version is a foundation or Viya one. A negative     *
 * result means that the SAS server version is less than the    *
 * version required.  A positive result means that the SAS      *
 * server version is greater than the version required. A       *
 * result of zero indicates that the SAS server is exactly the  *
 * version required.                                            *
 *                                                              *
 * NOTE: The *maint parameters are optional.                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCOMP_FV(fmajor, fminor, fmaint, vmajor, vminor, vmaint);
    %local major;
    %local minor;
    %local maint;
    %local CurMaj;
    %local CurMin;
    %local CurMnt;

    %* Pull the current version string apart.;
    %let CurMaj = %scan(&sysvlong, 1, %str(.));

    %* The Viya version number has a V on the front which means
       we need to adjust the Maint SCAN funtion index and also
       get the appropriate parameters for the major, minor, and
       maint values we need to check against (foundation or Viya);
    %if %eval(&CurMaj EQ V) %then
        %do;
		   %*   MM mm t           MM = Major version , mm = Minor version , t = Maint version ;
		   %* V.03.04M2P07112018 ;

            %let major = &vmajor;
            %let minor = &vminor;
            %let maint = &vmaint;
			%let CurMaj = %scan(&sysvlong, 2, %str(.));
			%* Index is purposely 2 because V is now one of the scan delimiters ;
			%let CurMin = %scan(&sysvlong, 2, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
			%let CurMnt = %scan(&sysvlong, 3, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
        %end;
    %else
        %do;
		    %* M mm    t           M = Major version , mm = Minor version , t = Maint version ;  
		    %* 9.01.02M0P11212005 ;

            %let major = &fmajor;
            %let minor = &fminor;
            %let maint = &fmaint;
			%let CurMin = %scan(&sysvlong, 2, %str(.));
			%let CurMnt = %scan(&sysvlong, 4, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
        %end;

    %* Now perform the version comparison.;
    %if %eval(&major NE &CurMaj) %then
        %eval(&CurMaj - &major);
    %else
        %if %eval(&minor NE &CurMin) %then
            %eval(&CurMin - &minor);
        %else
            %if "&maint" = "" %then
                %str(0);
            %else
                %eval(&CurMnt - &maint);
%mend _SAS_VERCOMP_FV;

%*--------------------------------------------------------------*
 * This macro calls _SAS_VERCONDCODE_FV() with the passed       *
 * version. If the current server version matches or is newer,  *
 * then the true code (tcode) is executed, else the false code  *
 * (fcode) is executed.                                         *
 * Example:                                                     *
 *  %let isV92 =                                                *
 *     %_SAS_VERCONDCODE(9,2,0,                                 *
 *         tcode=%nrstr(Yes),                                   *
 *         fcode=%nrstr(No))                                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCONDCODE( major, minor, maint, tcode=, fcode= );
    %_SAS_VERCONDCODE_FV( &major, &minor, &maint, &major, &minor, &maint, &tcode, fcode )
%mend _SAS_VERCONDCODE;

%*--------------------------------------------------------------*
 * This macro calls _SAS_VERCOMP_FV() with the passed versions. *
 * If the current server version matches or is newer, then the  *
 * true code (tcode) is executed, else the false code (fcode)   *
 * is executed.                                                 *
 * Example:                                                     *
 *  %let isV92 =                                                *
 *     %_SAS_VERCONDCODE_FV(9,2,0, 3,5,0                        *
 *         tcode=%nrstr(Yes),                                   *
 *         fcode=%nrstr(No))                                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCONDCODE_FV( fmajor, fminor, fmaint, vmajor, vminor, vmaint, tcode=, fcode= );
    %if %_SAS_VERCOMP_FV(&fmajor, &fminor, &fmaint, &vmajor, &vminor, &vmaint) >= 0 %then
        %do;
        &tcode
        %end;
    %else
        %do;
        &fcode
        %end;
%mend _SAS_VERCONDCODE_FV;

%*--------------------------------------------------------------*
 * Tests the current version to see if it is a Viya version     *
 * number.                                                      *
 * A result of 1 indicates that the SAS server is a Viya        *
 * server.                                                      *
 * A zero result indicates that the server version is not       *
 * that of a Viya server.                                       *
 *--------------------------------------------------------------*;
%macro _SAS_ISVIYA;
    %local Major;

    %* Get the major component of the current version string.;
    %let Major = %scan(&sysvlong, 1, %str(.));

    %* Check if it it V for Viya.;
    %if %eval(&Major EQ V) %then
        %str(1);
    %else
        %str(0);
%mend _SAS_ISVIYA;


ODS PROCTITLE;
OPTIONS DEV=SVG;
GOPTIONS XPIXELS=0 YPIXELS=0;
%macro HTML5AccessibleGraphSupported;
    %if %_SAS_VERCOMP_FV(9,4,4, 0,0,0) >= 0 %then ACCESSIBLE_GRAPH;
%mend;
FILENAME EGHTMLX TEMP;
ODS HTML5(ID=EGHTMLX) FILE=EGHTMLX
    OPTIONS(BITMAP_MODE='INLINE')
    %HTML5AccessibleGraphSupported
    ENCODING='utf-8'
    STYLE=HtmlBlue
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
;

/*   POCZATEK WEZLA: RL-Projekt.sas - Kopiuj   */
%LET _CLIENTTASKLABEL='RL-Projekt.sas - Kopiuj';
%LET _CLIENTPROCESSFLOWNAME='Przebieg  procesu';
%LET _CLIENTPROJECTPATH='Z:\RL_projekt\Projekt_RL.egp';
%LET _CLIENTPROJECTPATHHOST='DESKTOP-1MJKF6S';
%LET _CLIENTPROJECTNAME='Projekt_RL.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';

LIBNAME dane "Z:\RL_projekt";

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

/* USUNIECIE PUSTYCH OBSERWACJI */
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
/* ZMIENNA ZALEZNA */
data bulgaria2;
set bulgaria2;
if happy in (0,1,2,3,4,5) then happy =0;
if happy in (6,7,8,9,10) then happy=1;
run;

proc freq data=bulgaria2;
	table agea*marsts;
run;

/* ZMIENNE NIEZALEZNE */
data temp;
	set bulgaria2;

	if hhmmb = 1 then
		hhmmb_kat="1 osoba            ";
	if hhmmb = 2 then
		hhmmb_kat="2 osoby";
	if hhmmb in (3,4,5,6,7,8,9,11) then
		hhmmb_kat="3 i wiecej osob";

	if nbthcld = 66 then
		nbthcld_kat="Brak dzieci          ";
	if nbthcld = 1 then
		nbthcld_kat="1 dziecko";
	if nbthcld = 2 then
		nbthcld_kat="2 dzieci";
	if nbthcld in (3,4,5,6,7,8,9) then
		nbthcld_kat="3 i wiecej dzieci";

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
		marsts_kat="Nie w zwiazku lub w zwiazku nieformalnym";

	if estsz in (1,2) then
		estsz_kat="Ponizej 25       ";
	if estsz = 3 then
		estsz_kat="Miedzy 25 a 99";
	if estsz in (4,5) then
		estsz_kat="Powyzej 99";

	if njbspv = 66666 then
		njbspv_kat="Zwykle stanowisko       ";
	if njbspv < 66666 then
		njbspv_kat="Stanowisko kierownicze";

	if inprdsc  = 0 then
		inprdsc_kat="Z nikim                ";
	if inprdsc = 1 then
		inprdsc_kat="Z 1 osoba";
	if inprdsc in(2,3,4,5,6) then
		inprdsc_kat="Z 2 osobami i wiecej";

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
		happy= 'Jak bardzo jestes szczesliwy'
		agea = 'Wiek'
		gndr = 'Plec'
		marsts = 'Stan cywilny'
		hinctnta = 'Laczny dochod netto gospodarstwa domowego'
		nbthcld = 'Liczba dzieci'
		hhmmb = 'Liczba osob w gospodarstwie domowym'
		estsz = 'Liczba osob w miejscu pracy'
		njbspv = 'Jakiego typu stanowsko respondent obejmuje w pracy zwykle/kierownicze'
		inprdsc = 'Liczba osob z ktorymi respondent moze porozmawiac o sprawach osobistych'
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
	happy= 'Jak bardzo jestes szczesliwy'
		agea = 'Wiek'
		gndr_kat = 'Plec'
		marsts_kat = 'Stan cywilny'
		hinctnta_kat = 'Laczny dochod netto gospodarstwa domowego'
		nbthcld_kat = 'Liczba dzieci'
		hhmmb_kat = 'Liczba osob w gospodarstwie domowym'
		estsz_kat = 'Liczba osob w miejscu pracy'
		njbspv_kat = 'Jakiego typu stanowsko respondent obejmuje w pracy zwykle/kierownicze'
		inprdsc_kat = 'Liczba osob z ktorymi respondent moze porozmawiac o sprawach osobistych'
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
		happy= 'Jak bardzo jestes szczesliwy'
		agea = 'Wiek'
		gndr = 'Plec'
		marsts = 'Stan cywilny'
		hinctnta = 'Laczny dochod netto gospodarstwa domowego'
		nbthcld = 'Liczba dzieci'
		hhmmb = 'Liczba osob w gospodarstwie domowym'
		estsz = 'Liczba osob w miejscu pracy'
		njbspv = 'Jakiego typu stanowsko respondent obejmuje w pracy zwykle/kierownicze'
		inprdsc = 'Liczba osob z ktorymi respondent moze porozmawiac o sprawach osobistych'
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

/*libname dane "C:\Justyna\Zadania\Mgr\sem IV\Regresja_logistyczna_SAS\projekt\ESS9BG.sas";*/

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
 0 - 0.3 = 'slaba'
 0.3 - 0.6 = 'umiarkowana'
 0.6-1 = 'silna'
 -0.3 - 0 = 'slaba'
 -0.6 - -0.3 = 'umiarkowana'
 -1 - -0.6 = 'silna'
;
run;



/*Raport wynikow Pearsona po formatowaniu*/

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

/* -------------------------------------------------------------------
   Kod wygenerowany przez zadanie SAS-a

   Wygenerowany dnia: sobota, 21 maja 2022 o godz. 21:49:15
   Przez zadanie: RL wsteczna - redukcja

   Dane wejsciowe: Local:RL_LIB_3.BULGARIA_KAT_OPISOWE
   Serwer:  Local
   ------------------------------------------------------------------- */
ODS GRAPHICS ON;

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sortowanie zbioru Local:RL_LIB_3.BULGARIA_KAT_OPISOWE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Szczesliwy, T.wiek, T.plec, T.stan_cywilny, T.dochod_gospodarstwa_net, T.liczba_os_w_gosp_dom, T.liczba_os_w_pracy, T.liczba_podwladnych, T.liczba_os_do_rozmow, T.subiektyny_stan_zdrowia
	FROM RL_LIB_3.BULGARIA_KAT_OPISOWE as T
;
QUIT;
TITLE;
TITLE1 "Rezultaty regresji logistycznej";
FOOTNOTE;
FOOTNOTE1 "Wygenerowane przez SAS-a (&_SASSERVERNAME, &SYSSCPL) dnia %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) o godz. %TRIM(%QSYSFUNC(TIME(), NLTIMAP25.))";
PROC LOGISTIC DATA=WORK.SORTTempTableSorted
		PLOTS(ONLY)=ALL
	;
	CLASS plec 	(PARAM=REF) stan_cywilny 	(PARAM=REF) dochod_gospodarstwa_net 	(PARAM=REF) liczba_os_w_gosp_dom 	(PARAM=REF) liczba_os_w_pracy 	(PARAM=REF) liczba_podwladnych 	(PARAM=REF) liczba_os_do_rozmow 	(PARAM=REF) subiektyny_stan_zdrowia 	(PARAM=REF);
	MODEL Szczesliwy (Event = '1')=wiek plec stan_cywilny dochod_gospodarstwa_net liczba_os_w_gosp_dom liczba_os_w_pracy liczba_podwladnych liczba_os_do_rozmow subiektyny_stan_zdrowia wiek*plec wiek*stan_cywilny wiek*dochod_gospodarstwa_net wiek*liczba_os_w_gosp_dom wiek*liczba_os_w_pracy wiek*liczba_podwladnych wiek*liczba_os_do_rozmow wiek*subiektyny_stan_zdrowia plec*stan_cywilny plec*dochod_gospodarstwa_net plec*liczba_os_w_gosp_dom plec*liczba_os_w_pracy plec*liczba_podwladnych plec*liczba_os_do_rozmow plec*subiektyny_stan_zdrowia stan_cywilny*dochod_gospodarstwa_net stan_cywilny*liczba_os_w_gosp_dom stan_cywilny*liczba_os_w_pracy stan_cywilny*liczba_podwladnych stan_cywilny*subiektyny_stan_zdrowia stan_cywilny*liczba_os_do_rozmow dochod_gospodarstwa_net*liczba_os_w_gosp_dom dochod_gospodarstwa_net*liczba_os_w_pracy dochod_gospodarstwa_net*liczba_podwladnych dochod_gospodarstwa_net*liczba_os_do_rozmow dochod_gospodarstwa_net*subiektyny_stan_zdrowia liczba_os_w_gosp_dom*liczba_os_w_pracy liczba_os_w_gosp_dom*liczba_podwladnych liczba_os_w_gosp_dom*liczba_os_do_rozmow liczba_os_w_gosp_dom*subiektyny_stan_zdrowia liczba_os_w_pracy*liczba_podwladnych liczba_os_w_pracy*liczba_os_do_rozmow liczba_os_w_pracy*subiektyny_stan_zdrowia liczba_podwladnych*liczba_os_do_rozmow liczba_podwladnych*subiektyny_stan_zdrowia liczba_os_do_rozmow*subiektyny_stan_zdrowia		/
		SELECTION=BACKWARD
		SLS=0.05
		INCLUDE=9
		INFLUENCE
		LACKFIT
		AGGREGATE SCALE=NONE
		RSQUARE
		LINK=LOGIT
		CLPARM=WALD
		CLODDS=WALD
		ALPHA=0.05
	;
RUN;
QUIT;

/* -------------------------------------------------------------------
   Koniec kodu zadania
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
ODS GRAPHICS OFF;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%LET _SASPROGRAMFILEHOST=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
