  /*
 Creación de años de escolaridad para Argentina, para hacer el cálculo empleamos
 las  condiciones descritas para la encuesta 2019. Las variables emplear son: 
 
      Censo            Encuesta 
      PERSONA.P1808  = ch10: asistencia 
      PERSONA.P1909  = ch12: nivel educacional         
      PERSONA.P2010  = ch13: finalizo el nivel
      PERSONA.P2111A = ch14: a¤os de estudio en el nivel (solo si no lo termino)
  
  Otas variables 
    PERSONA.P03 = edad
 */
DEFINE PERSONA.TEMP1
AS SWITCH
INCASE PERSONA.P03 < 2   
ASSIGN 100                  /* Menores de 2 años */
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP1      
//SAVE "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP1"  overwrite
//FREQ TEMP1


DEFINE PERSONA.TEMP2
AS SWITCH
INCASE TEMP1 < 200   
ASSIGN TEMP1                  
INCASE PERSONA.P1808 = 3
ASSIGN 0                    /* Nunca asistio */ 
INCASE PERSONA.P1808 = 9
ASSIGN 99                    /* NS/NR asistio (NO DISPONIBLE) */ 
INCASE PERSONA.P1909 = 1
ASSIGN 0                    /* Preescolar */ 
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP2                                                          
//SAVE "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP2"  overwrite    
//FREQ TEMP2



DEFINE PERSONA.TEMP3
AS SWITCH
INCASE TEMP2 < 200   
ASSIGN TEMP2                  
INCASE PERSONA.P1909 = 2 AND PERSONA.P2010 =  1
ASSIGN 6                    
INCASE PERSONA.P1909 = 2 AND (PERSONA.P2111A >= 0 AND PERSONA.P2111A <= 6)
ASSIGN PERSONA.P2111A         /* ¨Primaria (0 - 6) */
INCASE PERSONA.P1909 = 2 
ASSIGN 99
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP3                                                          
//SAVE "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP3"  overwrite    
//FREQ TEMP3


DEFINE PERSONA.TEMP4
AS SWITCH
INCASE TEMP3 < 200   
ASSIGN TEMP3                  
/* EGB */
INCASE PERSONA.P1909 = 3 AND PERSONA.P2010 =  1
ASSIGN 9                      /* EBG completa*/
INCASE PERSONA.P1909 = 3 AND (PERSONA.P2111A >= 0 AND PERSONA.P2111A <= 9)
ASSIGN PERSONA.P2111A                              /* EBG completa ¨(0 - 9) */
INCASE PERSONA.P1909 = 3  
ASSIGN 99
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP4                                                          
//SAVE "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP4"  overwrite    
//FREQ TEMP4

DEFINE PERSONA.TEMP5
AS SWITCH
INCASE TEMP4 < 200   
ASSIGN TEMP4                  
 /* Secundario */
INCASE PERSONA.P1909 = 4 AND PERSONA.P2010 = 1     /* Completo */ 
ASSIGN 12       
INCASE PERSONA.P1909 = 4 AND (PERSONA.P2111A >= 0 AND PERSONA.P2111A <= 6)
ASSIGN 6 + PERSONA.P2111A 
INCASE PERSONA.P1909 = 4  
ASSIGN 6
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP5                                                          
 // save "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP5"  overwrite    
//FREQ TEMP5


DEFINE PERSONA.TEMP6
AS SWITCH
INCASE TEMP5 < 200   
ASSIGN TEMP5                  
/* Polimodal */
INCASE PERSONA.P1909 = 5 AND PERSONA.P2010 = 1
ASSIGN 12 
INCASE PERSONA.P1909 = 5 AND (PERSONA.P2111A >= 0 AND PERSONA.P2111A <= 3)
ASSIGN 9 + PERSONA.P2111A 
INCASE PERSONA.P1909 = 5  
ASSIGN 9
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP6                                                          
 // save "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP6"  overwrite    
//FREQ TEMP6

DEFINE PERSONA.TEMP7
AS SWITCH
INCASE TEMP6 < 200   
ASSIGN TEMP6                  
/* Terciario */
INCASE PERSONA.P1909 = 6 AND PERSONA.P2010 = 1
ASSIGN 16
INCASE PERSONA.P1909 = 6 AND (PERSONA.P2111A >= 0 AND PERSONA.P2111A <= 9)
ASSIGN 12 + PERSONA.P2111A 
INCASE PERSONA.P1909 = 6   
ASSIGN 12
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP7                                                          
 // save "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP7"  overwrite    
//FREQ TEMP7


DEFINE PERSONA.TEMP8
AS SWITCH
INCASE TEMP7 < 200   
ASSIGN TEMP7                  
/* Universitario */
INCASE PERSONA.P1909 = 7 AND PERSONA.P2010 = 1
ASSIGN 17
INCASE PERSONA.P1909 = 7 AND (PERSONA.P2111A >= 0 AND PERSONA.P2111A <= 9)
ASSIGN 12 + PERSONA.P2111A 
INCASE PERSONA.P1909 = 7 
ASSIGN 12
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP8                                                          
 // save "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP8"  overwrite    
//FREQ TEMP8

DEFINE PERSONA.TEMP9
AS SWITCH
INCASE TEMP8 < 200   
ASSIGN TEMP8                  
/* Posgrado */
INCASE PERSONA.P1909 = 8 AND PERSONA.P2010 = 1
ASSIGN 19
INCASE PERSONA.P1909 = 8 AND (PERSONA.P2111A >= 0 AND PERSONA.P2111A <= 9)
ASSIGN 17 + PERSONA.P2111A  
INCASE PERSONA.P1909 = 8
ASSIGN 17
ELSE 200
    TYPE INTEGER
    VARLABEL "AUX INDICADORA"
    RANGE 0 : 200
ALIAS TEMP9                                                          
 // save "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\TEMP9"  overwrite    
//FREQ TEMP9

DEFINE PERSONA.ANOEST
AS SWITCH
INCASE TEMP9 < 200   
ASSIGN TEMP9                  
 /* Educacion especial */
INCASE PERSONA.P1909 = 9 OR PERSONA.P1909 = 99
ASSIGN 99
ELSE 99
    TYPE INTEGER
    VARLABEL "ANOEST"
    VALUELABELS
        100 "NA"
        99 "NS/NR"
    RANGE 0 : 200
//ALIAS ANOEST                                                          
SAVE "V:\DAT\SAECEPAL\MrP\Argentina\R codes\redatam\ANOEST"  overwrite    
FREQ PERSONA.ANOEST
FREQ ANOEST BY PERSONA.P1909

