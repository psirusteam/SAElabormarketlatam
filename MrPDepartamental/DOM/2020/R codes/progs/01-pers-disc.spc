/* SAVE "V:\DAT\2015\SAECEPAL\MrP\Republica dominicana\R codes\vars\persona-disca.rbf"*/
DEFINE PERSONA.DISCA
	AS (PERSONA.P34A = 1 OR  PERSONA.P34B = 1 OR  PERSONA.P34C = 1 OR
      PERSONA.P34D = 1 OR  PERSONA.P34E = 1 OR  PERSONA.P34F= 1 OR 
      PERSONA.P34G = 1 OR  PERSONA.P34H = 1 OR  PERSONA.P34I = 1 OR
      PERSONA.P34J = 1 OR  PERSONA.P34K = 1 OR  PERSONA.P34L = 1 )
MISSING  9
NOTAPPLICABLE 9
    TYPE INTEGER 
	RANGE 0 - 1 
VALUELABELS
0 "No discapacitado"
1 "Discapacitado"
group    DISCAPACIDAD
VARLABEL "Tiene discapacidad"
SAVE "V:\DAT\2015\SAECEPAL\MrP\Republica dominicana\R codes\vars\persona-disca.rbf" overwrite

//Program generated frequency for the defined variables
freq PROVIC.IDPROVI
by  VIVIENDA.ZONA
by PERSONA.P27
by PERSONA.P29
by PERSONA.ANEST
by PERSONA.DISCA
outputfile xls "V:\DAT\2015\SAECEPAL\MrP\Republica dominicana\Data\Disca.xlsx" append



