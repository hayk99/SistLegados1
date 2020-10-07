       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK9.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUMF
           FILE STATUS IS FST.


       DATA DIVISION.
       FILE SECTION.
       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TAJETAREG.
           02 TNUMF      PIC 9(16).
           02 TPINF      PIC  9(4).


       WORKING-STORAGE SECTION.
       77 FST                      PIC  X(2).

       78 BLACK                   VALUE      0.
       78 BLUE                    VALUE      1.
       78 GREEN                   VALUE      2.
       78 CYAN                    VALUE      3.
       78 RED                     VALUE      4.
       78 MAGENTA                 VALUE      5.
       78 YELLOW                  VALUE      6.
       78 WHITE                   VALUE      7.

       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO               PIC   9(4).
               10 MES               PIC   9(2).
               10 DIA               PIC   9(2).
           05 HORA.
               10 HORAS             PIC   9(2).
               10 MINUTOS           PIC   9(2).
               10 SEGUNDOS          PIC   9(2).
               10 MILISEGUNDOS      PIC   9(2).
           05 DIF-GMT               PIC  S9(4).

       01 KEYBOARD-STATUS           PIC 9(4).
           88 ENTER-PRESSED          VALUE 0.
           88 PGUP-PRESSED        VALUE 2001.
           88 PGDN-PRESSED        VALUE 2002.
           88 UP-ARROW-PRESSED    VALUE 2003.
           88 DOWN-ARROW-PRESSED  VALUE 2004.
           88 ESC-PRESSED         VALUE 2005.

       77 LAST-MOV-NUM             PIC  9(35).
       77 PRESSED-KEY              PIC   9(4).
       77 PIN-ANTIGUO              PIC  9(4).
       77 PIN-INTRODUCIDO          PIC  9(4).
       77 PIN-INTRODUCIDO2         PIC  9(4).

       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 DATA-ACCEPT.
           05 TARJETA-ACCEPT BLANK ZERO AUTO LINE 10 COL 50
               PIC 9(4) USING PIN-INTRODUCIDO.
           05 TARJETA-ACCEPT BLANK ZERO AUTO LINE 12 COL 50
               PIC 9(4) USING PIN-INTRODUCIDO2.

       



       PROCEDURE DIVISION USING TNUM.
       IMPRIMIR-CABECERA.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.

           DISPLAY BLANK-SCREEN.
           DISPLAY "Cajero Automatico UnizarBank" LINE 2 COL 26
               WITH FOREGROUND-COLOR IS 1.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY DIA LINE 4 COL 32.
           DISPLAY "-" LINE 4 COL 34.
           DISPLAY MES LINE 4 COL 35.
           DISPLAY "-" LINE 4 COL 37.
           DISPLAY ANO LINE 4 COL 38.
           DISPLAY HORAS LINE 4 COL 44.
           DISPLAY ":" LINE 4 COL 46.
           DISPLAY MINUTOS LINE 4 COL 47.


         P1.
           INITIALIZE PIN-ANTIGUO.
           INITIALIZE PIN-INTRODUCIDO.
           INITIALIZE PIN-INTRODUCIDO2.
           MOVE TNUM TO TNUMF.
           
           DISPLAY "Introduce el nuevo pin:" LINE 10 COL 15.
           DISPLAY "Vuelve a introducir el nuevo pin:" 
                LINE 12 COL 15.
           ACCEPT DATA-ACCEPT ON EXCEPTION
               IF ESC-PRESSED
                   GO TO IMPRIMIR-CABECERA
               ELSE
                   GO TO P1.

           OPEN I-O TARJETAS.
           IF FST NOT = 00
               GO TO PSYS-ERR2.

           READ TARJETAS INVALID KEY GO TO PSYS-ERR2.

           IF PIN-INTRODUCIDO <> PIN-INTRODUCIDO2 
                GO TO PSYS-ERR
           ELSE 
                GO TO CAMBIO-PIN.


       PSYS-ERR.
            PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
            DISPLAY "Error, los pines introducidos son distintos" 
            LINE 11 COL 20 WITH FOREGROUND-COLOR IS BLACK
                BACKGROUND-COLOR IS RED.
            DISPLAY "Enter - Aceptar" LINE 24 COL 33.
           GO TO EXIT-ENTER.

       CAMBIO-PIN.
            PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
            MOVE TNUM TO TNUMF.
            MOVE PIN-INTRODUCIDO TO TPINF.
            REWRITE TAJETAREG INVALID KEY GO TO PSYS-ERR.
            DISPLAY "Se ha cambiado el pin correctamente." 
                LINE 9 COL 20.
            DISPLAY "Enter - Aceptar" LINE 24 COL 33
                WITH FOREGROUND-COLOR IS YELLOW.
           GO TO EXIT-ENTER.

       PSYS-ERR2.
            PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
            DISPLAY "Ha ocurrido un error..." LINE 9 COL 25
                WITH FOREGROUND-COLOR IS BLACK
                     BACKGROUND-COLOR IS RED.
            
            DISPLAY "Enter - Aceptar" LINE 24 COL 33.
           GO TO EXIT-ENTER.

       EXIT-ENTER.
           CLOSE TARJETAS.
           ACCEPT PRESSED-KEY LINE 24 COL 80 
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.
