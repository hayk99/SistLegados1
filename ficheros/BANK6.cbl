       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK6.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM-E
           FILE STATUS IS FST.

           SELECT F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.

           SELECT F-PROGRAMADAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS PROG-NUM
           FILE STATUS IS FSP.


       DATA DIVISION.
       FILE SECTION.
       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TAJETAREG.
           02 TNUM-E      PIC 9(16).
           02 TPIN-E      PIC  9(4).
       FD F-MOVIMIENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "movimientos.ubd".
       01 MOVIMIENTO-REG.
           02 MOV-NUM              PIC  9(35).
           02 MOV-TARJETA          PIC  9(16).
           02 MOV-ANO              PIC   9(4).
           02 MOV-MES              PIC   9(2).
           02 MOV-DIA              PIC   9(2).
           02 MOV-HOR              PIC   9(2).
           02 MOV-MIN              PIC   9(2).
           02 MOV-SEG              PIC   9(2).
           02 MOV-IMPORTE-ENT      PIC  S9(7).
           02 MOV-IMPORTE-DEC      PIC   9(2).
           02 MOV-CONCEPTO         PIC  X(35).
           02 MOV-SALDOPOS-ENT     PIC  S9(9).
           02 MOV-SALDOPOS-DEC     PIC   9(2).

       FD F-PROGRAMADAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "programadas.ubd".
       01 PROGRAMADAS-REG.
           02 PROG-NUM              PIC  9(35).
      *    TARJETA DE ORIGEN     
           02 PROG-TARJETA-O        PIC  9(16).
      *    TARJETA DE DESTINO     
           02 PROG-TARJETA-D        PIC  9(16).
           02 PROG-ANO              PIC   9(4).
           02 PROG-MES              PIC   9(2).
           02 PROG-DIA              PIC   9(2).
           02 PROG-IMPORTE-ENT      PIC   9(7).
           02 PROG-IMPORTE-DEC      PIC   9(2).
           02 PROG-CONCEPTO         PIC  X(35).


       WORKING-STORAGE SECTION.
       77 FST                      PIC   X(2).
       77 FSM                      PIC   X(2).
       77 FSP                      PIC   X(2).

       78 BLACK                  VALUE      0.
       78 BLUE                   VALUE      1.
       78 GREEN                  VALUE      2.
       78 CYAN                   VALUE      3.
       78 RED                    VALUE      4.
       78 MAGENTA                VALUE      5.
       78 YELLOW                 VALUE      6.
       78 WHITE                  VALUE      7.

       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO              PIC   9(4).
               10 MES              PIC   9(2).
               10 DIA              PIC   9(2).
           05 HORA.
               10 HORAS            PIC   9(2).
               10 MINUTOS          PIC   9(2).
               10 SEGUNDOS         PIC   9(2).
               10 MILISEGUNDOS     PIC   9(2).
           05 DIF-GMT              PIC  S9(4).

       01 KEYBOARD-STATUS          PIC  9(4).
           88 ENTER-PRESSED      VALUE     0.
           88 PGUP-PRESSED       VALUE  2001.
           88 PGDN-PRESSED       VALUE  2002.
           88 UP-ARROW-PRESSED   VALUE  2003.
           88 DOWN-ARROW-PRESSED VALUE  2004.
           88 ESC-PRESSED        VALUE  2005.

       77 PRESSED-KEY              PIC   9(4).

       77 LAST-MOV-NUM             PIC  9(35).
       77 LAST-USER-ORD-MOV-NUM    PIC  9(35).
       77 LAST-USER-DST-MOV-NUM    PIC  9(35).

       77 LAST-PROG-NUM             PIC  9(35).

       77 EURENT-USUARIO           PIC  S9(7).
       77 EURDEC-USUARIO           PIC   9(2).
       77 CUENTA-DESTINO           PIC  9(16).
       77 NOMBRE-DESTINO           PIC  X(35).

       77 CENT-SALDO-ORD-USER      PIC  S9(9).
       77 CENT-SALDO-DST-USER      PIC  S9(9).
       77 CENT-IMPOR-USER          PIC  S9(9).

       77 MSJ-ORD                  PIC  X(35) VALUE "Transferimos".
       77 MSJ-DST                  PIC  X(35) VALUE "Nos transfieren".

       77 DIA1-USUARIO              PIC   9(2).
       77 MES1-USUARIO              PIC   9(2).
       77 ANO1-USUARIO              PIC   9(4).

       77 MENSUALMENTE              PIC   9(2).

       77 FECHA-INDICADA            PIC   9(8).
       77 FECHA-ACTUAL              PIC   9(8).

       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 FILTRO-CUENTA.
           05 FILLER BLANK WHEN ZERO AUTO UNDERLINE
               LINE 12 COL 54 PIC 9(16) USING CUENTA-DESTINO.
           05 FILLER AUTO UNDERLINE
               LINE 14 COL 54 PIC X(15) USING NOMBRE-DESTINO.
      *>     05 FILLER BLANK WHEN ZERO AUTO UNDERLINE
           05 FILLER BLANK ZERO AUTO UNDERLINE
               SIGN IS LEADING SEPARATE
               LINE 16 COL 54 PIC -9(7) USING EURENT-USUARIO.
           05 FILLER BLANK ZERO UNDERLINE
               LINE 16 COL 63 PIC 9(2) USING EURDEC-USUARIO.
           05 DIA-MIN BLANK ZERO AUTO UNDERLINE
               LINE 18 COL 54 PIC 9(2) USING DIA1-USUARIO.
           05 MES-MIN BLANK ZERO AUTO UNDERLINE
               LINE 18 COL 57 PIC 9(2) USING MES1-USUARIO.
           05 ANO-MIN BLANK ZERO AUTO UNDERLINE
               LINE 18 COL 60 PIC 9(4) USING ANO1-USUARIO.
           05 FILLER AUTO UNDERLINE
               LINE 20 COL 54 PIC 9(2) USING MENSUALMENTE.

       01 SALDO-DISPLAY.
           05 FILLER SIGN IS LEADING SEPARATE
               LINE 10 COL 33 PIC -9(7) FROM MOV-SALDOPOS-ENT.
           05 FILLER LINE 10 COL 41 VALUE ",".
           05 FILLER LINE 10 COL 42 PIC 99 FROM MOV-SALDOPOS-DEC.
           05 FILLER LINE 10 COL 45 VALUE "EUR".


       PROCEDURE DIVISION USING TNUM.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.

           INITIALIZE CUENTA-DESTINO.
           INITIALIZE NOMBRE-DESTINO.
           INITIALIZE EURENT-USUARIO.
           INITIALIZE EURDEC-USUARIO.
           INITIALIZE LAST-MOV-NUM.
           INITIALIZE LAST-PROG-NUM.
           INITIALIZE LAST-USER-ORD-MOV-NUM.
           INITIALIZE LAST-USER-DST-MOV-NUM.
           INITIALIZE DIA1-USUARIO.
           INITIALIZE MES1-USUARIO.
           INITIALIZE ANO1-USUARIO.
           INITIALIZE MENSUALMENTE.

       IMPRIMIR-CABECERA.
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

       MOVIMIENTOS-OPEN.
           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 00 THEN
               GO TO PSYS-ERR
           END-IF.

       LECTURA-MOVIMIENTOS.
           READ F-MOVIMIENTOS NEXT RECORD AT END GO TO ORDENACION-TRF.
           IF MOV-TARJETA = TNUM THEN
               IF LAST-USER-ORD-MOV-NUM < MOV-NUM THEN
                   MOVE MOV-NUM TO LAST-USER-ORD-MOV-NUM
               END-IF
           END-IF.
           IF LAST-MOV-NUM < MOV-NUM THEN
               MOVE MOV-NUM TO LAST-MOV-NUM
           END-IF.
           GO TO LECTURA-MOVIMIENTOS.



       ORDENACION-TRF.
           CLOSE F-MOVIMIENTOS.

           DISPLAY "Ordenar Transferencia" LINE 8 COL 30.
           DISPLAY "Saldo Actual:" LINE 10 COL 19.

           DISPLAY "Enter - Confirmar" LINE 24 COL 2.
           DISPLAY "ESC - Cancelar" LINE 24 COL 66.

           IF LAST-USER-ORD-MOV-NUM = 0 THEN
               GO TO NO-MOVIMIENTOS
           END-IF.

           MOVE LAST-USER-ORD-MOV-NUM TO MOV-NUM.

      * Lee el fichero de movimientos para calcular saldo creo ??
           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.
           READ F-MOVIMIENTOS INVALID KEY GO PSYS-ERR.
           DISPLAY SALDO-DISPLAY.
           CLOSE F-MOVIMIENTOS.

       INDICAR-CTA-DST.
           DISPLAY "Indica la cuenta destino" LINE 12 COL 19.
           DISPLAY "y nombre del titular" LINE 14 COL 19.
           DISPLAY "Indique la cantidad a transferir" LINE 16 COL 19.
           DISPLAY "," LINE 16 COL 62.
           DISPLAY "EUR" LINE 16 COL 66.
           DISPLAY "Indique la fecha *                   /  /" 
              LINE 18 COL 19.
           DISPLAY "Realizar mensualmente durante ** " LINE 20 COL 19. 
           DISPLAY "* opcional" LINE 22 COL 19. 
           DISPLAY "** indicar el numero de meses" LINE 23 COL 19. 

           COMPUTE CENT-SALDO-ORD-USER = (MOV-SALDOPOS-ENT * 100)
                                         + MOV-SALDOPOS-DEC.

           ACCEPT FILTRO-CUENTA ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           ELSE
               GO TO INDICAR-CTA-DST
           END-IF.


                       

           COMPUTE CENT-IMPOR-USER = (EURENT-USUARIO * 100)
                                     + EURDEC-USUARIO.

           IF CENT-IMPOR-USER > CENT-SALDO-ORD-USER THEN
                   DISPLAY "Indique una cantidad menor!!" LINE 6 COL 27
                    WITH BACKGROUND-COLOR RED
                   GO TO INDICAR-CTA-DST
           END-IF.

           GO TO REALIZAR-TRF-VERIFICACION.

       NO-MOVIMIENTOS.
           DISPLAY "0" LINE 10 COL 51.
           DISPLAY "." LINE 10 COL 52.
           DISPLAY "00" LINE 10 COL 53.
           DISPLAY "EUR" LINE 10 COL 54.

           DISPLAY "Indica la cuenta destino " LINE 12 COL 19.
           DISPLAY "y nombre del titular" LINE 14 COL 19.
           DISPLAY "Indique la cantidad a transferir" LINE 16 COL 19.
           DISPLAY "," LINE 16 COL 62.
           DISPLAY "EUR" LINE 16 COL 66.

           ACCEPT FILTRO-CUENTA ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           END-IF.

           DISPLAY "Indique una cantidad menor!!" LINE 6 COL 19
            WITH BACKGROUND-COLOR RED.

           GO TO NO-MOVIMIENTOS.

       REALIZAR-TRF-VERIFICACION.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ordenar Transferencia" LINE 08 COL 30.
           DISPLAY "Va a transferir:" LINE 11 COL 19.
           DISPLAY EURENT-USUARIO LINE 11 COL 38.
           DISPLAY "." LINE 11 COL 45.
           DISPLAY EURDEC-USUARIO LINE 11 COL 46.
           DISPLAY "EUR de su cuenta" LINE 11 COL 49.
           DISPLAY "a la cuenta cuyo titular es" LINE 12 COL 19.
           DISPLAY NOMBRE-DESTINO LINE 12 COL 48.

           
           COMPUTE FECHA-INDICADA = (ANO1-USUARIO * 10000 
            + MES1-USUARIO*100 + DIA1-USUARIO).
           
           COMPUTE FECHA-ACTUAL = (ANO * 10000 + MES * 100 + DIA).


           IF FECHA-INDICADA < FECHA-ACTUAL THEN
            DISPLAY "Como la fecha introducida es menor a la actual" 
                LINE 14 COL 19
            DISPLAY "se va a utilizar la fecha de hoy" LINE 15 COL 19
           ELSE 
            DISPLAY "La transferencia se efectuara el dia   /  /"
                LINE 14 COL 19
            DISPLAY DIA1-USUARIO LINE 14 COL 56
            DISPLAY MES1-USUARIO LINE 14 COL 59
            DISPLAY ANO1-USUARIO LINE 14 COL 62
           END-IF

           DISPLAY "Enter - Confirmar" LINE 24 COL 2.
           DISPLAY "ESC - Cancelar" LINE 24 COL 66.

       ENTER-VERIFICACION.
           ACCEPT PRESSED-KEY LINE 24 COL 80 ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           ELSE
               GO TO VERIFICACION-CTA-CORRECTA
           END-IF.

       VERIFICACION-CTA-CORRECTA.
           OPEN I-O TARJETAS.
           IF FST <> 00
              DISPLAY "Error verif cta correcta" LINE 24 COL 10
              GO TO PSYS-ERR.

           MOVE CUENTA-DESTINO TO TNUM-E.
           READ TARJETAS INVALID KEY GO TO USER-BAD.
           CLOSE TARJETAS.

           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.
           MOVE 0 TO MOV-NUM.
           MOVE 0 TO LAST-USER-DST-MOV-NUM.

       LECTURA-SALDO-DST.
           READ F-MOVIMIENTOS NEXT RECORD AT END GO TO GUARDAR-TRF.
           IF MOV-TARJETA = CUENTA-DESTINO THEN
               IF LAST-USER-DST-MOV-NUM < MOV-NUM THEN
                   MOVE MOV-NUM TO LAST-USER-DST-MOV-NUM
               END-IF
           END-IF.

           GO TO LECTURA-SALDO-DST.

       GUARDAR-TRF.
           CLOSE F-MOVIMIENTOS.
           MOVE LAST-USER-DST-MOV-NUM TO MOV-NUM.
           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.
           READ F-MOVIMIENTOS INVALID KEY GO TO PSYS-ERR.
           DISPLAY "Error verif GUARDAR-TRF" LINE 24 COL 10

           COMPUTE CENT-SALDO-DST-USER = (MOV-SALDOPOS-ENT * 100)
                                         + MOV-SALDOPOS-DEC.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           ADD 1 TO LAST-MOV-NUM.

           IF (DIA1-USUARIO = 0) OR (MES1-USUARIO = 0) 
                OR (ANO1-USUARIO = 0)
                       MOVE ANO            TO ANO1-USUARIO
                       MOVE MES            TO MES1-USUARIO
                       MOVE DIA            TO DIA1-USUARIO.
                        
           IF (ANO*10000+MES*100+DIA) >= 
                (ANO1-USUARIO*10000+MES1-USUARIO*100+DIA1-USUARIO)
                       MOVE ANO            TO ANO1-USUARIO
                       MOVE MES            TO MES1-USUARIO
                       MOVE DIA            TO DIA1-USUARIO.

      * Caso de transferencia futura que hay que programar
           COMPUTE FECHA-ACTUAL = (ANO*10000 + MES*100 + DIA).
           COMPUTE FECHA-INDICADA = (ANO1-USUARIO*10000 +
                       MES1-USUARIO*100 +
                       DIA1-USUARIO)
           IF FECHA-ACTUAL < FECHA-INDICADA THEN                
               GO TO OPEN-PROGRAMADAS.

      *Caso en el que la transferencia es en el momento      
           MOVE LAST-MOV-NUM   TO MOV-NUM.
           MOVE TNUM           TO MOV-TARJETA.
           MOVE ANO1-USUARIO   TO MOV-ANO.
           MOVE MES1-USUARIO   TO MOV-MES.
           MOVE DIA1-USUARIO   TO MOV-DIA.
           MOVE HORAS          TO MOV-HOR.
           MOVE MINUTOS        TO MOV-MIN.
           MOVE SEGUNDOS       TO MOV-SEG.

           
      
           MULTIPLY -1 BY EURENT-USUARIO.
           MOVE EURENT-USUARIO TO MOV-IMPORTE-ENT.
           MULTIPLY -1 BY EURENT-USUARIO.
           MOVE EURDEC-USUARIO TO MOV-IMPORTE-DEC.

           MOVE MSJ-ORD        TO MOV-CONCEPTO.

           SUBTRACT CENT-IMPOR-USER FROM CENT-SALDO-ORD-USER.

           COMPUTE MOV-SALDOPOS-ENT = (CENT-SALDO-ORD-USER / 100).
           MOVE FUNCTION MOD(CENT-SALDO-ORD-USER, 100)
               TO MOV-SALDOPOS-DEC.

           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.

           ADD 1 TO LAST-MOV-NUM.

           MOVE LAST-MOV-NUM   TO MOV-NUM.
           MOVE CUENTA-DESTINO TO MOV-TARJETA.
           MOVE ANO1-USUARIO   TO MOV-ANO.
           MOVE MES1-USUARIO   TO MOV-MES.
           MOVE DIA1-USUARIO   TO MOV-DIA.
           MOVE HORAS          TO MOV-HOR.
           MOVE MINUTOS        TO MOV-MIN.
           MOVE SEGUNDOS       TO MOV-SEG.

           MOVE EURENT-USUARIO TO MOV-IMPORTE-ENT.
           MOVE EURDEC-USUARIO TO MOV-IMPORTE-DEC.

           MOVE MSJ-DST        TO MOV-CONCEPTO.

           ADD CENT-IMPOR-USER TO CENT-SALDO-DST-USER.
           COMPUTE MOV-SALDOPOS-ENT = (CENT-SALDO-DST-USER / 100).
           MOVE FUNCTION MOD(CENT-SALDO-DST-USER, 100)
               TO MOV-SALDOPOS-DEC.

           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.
           CLOSE F-MOVIMIENTOS.

      * Cosicas nuevas
           IF MENSUALMENTE <> 00
            DISPLAY BLANK-SCREEN.
            DISPLAY "La transferencia se ha realizara mensualmente" 
                LINE 12 COL 30
            DISPLAY MENSUALMENTE LINE 12 COL 73
            ACCEPT PRESSED-KEY LINE 24 COL 80 ON EXCEPTION
            IF ESC-PRESSED THEN
               EXIT PROGRAM
            ELSE
               GO TO OPEN-PROGRAMADAS
            END-IF.
              


       P-EXITO.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.

           DISPLAY "Ordenar transferencia" LINE 8 COL 30.
           DISPLAY "Transferencia realizada correctamente!"
               LINE 11 COL 19.
           DISPLAY "Enter - Aceptar" LINE 25 COL 33.
           GO TO EXIT-ENTER.

       P-PROG-EXITO.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Transferencia programada correctamente para el dia "
            LINE 11 COL 10
           DISPLAY "  /  /    " LINE 12 COL 20
           DISPLAY DIA1-USUARIO LINE 12 COL 20
           DISPLAY MES1-USUARIO LINE 12 COL 23
           DISPLAY ANO1-USUARIO LINE 12 COL 26
           DISPLAY LAST-PROG-NUM LINE 15 COL 10
           
           GO TO EXIT-ENTER.

       PSYS-ERR.
           CLOSE TARJETAS.
           CLOSE F-MOVIMIENTOS.
           CLOSE F-PROGRAMADAS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno" LINE 09 COL 25
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde" LINE 11 COL 32
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" LINE 24 COL 33.

           GO TO EXIT-ENTER.

       PSYS-ERR3.
           CLOSE TARJETAS.
           CLOSE F-MOVIMIENTOS.
           

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno 3" LINE 09 COL 25
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde" LINE 11 COL 32
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" LINE 24 COL 33.
           DISPLAY FSP LINE 12 COL 40
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS WHITE.
           CLOSE F-PROGRAMADAS.

           GO TO EXIT-ENTER.

       PSYS-ERR2.
           CLOSE TARJETAS.
           CLOSE F-MOVIMIENTOS.
           

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno 2" LINE 09 COL 25
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde" LINE 11 COL 32
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" LINE 24 COL 33.
           DISPLAY FSP LINE 12 COL 40
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS WHITE.
           CLOSE F-PROGRAMADAS.

           GO TO EXIT-ENTER.


       EXIT-ENTER.
           ACCEPT PRESSED-KEY LINE 24 COL 80 
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.

       USER-BAD.
           CLOSE TARJETAS.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "La cuenta introducida es incorrecta" LINE 9 COL 22
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Salir" LINE 24 COL 33.
           GO TO EXIT-ENTER.

       OPEN-PROGRAMADAS.
           OPEN I-O F-PROGRAMADAS.
           IF FSP <> 00
               GO TO PSYS-ERR3.

       ESCRITURA-PROGRAMADAS.
           READ F-PROGRAMADAS NEXT RECORD AT END 
                  GO TO ESCRITURA-PROGRAMADAS2.
           IF LAST-PROG-NUM < PROG-NUM THEN
               MOVE PROG-NUM TO LAST-PROG-NUM
           END-IF.
           DISPLAY LAST-PROG-NUM LINE 1 COL 20.
           GO TO ESCRITURA-PROGRAMADAS.

       ESCRITURA-PROGRAMADAS2.
      *Transferencia a repetir mensualmente
           IF MENSUALMENTE <> 00
      *Se programa mensualmente usando el dia actual
            IF FECHA-ACTUAL > FECHA-INDICADA THEN
                MOVE DIA TO DIA1-USUARIO
                MOVE MES TO MES1-USUARIO
                MOVE ANO TO ANO1-USUARIO
            END-IF

           PERFORM ESCRITURA-PROGRAMADAS-M WITH TEST 
            BEFORE UNTIL MENSUALMENTE = 0 

           ELSE
      * Transferencia no mensual     
            ADD 1 TO LAST-PROG-NUM
      *      DISPLAY LAST-PROG-NUM LINE 10 COL 10. 
      *      DISPLAY TNUM LINE 15 COL 10. 
      *      DISPLAY CUENTA-DESTINO LINE 16 COL 10. 
      *      DISPLAY MES1-USUARIO LINE 17 COL 10. 
      *      DISPLAY DIA1-USUARIO LINE 18 COL 10.      
      *      DISPLAY EURENT-USUARIO LINE 19 COL 10.
      *      DISPLAY EURDEC-USUARIO LINE 20 COL 10.     
      *      DISPLAY MSJ-DST LINE 21 COL 10.  

            MOVE LAST-PROG-NUM  TO PROG-NUM
            MOVE TNUM           TO PROG-TARJETA-O
            MOVE CUENTA-DESTINO TO PROG-TARJETA-D
            MOVE ANO1-USUARIO   TO PROG-ANO
            MOVE MES1-USUARIO   TO PROG-MES
            MOVE DIA1-USUARIO   TO PROG-DIA
            MOVE EURENT-USUARIO TO PROG-IMPORTE-ENT
            MOVE EURDEC-USUARIO TO PROG-IMPORTE-DEC
            MOVE "PROGRAMADO"   TO PROG-CONCEPTO

            WRITE PROGRAMADAS-REG INVALID KEY GO TO PSYS-ERR
           END-IF.

           CLOSE F-PROGRAMADAS.
           GO TO P-PROG-EXITO.

       ESCRITURA-PROGRAMADAS-M.
            ADD 1 TO LAST-PROG-NUM.
            SUBTRACT 1 FROM MENSUALMENTE.

            IF MES1-USUARIO = 12
               MOVE 1 TO MES1-USUARIO
               ADD 1 TO ANO1-USUARIO
            ELSE
               ADD 1 TO MES1-USUARIO
            END-IF

            DISPLAY LAST-PROG-NUM LINE 10 COL 10. 
            DISPLAY TNUM LINE 15 COL 10. 
            DISPLAY CUENTA-DESTINO LINE 16 COL 10.
            DISPLAY ANO1-USUARIO LINE 17 COL 10. 
            DISPLAY MES1-USUARIO LINE 18 COL 10. 
            DISPLAY DIA1-USUARIO LINE 19 COL 10.      
            DISPLAY EURENT-USUARIO LINE 20 COL 10.
            DISPLAY EURDEC-USUARIO LINE 21 COL 10.     
            DISPLAY MSJ-DST LINE 22 COL 10.  

            MOVE LAST-PROG-NUM  TO PROG-NUM.
            MOVE TNUM           TO PROG-TARJETA-O.
            MOVE CUENTA-DESTINO TO PROG-TARJETA-D.
            MOVE ANO1-USUARIO   TO PROG-ANO.
            MOVE MES1-USUARIO   TO PROG-MES.
            MOVE DIA1-USUARIO   TO PROG-DIA.
            MOVE EURENT-USUARIO TO PROG-IMPORTE-ENT.
            MOVE EURDEC-USUARIO TO PROG-IMPORTE-DEC.
            MOVE "PROGRAMADO MENSUAL"   TO PROG-CONCEPTO.

            WRITE PROGRAMADAS-REG INVALID KEY GO TO PSYS-ERR.
            


