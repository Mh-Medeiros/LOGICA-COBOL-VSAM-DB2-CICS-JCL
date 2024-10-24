      *=================================================================*
       PROGRAM-ID.                             RELAT.
      *=================================================================*
      * PROGRAMA   : RELAT
      * PROGRAMADOR: MATHEUS MEDEIROS
      * ANALISTA   : IVAN SANCHES
      * CONSULTORIA: FOURSYS
      * DATA.......: 02 / 08 / 2024
      *-----------------------------------------------------------------*
      * OBJETIVO...:
      *
      *-----------------------------------------------------------------*
      *    ARQUIVOS                I/O                  INCLUDE/BOOK
      *    CLIENTES               INPUT                  #MSGERRO
      *    RELAT                  OUTPUT                 #DETCLI
      *                                                  #BOOKCLI
      *                                                  #BOOKERRO
      *                                                  #BOOKCABEC
      *-----------------------------------------------------------------*
      * MODULOS....:
      *
      *-----------------------------------------------------------------*
      *                            ALTERACOES
      *-----------------------------------------------------------------*
ALT1  *    PROGRAMADOR: MATHEUS H MEDEIROS
.     *    ANALISTA   : IVAN SANCHES
.     *    CONSULTORIA: FOURSYS
.     *    DATA.......: 31 / 07 / 2024
ATL1  *    OBJETIVO...: INTRODUZIR QUANTIDADE DE PAGINAS
      *
      *-----------------------------------------------------------------*
ALT2  *    PROGRAMADOR: MATHEUS H MEDEIROS
.     *    ANALISTA   : IVAN SANCHES
.     *    CONSULTORIA: FOURSYS
.     *    DATA.......: 01 / 08 / 2024
.     *    OBJETIVO...: INTRODUZIR QUANTIDADE DE REGISTROS LIDOS -
ALT2  *     (NO FINAL DA PAGINA)
      *-----------------------------------------------------------------*
ALT3  *    PROGRAMADOR: MATHEUS H MEDEIROS
.     *    ANALISTA   : IVAN SANCHES
.     *    CONSULTORIA: FOURSYS
.     *    DATA.......: 02 / 08 / 2024
.     *    OBJETIVO...:
ALT3  *
      *=================================================================*


      *=================================================================*
       ENVIRONMENT                             DIVISION.
      *=================================================================*


      *-----------------------------------------------------------------*
       CONFIGURATION                           SECTION.
      *-----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *-----------------------------------------------------------------*
       INPUT-OUTPUT                            SECTION.
      *-----------------------------------------------------------------*
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO
               "C:\CURSOLOGICA\COBOL\RELATORIOS\CLIENTES.DAT"
                   FILE STATUS IS FS-CLIENTES.
ATL3       SELECT RELAT ASSIGN TO
.              "C:\CURSOLOGICA\COBOL\RELATORIOS\RELAT.TXT"
ATL3               FILE STATUS IS FS-RELAT.
      *=================================================================*
       DATA                                    DIVISION.
      *=================================================================*
      *-----------------------------------------------------------------*
       FILE                                    SECTION.
      *-----------------------------------------------------------------*
      *        INPUT -  DADOS DO ARQUIVO DE SAIDA(CLIENTES)
      *                               LRECL = 86
      *
      *-----------------------------------------------------------------*
       FD  CLIENTES.
       COPY "#BOOKCLI".

ATL3   FD  RELAT.
ATL3   01  REG-RELAT               PIC x(086).


       WORKING-STORAGE                         SECTION.
      *-----------------------------------------------------------------*

      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               "***** INICIO DA WORKING *****".
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               "----- VARIAVEL DE STATUS -----".
      *-----------------------------------------------------------------*
       01  FS-CLIENTES                 PIC X(002)          VALUE SPACES.
ATL3   01  FS-RELAT                    PIC X(002)          VALUE SPACES.
      *-----------------------------------------------------------------*
      *----------------------- BOOK DETALHE ----------------------------*
ATL2   COPY "#DETCLI".
.     *---------------------- BOOK DE MENSSAGEM ------------------------*
.      COPY "#MSGERRO".
      *----------------------- BOOK DE ERROS ---------------------------*
.      COPY "#BOOKERRO".
      *----------------------- BOOK DOS CABECALHOS ---------------------*
ATL2   COPY "#BOOKCABEC".
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               "  VARIAVEIS ACUMULADORAS " .
      *-----------------------------------------------------------------*
ATL2   01  WRK-QT-LINHAS               PIC 9(003)    COMP-3 VALUE ZEROS.
ATL1   01  ACU-LIDOS                   PIC 9(003)    COMP-3 VALUE ZEROS.
ATL4
       01  ACU-ASSIN                   PIC 9(007)V99 COMP-3 VALUE ZEROS.
       01  ACU-ASSIN-AUX               PIC 9(003)V99 COMP-3 VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " VARIAVEL DE APOIO ".
      *-----------------------------------------------------------------*
       01  WRK-MSG-ERRO                PIC X(030)          VALUE SPACES.
       01  WRK-CLIENTES                PIC X(008)          VALUE
               "CLIENTES".
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
               " RODAPE ".
      *-----------------------------------------------------------------*
       01  WRK-RODA-PE.
           05 FILLER                   PIC X(062)          VALUE
               "SUB-TOTAL :".
           05 ACU-ASSIN-ED             PIC ZZ9,99        VALUE ZEROS.
      *     05 ACU-TOTAL                PIC ZZ9,99        VALUE ZEROS.
      *-----------------------------------------------------------------*
       01  FILLER                      PIC X(050)           VALUE
           "***** FIM DA WORKING *****".
      *-----------------------------------------------------------------*

      *=================================================================*
       PROCEDURE                               DIVISION.
      *=================================================================*
       0000-PRINCIPAL.

               PERFORM 0100-INICIAR.
               PERFORM 0110-TESTAR-VAZIO.
               PERFORM 0200-PROCESSAR UNTIL FS-CLIENTES NOT EQUAL '00'.

               PERFORM 0300-FINALIZAR.


       0000-PRINCIPAL-FIM.EXIT.
      *-----------------------------------------------------------------*
       0100-INICIAR                            SECTION.
      *-----------------------------------------------------------------*

               OPEN INPUT  CLIENTES
ATL3                OUTPUT RELAT.

ATL3           PERFORM 0104-TESTAR-STATUS.

       0100-INICIAR-FIM.EXIT.
      *-----------------------------------------------------------------*
ATL3   0104-TESTAR-STATUS                      SECTION.
      *-----------------------------------------------------------------*
.      0105-TESTAR-STATUS-CLIENTES.

.              IF FS-CLIENTES                      NOT EQUAL '00'
.                  MOVE WRK-ERRO-ABERTURA          TO WRK-DESCRICAO-ERRO
.                  MOVE FS-CLIENTES                TO WRK-STATUS-ERRO
.                  MOVE '0104-TESTAR-CLIENTES'     TO WRK-AREA-ERRO
.                  PERFORM 9999-TRATA-ERRO
ATL3           END-IF.


       0104-TESTAR-STATUS-FIM.EXIT.

      *-----------------------------------------------------------------*
ATL3   0110-TESTAR-VAZIO                       SECTION.
      *-----------------------------------------------------------------*
.
                PERFORM 0120-LER-DADOS.
.               IF FS-CLIENTES                  NOT EQUAL '00'
.                   MOVE WRK-VAZIO              TO WRK-DESCRICAO-ERRO
.                   MOVE FS-CLIENTES            TO WRK-STATUS-ERRO
.                   MOVE '0105-TESTAR-VAZIO'    TO WRK-AREA-ERRO
.                   PERFORM 9999-TRATA-ERRO
.               ELSE
ATL3                PERFORM 0210-IMP-CABECALHO
               END-IF.

       0110-TESTAR-VAZIO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0120-LER-DADOS                          SECTION.

               READ CLIENTES.

       0120-LER-DADOS-FIM.EXIT.
      *-----------------------------------------------------------------*
       0200-PROCESSAR                          SECTION.
      *-----------------------------------------------------------------
ATL2           ADD 1 TO ACU-LIDOS.
               IF  WRK-QT-LINHAS GREATER THAN 7
                   PERFORM 0270-IMP-VALOR
                   PERFORM 0210-IMP-CABECALHO
               END-IF.
                   PERFORM 0205-SUB-TOTAL.
                   PERFORM 0250-IMP-DETALHE.
                   PERFORM 0120-LER-DADOS.


       0200-PROCESSAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0205-SUB-TOTAL                             SECTION.
      *-----------------------------------------------------------------*

               ADD REG-ASSINATURA TO ACU-ASSIN.
               MOVE ACU-ASSIN     TO ACU-ASSIN-ED.



       0205-SUB-TOTAL-FIM.EXIT.
      *-----------------------------------------------------------------*
       0210-IMP-CABECALHO                          SECTION.
      *-----------------------------------------------------------------*
               MOVE ALL "=" TO WRK-CABEC3.
ATL3
               IF WRK-PAG EQUAL '001'
                   MOVE WRK-CABEC1 TO REG-RELAT
.                  WRITE REG-RELAT AFTER 1 LINE
               ELSE
.                  MOVE WRK-CABEC1 TO REG-RELAT
                       WRITE REG-RELAT AFTER PAGE
      *             MOVE WRK-PULA-LINHA TO REG-RELAT
.     *                 WRITE REG-RELAT AFTER PAGE

               END-IF.

               MOVE WRK-CABEC3 TO REG-RELAT.
.                  WRITE REG-RELAT AFTER 1 LINE.
.
               MOVE WRK-CABEC2 TO REG-RELAT.
.                 WRITE REG-RELAT AFTER 1 LINE.
.              MOVE WRK-CABEC3 TO REG-RELAT.
.                  WRITE REG-RELAT AFTER 1 LINE.

ATL1


               MOVE 4 TO WRK-QT-LINHAS.
ATL1           ADD 1 TO WRK-PAG.
               MOVE 0 TO ACU-ASSIN.





       0210-IMP-CABECALHO-FIM.EXIT.
      *-----------------------------------------------------------------*
       0250-IMP-DETALHE                        SECTION.
      *-----------------------------------------------------------------*

               MOVE REG-CODIGO       TO WRK-DET-CODIGO.
               MOVE REG-NOME         TO WRK-DET-NOME.
               MOVE REG-EMAIL        TO WRK-DET-EMAIL.
               MOVE REG-STREAM       TO WRK-DET-STREAM.
               MOVE REG-ASSINATURA   TO WRK-DET-ASSIN.

ATL3

               MOVE WRK-DETALHE TO REG-RELAT.
ATL3           WRITE REG-RELAT AFTER 1 LINE.

               ADD 1 TO WRK-QT-LINHAS.



       0250-IMP-DETALHE-FIM.EXIT.
      *-----------------------------------------------------------------*
       0260-IMP-ESTATISTICA                    SECTION.
      *-----------------------------------------------------------------*
               DISPLAY WRK-CABEC3.
ATL2           DISPLAY "TOTAL DE REGISTROS LIDOS :"ACU-LIDOS.
ARL4           DISPLAY WRK-CABEC3.



       0260-IMP-ESTATISTICA-FIM.EXIT.
      *-----------------------------------------------------------------*
       0270-IMP-VALOR                          SECTION.
      *-----------------------------------------------------------------*
               MOVE WRK-CABEC3 TO REG-RELAT
                   WRITE REG-RELAT AFTER 1 LINE.

               MOVE WRK-RODA-PE TO REG-RELAT
                   WRITE REG-RELAT AFTER 1 LINE.
               MOVE ACU-ASSIN TO WRK-TOTAL-PAG.

               ADD ACU-ASSIN     TO ACU-ASSIN-AUX.


       0270-IMP-VALOR-FIM.EXIT.
      *-----------------------------------------------------------------*
       0300-FINALIZAR                          SECTION.
      *-----------------------------------------------------------------*
               PERFORM 0270-IMP-VALOR
      *-----------------------------------------------------------------*
               MOVE ACU-ASSIN-AUX  TO WRK-TOTAL-PAG.

               MOVE WRK-CABEC4    TO REG-RELAT.
               WRITE REG-RELAT AFTER 1 LINE.

      *-----------------------------------------------------------------*
               CLOSE CLIENTES
                     RELAT.

               IF FS-CLIENTES                  NOT EQUAL '00'
                   MOVE WRK-ARQ-FECHADO        TO WRK-MSG-ERRO
                   MOVE FS-CLIENTES            TO WRK-STATUS-ERRO
                    MOVE WRK-CLIENTES          TO WRK-ARQUIVO-ERRO
                   PERFORM 9999-TRATA-ERRO

               END-IF.
               PERFORM 0260-IMP-ESTATISTICA.
               GOBACK.

      *-----------------------------------------------------------------*
       0300-FINALIZAR-FIM.EXIT.
      *-----------------------------------------------------------------*
       9000-MENSSAGEM-ERRO                     SECTION.

               DISPLAY WRK-MSG-ERRO.

       9000-MENSSAGEM-ERRO-FIM.EXIT.
      *-----------------------------------------------------------------*
       9999-TRATA-ERRO                         SECTION.
      *-----------------------------------------------------------------*

               DISPLAY "===== ERRO NO PROGRAMA ====="
               DISPLAY "MENSSAGEM....:"    WRK-DESCRICAO-ERRO.
               DISPLAY "FILE STATUS..:"    WRK-STATUS-ERRO.
               DISPLAY "AREA / SECAO.:"    WRK-AREA-ERRO.
               GOBACK.

       9999-TRATA-ERRO-FIM.EXIT.
      *-----------------------------------------------------------------*
